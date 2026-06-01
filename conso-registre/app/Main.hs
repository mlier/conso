{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (catch, SomeException, displayException, bracket)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Database.SQLite.Simple (close)
import Options.Applicative
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Conso.Fr.SiteDB.Types (SiteId(..))
import Conso.Fr.SiteDB.Registry (withRegistry)
import Conso.Fr.SiteDB.Registry.Operations (listSites)
import Conso.Fr.SiteDB.Orchestration.Desinscription
  ( DesinscriptionCallbacks(..), supprimerSite )

import Conso.Fr.Elec.SiteDB.Orchestration.Adresse (codePostalPrm, arreterServicesSge)
import Conso.Fr.Elec.SiteDB.Storage.Connection (openSiteDbElec)
import Conso.Fr.Elec.SiteDB.Storage.Delete (deleteElecData)
import Conso.Fr.Elec.SiteDB.Cli
  ( ElecCommand(..), ElecCommandResult(..), ElecRattachement(..)
  , elecInscrirePrmParser, elecSupprimerPrmParser, elecIngererParser
  , runElecCommand )
import qualified Conso.Fr.Elec.SiteDB.Analysis.Aggregate as ElecAgg

import Conso.Fr.Gaz.SiteDB.Orchestration.Adresse (codePostalPce)
import Conso.Fr.Gaz.SiteDB.Storage.Connection (openSiteDbGaz)
import Conso.Fr.Gaz.SiteDB.Storage.Delete (deleteGazData)
import Conso.Fr.Gaz.SiteDB.Cli
  ( GazCommand(..), GazCommandResult(..), initSession
  , gazInscrirePceParser, gazSupprimerPceParser, gazIngererParser, runGazCommand )
import qualified Conso.Fr.Gaz.SiteDB.Analysis.Aggregate as GazAgg

import Display
  ( afficherResultat, afficherSites, afficherDesinscription
  , afficherIngererGaz, afficherIngererElec )
import Display.Histogram (barChart)


-- ---------------------------------------------------------------------------
-- Types de commandes

data GlobalOpts = GlobalOpts
  { optConfigDir :: Maybe FilePath
  , optSandbox   :: Bool
  , optVerbose   :: Bool
  , optCommand   :: Command
  }

data Command
  = CmdLister
  | CmdElec ElecCommand
  | CmdGaz  GazCommand
  | CmdSupprimerTout SiteId
  | CmdAffiche AfficheCommand

data Periode = Par5Min | Par10Min | Par15Min | Par30Min | ParHeure
             | ParJour | ParSemaine | ParMois | ParAn

data ElecSousType = ElecEnergie | ElecCourbe | ElecPmax | ElecIndex
data GazSousType  = GazConso | GazConsoInfo | GazInjection

data AfficheCommand
  = AfficheElec ElecSousType SiteId Periode Text Text
  | AfficheGaz  GazSousType  SiteId Periode Text Text


-- ---------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  opts <- execParser (info (globalParser <**> helper) (progDesc "Registre des sites de consommation"))
  home <- getHomeDirectory
  let configDir = fromMaybe (home </> ".conso") (optConfigDir opts)
      prod      = not (optSandbox opts)
      verbose   = optVerbose opts
      siteDbDir = configDir </> "sites"
  runCommand configDir prod verbose siteDbDir (optCommand opts)
    `catch` \e -> putStrLn $ "Erreur : " <> displayException (e :: SomeException)


runCommand :: FilePath -> Bool -> Bool -> FilePath -> Command -> IO ()
runCommand configDir _ _ _ CmdLister =
  withRegistry configDir $ \conn -> do
    sites <- listSites conn
    afficherSites sites

runCommand configDir prod verbose siteDbDir (CmdElec cmd) = do
  mSession <- case cmd of
    ElecInscrirePrm _ (Just (ERPce _ _))  -> Just <$> initSession prod False False
    ElecInscrirePrm _ (Just (ERSite _ _)) -> Just <$> initSession prod False False
    _                                      -> return Nothing
  let mGetCpPce = fmap codePostalPce mSession
  withRegistry configDir $ \conn -> do
    result <- runElecCommand conn configDir siteDbDir prod verbose mGetCpPce cmd
    case result of
      ElecInscrit    r -> afficherResultat r
      ElecDesinscrit r -> afficherDesinscription r
      ElecIngere     r -> afficherIngererElec r

runCommand configDir prod verbose siteDbDir (CmdGaz cmd) = do
  session <- initSession prod False False
  let mGetCpPrm = Just (codePostalPrm verbose prod)
  withRegistry configDir $ \conn -> do
    result <- runGazCommand conn siteDbDir prod verbose session mGetCpPrm cmd
    case result of
      GazInscrit    r -> afficherResultat r
      GazDesinscrit r -> afficherDesinscription r
      GazIngere     r -> afficherIngererGaz r

runCommand _ _ _ siteDbDir (CmdAffiche cmd) =
  runAfficheCommand siteDbDir cmd

runCommand configDir prod verbose siteDbDir (CmdSupprimerTout siteId) = do
  let callbacks = DesinscriptionCallbacks
        { cbDeleteElec  = deleteElecData
        , cbDeleteGaz   = deleteGazData
        , cbArreterSge  = arreterServicesSge verbose prod
        }
  withRegistry configDir $ \conn -> do
    result <- supprimerSite conn siteDbDir prod verbose callbacks siteId
    afficherDesinscription result


runAfficheCommand :: FilePath -> AfficheCommand -> IO ()
runAfficheCommand siteDbDir (AfficheElec sous siteId periode deb fin) =
  bracket (openSiteDbElec siteDbDir siteId) close $ \conn -> do
    let p = toElecPeriode periode
    rows <- case sous of
      ElecEnergie -> ElecAgg.aggregateEnergy     conn "CONS" "EA" p deb fin
      ElecCourbe  -> do
        rowsBest <- ElecAgg.aggregateCurve conn "CONS" "PA" "BEST" p deb fin
        if null rowsBest
          then ElecAgg.aggregateCurve conn "CONS" "PA" "BRUT" p deb fin
          else return rowsBest
      ElecPmax    -> ElecAgg.aggregatePmax       conn "PMA" p deb fin
      ElecIndex   -> ElecAgg.aggregateIndexDelta conn "EA"  p deb fin
    let title = elecTitle sous <> " — " <> deb <> " → " <> fin
    let lbl = shortenLabel periode
    barChart title [(lbl (ElecAgg.agPeriode r), ElecAgg.agSomme r) | r <- rows]

runAfficheCommand siteDbDir (AfficheGaz sous siteId periode deb fin) =
  bracket (openSiteDbGaz siteDbDir siteId) close $ \conn -> do
    let p = toGazPeriode periode
    rows <- case sous of
      GazConso     -> GazAgg.aggregateGazConso     conn p deb fin
      GazConsoInfo -> GazAgg.aggregateGazConsoInfo  conn p deb fin
      GazInjection -> GazAgg.aggregateGazInjection  conn p deb fin
    let title = gazTitle sous <> " — " <> deb <> " → " <> fin
    let lbl = shortenLabel periode
    barChart title [(lbl (GazAgg.gazAgPeriode r), GazAgg.gazAgSomme r) | r <- rows]

toElecPeriode :: Periode -> ElecAgg.AggregationPeriod
toElecPeriode Par5Min    = ElecAgg.Par5Min
toElecPeriode Par10Min   = ElecAgg.Par10Min
toElecPeriode Par15Min   = ElecAgg.Par15Min
toElecPeriode Par30Min   = ElecAgg.Par30Min
toElecPeriode ParHeure   = ElecAgg.ParHeure
toElecPeriode ParJour    = ElecAgg.ParJour
toElecPeriode ParSemaine = ElecAgg.ParSemaine
toElecPeriode ParMois    = ElecAgg.ParMois
toElecPeriode ParAn      = ElecAgg.ParAn

toGazPeriode :: Periode -> GazAgg.AggregationPeriod
toGazPeriode Par5Min    = GazAgg.ParJour
toGazPeriode Par10Min   = GazAgg.ParJour
toGazPeriode Par15Min   = GazAgg.ParJour
toGazPeriode Par30Min   = GazAgg.ParJour
toGazPeriode ParHeure   = GazAgg.ParJour
toGazPeriode ParJour    = GazAgg.ParJour
toGazPeriode ParSemaine = GazAgg.ParMois
toGazPeriode ParMois    = GazAgg.ParMois
toGazPeriode ParAn      = GazAgg.ParAn

shortenLabel :: Periode -> Text -> Text
shortenLabel Par5Min  = T.takeEnd 5
shortenLabel Par10Min = T.takeEnd 5
shortenLabel Par15Min = T.takeEnd 5
shortenLabel Par30Min = T.takeEnd 5
shortenLabel _        = T.takeEnd 2

elecTitle :: ElecSousType -> Text
elecTitle ElecEnergie = "Énergie (Wh)"
elecTitle ElecCourbe  = "Courbe de charge (Wh)"
elecTitle ElecPmax    = "Puissance max (VA)"
elecTitle ElecIndex   = "Index — deltas (Wh)"

gazTitle :: GazSousType -> Text
gazTitle GazConso     = "Consommations gaz (kWh)"
gazTitle GazConsoInfo = "Consommations informatives (kWh)"
gazTitle GazInjection = "Injections gaz (kWh)"


-- ---------------------------------------------------------------------------
-- Parsers optparse-applicative

globalParser :: Parser GlobalOpts
globalParser = GlobalOpts
  <$> optional (strOption (long "config-dir" <> metavar "DIR" <> help "Répertoire de config (défaut : ~/.conso)"))
  <*> switch (long "sandbox" <> help "Utiliser les serveurs sandbox/homologation (défaut : production)")
  <*> switch (long "verbose" <> short 'v' <> help "Afficher les détails des appels API")
  <*> subparser
    (  command "lister"    (info (pure CmdLister <**> helper)   (progDesc "Lister les sites inscrits"))
    <> command "inscrire"  (info (inscrireParser  <**> helper)   (progDesc "Inscrire un PRM ou PCE"))
    <> command "supprimer" (info (supprimerParser <**> helper)   (progDesc "Supprimer un PRM, PCE ou site"))
    <> command "ingerer"   (info (ingererParser   <**> helper)   (progDesc "Ingérer les données depuis les APIs"))
    <> command "affiche"   (info (afficheParser   <**> helper)   (progDesc "Afficher des données sous forme de graphique"))
    )

inscrireParser :: Parser Command
inscrireParser = subparser
  (  command "prm" (info (CmdElec <$> elecInscrirePrmParser <**> helper) (progDesc "Inscrire un PRM (Enedis SGE)"))
  <> command "pce" (info (CmdGaz  <$> gazInscrirePceParser  <**> helper) (progDesc "Inscrire un PCE (GRDF ADICT)"))
  )

ingererParser :: Parser Command
ingererParser = subparser
  (  command "gaz"  (info (CmdGaz  <$> gazIngererParser  <**> helper)
       (progDesc "Ingérer les données GRDF ADICT pour tous les PCEs inscrits"))
  <> command "elec" (info (CmdElec <$> elecIngererParser <**> helper)
       (progDesc "Ingérer les données Enedis via SFTP (R6x/C68)"))
  )

supprimerParser :: Parser Command
supprimerParser = subparser
  (  command "prm"  (info (CmdElec <$> elecSupprimerPrmParser <**> helper) (progDesc "Arrêt SGE + suppression données élec"))
  <> command "pce"  (info (CmdGaz  <$> gazSupprimerPceParser  <**> helper) (progDesc "Suppression données gaz"))
  <> command "tout" (info (CmdSupprimerTout <$> uuidArg "UUID du site à supprimer entièrement" <**> helper) (progDesc "Suppression complète du site (arrêt SGE si PRM + suppression fichier .db)"))
  )

uuidArg :: String -> Parser SiteId
uuidArg h = argument (eitherReader parseUUID) (metavar "UUID" <> help h)
  where
    parseUUID s = case UUID.fromString s of
      Just u  -> Right (SiteId u)
      Nothing -> Left $ "UUID invalide : " <> s

afficheParser :: Parser Command
afficheParser = CmdAffiche <$> subparser
  (  command "elec" (info (elecAfficheParser <**> helper) (progDesc "Données électricité"))
  <> command "gaz"  (info (gazAfficheParser  <**> helper) (progDesc "Données gaz"))
  )

elecAfficheParser :: Parser AfficheCommand
elecAfficheParser = subparser
  (  command "energie" (info (elecSousParser ElecEnergie <**> helper) (progDesc "Énergies quotidiennes (Wh)"))
  <> command "courbe"  (info (elecSousParser ElecCourbe  <**> helper) (progDesc "Courbe de charge agrégée (Wh)"))
  <> command "pmax"    (info (elecSousParser ElecPmax    <**> helper) (progDesc "Puissance maximale (VA)"))
  <> command "index"   (info (elecSousParser ElecIndex   <**> helper) (progDesc "Index — delta entre relevés (Wh)"))
  )

elecSousParser :: ElecSousType -> Parser AfficheCommand
elecSousParser sous = AfficheElec sous
  <$> uuidArg "UUID du site"
  <*> periodeOption
  <*> dateOption "debut" "Date de début (YYYY-MM-DD)"
  <*> dateOption "fin"   "Date de fin (YYYY-MM-DD)"

gazAfficheParser :: Parser AfficheCommand
gazAfficheParser = subparser
  (  command "conso"      (info (gazSousParser GazConso     <**> helper) (progDesc "Consommations publiées (kWh)"))
  <> command "conso-info" (info (gazSousParser GazConsoInfo <**> helper) (progDesc "Consommations informatives (kWh)"))
  <> command "injection"  (info (gazSousParser GazInjection <**> helper) (progDesc "Injections (kWh)"))
  )

gazSousParser :: GazSousType -> Parser AfficheCommand
gazSousParser sous = AfficheGaz sous
  <$> uuidArg "UUID du site"
  <*> periodeOption
  <*> dateOption "debut" "Date de début (YYYY-MM-DD)"
  <*> dateOption "fin"   "Date de fin (YYYY-MM-DD)"

periodeOption :: Parser Periode
periodeOption = option (eitherReader parsePeriode)
  (  long "par"
  <> metavar "GRANULARITE"
  <> value ParMois
  <> help "Granularité : 5min, 10min, 15min, 30min, heure, jour, semaine, mois, an (défaut : mois)"
  )
  where
    parsePeriode "5min"    = Right Par5Min
    parsePeriode "10min"   = Right Par10Min
    parsePeriode "15min"   = Right Par15Min
    parsePeriode "30min"   = Right Par30Min
    parsePeriode "heure"   = Right ParHeure
    parsePeriode "jour"    = Right ParJour
    parsePeriode "semaine" = Right ParSemaine
    parsePeriode "mois"    = Right ParMois
    parsePeriode "an"      = Right ParAn
    parsePeriode s         = Left $ "Granularité invalide : " <> s <> " (5min|10min|15min|30min|heure|jour|semaine|mois|an)"

dateOption :: String -> String -> Parser Text
dateOption l h = strOption (long l <> metavar "DATE" <> help h)
