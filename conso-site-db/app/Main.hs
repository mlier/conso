{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (catch, try, SomeException, displayException)
import Database.SQLite.Simple (Connection, execute_)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Data.Maybe (fromMaybe)
import Options.Applicative
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Conso.Fr.Gaz.Adict.Adict (initSession)
import Conso.Fr.SiteDB.Registry (withRegistry)
import Conso.Fr.SiteDB.Registry.Operations (listSites)
import Conso.Fr.SiteDB.Types (SiteId(..))
import Conso.Fr.SiteDB.Orchestration.Types
import Conso.Fr.SiteDB.Orchestration.Prm (inscrirePrm)
import Conso.Fr.SiteDB.Orchestration.Pce (inscrirePce)
import Conso.Fr.SiteDB.Orchestration.Desinscription
  ( DesinscriptionCallbacks(..), desinscrirePrm, desinscrirePce, supprimerSite )
import Display (afficherResultat, afficherSites, afficherDesinscription)


-- ---------------------------------------------------------------------------
-- Types de commandes

data GlobalOpts = GlobalOpts
  { optConfigDir :: Maybe FilePath
  , optSandbox   :: Bool
  , optVerbose   :: Bool
  , optCommand   :: Command
  }

data Command
  = CmdInscrirePrm InscriptionPrmParams (Maybe Rattachement')
  | CmdInscrirePce InscriptionPceParams (Maybe Rattachement')
  | CmdLister
  | CmdSupprimerPrm  SiteId
  | CmdSupprimerPce  SiteId
  | CmdSupprimerTout SiteId

data Rattachement'
  = RPce String Bool
  | RPrm String Bool
  | RSite String Bool
  deriving (Show)


-- ---------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  opts <- execParser (info (globalParser <**> helper) (progDesc "Registre des sites de consommation"))
  home <- getHomeDirectory
  let configDir = fromMaybe (home </> ".conso") (optConfigDir opts)
      prod      = not (optSandbox opts)
      verbose   = optVerbose opts
  runCommand configDir prod verbose (optCommand opts)
    `catch` \e -> putStrLn $ "Erreur : " <> displayException (e :: SomeException)


runCommand :: FilePath -> Bool -> Bool -> Command -> IO ()
runCommand configDir prod verbose (CmdInscrirePrm params mRatt) = do
  let rattachement = resolveRattPrm mRatt
      params' = params { ippRattachement = rattachement }
  mSession <- case rattachement of
    ParPce _ _ -> Just <$> initSession prod False False
    ParSite _ _ -> Just <$> initSession prod False False
    _           -> return Nothing
  withRegistry configDir $ \conn -> do
    result <- inscrirePrm conn prod verbose mSession params'
    afficherResultat result

runCommand configDir prod verbose (CmdInscrirePce params mRatt) = do
  let rattachement = resolveRattPce mRatt
      params' = params { ipeRattachement = rattachement }
  session <- initSession prod False False
  withRegistry configDir $ \conn -> do
    result <- inscrirePce conn prod verbose session params'
    afficherResultat result

runCommand configDir _ _ CmdLister =
  withRegistry configDir $ \conn -> do
    sites <- listSites conn
    afficherSites sites

runCommand configDir prod verbose (CmdSupprimerPrm siteId) = do
  let siteDbDir = configDir </> "sites"
      callbacks = DesinscriptionCallbacks deleteElecData deleteGazData
  withRegistry configDir $ \conn -> do
    result <- desinscrirePrm conn siteDbDir prod verbose callbacks siteId
    afficherDesinscription result

runCommand configDir _ _ (CmdSupprimerPce siteId) = do
  let siteDbDir = configDir </> "sites"
      callbacks = DesinscriptionCallbacks deleteElecData deleteGazData
  withRegistry configDir $ \conn -> do
    result <- desinscrirePce conn siteDbDir callbacks siteId
    afficherDesinscription result

runCommand configDir prod verbose (CmdSupprimerTout siteId) =
  withRegistry configDir $ \conn -> do
    let siteDbDir = configDir </> "sites"
    result <- supprimerSite conn siteDbDir prod verbose siteId
    afficherDesinscription result


-- Les fonctions de suppression sont définies ici car l'exécutable ne peut pas importer
-- conso-site-db-elec/conso-site-db-gaz sans créer un cycle de dépendances (ces packages
-- dépendent de conso-site-db). Les modules Delete.hs dans les extensions sont la référence
-- autoritaire pour les futurs consommateurs (ex. API web).
deleteElecData :: Connection -> IO ()
deleteElecData conn = mapM_ del
  ["ingestion_log", "curve_points", "index_values", "daily_energy", "daily_pmax", "billing_measures", "prm_info"]
  where del t = (try (execute_ conn ("DELETE FROM " <> t)) :: IO (Either SomeException ())) >> return ()

deleteGazData :: Connection -> IO ()
deleteGazData conn = mapM_ del
  ["gaz_ingestion_log", "gaz_consos", "gaz_injections", "gaz_infos_contractuelles", "gaz_infos_techniques"]
  where del t = (try (execute_ conn ("DELETE FROM " <> t)) :: IO (Either SomeException ())) >> return ()


resolveRattPrm :: Maybe Rattachement' -> Rattachement
resolveRattPrm Nothing              = Standalone
resolveRattPrm (Just (RPce p f))   = ParPce (T.pack p) f
resolveRattPrm (Just (RSite u f))  = case UUID.fromString u of
  Just uuid -> ParSite uuid f
  Nothing   -> error $ "UUID invalide : " <> u
resolveRattPrm (Just (RPrm _ _))   = Standalone

resolveRattPce :: Maybe Rattachement' -> Rattachement
resolveRattPce Nothing              = Standalone
resolveRattPce (Just (RPrm p f))   = ParPrm (T.pack p) f
resolveRattPce (Just (RSite u f))  = case UUID.fromString u of
  Just uuid -> ParSite uuid f
  Nothing   -> error $ "UUID invalide : " <> u
resolveRattPce (Just (RPce _ _))   = Standalone


-- ---------------------------------------------------------------------------
-- Parsers optparse-applicative

globalParser :: Parser GlobalOpts
globalParser = GlobalOpts
  <$> optional (strOption (long "config-dir" <> metavar "DIR" <> help "Répertoire de config (défaut : ~/.conso)"))
  <*> switch (long "sandbox" <> help "Utiliser les serveurs sandbox/homologation (défaut : production)")
  <*> switch (long "verbose" <> short 'v' <> help "Afficher les détails des appels API")
  <*> subparser
    (  command "lister"    (info (pure CmdLister <**> helper) (progDesc "Lister les sites inscrits"))
    <> command "inscrire"  (info (inscrireParser  <**> helper) (progDesc "Inscrire un PRM ou PCE"))
    <> command "supprimer" (info (supprimerParser <**> helper) (progDesc "Supprimer un PRM, PCE ou site"))
    )

inscrireParser :: Parser Command
inscrireParser = subparser
  (  command "prm" (info (prmParser <**> helper) (progDesc "Inscrire un PRM (Enedis SGE)"))
  <> command "pce" (info (pceParser <**> helper) (progDesc "Inscrire un PCE (GRDF ADICT)"))
  )

prmParser :: Parser Command
prmParser = CmdInscrirePrm
  <$> (InscriptionPrmParams
        <$> (T.pack <$> argument str (metavar "PRM" <> help "Identifiant PRM (14 chiffres)"))
        <*> some (option (eitherReader parseTypeFlux)
              (long "type" <> short 't' <> metavar "CDC|IDX|ENERGIE|PMAX"
               <> help "Type de flux (répétable, au moins un)"))
        <*> accordParser
        <*> pure Standalone)
  <*> optional rattachementPrmParser

pceParser :: Parser Command
pceParser = CmdInscrirePce
  <$> (InscriptionPceParams
        <$> (T.pack <$> argument str (metavar "PCE" <> help "Identifiant PCE (14 chiffres)"))
        <*> (T.pack <$> strOption (long "cp" <> metavar "CODE_POSTAL" <> help "Code postal du site"))
        <*> optional (T.pack <$> strOption (long "email" <> metavar "EMAIL" <> help "Courriel du titulaire"))
        <*> accordParser
        <*> pure Standalone)
  <*> optional rattachementPceParser

accordParser :: Parser Accord
accordParser =
  (AccordNom . T.pack <$> strOption (long "nom" <> metavar "NOM" <> help "Nom du titulaire (personne physique)"))
  <|>
  (AccordDenomination . T.pack <$> strOption (long "denomination" <> metavar "DENOMINATION" <> help "Dénomination sociale (personne morale)"))

rattachementPrmParser :: Parser Rattachement'
rattachementPrmParser =
  (RPce <$> strOption (long "pce" <> metavar "PCE" <> help "Rattacher au site du PCE existant") <*> forceFlag)
  <|>
  (RSite <$> strOption (long "site" <> metavar "UUID" <> help "Rattacher au site par UUID") <*> forceFlag)

rattachementPceParser :: Parser Rattachement'
rattachementPceParser =
  (RPrm <$> strOption (long "prm" <> metavar "PRM" <> help "Rattacher au site du PRM existant") <*> forceFlag)
  <|>
  (RSite <$> strOption (long "site" <> metavar "UUID" <> help "Rattacher au site par UUID") <*> forceFlag)

forceFlag :: Parser Bool
forceFlag = switch (long "force" <> help "Ignorer la vérification de code postal")

supprimerParser :: Parser Command
supprimerParser = subparser
  (  command "prm"  (info (CmdSupprimerPrm  <$> uuidArg "UUID du site dont le PRM doit être supprimé" <**> helper) (progDesc "Arrêt SGE + suppression données élec"))
  <> command "pce"  (info (CmdSupprimerPce  <$> uuidArg "UUID du site dont le PCE doit être supprimé" <**> helper) (progDesc "Suppression données gaz"))
  <> command "tout" (info (CmdSupprimerTout <$> uuidArg "UUID du site à supprimer entièrement"         <**> helper) (progDesc "Suppression complète du site (arrêt SGE si PRM + suppression fichier .db)"))
  )

uuidArg :: String -> Parser SiteId
uuidArg h = argument (eitherReader parseUUID) (metavar "UUID" <> help h)
  where
    parseUUID s = case UUID.fromString s of
      Just u  -> Right (SiteId u)
      Nothing -> Left $ "UUID invalide : " <> s

parseTypeFlux :: String -> Either String TypeFlux
parseTypeFlux "CDC"     = Right CDC
parseTypeFlux "IDX"     = Right IDX
parseTypeFlux "ENERGIE" = Right ENERGIE
parseTypeFlux "PMAX"    = Right PMAX
parseTypeFlux s         = Left $ "Type inconnu : " <> s <> " (valeurs : CDC, IDX, ENERGIE, PMAX)"
