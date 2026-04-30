{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (catch, SomeException, displayException)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Data.Maybe (fromMaybe)
import Options.Applicative
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Conso.Fr.Gaz.Adict.Adict (initSession)
import Conso.Fr.SiteDB.Registry (withRegistry)
import Conso.Fr.SiteDB.Registry.Operations (listSites)
import Conso.Fr.SiteDB.Orchestration.Types
import Conso.Fr.SiteDB.Orchestration.Prm (inscrirePrm)
import Conso.Fr.SiteDB.Orchestration.Pce (inscrirePce)
import Display (afficherResultat, afficherSites)


-- ---------------------------------------------------------------------------
-- Types de commandes

data GlobalOpts = GlobalOpts
  { optConfigDir :: Maybe FilePath
  , optProd      :: Bool
  , optVerbose   :: Bool
  , optCommand   :: Command
  }

data Command
  = CmdInscrirePrm InscriptionPrmParams (Maybe Rattachement')
  | CmdInscrirePce InscriptionPceParams (Maybe Rattachement')
  | CmdLister

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
      prod      = optProd opts
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
  <*> switch (long "prod" <> help "Utiliser les serveurs de production (défaut : homologation/sandbox)")
  <*> switch (long "verbose" <> short 'v' <> help "Afficher les détails des appels API")
  <*> subparser
    (  command "inscrire" (info inscrireParser (progDesc "Inscrire un PRM ou PCE"))
    <> command "lister"   (info (pure CmdLister) (progDesc "Lister les sites inscrits"))
    )

inscrireParser :: Parser Command
inscrireParser = subparser
  (  command "prm" (info prmParser (progDesc "Inscrire un PRM (Enedis SGE)"))
  <> command "pce" (info pceParser (progDesc "Inscrire un PCE (GRDF ADICT)"))
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

parseTypeFlux :: String -> Either String TypeFlux
parseTypeFlux "CDC"     = Right CDC
parseTypeFlux "IDX"     = Right IDX
parseTypeFlux "ENERGIE" = Right ENERGIE
parseTypeFlux "PMAX"    = Right PMAX
parseTypeFlux s         = Left $ "Type inconnu : " <> s <> " (valeurs : CDC, IDX, ENERGIE, PMAX)"
