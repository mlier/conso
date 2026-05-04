{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (catch, SomeException, displayException)
import Data.Maybe (fromMaybe)
import qualified Data.UUID as UUID
import Options.Applicative
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Conso.Fr.SiteDB.Types (SiteId(..))
import Conso.Fr.SiteDB.Registry (withRegistry)
import Conso.Fr.SiteDB.Registry.Operations (listSites)
import Conso.Fr.SiteDB.Orchestration.Desinscription
  ( DesinscriptionCallbacks(..), supprimerSite )

import Conso.Fr.Elec.SiteDB.Orchestration.Adresse (codePostalPrm, arreterServicesSge)
import Conso.Fr.Elec.SiteDB.Storage.Delete (deleteElecData)
import Conso.Fr.Elec.SiteDB.Cli
  ( ElecCommand(..), ElecCommandResult(..), ElecRattachement(..)
  , elecInscrirePrmParser, elecSupprimerPrmParser, elecIngererParser
  , runElecCommand )

import Conso.Fr.Gaz.SiteDB.Orchestration.Adresse (codePostalPce)
import Conso.Fr.Gaz.SiteDB.Storage.Delete (deleteGazData)
import Conso.Fr.Gaz.SiteDB.Cli
  ( GazCommand(..), GazCommandResult(..), initSession
  , gazInscrirePceParser, gazSupprimerPceParser, gazIngererParser, runGazCommand )

import Display
  ( afficherResultat, afficherSites, afficherDesinscription
  , afficherIngererGaz, afficherIngererElec )


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

runCommand configDir prod verbose siteDbDir (CmdSupprimerTout siteId) = do
  let callbacks = DesinscriptionCallbacks
        { cbDeleteElec  = deleteElecData
        , cbDeleteGaz   = deleteGazData
        , cbArreterSge  = arreterServicesSge verbose prod
        }
  withRegistry configDir $ \conn -> do
    result <- supprimerSite conn siteDbDir prod verbose callbacks siteId
    afficherDesinscription result


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
