{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SiteDB.Cli
  ( ElecCommand(..)
  , ElecCommandResult(..)
  , ElecRattachement(..)
  , elecInscrirePrmParser
  , elecSupprimerPrmParser
  , elecIngererParser
  , runElecCommand
  ) where

import qualified Data.Text as T
import qualified Data.UUID as UUID
import Options.Applicative
import Database.SQLite.Simple (Connection)

import Conso.Fr.Elec.Sge.Rfiles.LoadRFiles (PostDownload(..), DayLimit(..))

import Conso.Fr.SiteDB.Types (SiteId(..), Prm(..))
import Conso.Fr.SiteDB.Orchestration.Types
import Conso.Fr.SiteDB.Orchestration.Desinscription
  ( DesinscriptionCallbacks(..), DesinscriptionResult, desinscrirePrm )

import Conso.Fr.Elec.SiteDB.Orchestration.Inscription (inscrirePrm)
import Conso.Fr.Elec.SiteDB.Orchestration.Adresse (arreterServicesSge)
import Conso.Fr.Elec.SiteDB.Orchestration.Ingerer
  ( IngererElecParams(..), IngererElecReport, ingererElec )
import Conso.Fr.Elec.SiteDB.Storage.Delete (deleteElecData)


data ElecRattachement
  = ERPce String Bool
  | ERSite String Bool
  deriving (Show)

data ElecCommand
  = ElecInscrirePrm InscriptionPrmParams (Maybe ElecRattachement)
  | ElecSupprimerPrm SiteId
  | ElecIngerer IngererElecParams

data ElecCommandResult
  = ElecInscrit    InscriptionResult
  | ElecDesinscrit DesinscriptionResult
  | ElecIngere     IngererElecReport


runElecCommand :: Connection -> FilePath -> FilePath -> Bool -> Bool
               -> Maybe GetCodePostal
               -> ElecCommand
               -> IO ElecCommandResult
runElecCommand conn _configDir _siteDbDir prod verbose mGetCpPce (ElecInscrirePrm params mRatt) = do
  let rattachement = resolveRattPrm mRatt
      params' = params { ippRattachement = rattachement }
  result <- inscrirePrm conn prod verbose mGetCpPce params'
  return (ElecInscrit result)
runElecCommand conn _configDir siteDbDir prod verbose _mGetCpPce (ElecSupprimerPrm siteId) = do
  let callbacks = DesinscriptionCallbacks
        { cbDeleteElec  = deleteElecData
        , cbDeleteGaz   = \_ -> return ()
        , cbArreterSge  = arreterServicesSge verbose prod
        }
  result <- desinscrirePrm conn siteDbDir prod verbose callbacks siteId
  return (ElecDesinscrit result)
runElecCommand conn configDir siteDbDir _prod _verbose _mGetCpPce (ElecIngerer params) = do
  report <- ingererElec conn configDir siteDbDir params
  return (ElecIngere report)


resolveRattPrm :: Maybe ElecRattachement -> Rattachement
resolveRattPrm Nothing              = Standalone
resolveRattPrm (Just (ERPce p f))  = ParPce (T.pack p) f
resolveRattPrm (Just (ERSite u f)) = case UUID.fromString u of
  Just uuid -> ParSite uuid f
  Nothing   -> error $ "UUID invalide : " <> u


elecInscrirePrmParser :: Parser ElecCommand
elecInscrirePrmParser = ElecInscrirePrm
  <$> (InscriptionPrmParams
        <$> (T.pack <$> argument str (metavar "PRM" <> help "Identifiant PRM (14 chiffres)"))
        <*> some (option (eitherReader parseTypeFlux)
              (long "type" <> short 't' <> metavar "CDC|IDX|ENERGIE|PMAX"
               <> help "Type de flux (répétable, au moins un)"))
        <*> accordParser
        <*> pure Standalone)
  <*> optional rattachementPrmParser

elecSupprimerPrmParser :: Parser ElecCommand
elecSupprimerPrmParser = ElecSupprimerPrm <$> uuidArg "UUID du site dont le PRM doit être supprimé"

elecIngererParser :: Parser ElecCommand
elecIngererParser = ElecIngerer <$>
  (IngererElecParams
    <$> optional (some (Prm . T.pack <$>
          strOption (long "prm" <> metavar "PRM"
                     <> help "Filtrer sur ce PRM (répétable, défaut : tous)")))
    <*> dayLimitParser
    <*> postDownloadParser)

dayLimitParser :: Parser DayLimit
dayLimitParser =
  (Days <$> option auto (long "jours" <> metavar "N" <> help "Ne traiter que les fichiers des N derniers jours"))
  <|> pure AllDays

postDownloadParser :: Parser PostDownload
postDownloadParser =
  flag' Archive (long "archive" <> help "Archiver les fichiers sur le serveur après téléchargement")
  <|> flag' Remove (long "supprimer" <> help "Supprimer les fichiers du serveur après téléchargement")
  <|> pure Keep


accordParser :: Parser Accord
accordParser =
  (AccordNom . T.pack <$> strOption (long "nom" <> metavar "NOM" <> help "Nom du titulaire (personne physique)"))
  <|>
  (AccordDenomination . T.pack <$> strOption (long "denomination" <> metavar "DENOMINATION" <> help "Dénomination sociale (personne morale)"))

rattachementPrmParser :: Parser ElecRattachement
rattachementPrmParser =
  (ERPce <$> strOption (long "pce" <> metavar "PCE" <> help "Rattacher au site du PCE existant") <*> forceFlag)
  <|>
  (ERSite <$> strOption (long "site" <> metavar "UUID" <> help "Rattacher au site par UUID") <*> forceFlag)

forceFlag :: Parser Bool
forceFlag = switch (long "force" <> help "Ignorer la vérification de code postal")

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
