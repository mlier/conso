{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.SiteDB.Cli
  ( GazCommand(..)
  , GazCommandResult(..)
  , GazRattachement(..)
  , gazInscrirePceParser
  , gazSupprimerPceParser
  , gazIngererParser
  , runGazCommand
  , AdictSession
  , initSession
  ) where

import qualified Data.Text as T
import qualified Data.UUID as UUID
import Options.Applicative
import Database.SQLite.Simple (Connection)

import Conso.Fr.Gaz.Adict.Adict (AdictSession, initSession)

import Conso.Fr.SiteDB.Types (SiteId(..), Pce(..))
import Conso.Fr.SiteDB.Orchestration.Types
import Conso.Fr.SiteDB.Orchestration.Desinscription
  ( DesinscriptionCallbacks(..), DesinscriptionResult, desinscrirePce )

import Conso.Fr.Gaz.SiteDB.Orchestration.Inscription (inscrirePce)
import Conso.Fr.Gaz.SiteDB.Orchestration.Ingerer
  ( IngererGazParams(..), IngererGazReport, ingererGaz )
import Conso.Fr.Gaz.SiteDB.Storage.Delete (deleteGazData)


data GazRattachement
  = GRPrm String Bool
  | GRSite String Bool
  deriving (Show)

data GazCommand
  = GazInscrirePce InscriptionPceParams (Maybe GazRattachement)
  | GazSupprimerPce SiteId
  | GazIngerer IngererGazParams

data GazCommandResult
  = GazInscrit InscriptionResult
  | GazDesinscrit DesinscriptionResult
  | GazIngere IngererGazReport


runGazCommand :: Connection -> FilePath -> Bool -> Bool
              -> AdictSession
              -> Maybe GetCodePostal
              -> GazCommand
              -> IO GazCommandResult
runGazCommand conn _siteDbDir prod verbose session mGetCpPrm (GazInscrirePce params mRatt) = do
  let rattachement = resolveRattPce mRatt
      params' = params { ipeRattachement = rattachement }
  result <- inscrirePce conn prod verbose session mGetCpPrm params'
  return (GazInscrit result)
runGazCommand conn siteDbDir _prod _verbose _session _mGetCpPrm (GazSupprimerPce siteId) = do
  let callbacks = DesinscriptionCallbacks
        { cbDeleteElec  = \_ -> return ()
        , cbDeleteGaz   = deleteGazData
        , cbArreterSge  = \_ -> return []
        }
  result <- desinscrirePce conn siteDbDir callbacks siteId
  return (GazDesinscrit result)
runGazCommand conn siteDbDir _prod _verbose session _mGetCpPrm (GazIngerer params) = do
  report <- ingererGaz conn session siteDbDir params
  return (GazIngere report)


resolveRattPce :: Maybe GazRattachement -> Rattachement
resolveRattPce Nothing              = Standalone
resolveRattPce (Just (GRPrm p f))  = ParPrm (T.pack p) f
resolveRattPce (Just (GRSite u f)) = case UUID.fromString u of
  Just uuid -> ParSite uuid f
  Nothing   -> error $ "UUID invalide : " <> u


gazInscrirePceParser :: Parser GazCommand
gazInscrirePceParser = GazInscrirePce                                                                                                                                                 
  <$> (InscriptionPceParams . T.pack                  
        <$> argument str (metavar "PCE" <> help "Identifiant PCE (14 chiffres)")
        <*> (T.pack <$> strOption (long "cp" <> metavar "CODE_POSTAL" <> help "Code postal du site"))                                                                                 
        <*> optional (T.pack <$> strOption (long "email" <> metavar "EMAIL" <> help "Courriel du titulaire"))                                                                         
        <*> accordParser                                                                                                                                                              
        <*> pure Standalone                                                                                                                                                           
        <*> switch (long "avec-injections" <> help "Inclure le périmètre données d'injection dans le droit d'accès GRDF"))
  <*> optional rattachementPceParser 

gazSupprimerPceParser :: Parser GazCommand
gazSupprimerPceParser = GazSupprimerPce <$> uuidArg "UUID du site dont le PCE doit être supprimé"

gazIngererParser :: Parser GazCommand
gazIngererParser = GazIngerer . IngererGazParams <$>
  optional (some (Pce . T.pack <$>
    strOption (long "pce" <> metavar "PCE"
               <> help "Filtrer sur ce PCE (répétable, défaut : tous)")))



accordParser :: Parser Accord
accordParser =
  (AccordNom . T.pack <$> strOption (long "nom" <> metavar "NOM" <> help "Nom du titulaire (personne physique)"))
  <|>
  (AccordDenomination . T.pack <$> strOption (long "denomination" <> metavar "DENOMINATION" <> help "Dénomination sociale (personne morale)"))

rattachementPceParser :: Parser GazRattachement
rattachementPceParser =
  (GRPrm <$> strOption (long "prm" <> metavar "PRM" <> help "Rattacher au site du PRM existant") <*> forceFlag)
  <|>
  (GRSite <$> strOption (long "site" <> metavar "UUID" <> help "Rattacher au site par UUID") <*> forceFlag)

forceFlag :: Parser Bool
forceFlag = switch (long "force" <> help "Ignorer la vérification de code postal")

uuidArg :: String -> Parser SiteId
uuidArg h = argument (eitherReader parseUUID) (metavar "UUID" <> help h)
  where
    parseUUID s = case UUID.fromString s of
      Just u  -> Right (SiteId u)
      Nothing -> Left $ "UUID invalide : " <> s
