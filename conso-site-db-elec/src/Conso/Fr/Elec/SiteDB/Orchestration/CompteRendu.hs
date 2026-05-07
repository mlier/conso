{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SiteDB.Orchestration.CompteRendu
  ( CrResult(..)
  , processCrDirectory
  ) where

import           Control.Exception          (try, SomeException)
import           Data.Aeson                 (decode, Value, (.:), (.:?), withObject)
import           Data.Aeson.Types           (parseMaybe, Parser)
import           Data.Maybe                 (fromMaybe, catMaybes)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import           Data.Char                  (toLower)
import           Data.List                  (isInfixOf, isSuffixOf)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time
import           Database.SQLite.Simple
import           System.Directory           (listDirectory, doesDirectoryExist)
import           System.FilePath            ((</>), takeFileName)

data CrResult = CrResult
  { crAffaireId :: Text
  , crFichier   :: FilePath
  , crStatut    :: Text     -- "ACCEPTE" | "NON_PUBLIE" | "NON_RECEVABLE" | "INCONNU"
  , crMessage   :: Text
  } deriving (Show)

-- | Parcourt le répertoire local des rfiles, identifie les CR déchiffrés,
-- met à jour elec_backfill_log de la connexion fournie pour les affaire_id trouvés.
processCrDirectory :: FilePath -> Connection -> IO [CrResult]
processCrDirectory dir conn = do
  crFiles <- collectCrFiles dir
  results <- mapM (processCrFile conn) crFiles
  return $ catMaybes results

collectCrFiles :: FilePath -> IO [FilePath]
collectCrFiles dir = do
  exist <- doesDirectoryExist dir
  if not exist
    then return []
    else do
      entries <- listDirectory dir
      concat <$> mapM (processEntry dir) entries
  where
    processEntry d name = do
      let path = d </> name
      isDir <- doesDirectoryExist path
      if isDir
        then collectCrFiles path
        else return ([path | "_CR_" `isInfixOf` name && ".json" `isSuffixOf` map toLower name])

processCrFile :: Connection -> FilePath -> IO (Maybe CrResult)
processCrFile conn path = do
  case extractAffaireId (takeFileName path) of
    Nothing    -> return Nothing
    Just affId -> do
      rows <- query conn
        "SELECT COUNT(*) FROM elec_backfill_log WHERE affaire_id = ? AND statut_cr IS NULL"
        (Only affId) :: IO [Only Int]
      let pending = case rows of { [Only n] -> n > 0; _ -> False }
      if not pending
        then return Nothing
        else do
          res <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
          case res of
            Left  _  -> return Nothing
            Right bs -> do
              let (statut, msg) = parseStatutCr bs
              now <- getCurrentTime
              let nowStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now
              execute conn
                "UPDATE elec_backfill_log SET statut_cr = ?, date_cr = ? \
                \ WHERE affaire_id = ? AND statut_cr IS NULL"
                (statut, nowStr, affId)
              return $ Just (CrResult affId path statut msg)

-- | Extrait l'affaire_id du nom de fichier CR.
-- Format attendu : ENEDIS_R66_CR_P_PMax_M0A7P3G5_20260507075900.json
-- Renvoie le segment situé après CR_P_<TypeNom>_ (ici "M0A7P3G5").
extractAffaireId :: FilePath -> Maybe Text
extractAffaireId name =
  let parts = T.splitOn "_" (T.pack name)
  in case dropWhile (/= "CR") parts of
       (_:"P":_:affId:_) -> Just affId
       _                 -> Nothing

parseStatutCr :: BS.ByteString -> (Text, Text)
parseStatutCr bs =
  case decode (LBS.fromStrict bs) :: Maybe Value of
    Nothing -> ("INCONNU", "JSON invalide ou données non déchiffrées")
    Just v  -> fromMaybe ("INCONNU", "Structure JSON inattendue") (parseMaybe parseCr v)

parseCr :: Value -> Parser (Text, Text)
parseCr = withObject "cr" $ \o -> do
  demandes <- o .:  "prmDemandes"    :: Parser [Value]
  fichiers <- fromMaybe [] <$> (o .:? "fichiersPublies" :: Parser (Maybe [Value]))
  mNomFichier <- case fichiers of
    (f:_) -> withObject "f" (.:? "nomFichier") f
    []    -> return Nothing
  case demandes of
    [] -> return ("INCONNU", "Aucun PRM dans prmDemandes")
    (d:_) -> withObject "prm" (\dm -> do
               recv  <- dm .:? "recevabilite" :: Parser (Maybe Text)
               pub   <- dm .:? "publication"  :: Parser (Maybe Text)
               motif <- dm .:? "motif"        :: Parser (Maybe Text)
               return $ case recv of
                 Just "NON_RECEVABLE" ->
                   ("NON_RECEVABLE", fromMaybe "Demande non recevable" motif)
                 _ -> case pub of
                   Just "PUBLIE"     -> ("ACCEPTE",    fromMaybe "Données publiées"     mNomFichier)
                   Just "NON_PUBLIE" -> ("NON_PUBLIE", fromMaybe "Données non publiées" motif)
                   Just other        -> ("INCONNU",    other)
                   Nothing           -> ("INCONNU",    "Champ publication absent")
             ) d
