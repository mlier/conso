{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Site.SiteDB.Elec.Ingestion.FromRfiles
Description : Ingestion en lot des fichiers JSON déchiffrés par manage-rfiles

Pipeline principal d'alimentation de la base élec à partir des fichiers R
déposés par Enedis et déchiffrés par l'exécutable @manage-rfiles@ :

@
Dossier local (JSON déchiffrés) → detectCodeFlux → Batch.ingestFile
  → Registry.lookupOrCreateByPrm → openSiteDb → INSERT tables elec_*
@

Usage typique :

> results <- ingestDirectory "~\/.conso" "~\/.conso\/sites" "~\/.conso\/rfiles"
> mapM_ print results

Le 'CodeFlux' est détecté automatiquement :

  1. Depuis le champ @header.codeFlux@ dans le JSON (cas M023 standard)
  2. En recherchant un code connu (@R63@, @R64@, …, @C68@) dans le nom du fichier
-}
module Conso.Fr.Site.SiteDB.Elec.Ingestion.FromRfiles
  ( ingestDirectory
  , ingestJsonFile
  , detectCodeFlux
  , IngestDirResult(..)
  ) where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Aeson                 (decode, (.:?), withObject)
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Types           (parseMaybe)
import qualified Data.ByteString.Lazy       as LBS
import           System.FilePath            ((</>), takeFileName)
import           System.Directory           (listDirectory)
import           Control.Exception          (try, SomeException)

import           Conso.Fr.Site.Types                               (Prm(..))
import           Conso.Fr.Site.Registry                            (openRegistry, lookupOrCreateByPrm)
import           Conso.Fr.Site.SiteDB.Elec.Storage.Connection      (openSiteDb)
import           Conso.Fr.Site.SiteDB.Elec.Types.Common            (PrmId(..))
import           Conso.Fr.Site.SiteDB.Elec.Types.Header            (CodeFlux, codeFluxFromText)
import           Conso.Fr.Site.SiteDB.Elec.Ingestion.Batch         (ingestFile, IngestResult(..))

-- | Résultat de l'ingestion d'un fichier entier (contient 1 résultat par PRM trouvé).
data IngestDirResult
  = FileOk   FilePath [IngestResult] -- ^ Fichier traité, résultats par PRM
  | FileSkip FilePath Text           -- ^ Fichier ignoré (CodeFlux non détecté)
  | FileErr  FilePath Text           -- ^ Erreur de lecture / parsing
  deriving (Show)

-- | Ingère tous les fichiers @.json@ présents dans @inputDir@.
-- Parcourt les fichiers (non récursif), détecte le CodeFlux de chacun,
-- et appelle 'ingestFile' via le registre central pour trouver/créer le site.
ingestDirectory
  :: FilePath  -- ^ Répertoire de configuration (contient @registry.db@)
  -> FilePath  -- ^ Répertoire des bases SQLite site (@{uuid}.db@)
  -> FilePath  -- ^ Répertoire contenant les fichiers JSON déchiffrés
  -> IO [IngestDirResult]
ingestDirectory configDir siteDbDir inputDir = do
  entries <- listDirectory inputDir
  let jsonFiles = filter (T.isSuffixOf ".json" . T.pack) entries
  mapM (ingestJsonFile configDir siteDbDir inputDir) jsonFiles

-- | Ingère un fichier JSON unique.
-- Détecte le CodeFlux, ouvre/crée les bases nécessaires via le registre.
ingestJsonFile
  :: FilePath  -- ^ Répertoire de configuration
  -> FilePath  -- ^ Répertoire des bases SQLite site
  -> FilePath  -- ^ Répertoire contenant le fichier
  -> FilePath  -- ^ Nom du fichier (sans répertoire)
  -> IO IngestDirResult
ingestJsonFile configDir siteDbDir inputDir fileName = do
  let path = inputDir </> fileName
  result <- try (BS.readFile path) :: IO (Either SomeException ByteString)
  case result of
    Left ex -> return $ FileErr path (T.pack (show ex))
    Right bs ->
      case detectCodeFlux bs fileName of
        Nothing -> return $ FileSkip path
          ("CodeFlux non reconnu dans le fichier ni dans le nom : " <> T.pack fileName)
        Just cf -> do
          let openConn (PrmId prmText) = do
                reg     <- openRegistry configDir
                siteId  <- lookupOrCreateByPrm reg (Prm prmText)
                openSiteDb siteDbDir siteId
          results <- ingestFile openConn cf (Just (T.pack (takeFileName path))) bs
          return $ FileOk path results

-- | Détecte le 'CodeFlux' d'un fichier JSON M023 :
--
--   1. Tente de parser @header.codeFlux@ dans le JSON
--   2. Si absent ou non reconnu, cherche un code connu dans le nom du fichier
--
-- Retourne 'Nothing' si aucune méthode ne réussit.
detectCodeFlux :: ByteString -> FilePath -> Maybe CodeFlux
detectCodeFlux bs fileName =
  fromJsonHeader bs <|> fromFileName fileName
  where
    fromJsonHeader :: ByteString -> Maybe CodeFlux
    fromJsonHeader content = do
      obj  <- decode (LBS.fromStrict content) :: Maybe Aeson.Value
      code <- parseMaybe (withObject "root" $ \o ->
        o .:? "codeFlux") obj
      code >>= codeFluxFromText

    fromFileName :: FilePath -> Maybe CodeFlux
    fromFileName name =
      let candidates = ["R63B","R63A","R64B","R64A","R66B","R63","R64","R65","R66","R67","C68"]
          nameUpper  = T.toUpper (T.pack name)
      in foldr (\c acc -> case acc of
                  Just _  -> acc
                  Nothing -> if c `T.isInfixOf` nameUpper
                             then codeFluxFromText c
                             else Nothing) Nothing candidates

    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing <|> b = b
    a       <|> _ = a
