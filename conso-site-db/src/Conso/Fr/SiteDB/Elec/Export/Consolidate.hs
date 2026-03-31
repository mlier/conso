{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Conso.Fr.SiteDB.Elec.Export.Consolidate
Description : Consolidation et requêtes multi-site sur les bases SiteDB

Fournit trois fonctions pour travailler sur l'ensemble des bases site :

  * 'listAllSiteDbs'      — liste tous les fichiers @{uuid}.db@ dans le répertoire plat
  * 'consolidateAllSites' — itère sur chaque base site, une connexion à la fois
  * 'attachAndQuery'      — attache jusqu'à ~125 bases via @ATTACH DATABASE@

__Limite SQLite__ : @ATTACH DATABASE@ est limité à ~125 bases simultanées.
Pour des volumes plus importants, utiliser 'consolidateAllSites' qui ouvre
les connexions une à une sans cumuler les attaches.
-}
module Conso.Fr.SiteDB.Elec.Export.Consolidate
  ( attachAndQuery
  , consolidateAllSites
  , listAllSiteDbs
  ) where

import           Database.SQLite.Simple
import qualified Data.Text              as T
import           Data.Maybe             (catMaybes)
import           System.FilePath        ((</>), takeBaseName)
import           System.Directory       (listDirectory)
import           Control.Exception      (try, SomeException)
import qualified Data.UUID              as UUID
import           Conso.Fr.SiteDB.Types                        (SiteId(..))
import           Conso.Fr.SiteDB.Elec.Storage.Connection (siteDbPath)

-- | Attache plusieurs bases site à une connexion @:memory:@ et exécute une requête.
-- Les bases sont attachées sous des alias @site0@, @site1@, …
-- La requête SQL doit référencer les tables avec ces alias (ex. @site0.curve_points@).
--
-- __Limite SQLite__ : ~125 bases simultanées maximum.
attachAndQuery
  :: FromRow r
  => FilePath  -- ^ Répertoire des bases SQLite site
  -> [SiteId]  -- ^ Liste des sites à attacher
  -> Query     -- ^ Requête SQL utilisant les alias @site0@, @site1@, …
  -> IO [r]
attachAndQuery siteDbDir sites q = do
  mainConn <- open ":memory:"
  configurePragmas mainConn
  mapM_ (\(i, siteId) -> do
    let dbPath = siteDbPath siteDbDir siteId
        alias  = "site" ++ show (i :: Int)
    execute_ mainConn
      (Query $ "ATTACH DATABASE '" <> T.pack dbPath <> "' AS " <> T.pack alias)
    ) (zip [0..] sites)
  result <- query_ mainConn q
  close mainConn
  return result
  where
    configurePragmas conn = do
      execute_ conn "PRAGMA foreign_keys = ON"
      execute_ conn "PRAGMA busy_timeout = 5000"

-- | Itère sur toutes les bases site du répertoire et consolide les résultats.
-- Ouvre une connexion par base (sans les cumuler), tolère les erreurs individuelles.
consolidateAllSites
  :: FilePath               -- ^ Répertoire des bases SQLite site
  -> (Connection -> IO [a]) -- ^ Fonction extractrice appliquée à chaque connexion
  -> IO [(SiteId, [a])]
consolidateAllSites siteDbDir extractor = do
  dbPaths <- listAllSiteDbs siteDbDir
  results <- mapM processOne dbPaths
  return (catMaybes results)
  where
    processOne (siteId, path) = do
      result <- try $ do
        conn <- open path
        execute_ conn "PRAGMA foreign_keys = ON"
        execute_ conn "PRAGMA busy_timeout = 5000"
        items <- extractor conn
        close conn
        return (siteId, items)
      case result of
        Left  (_ex :: SomeException) -> return Nothing
        Right pair                   -> return (Just pair)

-- | Liste tous les fichiers @{uuid}.db@ présents dans le répertoire plat.
-- Chaque entrée associe le 'SiteId' (UUID déduit du nom de fichier) au chemin absolu.
-- Les fichiers dont le nom n'est pas un UUID valide sont ignorés.
listAllSiteDbs :: FilePath -> IO [(SiteId, FilePath)]
listAllSiteDbs siteDbDir = do
  entries <- safeListDir siteDbDir
  return
    [ (SiteId uuid, siteDbDir </> f)
    | f <- entries
    , ".db" `T.isSuffixOf` T.pack f
    , Just uuid <- [UUID.fromString (takeBaseName f)]
    ]

safeListDir :: FilePath -> IO [FilePath]
safeListDir path = do
  result <- try (listDirectory path) :: IO (Either SomeException [FilePath])
  case result of
    Left  _ -> return []
    Right xs -> return xs
