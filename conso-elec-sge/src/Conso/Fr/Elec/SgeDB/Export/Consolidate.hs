{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Conso.Fr.Elec.SgeDB.Export.Consolidate
Description : Consolidation et requêtes multi-PRM sur les bases SgeDB

Fournit trois fonctions pour travailler sur l'ensemble des bases PRM :

  * 'listAllPrmDbs'      — liste tous les fichiers @.db@ dans l'arborescence shardée
  * 'consolidateAllPrm'  — itère sur chaque base PRM, une connexion à la fois
  * 'attachAndQuery'     — attache jusqu'à ~125 bases via @ATTACH DATABASE@ et exécute une requête unique

__Limite SQLite__ : @ATTACH DATABASE@ est limité à ~125 bases simultanées.
Pour des volumes plus importants, utiliser 'consolidateAllPrm' qui ouvre
les connexions une à une sans cumuler les attaches.
-}
module Conso.Fr.Elec.SgeDB.Export.Consolidate
  ( attachAndQuery
  , consolidateAllPrm
  , listAllPrmDbs
  ) where

import           Database.SQLite.Simple
import qualified Data.Text              as T
import           Data.Maybe             (catMaybes)
import           System.FilePath        ((</>), takeBaseName)
import           System.Directory       (listDirectory)
import           Control.Exception      (try, SomeException)
import           Conso.Fr.Elec.SgeDB.Types.Common  (PrmId(..))
import           Conso.Fr.Elec.SgeDB.Storage.Connection (prmDbPath)

-- | Attache plusieurs bases PRM à une connexion @:memory:@ et exécute une requête.
-- Les bases sont attachées sous des alias @prm0@, @prm1@, …
-- La requête SQL doit référencer les tables avec ces alias (ex. @prm0.curve_points@).
--
-- __Limite SQLite__ : ~125 bases simultanées maximum.
attachAndQuery
  :: FromRow r
  => FilePath -- ^ Répertoire racine des bases SQLite
  -> [PrmId]  -- ^ Liste des PRM à attacher
  -> Query    -- ^ Requête SQL utilisant les alias @prm0@, @prm1@, …
  -> IO [r]
attachAndQuery baseDir prms q = do
  mainConn <- open ":memory:"
  configurePragmas mainConn
  -- Attacher chaque base PRM
  mapM_ (\(i, prm) -> do
    let dbPath = prmDbPath baseDir prm
        alias  = "prm" ++ show (i :: Int)
    execute_ mainConn
      (Query $ "ATTACH DATABASE '" <> T.pack dbPath <> "' AS " <> T.pack alias)
    ) (zip [0..] prms)
  result <- query_ mainConn q
  close mainConn
  return result
  where
    configurePragmas conn = do
      execute_ conn "PRAGMA foreign_keys = ON"
      execute_ conn "PRAGMA busy_timeout = 5000"

-- | Itère sur toutes les bases PRM de l'arborescence et consolide les résultats.
-- Ouvre une connexion par base (sans les cumuler), tolère les erreurs individuelles
-- (une base inaccessible n'interrompt pas les autres).
consolidateAllPrm
  :: FilePath             -- ^ Répertoire racine des bases SQLite
  -> (Connection -> IO [a]) -- ^ Fonction extractrice appliquée à chaque connexion PRM
  -> IO [(PrmId, [a])]
consolidateAllPrm baseDir extractor = do
  dbPaths <- listAllPrmDbs baseDir
  results <- mapM processOne dbPaths
  return (catMaybes results)
  where
    processOne (prm, path) = do
      result <- try $ do
        conn <- open path
        execute_ conn "PRAGMA foreign_keys = ON"
        execute_ conn "PRAGMA busy_timeout = 5000"
        items <- extractor conn
        close conn
        return (prm, items)
      case result of
        Left  (_ex :: SomeException) -> return Nothing
        Right pair                   -> return (Just pair)

-- | Liste tous les fichiers @.db@ présents dans l'arborescence shardée 3 niveaux.
-- Chaque entrée associe le 'PrmId' (déduit du nom de fichier sans extension)
-- au chemin absolu de la base.
listAllPrmDbs :: FilePath -> IO [(PrmId, FilePath)]
listAllPrmDbs baseDir = do
  lvl1 <- safeListDir baseDir
  concat <$> mapM (\d1 -> do
    lvl2 <- safeListDir (baseDir </> d1)
    concat <$> mapM (\d2 -> do
      lvl3 <- safeListDir (baseDir </> d1 </> d2)
      concat <$> mapM (\d3 -> do
        files <- safeListDir (baseDir </> d1 </> d2 </> d3)
        return [ (PrmId (T.pack (takeBaseName f)),
                  baseDir </> d1 </> d2 </> d3 </> f)
               | f <- files, T.isSuffixOf ".db" (T.pack f) ]
        ) lvl3
      ) lvl2
    ) lvl1

safeListDir :: FilePath -> IO [FilePath]
safeListDir path = do
  result <- try (listDirectory path) :: IO (Either SomeException [FilePath])
  case result of
    Left  _ -> return []
    Right xs -> return xs
