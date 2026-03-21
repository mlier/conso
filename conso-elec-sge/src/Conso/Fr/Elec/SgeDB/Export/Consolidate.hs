{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Conso.Fr.Elec.SgeDB.Export.Consolidate
  ( attachAndQuery
  , consolidateAllPrm
  , listAllPrmDbs
  ) where

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow (FromRow)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           System.FilePath        ((</>), takeBaseName)
import           System.Directory       (doesFileExist, listDirectory)
import           Control.Exception      (try, SomeException)
import           Conso.Fr.Elec.SgeDB.Types.Common  (PrmId(..))
import           Conso.Fr.Elec.SgeDB.Storage.Connection (prmDbPath, openPrmDb)

-- | Attache plusieurs bases PRM à une connexion principale et exécute une requête.
-- Les bases sont attachées sous des alias "prm0", "prm1", …
-- Limite pratique : ~125 bases simultanées.
attachAndQuery
  :: FromRow r
  => FilePath    -- répertoire des bases
  -> [PrmId]     -- PRM à attacher
  -> Query       -- requête SQL (utilise les alias prm0, prm1, ...)
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

-- | Itère sur toutes les bases PRM trouvées dans le répertoire et consolide les résultats.
consolidateAllPrm
  :: FilePath                    -- répertoire des bases
  -> (Connection -> IO [a])      -- extracteur par connexion PRM
  -> IO [(PrmId, [a])]
consolidateAllPrm baseDir extractor = do
  dbPaths <- listAllPrmDbs baseDir
  results <- mapM processOne dbPaths
  return [r | Just r <- results]
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

-- | Liste tous les fichiers .db présents dans l'arborescence shardée.
listAllPrmDbs :: FilePath -> IO [(PrmId, FilePath)]
listAllPrmDbs baseDir = do
  lvl1 <- safeListDir baseDir
  fmap concat $ mapM (\d1 -> do
    lvl2 <- safeListDir (baseDir </> d1)
    fmap concat $ mapM (\d2 -> do
      lvl3 <- safeListDir (baseDir </> d1 </> d2)
      fmap concat $ mapM (\d3 -> do
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
