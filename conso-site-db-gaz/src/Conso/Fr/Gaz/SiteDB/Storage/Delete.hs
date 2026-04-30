{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.SiteDB.Storage.Delete
  ( deleteGazData
  ) where

import Control.Exception      (try, SomeException)
import Database.SQLite.Simple

-- | Supprime toutes les données gaz d'une base site (dans une transaction).
-- Les tables absentes sont ignorées silencieusement.
deleteGazData :: Connection -> IO ()
deleteGazData conn = withTransaction conn $
  mapM_ deleteTable
    [ "gaz_ingestion_log"
    , "gaz_consos"
    , "gaz_injections"
    , "gaz_infos_contractuelles"
    , "gaz_infos_techniques"
    ]
  where
    deleteTable t = do
      result <- try (execute_ conn ("DELETE FROM " <> t)) :: IO (Either SomeException ())
      case result of
        Left  _ -> return ()
        Right _ -> return ()
