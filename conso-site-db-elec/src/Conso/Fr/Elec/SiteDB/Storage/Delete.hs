{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SiteDB.Storage.Delete
  ( deleteElecData
  ) where

import Control.Exception      (try, SomeException)
import Database.SQLite.Simple

-- | Supprime toutes les données élec d'une base site (dans une transaction).
-- Les tables absentes sont ignorées silencieusement.
deleteElecData :: Connection -> IO ()
deleteElecData conn = withTransaction conn $
  mapM_ deleteTable
    [ "ingestion_log"
    , "curve_points"
    , "index_values"
    , "daily_energy"
    , "daily_pmax"
    , "billing_measures"
    , "prm_info"
    ]
  where
    deleteTable t = do
      result <- try (execute_ conn ("DELETE FROM " <> t)) :: IO (Either SomeException ())
      case result of
        Left  _ -> return ()
        Right _ -> return ()
