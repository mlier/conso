{-# LANGUAGE OverloadedStrings #-}
module Display
  ( afficherResultat
  , afficherSites
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID

import Conso.Fr.SiteDB.Types (SiteId(..), Prm(..), Pce(..), SiteRef(..))
import Conso.Fr.SiteDB.Orchestration.Types


afficherResultat :: InscriptionResult -> IO ()
afficherResultat r = do
  let (SiteId uuid) = irSiteId r
      tag = if irCreated r then " (nouveau)" else " (existant)"
  putStrLn $ "Site UUID : " <> UUID.toString uuid <> tag
  mapM_ afficherSge (irSgeResults r)
  case irAdictResult r of
    Nothing  -> return ()
    Just res -> afficherAdict res

afficherSge :: (TypeFlux, Either (String, String) ()) -> IO ()
afficherSge (t, Right ()) =
  putStrLn $ "  SGE " <> show t <> " : souscrit"
afficherSge (t, Left (code, lbl)) =
  putStrLn $ "  SGE " <> show t <> " : " <> code <> " — " <> lbl

afficherAdict :: Either String Text -> IO ()
afficherAdict (Right idAcces) =
  putStrLn $ "  ADICT : droit d'accès " <> T.unpack idAcces
afficherAdict (Left err) =
  putStrLn $ "  ADICT : erreur — " <> err


afficherSites :: [SiteRef] -> IO ()
afficherSites [] = putStrLn "(aucun site inscrit)"
afficherSites sites = do
  putStrLn $ pad 36 "UUID" <> "  " <> pad 16 "PRM" <> "  " <> pad 16 "PCE"
  putStrLn $ replicate 36 '-' <> "  " <> replicate 16 '-' <> "  " <> replicate 16 '-'
  mapM_ afficherSite sites

afficherSite :: SiteRef -> IO ()
afficherSite sr = do
  let (SiteId uuid) = srSiteId sr
      prmStr = maybe "(aucun)" (\(Prm t) -> T.unpack t) (srPrm sr)
      pceStr = maybe "(aucun)" (\(Pce t) -> T.unpack t) (srPce sr)
  putStrLn $ pad 36 (UUID.toString uuid) <> "  " <> pad 16 prmStr <> "  " <> pad 16 pceStr

pad :: Int -> String -> String
pad n s = take n (s <> repeat ' ')
