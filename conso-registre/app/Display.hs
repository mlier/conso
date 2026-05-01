{-# LANGUAGE OverloadedStrings #-}
module Display
  ( afficherResultat
  , afficherSites
  , afficherDesinscription
  , afficherIngererGaz
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Data.Foldable (forM_)

import Conso.Fr.SiteDB.Types (SiteId(..), Prm(..), Pce(..), SiteRef(..))
import Conso.Fr.SiteDB.Orchestration.Types
import Conso.Fr.SiteDB.Orchestration.Desinscription (DesinscriptionResult(..))
import Conso.Fr.Gaz.SiteDB.Orchestration.Ingerer
  ( IngererGazReport(..), PceIngestionReport(..) )
import Conso.Fr.Gaz.SiteDB.Ingestion.FromApi
  ( ChangementInfosContract(..), ChangementInfosTech(..) )


afficherResultat :: InscriptionResult -> IO ()
afficherResultat r = do
  let (SiteId uuid) = irSiteId r
      tag = if irCreated r then " (nouveau)" else " (existant)"
  putStrLn $ "Site UUID : " <> UUID.toString uuid <> tag
  mapM_ afficherSge (irSgeResults r)
  forM_ (irAdictResult r) afficherAdict

afficherSge :: (TypeFlux, Either (String, String) SgeAbonnement) -> IO ()
afficherSge (t, Right SgeNouveau)   = putStrLn $ "  SGE " <> show t <> " : souscrit"
afficherSge (t, Right SgeRenouvele) = putStrLn $ "  SGE " <> show t <> " : renouvelé"
afficherSge (t, Left (code, lbl))   = putStrLn $ "  SGE " <> show t <> " : " <> code <> " — " <> lbl

afficherAdict :: Either String Text -> IO ()
afficherAdict (Right idAcces) =
  putStrLn $ "  ADICT : droit d'accès " <> T.unpack idAcces
afficherAdict (Left err) =
  putStrLn $ "  ADICT : erreur — " <> err


afficherDesinscription :: DesinscriptionResult -> IO ()
afficherDesinscription r = do
  let (SiteId uuid) = drSiteId r
      tag = if drSiteDeleted r then " (supprimé)" else " (données effacées)"
  putStrLn $ "Site UUID : " <> UUID.toString uuid <> tag
  mapM_ afficherArret (drSgeResults r)

afficherArret :: (String, Either (String, String) ()) -> IO ()
afficherArret (sid, Right ())         = putStrLn $ "  SGE " <> sid <> " : arrêté"
afficherArret (sid, Left (code, lbl)) = putStrLn $ "  SGE " <> sid <> " : " <> code <> " — " <> lbl

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


afficherIngererGaz :: IngererGazReport -> IO ()
afficherIngererGaz r = do
  putStrLn $ "=== Ingestion gaz : " <> show (igrTotal r) <> " PCE(s) traité(s) ==="
  mapM_ afficherPceReport (igrDetails r)
  mapM_ afficherErreurPce (igrErrors r)

afficherPceReport :: PceIngestionReport -> IO ()
afficherPceReport r = do
  let Pce pce = pirPce r
  putStrLn $ "\n--- PCE " <> T.unpack pce <> " ---"
  putStrLn $ "  Consos publiées   : " <> afficherNb (pirConsoPub r)
  putStrLn $ "  Consos informatives: " <> afficherNb (pirConsoInfo r)
  putStrLn $ "  Injections        : " <> afficherNb (pirInjections r)
  putStrLn $ "  Contractuelles    : " <> afficherChangementContract (pirContractuelles r)
  putStrLn $ "  Techniques        : " <> afficherChangementTech (pirTechniques r)
  case pirTrous r of
    []    -> return ()
    trous -> do
      putStrLn $ "  Trous détectés (" <> show (length trous) <> ") :"
      mapM_ (\(d1, d2) -> putStrLn $ "    " <> T.unpack d1 <> " → " <> T.unpack d2) trous

afficherErreurPce :: (Pce, Text) -> IO ()
afficherErreurPce (Pce pce, err) =
  putStrLn $ "\n  ERREUR PCE " <> T.unpack pce <> " : " <> T.unpack err

afficherNb :: Either Text Int -> String
afficherNb (Left err) = "ERREUR — " <> T.unpack err
afficherNb (Right n)  = show n <> " ligne(s)"

afficherChangementContract :: Either Text ChangementInfosContract -> String
afficherChangementContract (Left err)                        = "ERREUR — " <> T.unpack err
afficherChangementContract (Right ContractuellesPasDeChangement)  = "inchangées"
afficherChangementContract (Right (ContractuellesNouvellesInfos _)) = "mise à jour stockée"

afficherChangementTech :: Either Text ChangementInfosTech -> String
afficherChangementTech (Left err)                      = "ERREUR — " <> T.unpack err
afficherChangementTech (Right TechniquesPasDeChangement)    = "inchangées"
afficherChangementTech (Right (TechniquesNouvellesInfos _)) = "mise à jour stockée"
