{-# LANGUAGE OverloadedStrings #-}
module Display
  ( afficherResultat
  , afficherSites
  , afficherDesinscription
  , afficherIngererGaz
  , afficherIngererElec
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Data.Foldable (forM_)
import Data.Time (Day, showGregorian, addDays)

import Conso.Fr.SiteDB.Types (SiteId(..), Prm(..), Pce(..), SiteRef(..))
import Conso.Fr.SiteDB.Orchestration.Types
import Conso.Fr.SiteDB.Orchestration.Desinscription (DesinscriptionResult(..))
import Conso.Fr.Gaz.SiteDB.Orchestration.Ingerer
  ( IngererGazReport(..), PceIngestionReport(..), TrouBackfill(..) )
import Conso.Fr.Gaz.SiteDB.Ingestion.FromApi
  ( ChangementInfosContract(..), ChangementInfosTech(..) )
import Conso.Fr.Elec.SiteDB.Orchestration.Ingerer
  ( IngererElecReport(..), PrmIngestionReport(..), PrmInfoC68(..) )
import Conso.Fr.Elec.SiteDB.Storage.Query (ServiceArretRow(..))
import Conso.Fr.Elec.SiteDB.Orchestration.Backfill
  ( BackfillBatch(..) )
import Conso.Fr.Elec.SiteDB.Orchestration.CompteRendu
  ( CrResult(..) )


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
             <> afficherDate (pirDerniereConsoPub r)
  putStrLn $ "  Consos informatives: " <> afficherNb (pirConsoInfo r)
             <> afficherDate (pirDerniereConsoInfo r)
  putStrLn $ "  Injections        : " <>
    if pirAvecInjections r
      then afficherNb (pirInjections r) <> afficherDate (pirDerniereInj r)
      else "non souscrit"
  putStrLn $ "  Contractuelles    : " <> afficherChangementContract (pirContractuelles r)
             <> afficherDate (pirDerniereContract r)
  putStrLn $ "  Techniques        : " <> afficherChangementTech (pirTechniques r)
             <> afficherDate (pirDerniereTech r)
  afficherTrous "Trous consos pub  " (pirTrousConso r)
  afficherTrous "Trous consos info " (pirTrousInfo  r)
  afficherTrous "Trous injections  " (pirTrousInj   r)

afficherTrous :: String -> [TrouBackfill] -> IO ()
afficherTrous _     []    = return ()
afficherTrous label trous = do
  putStrLn $ "  " <> label <> "(" <> show (length trous) <> ") :"
  forM_ trous $ \tb ->
    putStrLn $ "    " <> T.unpack (tbDebut tb) <> " → " <> T.unpack (tbFin tb)
            <> " : " <> afficherNb (tbBackfill tb)

afficherErreurPce :: (Pce, Text) -> IO ()
afficherErreurPce (Pce pce, err) =
  putStrLn $ "\n  ERREUR PCE " <> T.unpack pce <> " : " <> T.unpack err

afficherDate :: Maybe Text -> String
afficherDate Nothing  = ""
afficherDate (Just d) = "  [dernière : " <> T.unpack d <> "]"

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


afficherIngererElec :: IngererElecReport -> IO ()
afficherIngererElec r = do
  putStrLn $ "=== Ingestion élec : " <> show (ierFichiersTotal r) <> " fichier(s) traité(s)"
          <> " (" <> show (ierFichiersIgnores r) <> " ignoré(s)"
          <> ", " <> show (ierFichiersErreur r) <> " erreur(s)) ==="
  mapM_ afficherPrmReport (ierDetails r)
  mapM_ afficherErreurPrm (ierErrors r)
  let parseErrs = ierErreursParser r
  if null parseErrs
    then return ()
    else do
      putStrLn $ "\n=== Erreurs de parsing JSON (" <> show (length parseErrs) <> ") ==="
      forM_ parseErrs $ \(fichier, msg) ->
        putStrLn $ "  ERREUR " <> T.unpack fichier <> " : " <> T.unpack msg
  let backfill = ierBackfill r
  if null backfill
    then return ()
    else do
      putStrLn $ "\n=== Demandes M023 envoyées : " <> show (length backfill) <> " ==="
      mapM_ afficherBackfill backfill
  let crs = ierCR r
  if null crs
    then return ()
    else do
      putStrLn $ "\n=== Comptes-Rendus Enedis reçus : " <> show (length crs) <> " ==="
      mapM_ afficherCr crs

afficherCr :: CrResult -> IO ()
afficherCr cr =
  putStrLn $ "  " <> T.unpack (crAffaireId cr)
          <> "  " <> T.unpack (crStatut cr)
          <> " : " <> T.unpack (crMessage cr)

afficherBackfill :: BackfillBatch -> IO ()
afficherBackfill b = do
  let nbPrm = length (bbPrms b)
  putStrLn $ "  [" <> T.unpack (bbFlux b) <> "] "
          <> T.unpack (bbDebut b)
          <> (if T.null (bbFin b) then "" else " → " <> T.unpack (bbFin b))
          <> " (" <> show nbPrm <> " PRM(s))"
          <> " : " <> either (\e -> "ERREUR — " <> T.unpack e) T.unpack (bbAffaireId b)

afficherPrmReport :: PrmIngestionReport -> IO ()
afficherPrmReport r = do
  let Prm prm = prirPrm r
  putStrLn $ "\n--- PRM " <> T.unpack prm <> " ---"
  putStrLn $ "  Fichiers ingérés  : " <> show (prirFichiersOk r)
  putStrLn $ "  Fichiers ignorés  : " <> show (prirFichiersSkip r)
  putStrLn $ "  Courbes (R63)     : " <> afficherDateElec (prirDerniereCourbe r)
  putStrLn $ "  Index   (R64)     : " <> afficherDateElec (prirDerniereIndex r)
  putStrLn $ "  Énergie (R65)     : " <> afficherDateElec (prirDerniereEnergie r)
  putStrLn $ "  Pmax    (R66)     : " <> afficherDateElec (prirDernierePmax r)
  forM_ (prirInfoC68 r) afficherInfoC68
  afficherTrousDays "Trous courbes  " (prirTrousCourbes r)
  afficherTrousDays "Trous énergie  " (prirTrousEnergie r)
  afficherTrousDays "Trous Pmax     " (prirTrousPmax r)
  forM_ (prirServicesArrets r) afficherServiceArret
  forM_ (prirErreurs r) $ \(f, e) ->
    putStrLn $ "  ERREUR " <> T.unpack f <> " : " <> T.unpack e

afficherInfoC68 :: PrmInfoC68 -> IO ()
afficherInfoC68 c = do
  putStrLn "  --- C68 (dernières infos) ---"
  forM_ (picSegment c)              $ \v -> putStrLn $ "    Segment             : " <> T.unpack v
  forM_ (picEtatContractuel c)      $ \v -> putStrLn $ "    État contractuel     : " <> T.unpack v
  forM_ (picFormuleTarifaire c)     $ \v -> putStrLn $ "    FTA                 : " <> T.unpack v
  forM_ (picPuissanceSouscrite c)   $ \v -> putStrLn $ "    Puissance souscrite : " <> T.unpack v
  forM_ (picAdresse c)              $ \v -> putStrLn $ "    Adresse             : " <> T.unpack v
  forM_ (picMatriculeCompteur c)    $ \v -> putStrLn $ "    Compteur            : " <> T.unpack v
  forM_ (picLinky c)                $ \v -> putStrLn $ "    Linky déployé le    : " <> T.unpack v
  forM_ (picTitulaireNom c)         $ \v -> putStrLn $ "    Titulaire           : " <> T.unpack v

groupRanges :: [Day] -> [(Day, Day)]
groupRanges []     = []
groupRanges (d:ds) = go d d ds
  where
    go s e []     = [(s, e)]
    go s e (x:xs)
      | x == addDays 1 e = go s x xs
      | otherwise        = (s, e) : go x x xs

afficherTrousDays :: String -> [Day] -> IO ()
afficherTrousDays _     []   = return ()
afficherTrousDays label days = do
  putStrLn $ "  " <> label <> "(" <> show (length days) <> " j) :"
  mapM_ (putStrLn . ("    " <>) . showRange) (groupRanges days)
  where
    showRange (s, e)
      | s == e    = showGregorian s
      | otherwise = showGregorian s <> "->" <> showGregorian e

afficherErreurPrm :: (Prm, Text) -> IO ()
afficherErreurPrm (Prm prm, err) =
  putStrLn $ "\n  ERREUR PRM " <> T.unpack prm <> " : " <> T.unpack err

afficherServiceArret :: ServiceArretRow -> IO ()
afficherServiceArret s =
  putStrLn $ "  SERVICE ARRETÉ " <> T.unpack (sarTypeServiceSouscrit s)
          <> " " <> T.unpack (sarTypeService s)
          <> " (" <> T.unpack (sarSegment s) <> ")"
          <> " fin " <> T.unpack (sarDateFin s)
          <> maybe "" (\m -> " — " <> T.unpack m) (sarMotifFinLibelle s)
          <> maybe "" (\c -> " (code " <> show c <> ")") (sarMotifFinCode s)

afficherDateElec :: Maybe Text -> String
afficherDateElec Nothing  = "(aucune donnée)"
afficherDateElec (Just d) = "[dernière : " <> T.unpack d <> "]"
