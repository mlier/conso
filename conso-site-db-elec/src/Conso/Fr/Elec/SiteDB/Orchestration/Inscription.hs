{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SiteDB.Orchestration.Inscription
  ( inscrirePrm
  ) where

import Control.Monad (forM, when, void)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.IO (hPutStrLn, stderr)

import Database.SQLite.Simple (Connection)

import Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10
  ( initType, initTypeTest, wsRequest, wsRequestTest
  , AccordPersonneType(..), Sens(..), Periodicite(..) )
import Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10Type
  (CommanderServicesAccesDonneesResponseType)
import qualified Conso.Fr.Elec.Sge.CommanderRenouvellementServicesAccesDonneesV10 as RRen
import Conso.Fr.Elec.Sge.CommanderRenouvellementServicesAccesDonneesV10Type
  (RenouvelerServicesAccesResponseType)

import Conso.Fr.SiteDB.Types (SiteId(..), Prm(..), Pce(..), SiteRef(..))
import Conso.Fr.SiteDB.Registry.Operations
  ( lookupByPrm, lookupByPce, lookupBySiteId, createSite, linkPrm )
import Conso.Fr.SiteDB.Orchestration.Types
import Conso.Fr.SiteDB.Orchestration.Adresses (verifierCoherence)

import Conso.Fr.Elec.SiteDB.Orchestration.Adresse (codePostalPrm, rechercherServicesActifs)


inscrirePrm :: Connection -> Bool -> Bool
            -> Maybe GetCodePostal  -- ^ code postal PCE, fourni par conso-site-db-gaz si disponible
            -> InscriptionPrmParams
            -> IO InscriptionResult
inscrirePrm conn prod verbose mGetCpPce params = do
  (siteId, created) <- resoudreSite
  sgeResults <- abonnerSge prod verbose (ippPrm params) (ippAccord params) (ippTypes params)
  return $ InscriptionResult siteId created sgeResults Nothing
  where
    prm = Prm (ippPrm params)

    resoudreSite = case ippRattachement params of
      Standalone         -> creerOuTrouver conn prm
      ParPce pceT force  -> rattacherAuPce conn prod verbose mGetCpPce prm (Pce pceT) force
      ParSite uuid force -> rattacherAuSite conn prod verbose mGetCpPce prm (SiteId uuid) force
      ParPrm _ _         -> fail "ParPrm invalide dans inscrirePrm"


creerOuTrouver :: Connection -> Prm -> IO (SiteId, Bool)
creerOuTrouver conn prm = do
  mExisting <- lookupByPrm conn prm
  case mExisting of
    Just sid -> return (sid, False)
    Nothing  -> do
      sid <- createSite conn (Just prm) Nothing Nothing
      return (sid, True)


rattacherAuPce :: Connection -> Bool -> Bool -> Maybe GetCodePostal -> Prm -> Pce -> Bool -> IO (SiteId, Bool)
rattacherAuPce conn prod verbose mGetCpPce prm pce@(Pce pceT) force = do
  mPceSite <- lookupByPce conn pce
  targetSiteId <- maybe (fail $ "PCE " <> T.unpack pceT <> " non inscrit dans le registre") return mPceSite
  verifierConflitPrm conn prm targetSiteId
  verifierAdresseSiNecessaire verbose prod mGetCpPce (unPrm prm) pceT force
  mPrmSite <- lookupByPrm conn prm
  when (isNothing mPrmSite) $ linkPrm conn targetSiteId prm
  return (targetSiteId, isNothing mPrmSite)


rattacherAuSite :: Connection -> Bool -> Bool -> Maybe GetCodePostal -> Prm -> SiteId -> Bool -> IO (SiteId, Bool)
rattacherAuSite conn prod verbose mGetCpPce prm siteId force = do
  mSite <- lookupBySiteId conn siteId
  site  <- maybe (fail $ "Site non trouvé dans le registre : " <> show siteId) return mSite
  verifierConflitPrm conn prm siteId
  case (srPce site, mGetCpPce) of
    (Just (Pce pceT), Just getCpPce) -> checkAdresses verbose prod getCpPce (unPrm prm) pceT force
    _                                -> return ()
  mPrmSite <- lookupByPrm conn prm
  when (isNothing mPrmSite) $ linkPrm conn siteId prm
  return (siteId, isNothing mPrmSite)


verifierConflitPrm :: Connection -> Prm -> SiteId -> IO ()
verifierConflitPrm conn prm targetSiteId = do
  mPrmSite <- lookupByPrm conn prm
  case mPrmSite of
    Just sid | sid /= targetSiteId ->
      fail $ "PRM déjà inscrit sous un site différent : " <> show sid
    _ -> return ()


verifierAdresseSiNecessaire :: Bool -> Bool -> Maybe GetCodePostal -> Text -> Text -> Bool -> IO ()
verifierAdresseSiNecessaire _ _ _ _ _ True    = return ()
verifierAdresseSiNecessaire verbose prod (Just getCpPce) prmT pceT False =
  checkAdresses verbose prod getCpPce prmT pceT False
verifierAdresseSiNecessaire _ _ Nothing _ _ False =
  fail "GetCodePostal PCE requis pour la vérification d'adresse (--pce)"


checkAdresses :: Bool -> Bool -> GetCodePostal -> Text -> Text -> Bool -> IO ()
checkAdresses _ _ _ _ _ True = return ()
checkAdresses verbose prod getCpPce prmT pceT False = do
  verif <- verifierCoherence verbose (codePostalPrm verbose prod) getCpPce prmT pceT
  case verif of
    CodePostauxIdentiques -> return ()
    Mismatch cpP cpC ->
      fail $ "Codes postaux différents — PRM: " <> T.unpack cpP
          <> ", PCE: " <> T.unpack cpC
          <> "\nUtilisez --force pour ignorer."
    VerifImpossible e ->
      fail $ "Vérification d'adresse impossible : " <> e


abonnerSge :: Bool -> Bool -> Text -> Accord -> [TypeFlux] -> IO [(TypeFlux, Either (String, String) SgeAbonnement)]
abonnerSge prod verbose prmT accord types = do
  actifMap <- rechercherServicesActifs verbose prod prmT
  forM types $ \t -> do
    r <- case Map.lookup (typeFluxToStr t) actifMap of
           Just sid -> do
             logV verbose $ "SGE renouveler " <> typeFluxToStr t <> " (sid=" <> sid <> ")"
             raw <- renouvelerSge prod prmT accord sid
             let result = case raw of
                   Left ("SGT570", _) -> Right ()
                   other              -> other
             return $ fmap (const SgeRenouvele) result
           Nothing  -> do
             logV verbose $ "SGE souscrire " <> typeFluxToStr t
             fmap (const SgeNouveau) <$> subscribeSge prod prmT accord t
    return (t, r)


renouvelerSge :: Bool -> Text -> Accord -> String -> IO (Either (String, String) ())
renouvelerSge prod prmT accord sid = do
  req <- mkInit (T.unpack prmT) RRen.SensSOUTIRAGE accordType [sid] (Just 730)
  resp <- mkWs req :: IO (Either (String, String) RenouvelerServicesAccesResponseType)
  return $ void resp
  where
    accordType = case accord of
      AccordNom nom        -> RRen.AccordPersonnePhysiqueNom (T.unpack nom)
      AccordDenomination d -> RRen.AccordPersonneMoraleDenominationSociale (T.unpack d)
    mkInit = if prod then RRen.initType else RRen.initTypeTest
    mkWs   = if prod then RRen.wsRequest else RRen.wsRequestTest


subscribeSge :: Bool -> Text -> Accord -> TypeFlux -> IO (Either (String, String) ())
subscribeSge prod prmT accord t = do
  req <- mkInit (T.unpack prmT) SensSOUTIRAGE (Just accordType) (typeFluxToStr t) (Just 730)
           (periodiciteFor t)
  resp <- mkWs req :: IO (Either (String, String) CommanderServicesAccesDonneesResponseType)
  return $ void resp
  where
    accordType = case accord of
      AccordNom nom        -> AccordPersonnePhysiqueNom (T.unpack nom)
      AccordDenomination d -> AccordPersonneMoraleDenominationSociale (T.unpack d)
    mkInit = if prod then initType else initTypeTest
    mkWs   = if prod then wsRequest else wsRequestTest

periodiciteFor :: TypeFlux -> Maybe Periodicite
periodiciteFor IDX = Nothing
periodiciteFor _   = Just P1D


logV :: Bool -> String -> IO ()
logV True  msg = hPutStrLn stderr $ "[verbose] " <> msg
logV False _   = return ()


unPrm :: Prm -> Text
unPrm (Prm t) = t
