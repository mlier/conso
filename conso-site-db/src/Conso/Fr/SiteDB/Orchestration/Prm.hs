{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.SiteDB.Orchestration.Prm
  ( inscrirePrm
  ) where

import Control.Monad (forM, when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T


import Database.SQLite.Simple (Connection)

import Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10
  ( initType, initTypeTest
  , wsRequest, wsRequestTest
  , AccordPersonneType(..), Sens(..)
  )
import Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10Type
  (CommanderServicesAccesDonneesResponseType)

import Conso.Fr.Gaz.Adict.Adict (AdictSession)

import Conso.Fr.SiteDB.Types (SiteId(..), Prm(..), Pce(..), SiteRef(..))
import Conso.Fr.SiteDB.Registry.Operations
  ( lookupByPrm, lookupByPce, lookupBySiteId
  , createSite, linkPrm
  )
import Conso.Fr.SiteDB.Orchestration.Types
import Conso.Fr.SiteDB.Orchestration.Adresses (verifierAdresses)


inscrirePrm :: Connection -> Bool -> Bool -> Maybe AdictSession -> InscriptionPrmParams -> IO InscriptionResult
inscrirePrm conn prod verbose mSession params = do
  (siteId, created) <- resoudreSite
  sgeResults <- abonnerSge prod (ippPrm params) (ippAccord params) (ippTypes params)
  return $ InscriptionResult siteId created sgeResults Nothing
  where
    prm = Prm (ippPrm params)

    resoudreSite = case ippRattachement params of
      Standalone         -> creerOuTrouver conn prm
      ParPce pceT force  -> rattacherAuPce conn prod verbose mSession prm (Pce pceT) force
      ParSite uuid force -> rattacherAuSite conn prod verbose mSession prm (SiteId uuid) force
      ParPrm _ _         -> fail "ParPrm invalide dans inscrirePrm"


creerOuTrouver :: Connection -> Prm -> IO (SiteId, Bool)
creerOuTrouver conn prm = do
  mExisting <- lookupByPrm conn prm
  case mExisting of
    Just sid -> return (sid, False)
    Nothing  -> do
      sid <- createSite conn (Just prm) Nothing Nothing
      return (sid, True)


rattacherAuPce :: Connection -> Bool -> Bool -> Maybe AdictSession -> Prm -> Pce -> Bool -> IO (SiteId, Bool)
rattacherAuPce conn prod verbose mSession prm pce@(Pce pceT) force = do
  mPceSite <- lookupByPce conn pce
  targetSiteId <- maybe (fail $ "PCE " <> T.unpack pceT <> " non inscrit dans le registre") return mPceSite
  verifierConflitPrm conn prm targetSiteId
  verifierAdresseSiNecessaire verbose prod mSession (unPrm prm) pceT force
  mPrmSite <- lookupByPrm conn prm
  when (isNothing mPrmSite) $ linkPrm conn targetSiteId prm
  return (targetSiteId, isNothing mPrmSite)


rattacherAuSite :: Connection -> Bool -> Bool -> Maybe AdictSession -> Prm -> SiteId -> Bool -> IO (SiteId, Bool)
rattacherAuSite conn prod verbose mSession prm siteId force = do
  mSite <- lookupBySiteId conn siteId
  site  <- maybe (fail $ "Site non trouvé dans le registre : " <> show siteId) return mSite
  verifierConflitPrm conn prm siteId
  case (srPce site, mSession) of
    (Just (Pce pceT), Just session) -> checkAdresses verbose prod session (unPrm prm) pceT force
    _                               -> return ()
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


verifierAdresseSiNecessaire :: Bool -> Bool -> Maybe AdictSession -> Text -> Text -> Bool -> IO ()
verifierAdresseSiNecessaire _ _ _ _ _ True    = return ()
verifierAdresseSiNecessaire verbose prod (Just s) p c False = checkAdresses verbose prod s p c False
verifierAdresseSiNecessaire _ _ Nothing _ _ False =
  fail "Session ADICT requise pour la vérification d'adresse (--pce)"


checkAdresses :: Bool -> Bool -> AdictSession -> Text -> Text -> Bool -> IO ()
checkAdresses _ _ _ _ _ True = return ()
checkAdresses verbose prod session prmT pceT False = do
  verif <- verifierAdresses verbose prod session prmT pceT
  case verif of
    CodePostauxIdentiques -> return ()
    Mismatch cpP cpC ->
      fail $ "Codes postaux différents — PRM: " <> T.unpack cpP
          <> ", PCE: " <> T.unpack cpC
          <> "\nUtilisez --force pour ignorer."
    VerifImpossible e ->
      fail $ "Vérification d'adresse impossible : " <> e


abonnerSge :: Bool -> Text -> Accord -> [TypeFlux] -> IO [(TypeFlux, Either (String, String) ())]
abonnerSge prod prmT accord types = forM types $ \t -> do
  r <- subscribeSge prod prmT accord t
  return (t, r)


subscribeSge :: Bool -> Text -> Accord -> TypeFlux -> IO (Either (String, String) ())
subscribeSge prod prmT accord t = do
  req <- mkInit (T.unpack prmT) SensSOUTIRAGE (Just accordType) (typeFluxToStr t) (Just 730)
  resp <- mkWs req :: IO (Either (String, String) CommanderServicesAccesDonneesResponseType)
  return $ fmap (const ()) resp
  where
    accordType = case accord of
      AccordNom nom        -> AccordPersonnePhysiqueNom (T.unpack nom)
      AccordDenomination d -> AccordPersonneMoraleDenominationSociale (T.unpack d)
    mkInit = if prod then initType else initTypeTest
    mkWs   = if prod then wsRequest else wsRequestTest


unPrm :: Prm -> Text
unPrm (Prm t) = t
