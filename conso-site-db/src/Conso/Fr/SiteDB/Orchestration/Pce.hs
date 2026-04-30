{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.SiteDB.Orchestration.Pce
  ( inscrirePce
  ) where

import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getZonedTime, zonedTimeToLocalTime, localDay, addGregorianYearsRollOver, formatTime, defaultTimeLocale)


import Database.SQLite.Simple (Connection)

import Conso.Fr.Gaz.Adict.Adict (AdictSession)
import Conso.Fr.Gaz.Adict.DroitAcces (declarerDroitAcces)
import Conso.Fr.Gaz.Adict.Types (DemandeAccesIn(..), RetourDemandeAcces(..))

import Conso.Fr.SiteDB.Types (SiteId(..), Prm(..), Pce(..), SiteRef(..))
import Conso.Fr.SiteDB.Registry.Operations
  ( lookupByPce, lookupByPrm, lookupBySiteId
  , createSite, linkPce
  )
import Conso.Fr.SiteDB.Orchestration.Types
import Conso.Fr.SiteDB.Orchestration.Adresses (verifierAdresses)


inscrirePce :: Connection -> Bool -> Bool -> AdictSession -> InscriptionPceParams -> IO InscriptionResult
inscrirePce conn prod verbose session params = do
  (siteId, created) <- resoudreSite
  adictResult <- declarerAcces session (ipePce params) (ipeCodePostal params) (ipeAccord params)
  return $ InscriptionResult siteId created [] (Just adictResult)
  where
    pce = Pce (ipePce params)

    resoudreSite = case ipeRattachement params of
      Standalone         -> creerOuTrouver conn pce
      ParPrm prmT force  -> rattacherAuPrm conn prod verbose session pce (Prm prmT) force
      ParSite uuid force -> rattacherAuSite conn prod verbose session pce (SiteId uuid) force
      ParPce _ _         -> fail "ParPce invalide dans inscrirePce"


creerOuTrouver :: Connection -> Pce -> IO (SiteId, Bool)
creerOuTrouver conn pce = do
  mExisting <- lookupByPce conn pce
  case mExisting of
    Just sid -> return (sid, False)
    Nothing  -> do
      sid <- createSite conn Nothing (Just pce) Nothing
      return (sid, True)


rattacherAuPrm :: Connection -> Bool -> Bool -> AdictSession -> Pce -> Prm -> Bool -> IO (SiteId, Bool)
rattacherAuPrm conn prod verbose session pce@(Pce pceT) prm@(Prm prmT) force = do
  mPrmSite <- lookupByPrm conn prm
  targetSiteId <- maybe (fail $ "PRM " <> T.unpack prmT <> " non inscrit dans le registre") return mPrmSite
  verifierConflitPce conn pce targetSiteId
  checkAdresses verbose prod session prmT pceT force
  mPceSite <- lookupByPce conn pce
  when (isNothing mPceSite) $ linkPce conn targetSiteId pce
  return (targetSiteId, isNothing mPceSite)


rattacherAuSite :: Connection -> Bool -> Bool -> AdictSession -> Pce -> SiteId -> Bool -> IO (SiteId, Bool)
rattacherAuSite conn prod verbose session pce@(Pce pceT) siteId force = do
  mSite <- lookupBySiteId conn siteId
  site  <- maybe (fail $ "Site non trouvé dans le registre : " <> show siteId) return mSite
  verifierConflitPce conn pce siteId
  case srPrm site of
    Just (Prm prmT) -> checkAdresses verbose prod session prmT pceT force
    Nothing         -> return ()
  mPceSite <- lookupByPce conn pce
  when (isNothing mPceSite) $ linkPce conn siteId pce
  return (siteId, isNothing mPceSite)


verifierConflitPce :: Connection -> Pce -> SiteId -> IO ()
verifierConflitPce conn pce targetSiteId = do
  mPceSite <- lookupByPce conn pce
  case mPceSite of
    Just sid | sid /= targetSiteId ->
      fail $ "PCE déjà inscrit sous un site différent : " <> show sid
    _ -> return ()


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


declarerAcces :: AdictSession -> Text -> Text -> Accord -> IO (Either String Text)
declarerAcces session pceT cp accord = do
  today <- localDay . zonedTimeToLocalTime <$> getZonedTime
  let debut = formatTime defaultTimeLocale "%Y-%m-%d" today
      fin   = formatTime defaultTimeLocale "%Y-%m-%d" (addGregorianYearsRollOver 3 today)
      demande = DemandeAccesIn
        { din_role_tiers                        = "AUTORISE_CONTRAT_FOURNITURE"
        , din_raison_sociale                    = raisonSociale
        , din_nom_titulaire                     = nomTitulaire
        , din_code_postal                       = cp
        , din_courriel_titulaire                = Nothing
        , din_numero_telephone_mobile_titulaire = Nothing
        , din_date_debut_droit_acces            = Just (T.pack debut)
        , din_date_fin_droit_acces              = Just (T.pack fin)
        , din_perim_donnees_conso_debut         = Just (T.pack debut)
        , din_perim_donnees_conso_fin           = Just (T.pack fin)
        , din_perim_donnees_inj_debut           = Nothing
        , din_perim_donnees_inj_fin             = Nothing
        , din_perim_donnees_contractuelles      = Just "true"
        , din_perim_donnees_techniques          = Just "true"
        , din_perim_donnees_informatives        = Just "true"
        , din_perim_donnees_publiees            = Just "true"
        }
  result <- declarerDroitAcces session pceT demande
  return $ case result of
    Left err -> Left (show err)
    Right da -> Right (maybe "(aucun id)" id (rda_id_droit_acces da))
  where
    (nomTitulaire, raisonSociale) = case accord of
      AccordNom nom        -> (Just nom, Nothing)
      AccordDenomination d -> (Nothing, Just d)
