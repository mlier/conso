{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.SiteDB.Orchestration.Prm
  ( inscrirePrm
  ) where

import Control.Monad (forM, when, void)
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import System.IO (hPutStrLn, stderr)

import Database.SQLite.Simple (Connection)

import Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10
  ( initType, initTypeTest
  , wsRequest, wsRequestTest
  , AccordPersonneType(..), Sens(..)
  )
import Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10Type
  (CommanderServicesAccesDonneesResponseType)

import qualified Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10 as RSD
import           Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10Type
  ( RechercherServicesAccesDonneesReponseType(..)
  , ServicesSouscritsType(..)
  , ServiceSouscritType(..)
  )
import           Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50
  ( MesureTypeCodeType(..), Chaine15Type(..) )
import qualified Conso.Fr.Elec.Sge.CommanderRenouvellementServicesAccesDonneesV10 as RRen
import           Conso.Fr.Elec.Sge.CommanderRenouvellementServicesAccesDonneesV10Type
  (RenouvelerServicesAccesResponseType)

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
  sgeResults <- abonnerSge prod verbose (ippPrm params) (ippAccord params) (ippTypes params)
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


logV :: Bool -> String -> IO ()
logV True  msg = hPutStrLn stderr $ "[verbose] " <> msg
logV False _   = return ()


abonnerSge :: Bool -> Bool -> Text -> Accord -> [TypeFlux] -> IO [(TypeFlux, Either (String, String) SgeAbonnement)]
abonnerSge prod verbose prmT accord types = do
  actifMap <- rechercherServicesActifs prod verbose prmT
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
             fmap (const SgeNouveau)  <$> subscribeSge prod prmT accord t
    return (t, r)


rechercherServicesActifs :: Bool -> Bool -> Text -> IO (Map.Map String String)
rechercherServicesActifs prod verbose prmT = do
  logV verbose $ "SGE RechercherServicesAccesDonnees → PRM " <> T.unpack prmT
  req <- if prod then RSD.initType prmStr else RSD.initTypeTest prmStr
  resp <- (if prod then RSD.wsRequest else RSD.wsRequestTest) req
            :: IO (Either (String, String) RechercherServicesAccesDonneesReponseType)
  case resp of
    Left (code, lbl) -> do
      logV verbose $ "SGE RechercherServicesAccesDonnees erreur : " <> code <> " " <> lbl
      return Map.empty
    Right r -> do
      let services = maybe [] servicesSouscritsType_serviceSouscrit
                       (rechercherServicesAccesDonneesReponseType_servicesSouscrits r)
          pairs = mapMaybe toPair services
      logV verbose $ "SGE services actifs : "
        <> show [ (c, e, sid)
                | s <- services
                , let e   = concatMap (\x -> [simpleText15 x]) (serviceSouscritType_etatCode s)
                      sid = simpleText15 (serviceSouscritType_serviceSouscritId s)
                      c   = maybe "?" simpleText (serviceSouscritType_mesuresTypeCode s)
                ]
      return (Map.fromList pairs)
  where
    prmStr = T.unpack prmT
    toPair s =
      let etats = map simpleText15 (serviceSouscritType_etatCode s)
      in if "ACTIF" `notElem` etats then Nothing
         else case serviceSouscritType_mesuresTypeCode s of
           Nothing   -> Nothing
           Just code ->
             let codeStr = simpleText code
                 sidStr  = simpleText15 (serviceSouscritType_serviceSouscritId s)
             in Just (codeStr, sidStr)
    simpleText  (MesureTypeCodeType (Xsd.XsdString s)) = s
    simpleText15 (Chaine15Type (Xsd.XsdString s))      = s


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
  resp <- mkWs req :: IO (Either (String, String) CommanderServicesAccesDonneesResponseType)
  return $ void resp
  where
    accordType = case accord of
      AccordNom nom        -> AccordPersonnePhysiqueNom (T.unpack nom)
      AccordDenomination d -> AccordPersonneMoraleDenominationSociale (T.unpack d)
    mkInit = if prod then initType else initTypeTest
    mkWs   = if prod then wsRequest else wsRequestTest


unPrm :: Prm -> Text
unPrm (Prm t) = t
