{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SiteDB.Orchestration.Backfill
  ( BackfillBesoin(..)
  , BackfillBatch(..)
  , grouperBesoins
  , filtrerDejaDemandes
  , itcDejaDemandeeAujourdhui
  , envoyerBatchMfi
  , envoyerBatchItc
  , chunksOf
  ) where

import           Control.Exception                  (try, SomeException, displayException)
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time

import           Database.SQLite.Simple

import           Conso.Fr.SiteDB.Types              (Prm(..))

import qualified Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10 as M23
import qualified Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10Type as M23T
import qualified Conso.Fr.Elec.Sge.DemandePublicationInformationsTechniquesContractuellesM23V10 as Itc
import qualified Conso.Fr.Elec.Sge.DemandePublicationInformationsTechniquesContractuellesM23V10Type as ItcT
import           Text.XML.HaXml.Schema.PrimitiveTypes   (XsdString(..))


-- ---------------------------------------------------------------------------
-- Types

data BackfillBesoin = BackfillBesoin
  { besoinPrm      :: Prm
  , besoinTypeCode :: Text   -- "ENERGIE", "PMAX", "COURBES", "INDEX"
  , besoinFlux     :: Text   -- "R65", "R66", "R63", "R64"
  , besoinDebut    :: Day
  , besoinFin      :: Day
  } deriving (Show)

data BackfillBatch = BackfillBatch
  { bbFlux      :: Text
  , bbPrms      :: [Prm]
  , bbDebut     :: Text
  , bbFin       :: Text
  , bbAffaireId :: Either Text Text
  } deriving (Show)


-- ---------------------------------------------------------------------------
-- Groupement

grouperBesoins :: [BackfillBesoin] -> Map (Text, Day, Day) (Text, [Prm])
grouperBesoins = foldr step Map.empty
  where
    step b m =
      let key = (besoinTypeCode b, besoinDebut b, besoinFin b)
          val = (besoinFlux b, [besoinPrm b])
      in Map.insertWith (\(fl, ps) (_, ps') -> (fl, ps ++ ps')) key val m


-- ---------------------------------------------------------------------------
-- Déduplication

filtrerDejaDemandes :: Connection -> Text -> Day -> Day -> [Prm] -> IO [Prm]
filtrerDejaDemandes conn typeCode debut fin prms = do
  rows <- query conn
    "SELECT prm FROM elec_backfill_log \
    \ WHERE prm IS NOT NULL \
    \   AND debut = ? AND fin = ? \
    \   AND (   (type_mesure = ? AND date_envoi >= datetime('now', '-7 days')) \
    \        OR statut_cr = 'NON_PUBLIE' \
    \       )"
    (show debut, show fin, typeCode) :: IO [Only Text]
  let deja = Set.fromList (map (\(Only t) -> t) rows)
  return [p | p@(Prm t) <- prms, not (Set.member t deja)]

itcDejaDemandeeAujourdhui :: Connection -> IO Bool
itcDejaDemandeeAujourdhui conn = do
  rows <- query_ conn
    "SELECT COUNT(*) FROM elec_backfill_log \
    \ WHERE type_mesure = 'ITC' \
    \   AND date_envoi >= date('now')" :: IO [Only Int]
  return $ case rows of { [Only n] -> n > 0; _ -> False }


-- ---------------------------------------------------------------------------
-- Envoi

envoyerBatchMfi :: Connection -> [Prm] -> Text -> Text -> (Day, Day) -> IO (Maybe BackfillBatch)
envoyerBatchMfi _    []   _     _     _             = return Nothing
envoyerBatchMfi conn prms flux typeCode (debut, fin) = do
  let prmStrs  = map (\(Prm t) -> T.unpack t) prms
      debutStr = show debut
      finStr   = show fin
      tc       = parseTypeCode typeCode
  result <- try $ do
    req <- M23.initType prmStrs tc (mesuresCorrFor typeCode) debutStr finStr M23T.SensSOUTIRAGE M23T.CadreAccesACCORDCLIENT
    M23.wsRequest req :: IO (Either (String, String) M23T.AffaireId)
  now <- getCurrentTime
  let nowStr  = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now
      affaire = case result of
        Left  e                                       -> Left (T.pack (displayException (e :: SomeException)))
        Right (Left  (code, lbl))                     -> Left (T.pack code <> " — " <> T.pack lbl)
        Right (Right (M23T.AffaireId (XsdString s))) -> Right (T.pack s)
      mAffId  = either (const Nothing) Just affaire
  mapM_ (\(Prm t) ->
    execute conn
      "INSERT INTO elec_backfill_log (type_mesure, prm, debut, fin, date_envoi, affaire_id) \
      \ VALUES (?,?,?,?,?,?)"
      (typeCode, t, debutStr, finStr, nowStr, mAffId)
    ) prms
  return $ Just BackfillBatch
    { bbFlux      = flux
    , bbPrms      = prms
    , bbDebut     = T.pack debutStr
    , bbFin       = T.pack finStr
    , bbAffaireId = affaire
    }

envoyerBatchItc :: Connection -> [Prm] -> IO (Maybe BackfillBatch)
envoyerBatchItc _    []   = return Nothing
envoyerBatchItc conn prms = do
  let prmStrs = map (\(Prm t) -> T.unpack t) prms
  result <- try $ do
    req <- Itc.initType prmStrs ItcT.Sens_SOUTIRAGE ItcT.CadreAcces_ACCORD_CLIENT
    Itc.wsRequest req :: IO (Either (String, String) ItcT.AffaireId)
  now <- getCurrentTime
  let nowStr  = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now
      affaire = case result of
        Left  e                                        -> Left (T.pack (displayException (e :: SomeException)))
        Right (Left  (code, lbl))                      -> Left (T.pack code <> " — " <> T.pack lbl)
        Right (Right (ItcT.AffaireId (XsdString s)))  -> Right (T.pack s)
      mAffId  = either (const Nothing) Just affaire
  mapM_ (\(Prm t) ->
    execute conn
      "INSERT INTO elec_backfill_log (type_mesure, prm, debut, fin, date_envoi, affaire_id) \
      \ VALUES (?,?,?,?,?,?)"
      ("ITC" :: Text, t, "" :: Text, "" :: Text, nowStr, mAffId)
    ) prms
  return $ Just BackfillBatch
    { bbFlux      = "C68"
    , bbPrms      = prms
    , bbDebut     = ""
    , bbFin       = ""
    , bbAffaireId = affaire
    }


-- ---------------------------------------------------------------------------
-- Helpers

parseTypeCode :: Text -> M23T.MesuresTypeCode
parseTypeCode "COURBES" = M23T.MesuresTypeCodeCOURBES
parseTypeCode "PMAX"    = M23T.MesuresTypeCodePMAX
parseTypeCode "INDEX"   = M23T.MesuresTypeCodeINDEX
parseTypeCode _         = M23T.MesuresTypeCodeENERGIE

mesuresCorrFor :: Text -> Maybe M23T.MesuresCorrigees
mesuresCorrFor "COURBES" = Just (M23T.MesuresCorrigees False)
mesuresCorrFor _         = Nothing

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
