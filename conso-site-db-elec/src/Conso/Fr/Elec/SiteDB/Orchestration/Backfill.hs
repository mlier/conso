{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SiteDB.Orchestration.Backfill
  ( BackfillDemande(..)
  , envoyerBackfill
  , envoyerSiNonRecent
  ) where

import           Control.Exception                  (try, SomeException, displayException)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time

import           Database.SQLite.Simple

import           Conso.Fr.SiteDB.Types              (Prm(..))

import qualified Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10 as M23
import           Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10Type
  ( MesuresTypeCode(..), CadreAcces(..), Sens(..), AffaireId(..) )
import           Text.XML.HaXml.Schema.PrimitiveTypes   (XsdString(..))

data BackfillDemande = BackfillDemande
  { bdPrm       :: Prm
  , bdFlux      :: Text
  , bdDebut     :: Text
  , bdFin       :: Text
  , bdAffaireId :: Either Text Text
  } deriving (Show)

envoyerBackfill :: Prm -> Text -> Text -> (Day, Day) -> IO BackfillDemande
envoyerBackfill prm@(Prm prmText) fluxLabel typeCode (debut, fin) = do
  let debutStr = show debut
      finStr   = show fin
      tc       = parseTypeCode typeCode
  result <- try $ do
    req <- M23.initType [T.unpack prmText] tc Nothing debutStr finStr
              SensSOUTIRAGE CadreAccesACCORDCLIENT
    M23.wsRequest req :: IO (Either (String, String) AffaireId)
  return $ BackfillDemande prm fluxLabel (T.pack debutStr) (T.pack finStr) $
    case result of
      Left  e          -> Left (T.pack (displayException (e :: SomeException)))
      Right (Left  (code, lbl)) -> Left (T.pack code <> " — " <> T.pack lbl)
      Right (Right (AffaireId (XsdString s))) -> Right (T.pack s)

parseTypeCode :: Text -> MesuresTypeCode
parseTypeCode "COURBES" = MesuresTypeCodeCOURBES
parseTypeCode "PMAX"    = MesuresTypeCodePMAX
parseTypeCode "INDEX"   = MesuresTypeCodeINDEX
parseTypeCode _         = MesuresTypeCodeENERGIE

-- | Envoie une demande M023 uniquement si aucune demande identique n'a été
-- envoyée dans les 7 derniers jours. Logue le résultat dans @elec_backfill_log@.
envoyerSiNonRecent
  :: Connection
  -> Prm
  -> Text       -- ^ Libellé flux (ex. @"R63"@, @"R65/R66"@)
  -> Text       -- ^ Type mesure M023 (ex. @"COURBES"@, @"ENERGIE"@, @"INDEX"@)
  -> (Day, Day)
  -> IO (Maybe BackfillDemande)
envoyerSiNonRecent conn prm fluxLabel typeCode periode@(debut, fin) = do
  [Only n] <- query conn
    "SELECT COUNT(*) FROM elec_backfill_log \
    \ WHERE type_mesure = ? AND debut = ? AND fin = ? \
    \   AND date_envoi >= datetime('now', '-7 days')"
    (typeCode, show debut, show fin) :: IO [Only Int]
  if n > 0
    then return Nothing
    else do
      demande <- envoyerBackfill prm fluxLabel typeCode periode
      now <- getCurrentTime
      let nowStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now
          mAffId = case bdAffaireId demande of
                     Left  _ -> Nothing
                     Right t -> Just t
      execute conn
        "INSERT INTO elec_backfill_log (type_mesure, debut, fin, date_envoi, affaire_id) \
        \ VALUES (?,?,?,?,?)"
        (typeCode, show debut, show fin, nowStr, mAffId)
      return (Just demande)
