{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SiteDB.Orchestration.Backfill
  ( BackfillDemande(..)
  , envoyerBackfill
  ) where

import           Control.Exception                  (try, SomeException, displayException)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time                          (Day)

import           Conso.Fr.SiteDB.Types              (Prm(..))

import qualified Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10 as M23
import           Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10Type
  ( MesuresTypeCode(..), CadreAcces(..), Sens(..), AffaireId )

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
      Right (Right affId)       -> Right (T.pack (show affId))

parseTypeCode :: Text -> MesuresTypeCode
parseTypeCode "COURBES" = MesuresTypeCodeCOURBES
parseTypeCode "PMAX"    = MesuresTypeCodePMAX
parseTypeCode "INDEX"   = MesuresTypeCodeINDEX
parseTypeCode _         = MesuresTypeCodeENERGIE
