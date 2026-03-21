{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Export.JSON
  ( exportCurveJSON
  , exportEnergyJSON
  , exportPrmInfoJSON
  ) where

import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import           Data.Aeson
import           Database.SQLite.Simple     (Connection)
import           Conso.Fr.Elec.SgeDB.Storage.Query

-- | Sérialise les courbes de charge en JSON
exportCurveJSON
  :: Connection
  -> Maybe Text -> Maybe Text -> Maybe Text
  -> Text -> Text
  -> IO Value
exportCurveJSON conn mEm mGm mGp deb fin = do
  rows <- queryCurvePoints conn mEm mGm mGp deb fin
  return $ toJSON (map curveRowToJSON rows)
  where
    curveRowToJSON r = object
      [ "etapeMetier"      .= crEtapeMetier r
      , "grandeurMetier"   .= crGrandeurMetier r
      , "grandeurPhysique" .= crGrandeurPhysique r
      , "unite"            .= crUnite r
      , "horodate"         .= crHorodate r
      , "valeur"           .= crValeur r
      , "pas"              .= crPas r
      , "nature"           .= crNature r
      , "typeCompletion"   .= crTypeCompletion r
      , "iv"               .= crIv r
      , "ec"               .= crEc r
      ]

-- | Sérialise les énergies quotidiennes en JSON
exportEnergyJSON
  :: Connection
  -> Maybe Text
  -> Text -> Text
  -> IO Value
exportEnergyJSON conn mGm deb fin = do
  rows <- queryDailyEnergy conn mGm deb fin
  return $ toJSON (map energyRowToJSON rows)
  where
    energyRowToJSON r = object
      [ "etapeMetier"      .= erEtapeMetier r
      , "grandeurMetier"   .= erGrandeurMetier r
      , "grandeurPhysique" .= erGrandeurPhysique r
      , "unite"            .= erUnite r
      , "modeCalcul"       .= erModeCalcul r
      , "dateMesure"       .= erDateMesure r
      , "valeur"           .= erValeur r
      ]

-- | Retourne le JSON brut des informations techniques courantes
exportPrmInfoJSON :: Connection -> IO (Maybe Value)
exportPrmInfoJSON conn = do
  mRow <- queryPrmInfo conn
  case mRow of
    Nothing -> return Nothing
    Just r  ->
      -- Le raw_json est stocké comme texte JSON : on le re-parse
      return $ decodeStrict (TE.encodeUtf8 (piRawJson r))
