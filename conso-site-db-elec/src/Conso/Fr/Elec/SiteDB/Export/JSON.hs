{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SiteDB.Export.JSON
  ( exportCurveJSON
  , exportEnergyJSON
  ) where

import           Data.Text                  (Text)
import           Data.Aeson
import           Database.SQLite.Simple     (Connection)
import           Conso.Fr.Elec.SiteDB.Storage.Query

-- | Sérialise les courbes de charge en tableau JSON Aeson.
exportCurveJSON
  :: Connection
  -> Maybe Text -- ^ Filtre @etape_metier@
  -> Maybe Text -- ^ Filtre @grandeur_metier@
  -> Maybe Text -- ^ Filtre @grandeur_physique@
  -> Text       -- ^ Horodate début (ISO 8601)
  -> Text       -- ^ Horodate fin (ISO 8601)
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

-- | Sérialise les énergies quotidiennes en tableau JSON Aeson.
exportEnergyJSON
  :: Connection
  -> Maybe Text -- ^ Filtre @grandeur_metier@
  -> Text       -- ^ Date début (@YYYY-MM-DD@)
  -> Text       -- ^ Date fin (@YYYY-MM-DD@)
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
      , "dateMesure"       .= erDate r
      , "valeur"           .= erValeur r
      ]

