{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Site.SiteDB.Elec.Export.JSON
Description : Re-sérialisation JSON des mesures SgeDB (via Aeson)

Les fonctions retournent un 'Value' Aeson avec des noms de champs camelCase
(ex. @etapeMetier@, @grandeurMetier@) cohérents avec la structure d'origine
des flux Enedis.

'exportPrmInfoJSON' est particulier : il re-parse le texte JSON brut stocké
dans la colonne @raw_json@ de @prm_info@, permettant de retourner l'arbre
JSON C68 complet.
-}
module Conso.Fr.Site.SiteDB.Elec.Export.JSON
  ( exportCurveJSON
  , exportEnergyJSON
  , exportPrmInfoJSON
  ) where

import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import           Data.Aeson
import           Database.SQLite.Simple     (Connection)
import           Conso.Fr.Site.SiteDB.Elec.Storage.Query

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
      , "dateMesure"       .= erDateMesure r
      , "valeur"           .= erValeur r
      ]

-- | Retourne le JSON C68 complet de la dernière 'PrmInfoRow' ingérée.
-- Re-parse le texte @raw_json@ stocké en base vers un 'Value' Aeson.
-- Retourne 'Nothing' si aucune info C68 n'a été ingérée pour ce PRM.
exportPrmInfoJSON :: Connection -> IO (Maybe Value)
exportPrmInfoJSON conn = do
  mRow <- queryPrmInfo conn
  case mRow of
    Nothing -> return Nothing
    Just r  ->
      -- Le raw_json est stocké comme texte JSON : on le re-parse
      return $ decodeStrict (TE.encodeUtf8 (piRawJson r))
