{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SiteDB.Export.CSV
Description : Export CSV des mesures SgeDB (séparateur @;@, encodage UTF-8)

Chaque fonction lit les données via la couche Query et retourne un 'Text'
CSV avec en-tête, prêt à être écrit dans un fichier ou renvoyé via HTTP.
Le séparateur est le point-virgule (@;@) pour la compatibilité Excel FR.
-}
module Conso.Fr.Elec.SiteDB.Export.CSV
  ( exportCurveCSV
  , exportEnergyCSV
  , exportPmaxCSV
  , exportBillingCSV
  ) where

import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database.SQLite.Simple (Connection)
import           Conso.Fr.Elec.SiteDB.Storage.Query

-- | En-tête CSV pour les courbes de charge
curveHeader :: Text
curveHeader = "etape_metier;grandeur_metier;grandeur_physique;unite;horodate;valeur;pas;nature;type_completion;iv;ec\n"

-- | En-tête CSV pour les énergies quotidiennes
energyHeader :: Text
energyHeader = "etape_metier;grandeur_metier;grandeur_physique;unite;mode_calcul;date_mesure;valeur\n"

-- | En-tête CSV pour les Pmax
pmaxHeader :: Text
pmaxHeader = "etape_metier;grandeur_metier;grandeur_physique;unite;horodate;valeur\n"

-- | En-tête CSV pour les mesures facturantes
billingHeader :: Text
billingHeader = "etape_metier;id_motif_releve;grandeur_metier;grandeur_physique;unite;libelle_grille;libelle_calendrier;id_classe;dbt_mesure;fin_mesure;quantite;libelle_nature;libelle_statut\n"

-- | Export CSV des courbes de charge.
-- Colonnes : @etape_metier;grandeur_metier;grandeur_physique;unite;horodate;valeur;pas;nature;type_completion;iv;ec@
exportCurveCSV
  :: Connection
  -> Maybe Text -- ^ Filtre @etape_metier@ (@\"BRUT\"@ ou @\"BEST\"@, ou 'Nothing')
  -> Maybe Text -- ^ Filtre @grandeur_metier@ (@\"CONS\"@ ou @\"PROD\"@, ou 'Nothing')
  -> Maybe Text -- ^ Filtre @grandeur_physique@ (@\"PA\"@, …, ou 'Nothing')
  -> Text       -- ^ Horodate début (ISO 8601)
  -> Text       -- ^ Horodate fin (ISO 8601)
  -> IO Text
exportCurveCSV conn mEm mGm mGp deb fin = do
  rows <- queryCurvePoints conn mEm mGm mGp deb fin
  return $ curveHeader <> T.concat (map rowToCSV rows)
  where
    rowToCSV r = T.intercalate ";"
      [ crEtapeMetier r, crGrandeurMetier r, crGrandeurPhysique r, crUnite r
      , crHorodate r, crValeur r, crPas r, crNature r
      , maybe "" id (crTypeCompletion r)
      , maybe "" (T.pack . show) (crIv r)
      , maybe "" (T.pack . show) (crEc r)
      ] <> "\n"

-- | Export CSV des énergies quotidiennes.
-- Colonnes : @etape_metier;grandeur_metier;grandeur_physique;unite;mode_calcul;date_mesure;valeur@
exportEnergyCSV
  :: Connection
  -> Maybe Text -- ^ Filtre @grandeur_metier@ (ou 'Nothing' pour tout)
  -> Text       -- ^ Date début (@YYYY-MM-DD@)
  -> Text       -- ^ Date fin (@YYYY-MM-DD@)
  -> IO Text
exportEnergyCSV conn mGm deb fin = do
  rows <- queryDailyEnergy conn mGm deb fin
  return $ energyHeader <> T.concat (map rowToCSV rows)
  where
    rowToCSV r = T.intercalate ";"
      [ erEtapeMetier r, erGrandeurMetier r, erGrandeurPhysique r
      , erUnite r, erModeCalcul r, erDateMesure r, erValeur r
      ] <> "\n"

-- | Export CSV des Pmax quotidiennes.
-- Colonnes : @etape_metier;grandeur_metier;grandeur_physique;unite;horodate;valeur@
exportPmaxCSV
  :: Connection
  -> Maybe Text -- ^ Filtre @grandeur_metier@ (ou 'Nothing')
  -> Text       -- ^ Horodate début (ISO 8601)
  -> Text       -- ^ Horodate fin (ISO 8601)
  -> IO Text
exportPmaxCSV conn mGm deb fin = do
  rows <- queryDailyPmax conn mGm deb fin
  return $ pmaxHeader <> T.concat (map rowToCSV rows)
  where
    rowToCSV r = T.intercalate ";"
      [ pmEtapeMetier r, pmGrandeurMetier r, pmGrandeurPhysique r
      , pmUnite r, pmHorodate r, pmValeur r
      ] <> "\n"

-- | Export CSV des mesures facturantes.
-- Colonnes : @etape_metier;id_motif_releve;grandeur_metier;grandeur_physique;unite;libelle_grille;libelle_calendrier;id_classe;dbt_mesure;fin_mesure;quantite;libelle_nature;libelle_statut@
exportBillingCSV
  :: Connection
  -> Maybe Text -- ^ Filtre @grandeur_metier@ (ou 'Nothing')
  -> Text       -- ^ Date début (@YYYY-MM-DD@)
  -> Text       -- ^ Date fin (@YYYY-MM-DD@)
  -> IO Text
exportBillingCSV conn mGm deb fin = do
  rows <- queryBillingMeasures conn mGm deb fin
  return $ billingHeader <> T.concat (map rowToCSV rows)
  where
    rowToCSV r = T.intercalate ";"
      [ brEtapeMetier r, brIdMotifReleve r, brGrandeurMetier r
      , brGrandeurPhysique r, brUnite r
      , brLibelleGrille r
      , brLibelleCalendrier r
      , brIdClasse r
      , brDbtMesure r, brFinMesure r
      , T.pack (show (brQuantite r))
      , brLibelleNature r, brLibelleStatut r
      ] <> "\n"
