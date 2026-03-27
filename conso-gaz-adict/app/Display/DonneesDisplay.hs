{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Display.DonneesDisplay () where

import           Brick
import           Data.Aeson             ( Value, encode )
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T

import           Display
import           Conso.Fr.Gaz.Adict.Types


instance Renderable RetourDonneesContractuelles where
    toWidget (Left  err) = renderError err
    toWidget (Right r)   = section "Données contractuelles" lignes
      where
        lignes = [maybeField "PCE" (fmap (T.unpack . id_pce) (rdc_pce r))]
              ++ maybe [] renderContrat (rdc_donnees r)

renderContrat :: DonneesContractuelles -> [Widget ()]
renderContrat dc =
    [ maybeField "Date MES"           (fmap T.unpack (dc_date_mes dc))
    , maybeField "Tarif acheminement" (fmap T.unpack (dc_tarif_acheminement dc))
    , maybeField "Date publication"   (fmap T.unpack (dc_date_publication dc))
    , maybeField "Conso plafond"      (fmap T.unpack (dc_consommation_journaliere_plafond dc))
    , maybe emptyWidget (\v -> field "CAR"       (jsonCompact v)) (dc_car dc)
    , maybe emptyWidget (\v -> field "CJA"       (jsonCompact v)) (dc_cja dc)
    , maybe emptyWidget (\v -> field "Profil"    (jsonCompact v)) (dc_profil dc)
    , maybe emptyWidget (\v -> field "Modulation"(jsonCompact v)) (dc_modulation dc)
    ]

jsonCompact :: Value -> String
jsonCompact = T.unpack . T.decodeUtf8 . LBS.toStrict . encode


instance Renderable RetourDonneesTechniques where
    toWidget (Left  err) = renderError err
    toWidget (Right r)   = section "Données techniques" lignes
      where
        lignes = [maybeField "PCE" (fmap (T.unpack . id_pce) (rdt_pce r))]
              ++ maybe [] renderTech (rdt_donnees r)

renderTech :: DonneesTechniques -> [Widget ()]
renderTech dt =
    maybe [] renderSituation (dt_situation_compteur dt)
    ++ [renderPitd (dt_pitd dt)]
    ++ [ maybe emptyWidget (\v -> field "Caractéristiques" (jsonCompact v))
                           (dt_caracteristiques_compteur dt)
       , maybe emptyWidget (\v -> field "Régime propriété" (jsonCompact v))
                           (dt_regime_propriete dt)
       ]

renderSituation :: SituationCompteurDetail -> [Widget ()]
renderSituation s =
    [ maybeField "Numéro rue"  (fmap T.unpack (numero_rue s))
    , maybeField "Nom rue"     (fmap T.unpack (nom_rue s))
    , maybeField "Complément"  (fmap T.unpack (complement_adresse s))
    , maybeField "Code postal" (fmap T.unpack (scd_code_postal s))
    , maybeField "Commune"     (fmap T.unpack (commune s))
    ]

renderPitd :: Maybe PitdDetail -> Widget ()
renderPitd Nothing  = emptyWidget
renderPitd (Just p) = vBox
    [ maybeField "PITD id"      (fmap T.unpack (identifiant_pitd p))
    , maybeField "PITD libellé" (fmap T.unpack (libelle_pitd p))
    ]
