{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Display.ConsoDisplay () where

import           Brick
import qualified Data.Text                  as T

import           Display
import           Conso.Fr.Gaz.Adict.Types


-- | Instance Renderable pour une liste de ConsoRestit (résultat NDJSON).
instance Renderable [ConsoRestit] where
    toWidget (Left  err) = renderError err
    toWidget (Right lst)
        | null lst  = section "Consommations" [ustr "Aucune donnée."]
        | otherwise = vBox $ map renderConso lst


instance Renderable [InjectionRestit] where
    toWidget (Left  err) = renderError err
    toWidget (Right lst)
        | null lst  = section "Injections" [ustr "Aucune donnée."]
        | otherwise = vBox $ map renderInjection lst


renderInjection :: InjectionRestit -> Widget ()
renderInjection ir = section titre lignes
  where
    titre = maybe "Injection" (\i -> maybe "Injection" T.unpack (inj_journee_gaziere i)) (ir_injection ir)
         <> maybe "" (\p -> " [" <> T.unpack (valeur p) <> "]") (ir_periode ir)
    lignes =
        [ maybeField "PCE"          (fmap (T.unpack . id_pce) (ir_pce ir))
        , renderInjectionDetail (ir_injection ir)
        , renderStatut (ir_statut_restitution ir)
        ]


renderInjectionDetail :: Maybe Injection -> Widget ()
renderInjectionDetail Nothing  = emptyWidget
renderInjectionDetail (Just i) = vBox
    [ maybeField "Début"           (fmap T.unpack (date_debut_injection i))
    , maybeField "Fin"             (fmap T.unpack (date_fin_injection i))
    , maybeField "Énergie (kWh)"   (fmap show (inj_energie i))
    , maybeField "Volume brut"     (fmap show (inj_volume_brut i))
    , maybeField "Volume conv."    (fmap show (inj_volume_converti i))
    , maybeField "Qualif."         (fmap T.unpack (type_qualif_injection i))
    , maybeField "Statut"          (fmap T.unpack (statut_injection i))
    , maybeField "Type injection"  (fmap T.unpack (type_injection i))
    , renderCoeff (inj_coeff_calcul i)
    ]


renderConso :: ConsoRestit -> Widget ()
renderConso cr = section titre lignes
  where
    titre = maybe "Consommation" (\c -> maybe "Consommation" T.unpack (journee_gaziere c)) (cr_consommation cr)
         <> maybe "" (\p -> " [" <> T.unpack (valeur p) <> "]") (cr_periode cr)
    lignes =
        [ maybeField "PCE"          (fmap (T.unpack . id_pce) (cr_pce cr))
        , renderConsoDetail (cr_consommation cr)
        , renderStatut (cr_statut_restitution cr)
        ]


renderConsoDetail :: Maybe Consommation -> Widget ()
renderConsoDetail Nothing  = emptyWidget
renderConsoDetail (Just c) = vBox
    [ maybeField "Début"        (fmap T.unpack (date_debut_consommation c))
    , maybeField "Fin"          (fmap T.unpack (date_fin_consommation c))
    , maybeField "Énergie (kWh)" (fmap show (energie c))
    , maybeField "Volume brut"  (fmap show (volume_brut c))
    , maybeField "Volume conv." (fmap show (volume_converti c))
    , maybeField "Qualif."      (fmap T.unpack (type_qualif_conso c))
    , maybeField "Statut"       (fmap T.unpack (statut_conso c))
    , maybeField "Type conso"   (fmap T.unpack (type_conso c))
    , renderCoeff (coeff_calcul c)
    ]


renderCoeff :: Maybe CoeffCalcul -> Widget ()
renderCoeff Nothing  = emptyWidget
renderCoeff (Just k) = vBox
    [ maybeField "Coeff PTA"   (fmap show (coeff_pta k))
    , maybeField "PCS"         (fmap show (valeur_pcs k))
    , maybeField "Coeff conv." (fmap show (coeff_conversion k))
    ]


renderStatut :: Maybe StatutRestitution -> Widget ()
renderStatut Nothing  = emptyWidget
renderStatut (Just s) = case (sr_code s, sr_message s) of
    (Just c, Just m) -> withAttr errorAttr $ ustr $ T.unpack c <> " — " <> T.unpack m
    (Just c, _)      -> withAttr errorAttr $ ustr $ T.unpack c
    _                -> emptyWidget
