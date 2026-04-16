{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Display.DroitsAccesDisplay () where

import           Brick
import qualified Data.Text              as T

import           Display
import           Conso.Fr.Gaz.Adict.Types


instance Renderable [DroitAcces] where
    toWidget (Left  err) = renderError err
    toWidget (Right lst)
        | null lst  = section "Droits d'accès" [ustr "Aucun droit d'accès."]
        | otherwise = vBox $ map renderDroitAcces lst


renderDroitAcces :: DroitAcces -> Widget ()
renderDroitAcces da = section titre lignes
  where
    titre = maybe "Droit d'accès" T.unpack (da_id_droit_acces da)
    lignes =
        [ maybeField "PCE"              (fmap T.unpack (da_id_pce da))
        , maybeField "Rôle"             (fmap (T.unpack . roleTiersText) (da_role_tiers da))
        , maybeField "Tiers"            (fmap T.unpack (da_raison_sociale_du_tiers da))
        , maybeField "Titulaire"        (fmap T.unpack (da_nom_titulaire da))
        , maybeField "Raison soc. tit." (fmap T.unpack (da_raison_sociale_du_titulaire da))
        , maybeField "Email tit."       (fmap T.unpack (da_courriel_titulaire da))
        , maybeField "Code postal"      (fmap T.unpack (da_code_postal da))
        , maybeField "Début accès"      (fmap T.unpack (da_date_debut_droit_acces da))
        , maybeField "Fin accès"        (fmap T.unpack (da_date_fin_droit_acces da))
        , maybeField "État"             (fmap (T.unpack . etatDroitAccesText) (da_etat_droit_acces da))
        , maybeField "Parcours"         (fmap T.unpack (da_parcours da))
        , maybeField "Périm. conso dbt" (fmap T.unpack (da_perim_donnees_conso_debut da))
        , maybeField "Périm. conso fin" (fmap T.unpack (da_perim_donnees_conso_fin da))
        , maybeField "Périm. contrat."  (fmap T.unpack (da_perim_donnees_contractuelles da))
        , maybeField "Périm. tech."     (fmap T.unpack (da_perim_donnees_techniques da))
        , maybeField "Périm. info."     (fmap T.unpack (da_perim_donnees_informatives da))
        , maybeField "Périm. publ."     (fmap T.unpack (da_perim_donnees_publiees da))
        , maybeField "Statut preuve"    (fmap (T.unpack . statutControlePreuveText) (da_statut_controle_preuve da))
        , maybeField "Lim. preuve"      (fmap T.unpack (da_date_limite_transmission_preuve da))
        ]


instance Renderable RetourDemandeAcces where
    toWidget (Left  err) = renderError err
    toWidget (Right r) = section "Déclaration droit d'accès" lignes
      where
        lignes =
            [ maybeField "Code statut"   (fmap T.unpack (rda_code_statut_traitement r))
            , maybeField "Message"       (fmap T.unpack (rda_message_retour_traitement r))
            , maybeField "ID droit créé" (fmap T.unpack (rda_id_droit_acces r))
            ]


instance Renderable RetourFinAcces where
    toWidget (Left  err) = renderError err
    toWidget (Right r) = section "Révocation droit d'accès" lignes
      where
        lignes =
            [ maybeField "Code statut" (fmap T.unpack (rfa_code_statut_traitement r))
            , maybeField "Message"     (fmap T.unpack (rfa_message_retour_traitement r))
            ]
