{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Gaz.Adict.DroitAcces
Description : Webservice GRDF ADICT — PUT \/pce\/{id_pce}\/droit_acces et PATCH \/droit_acces\/{id}

Deux opérations sur un droit d'accès individuel :

  * @PUT \/pce\/{id_pce}\/droit_acces@ — déclarer un nouveau droit d'accès
  * @PATCH \/droit_acces\/{id_droit_acces}@ — révoquer un droit d'accès existant

Usage :

> session <- initSession False
> -- Déclarer un droit d'accès :
> let demande = DemandeAccesIn
>       { din_role_tiers        = "AUTORISE_CONTRAT_FOURNITURE"
>       , din_raison_sociale    = Just "Ma Société SAS"
>       , din_nom_titulaire     = Nothing
>       , din_code_postal       = "75001"
>       , din_courriel_titulaire = Just "client@example.com"
>       , din_numero_telephone_mobile_titulaire = Nothing
>       , din_date_debut_droit_acces = Just "2024-01-01"
>       , din_date_fin_droit_acces   = Just "2025-01-01"
>       , din_perim_donnees_conso_debut = Just "2023-01-01"
>       , din_perim_donnees_conso_fin   = Just "2025-01-01"
>       , din_perim_donnees_inj_debut   = Nothing
>       , din_perim_donnees_inj_fin     = Nothing
>       , din_perim_donnees_contractuelles = Just True
>       , din_perim_donnees_techniques     = Just True
>       , din_perim_donnees_informatives   = Just True
>       , din_perim_donnees_publiees       = Just True
>       }
> rep <- declarerDroitAcces session "12345678901234" demande
>
> -- Révoquer un droit d'accès :
> rep <- revoquerDroitAcces session "3044b042-2f6a-4172-9a75-b7e1bbbb0cfd"
-}
module Conso.Fr.Gaz.Adict.DroitAcces
  ( declarerDroitAcces
  , declarerDroitAccesSandbox
  , revoquerDroitAcces
  , revoquerDroitAccesSandbox
  ) where

import           Data.Text                  ( Text )
import qualified Data.Text                  as T

import           Conso.Fr.Gaz.Adict.Adict
import           Conso.Fr.Gaz.Adict.Types


-- | Déclare un droit d'accès aux données d'un PCE (@PUT \/pce\/{id_pce}\/droit_acces@).
--
-- Retourne le résultat de la demande contenant le code statut et éventuellement
-- l'identifiant du droit créé.
declarerDroitAcces
    :: AdictSession
    -> Text             -- ^ Identifiant du PCE
    -> DemandeAccesIn   -- ^ Paramètres de la demande d'accès
    -> IO (Either AdictError RetourDemandeAcces)
declarerDroitAcces session pce =
    adictPut session ("/pce/" <> T.unpack pce <> "/droit_acces")


-- | Comme 'declarerDroitAcces' avec une session bac à sable auto-initialisée.
declarerDroitAccesSandbox
    :: Text -> DemandeAccesIn -> IO (Either AdictError RetourDemandeAcces)
declarerDroitAccesSandbox pce demande = do
    session <- initSession False False False
    declarerDroitAcces session pce demande


-- | Révoque un droit d'accès (@PATCH \/droit_acces\/{id_droit_acces}@).
--
-- L'identifiant du droit d'accès est l'UUID retourné par 'declarerDroitAcces'
-- ou listé dans 'consulterDroitsAcces'.
revoquerDroitAcces
    :: AdictSession
    -> Text    -- ^ UUID du droit d'accès à révoquer
    -> IO (Either AdictError RetourFinAcces)
revoquerDroitAcces session idDroitAcces =
    adictPatch session ("/droit_acces/" <> T.unpack idDroitAcces)


-- | Comme 'revoquerDroitAcces' avec une session bac à sable auto-initialisée.
revoquerDroitAccesSandbox :: Text -> IO (Either AdictError RetourFinAcces)
revoquerDroitAccesSandbox idDroitAcces = do
    session <- initSession False False False
    revoquerDroitAcces session idDroitAcces
