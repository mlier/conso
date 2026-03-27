{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Gaz.Adict.DroitsAcces
Description : Webservice GRDF ADICT — GET\/POST \/droits_acces

Deux opérations :

  * @GET \/droits_acces@ — consulter l'ensemble de mes droits d'accès
  * @POST \/droits_acces@ — rechercher des droits d'accès avec filtres

Les deux retournent du NDJSON (une ligne = un 'DroitAcces').

Usage :

> session <- initSession False
> -- Tous mes droits d'accès :
> rep <- consulterDroitsAcces session
> -- Avec filtres :
> let filtre = FiltreAcces (Just "AUTORISE_CONTRAT_FOURNITURE") (Just "12345678901234") Nothing Nothing
> rep <- rechercherDroitsAcces session filtre
-}
module Conso.Fr.Gaz.Adict.DroitsAcces
  ( consulterDroitsAcces
  , consulterDroitsAccesSandbox
  , rechercherDroitsAcces
  , rechercherDroitsAccesSandbox
  , consulterPreuvesAFournir
  ) where

import           Data.Aeson                 ( toJSON )

import           Conso.Fr.Gaz.Adict.Adict
import           Conso.Fr.Gaz.Adict.Types


-- | Consulte tous mes droits d'accès (@GET \/droits_acces@).
consulterDroitsAcces :: AdictSession -> IO (Either AdictError [DroitAcces])
consulterDroitsAcces session = adictGetNDJSON session "/droits_acces"


-- | Comme 'consulterDroitsAcces' avec une session bac à sable auto-initialisée.
consulterDroitsAccesSandbox :: IO (Either AdictError [DroitAcces])
consulterDroitsAccesSandbox = do
    session <- initSession False False
    consulterDroitsAcces session


-- | Recherche des droits d'accès avec filtres (@POST \/droits_acces@).
rechercherDroitsAcces
    :: AdictSession
    -> FiltreAcces
    -> IO (Either AdictError [DroitAcces])
rechercherDroitsAcces session filtre =
    adictPostNDJSON session "/droits_acces" (toJSON filtre)


-- | Consulte les droits d'accès dont la preuve est en attente de transmission
--   (@GET \/droits_acces?statut_controle=1@).
consulterPreuvesAFournir :: AdictSession -> IO (Either AdictError [DroitAcces])
consulterPreuvesAFournir session =
    adictGetNDJSON session "/droits_acces?statut_controle=1"


-- | Comme 'rechercherDroitsAcces' avec une session bac à sable auto-initialisée.
rechercherDroitsAccesSandbox :: FiltreAcces -> IO (Either AdictError [DroitAcces])
rechercherDroitsAccesSandbox filtre = do
    session <- initSession False False
    rechercherDroitsAcces session filtre
