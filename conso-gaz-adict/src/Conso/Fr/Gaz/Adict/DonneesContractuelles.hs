{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Gaz.Adict.DonneesContractuelles
Description : Webservice GRDF ADICT — GET \/pce\/{id_pce}\/donnees_contractuelles

Permet de consulter les données contractuelles d'un PCE :
tarif d'acheminement, consommation annuelle de référence (CAR),
consommation journalière annualisée (CJA), profil et modulation.

La réponse est un unique objet JSON (non NDJSON).

Usage :

> session <- initSession False
> rep     <- consulterDonneesContractuelles session "12345678901234" []
-}
module Conso.Fr.Gaz.Adict.DonneesContractuelles
  ( consulterDonneesContractuelles
  , consulterDonneesContractuellesSandbox
  ) where

import           Data.Text                    ( Text )
import qualified Data.Text                    as T

import           Conso.Fr.Gaz.Adict.Adict
import           Conso.Fr.Gaz.Adict.Types


-- | Consulte les données contractuelles d'un PCE.
--
-- Le paramètre @filtres@ permet de limiter la réponse à certains blocs
-- (ex. @[\"cja\", \"car\"]@). Passer @[]@ pour tout récupérer.
consulterDonneesContractuelles
    :: AdictSession
    -> Text          -- ^ Identifiant du PCE
    -> [Text]        -- ^ Filtres optionnels (ex. @[\"cja\",\"car\"]@)
    -> IO (Either AdictError RetourDonneesContractuelles)
consulterDonneesContractuelles session pce filtres =
    adictGet session path
  where
    path = "/pce/" <> T.unpack pce <> "/donnees_contractuelles" <> queryString
    queryString
        | null filtres = ""
        | otherwise    = "?" <> concatMap (\f -> "filtre=" <> T.unpack f <> "&") filtres


-- | Comme 'consulterDonneesContractuelles' avec une session bac à sable auto-initialisée.
consulterDonneesContractuellesSandbox
    :: Text -> [Text] -> IO (Either AdictError RetourDonneesContractuelles)
consulterDonneesContractuellesSandbox pce filtres = do
    session <- initSession False False False
    consulterDonneesContractuelles session pce filtres
