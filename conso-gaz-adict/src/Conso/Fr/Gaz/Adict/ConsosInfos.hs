{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Gaz.Adict.ConsosInfos
Description : Webservice GRDF ADICT — GET \/pce\/{id_pce}\/donnees_consos_informatives

Permet de consulter les données de consommation informatives d'un PCE pour une
période de restitution donnée. Les consommations informatives sont des données
journalières estimées, disponibles sur une profondeur maximale de 3 ans.

La réponse est au format NDJSON : chaque ligne est un objet 'ConsoRestit'.

Usage :

> session <- initSession False
> rep     <- consulterConsosInfos session "12345678901234" (ByPeriode "2024")
-}
module Conso.Fr.Gaz.Adict.ConsosInfos
  ( consulterConsosInfos
  , consulterConsosInfosSandbox
  ) where

import           Data.Text                    ( Text )
import qualified Data.Text                    as T

import           Conso.Fr.Gaz.Adict.Adict
import           Conso.Fr.Gaz.Adict.Types
import           Conso.Fr.Gaz.Adict.ConsosPubliees ( PeriodeParam(..) )


-- | Construit les paramètres de requête.
buildQueryString :: PeriodeParam -> String
buildQueryString (ByPeriode p)     = "?periode=" <> T.unpack p
buildQueryString (ByDateRange d f) =
    "?date_debut=" <> T.unpack d <> "&date_fin=" <> T.unpack f


-- | Consulte les consommations informatives d'un PCE.
consulterConsosInfos
    :: AdictSession
    -> Text          -- ^ Identifiant du PCE
    -> PeriodeParam  -- ^ Période de consultation
    -> IO (Either AdictError [ConsoRestit])
consulterConsosInfos session pce periode =
    adictGetNDJSON session path
  where
    path = "/pce/" <> T.unpack pce <> "/donnees_consos_informatives" <> buildQueryString periode


-- | Comme 'consulterConsosInfos' avec une session bac à sable auto-initialisée.
consulterConsosInfosSandbox :: Text -> PeriodeParam -> IO (Either AdictError [ConsoRestit])
consulterConsosInfosSandbox pce periode = do
    session <- initSession False False False
    consulterConsosInfos session pce periode
