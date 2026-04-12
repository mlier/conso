{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Gaz.Adict.InjectionsPubliees
Description : Webservice GRDF ADICT — GET \/pce\/{id_pce}\/donnees_injections_publiees

Permet de consulter les données d'injection publiées d'un PCE
(applicable aux producteurs raccordés au réseau de distribution GRDF).

La réponse est au format NDJSON : chaque ligne est un objet 'InjectionRestit'.

Usage :

> session <- initSession False
> rep     <- consulterInjectionsPubliees session "12345678901234" (ByPeriode "2024")
-}
module Conso.Fr.Gaz.Adict.InjectionsPubliees
  ( consulterInjectionsPubliees
  , consulterInjectionsPublieesSandbox
  ) where

import           Data.Text                    ( Text )
import qualified Data.Text                    as T

import           Conso.Fr.Gaz.Adict.Adict
import           Conso.Fr.Gaz.Adict.Types
import           Conso.Fr.Gaz.Adict.ConsosPubliees ( PeriodeParam(..) )


buildQueryString :: PeriodeParam -> String
buildQueryString (ByPeriode p)     = "?periode=" <> T.unpack p
buildQueryString (ByDateRange d f) =
    "?date_debut=" <> T.unpack d <> "&date_fin=" <> T.unpack f


-- | Consulte les injections publiées d'un PCE.
consulterInjectionsPubliees
    :: AdictSession
    -> Text          -- ^ Identifiant du PCE
    -> PeriodeParam  -- ^ Période de consultation
    -> IO (Either AdictError [InjectionRestit])
consulterInjectionsPubliees session pce periode =
    adictGetNDJSON session path
  where
    path = "/pce/" <> T.unpack pce <> "/donnees_injections_publiees" <> buildQueryString periode


-- | Comme 'consulterInjectionsPubliees' avec une session bac à sable auto-initialisée.
consulterInjectionsPublieesSandbox :: Text -> PeriodeParam -> IO (Either AdictError [InjectionRestit])
consulterInjectionsPublieesSandbox pce periode = do
    session <- initSession False False False
    consulterInjectionsPubliees session pce periode
