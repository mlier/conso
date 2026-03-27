{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Gaz.Adict.ConsosPubliees
Description : Webservice GRDF ADICT — GET \/pce\/{id_pce}\/donnees_consos_publiees

Permet de consulter les données de consommation publiées d'un PCE pour une
période de restitution donnée.

La réponse est au format NDJSON : chaque ligne est un objet 'ConsoRestit'
correspondant à un bordereau de publication.

Usage :

> session <- initSession False   -- bac à sable
> rep     <- consulterConsosPubliees session "12345678901234" (ByPeriode "2024")
> case rep of
>     Left  err -> print err
>     Right lst -> mapM_ print lst

-}
module Conso.Fr.Gaz.Adict.ConsosPubliees
  ( -- * Paramètre de période
    PeriodeParam(..)
    -- * Requêtes
  , consulterConsosPubliees
  , consulterConsosPublieesSandbox
  ) where

import           Data.Text                      ( Text )
import qualified Data.Text                      as T

import           Conso.Fr.Gaz.Adict.Adict
import           Conso.Fr.Gaz.Adict.Types


-- | Spécification de la période de consultation.
data PeriodeParam
    = ByPeriode   Text          -- ^ Période agrégée (ex. @\"2024\"@, @\"2024-01\"@, @\"2024-W03\"@)
    | ByDateRange Text Text     -- ^ Plage de dates @date_debut@ / @date_fin@ (format @YYYY-MM-DD@)
    deriving (Show)


-- | Construit les paramètres de requête à partir d'un 'PeriodeParam'.
buildQueryString :: PeriodeParam -> String
buildQueryString (ByPeriode p)     = "?periode=" <> T.unpack p
buildQueryString (ByDateRange d f) =
    "?date_debut=" <> T.unpack d <> "&date_fin=" <> T.unpack f


-- | Consulte les consommations publiées d'un PCE.
--
-- Retourne la liste des enregistrements NDJSON ou une erreur 'AdictError'.
consulterConsosPubliees
    :: AdictSession   -- ^ Session ADICT (production ou bac à sable)
    -> Text           -- ^ Identifiant du PCE (14 chiffres ou GI + 6 chiffres)
    -> PeriodeParam   -- ^ Période de consultation
    -> IO (Either AdictError [ConsoRestit])
consulterConsosPubliees session pce periode =
    adictGetNDJSON session path
  where
    path = "/pce/" <> T.unpack pce <> "/donnees_consos_publiees" <> buildQueryString periode


-- | Comme 'consulterConsosPubliees' mais initialise automatiquement une session bac à sable.
--   Pratique pour les tests rapides depuis GHCi.
consulterConsosPublieesSandbox :: Text -> PeriodeParam -> IO (Either AdictError [ConsoRestit])
consulterConsosPublieesSandbox pce periode = do
    session <- initSession False False
    consulterConsosPubliees session pce periode
