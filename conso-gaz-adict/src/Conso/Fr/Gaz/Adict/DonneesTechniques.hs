{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Gaz.Adict.DonneesTechniques
Description : Webservice GRDF ADICT — GET \/pce\/{id_pce}\/donnees_techniques

Permet de consulter les données techniques d'un PCE :
adresse du compteur, caractéristiques techniques, PITD et régime de propriété.

La réponse est un unique objet JSON.

Usage :

> session <- initSession False
> rep     <- consulterDonneesTechniques session "12345678901234"
-}
module Conso.Fr.Gaz.Adict.DonneesTechniques
  ( consulterDonneesTechniques
  , consulterDonneesTechniquesSandbox
  ) where

import           Data.Text                  ( Text )
import qualified Data.Text                  as T

import           Conso.Fr.Gaz.Adict.Adict
import           Conso.Fr.Gaz.Adict.Types


-- | Consulte les données techniques d'un PCE.
consulterDonneesTechniques
    :: AdictSession
    -> Text    -- ^ Identifiant du PCE
    -> IO (Either AdictError RetourDonneesTechniques)
consulterDonneesTechniques session pce =
    adictGet session ("/pce/" <> T.unpack pce <> "/donnees_techniques")


-- | Comme 'consulterDonneesTechniques' avec une session bac à sable auto-initialisée.
consulterDonneesTechniquesSandbox :: Text -> IO (Either AdictError RetourDonneesTechniques)
consulterDonneesTechniquesSandbox pce = do
    session <- initSession False False False
    consulterDonneesTechniques session pce
