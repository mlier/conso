{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-|
Module      : Conso.Fr.Gaz.Adict.Api
Description : Définition Servant de l'API GRDF ADICT et fonctions client générées

Ce module définit :

  * Le type de contenu 'NDJSON' (@application\/x-ndjson@) avec son instance
    'MimeUnrender' qui parse chaque ligne JSON séparément.
  * Le type 'AdictAPI' qui décrit l'ensemble des endpoints de l'API GRDF ADICT
    sous forme de types Servant.
  * Les fonctions client générées via 'client' : 'getConsosPubliees',
    'getDroitsAcces', etc. — utilisables avec 'runAdictClient'.

Usage depuis GHCi ou un module service :

> session <- initSession False
> rep <- runAdictClient session (getDonneesTechniques "12345678901234")
-}
module Conso.Fr.Gaz.Adict.Api
  ( -- * Content type NDJSON
    NDJSON
    -- * Définition de l'API
  , AdictAPI
  , adictAPI
    -- * Fonctions client générées
  , getConsosPubliees
  , getConsosInfos
  , getInjectionsPubliees
  , getDonneesContractuelles
  , getDonneesTechniques
  , getDroitsAcces
  , postDroitsAcces
  , putDroitAcces
  , patchDroitAcces
  ) where

import qualified Data.ByteString.Lazy         as LBS
import           Data.Aeson                   ( FromJSON, eitherDecode )
import           Data.Proxy                   ( Proxy(..) )
import           Data.Text                    ( Text )
import           Network.HTTP.Media           ( (//) )
import           Servant.API
import           Servant.Client               ( ClientM, client )

import           Conso.Fr.Gaz.Adict.Types


-- ---------------------------------------------------------------------------
-- Content type NDJSON

-- | Type fantôme représentant le format @application\/x-ndjson@ :
--   chaque ligne du corps de réponse est un objet JSON indépendant.
data NDJSON

instance Accept NDJSON where
    contentType _ = "application" // "x-ndjson"

-- | Décode un corps NDJSON en liste : chaque ligne non vide est décodée
--   séparément.  Le premier échec de décodage est propagé comme erreur.
instance FromJSON a => MimeUnrender NDJSON [a] where
    mimeUnrender _ bs =
        let ls = filter (not . LBS.null) $ LBS.split 10 bs
        in mapM eitherDecode ls


-- ---------------------------------------------------------------------------
-- Définition de l'API GRDF ADICT

-- | Type Servant décrivant l'ensemble des endpoints de l'API GRDF ADICT v2.
--
-- Notes :
--
--   * Les endpoints NDJSON utilisent le content type 'NDJSON'.
--   * Les paramètres de période sont optionnels ; les modules service
--     (@ConsosPubliees@, etc.) construisent les bons paramètres.
--   * Le endpoint @POST \/pce\/{id_pce}\/preuves@ (multipart) est géré
--     séparément dans "Conso.Fr.Gaz.Adict.Preuves".
type AdictAPI =
         -- GET /pce/{id_pce}/donnees_consos_publiees
         "pce" :> Capture "id_pce" Text
               :> "donnees_consos_publiees"
               :> QueryParam "periode"    Text
               :> QueryParam "date_debut" Text
               :> QueryParam "date_fin"   Text
               :> Get '[NDJSON] [ConsoRestit]
    -- GET /pce/{id_pce}/donnees_consos_informatives
    :<|> "pce" :> Capture "id_pce" Text
               :> "donnees_consos_informatives"
               :> QueryParam "periode"    Text
               :> QueryParam "date_debut" Text
               :> QueryParam "date_fin"   Text
               :> Get '[NDJSON] [ConsoRestit]
    -- GET /pce/{id_pce}/donnees_injections_publiees
    :<|> "pce" :> Capture "id_pce" Text
               :> "donnees_injections_publiees"
               :> QueryParam "periode"    Text
               :> QueryParam "date_debut" Text
               :> QueryParam "date_fin"   Text
               :> Get '[NDJSON] [InjectionRestit]
    -- GET /pce/{id_pce}/donnees_contractuelles
    :<|> "pce" :> Capture "id_pce" Text
               :> "donnees_contractuelles"
               :> QueryParams "filtre" Text
               :> Get '[JSON] RetourDonneesContractuelles
    -- GET /pce/{id_pce}/donnees_techniques
    :<|> "pce" :> Capture "id_pce" Text
               :> "donnees_techniques"
               :> Get '[JSON] RetourDonneesTechniques
    -- GET /droits_acces
    :<|> "droits_acces"
               :> QueryParam "statut_controle" Int
               :> Get '[NDJSON] [DroitAcces]
    -- POST /droits_acces
    :<|> "droits_acces"
               :> ReqBody '[JSON] FiltreAcces
               :> Post '[NDJSON] [DroitAcces]
    -- PUT /pce/{id_pce}/droit_acces
    :<|> "pce" :> Capture "id_pce" Text
               :> "droit_acces"
               :> ReqBody '[JSON] DemandeAccesIn
               :> Put '[JSON] RetourDemandeAcces
    -- PATCH /droit_acces/{id_droit_acces}
    :<|> "droit_acces" :> Capture "id_droit_acces" Text
               :> Patch '[JSON] RetourFinAcces


-- | Proxy de 'AdictAPI', passé à 'client' pour générer les fonctions client.
adictAPI :: Proxy AdictAPI
adictAPI = Proxy


-- ---------------------------------------------------------------------------
-- Fonctions client générées

-- | Consommations publiées d'un PCE.
getConsosPubliees
    :: Text         -- ^ @id_pce@
    -> Maybe Text   -- ^ @periode@ (ex. @\"2024\"@, @\"2024-01\"@)
    -> Maybe Text   -- ^ @date_debut@ (format @YYYY-MM-DD@)
    -> Maybe Text   -- ^ @date_fin@   (format @YYYY-MM-DD@)
    -> ClientM [ConsoRestit]

-- | Consommations informatives d'un PCE.
getConsosInfos
    :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM [ConsoRestit]

-- | Injections publiées d'un PCE.
getInjectionsPubliees
    :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM [InjectionRestit]

-- | Données contractuelles d'un PCE.
getDonneesContractuelles
    :: Text     -- ^ @id_pce@
    -> [Text]   -- ^ Filtres optionnels (ex. @[\"cja\",\"car\"]@)
    -> ClientM RetourDonneesContractuelles

-- | Données techniques d'un PCE.
getDonneesTechniques :: Text -> ClientM RetourDonneesTechniques

-- | Consulte tous les droits d'accès (@statut_controle@ optionnel).
getDroitsAcces :: Maybe Int -> ClientM [DroitAcces]

-- | Recherche des droits d'accès avec filtres.
postDroitsAcces :: FiltreAcces -> ClientM [DroitAcces]

-- | Déclare un droit d'accès sur un PCE.
putDroitAcces :: Text -> DemandeAccesIn -> ClientM RetourDemandeAcces

-- | Révoque un droit d'accès.
patchDroitAcces :: Text -> ClientM RetourFinAcces

getConsosPubliees
    :<|> getConsosInfos
    :<|> getInjectionsPubliees
    :<|> getDonneesContractuelles
    :<|> getDonneesTechniques
    :<|> getDroitsAcces
    :<|> postDroitsAcces
    :<|> putDroitAcces
    :<|> patchDroitAcces
    = client adictAPI
