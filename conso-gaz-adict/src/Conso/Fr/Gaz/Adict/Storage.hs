{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Gaz.Adict.Storage
Description : Conversion ADICT → types SQLite et construction des callbacks AdictFetchCallbacks

Fait le lien entre les types API ADICT (@ConsoRestit@, @InjectionRestit@, …) définis
dans ce package et les types de stockage SQLite (@GazConso@, @GazInjection@, …) de
@conso-site-db@.

Fournit 'buildAdictCallbacks' qui assemble un 'AdictFetchCallbacks' à partir d'une
session ADICT existante. Exemple d'utilisation depuis une app :

@
session  <- initSession True False          -- production
let cbs  = buildAdictCallbacks session
report   <- ingestFromAdict cbs "~\/.conso" "~\/.conso\/sites"
              (Pce "12345678901234") "2024-01-01" "2024-12-31"
@
-}
module Conso.Fr.Gaz.Adict.Storage
  ( toGazConso
  , toGazInjection
  , toGazInfosContractuelles
  , toGazInfosTechniques
  , buildAdictCallbacks
  ) where

import qualified Data.Aeson                    as A
import qualified Data.ByteString.Lazy          as LBS
import           Data.Maybe                    (mapMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE

import           Conso.Fr.Gaz.Adict.Adict
    ( AdictError(..), AdictSession )
import           Conso.Fr.Gaz.Adict.ConsosInfos
    ( consulterConsosInfos )
import           Conso.Fr.Gaz.Adict.ConsosPubliees
    ( PeriodeParam(..), consulterConsosPubliees )
import           Conso.Fr.Gaz.Adict.DonneesContractuelles
    ( consulterDonneesContractuelles )
import           Conso.Fr.Gaz.Adict.DonneesTechniques
    ( consulterDonneesTechniques )
import           Conso.Fr.Gaz.Adict.InjectionsPubliees
    ( consulterInjectionsPubliees )
import           Conso.Fr.Gaz.Adict.Types

import           Conso.Fr.SiteDB.Gaz.Ingestion.FromApi
    ( AdictFetchCallbacks(..) )
import           Conso.Fr.SiteDB.Gaz.Types
import           Conso.Fr.SiteDB.Types
    ( Pce(..) )


-- ---------------------------------------------------------------------------
-- Helpers internes

-- | Encode un objet JSON en Text (UTF-8 garanti).
encodeText :: A.ToJSON a => a -> Text
encodeText = TE.decodeUtf8 . LBS.toStrict . A.encode

-- | Convertit une erreur ADICT en message texte.
adictErrorToText :: AdictError -> Text
adictErrorToText (HttpError code body) =
    "HTTP " <> T.pack (show code) <> ": " <> body
adictErrorToText (ParseError msg)  = "Parse error: " <> msg
adictErrorToText (AuthError msg)   = "Auth error: " <> msg
adictErrorToText (NetworkError msg)= "Network error: " <> msg

-- | Infère la granularité depuis la valeur de période ADICT.
-- Une valeur de 10 caractères (\"YYYY-MM-DD\") correspond à une journée gazière.
inferPeriode :: Maybe Text -> PeriodeGaz
inferPeriode (Just v) | T.length v == 10 = PJournalier
inferPeriode _                            = PMensuel


-- ---------------------------------------------------------------------------
-- Conversions

-- | Convertit un 'ConsoRestit' ADICT en 'GazConso' SQLite.
-- Retourne 'Nothing' si les dates sont absentes (ligne inutilisable).
-- Le 'TypeDonnee' est fourni explicitement car l'endpoint ne le retourne pas.
toGazConso :: TypeDonnee -> ConsoRestit -> Maybe GazConso
toGazConso td cr = do
  let conso = cr_consommation cr
      per   = cr_periode cr
  d1 <- (conso >>= date_debut_consommation) <> (per >>= date_debut)
  d2 <- (conso >>= date_fin_consommation)   <> (per >>= date_fin)
  pure GazConso
    { gcDateDebut       = d1
    , gcDateFin         = d2
    , gcPeriode         = inferPeriode (valeur <$> per)
    , gcTypeDonnee      = td
    , gcEnergie         = conso >>= energie
    , gcVolumeBrut      = conso >>= volume_brut
    , gcVolumeConverti  = conso >>= volume_converti
    , gcCoeffConversion = (conso >>= coeff_calcul) >>= coeff_conversion
    , gcCoeffPta        = (conso >>= coeff_calcul) >>= coeff_pta
    , gcRawJson         = encodeText cr
    }

-- | Convertit un 'InjectionRestit' ADICT en 'GazInjection' SQLite.
-- Retourne 'Nothing' si les dates sont absentes.
toGazInjection :: TypeDonnee -> InjectionRestit -> Maybe GazInjection
toGazInjection td ir = do
  let inj = ir_injection ir
      per = ir_periode ir
  d1 <- (inj >>= date_debut_injection) <> (per >>= date_debut)
  d2 <- (inj >>= date_fin_injection)   <> (per >>= date_fin)
  pure GazInjection
    { giDateDebut      = d1
    , giDateFin        = d2
    , giPeriode        = inferPeriode (valeur <$> per)
    , giTypeDonnee     = td
    , giEnergie        = inj >>= inj_energie
    , giVolumeBrut     = inj >>= inj_volume_brut
    , giVolumeConverti = inj >>= inj_volume_converti
    , giRawJson        = encodeText ir
    }

-- | Convertit un 'RetourDonneesContractuelles' en 'GazInfosContractuelles'.
-- Les champs segment_client et num_compteur ne sont pas dans la réponse ADICT actuelle.
toGazInfosContractuelles :: RetourDonneesContractuelles -> GazInfosContractuelles
toGazInfosContractuelles r = GazInfosContractuelles
  { icDateDebut     = rdc_donnees r >>= dc_date_mes
  , icDateFin       = Nothing
  , icSegmentClient = Nothing
  , icNumCompteur   = Nothing
  , icTarif         = rdc_donnees r >>= dc_tarif_acheminement
  , icRawJson       = encodeText r
  }

-- | Convertit un 'RetourDonneesTechniques' en 'GazInfosTechniques'.
-- Les caractéristiques détaillées (type_compteur, pression, etc.) sont stockées
-- dans le JSON brut ; les champs typés sont laissés à 'Nothing' faute de mapping
-- direct dans l'API actuelle.
toGazInfosTechniques :: RetourDonneesTechniques -> GazInfosTechniques
toGazInfosTechniques r = GazInfosTechniques
  { itTypeCompteur = Nothing
  , itPression     = Nothing
  , itDateReleve   = Nothing
  , itEtatCompteur = Nothing
  , itRawJson      = encodeText r
  }


-- ---------------------------------------------------------------------------
-- Construction des callbacks

-- | Construit un 'AdictFetchCallbacks' prêt à être passé à 'ingestFromAdict'.
--
-- Chaque callback appelle le webservice ADICT correspondant, convertit la réponse
-- et renvoie @Either Text [Gaz*]@. Les enregistrements sans dates valides sont
-- silencieusement ignorés ('mapMaybe').
buildAdictCallbacks :: AdictSession -> AdictFetchCallbacks
buildAdictCallbacks session = AdictFetchCallbacks
  { fetchConsosPubliees = \pce d1 d2 -> do
      res <- consulterConsosPubliees session (pceText pce) (ByDateRange d1 d2)
      return $ case res of
        Left  err    -> Left (adictErrorToText err)
        Right consos -> Right (mapMaybe (toGazConso TDPubliee) consos)

  , fetchConsosInfos = \pce d1 d2 -> do
      res <- consulterConsosInfos session (pceText pce) (ByDateRange d1 d2)
      return $ case res of
        Left  err    -> Left (adictErrorToText err)
        Right consos -> Right (mapMaybe (toGazConso TDInformative) consos)

  , fetchInjections = \pce d1 d2 -> do
      res <- consulterInjectionsPubliees session (pceText pce) (ByDateRange d1 d2)
      return $ case res of
        Left  err  -> Left (adictErrorToText err)
        Right injs -> Right (mapMaybe (toGazInjection TDPubliee) injs)

  , fetchInfosContractuelles = \pce -> do
      res <- consulterDonneesContractuelles session (pceText pce) []
      return $ case res of
        Left  err    -> Left (adictErrorToText err)
        Right retour -> Right (Just (toGazInfosContractuelles retour))

  , fetchInfosTechniques = \pce -> do
      res <- consulterDonneesTechniques session (pceText pce)
      return $ case res of
        Left  err    -> Left (adictErrorToText err)
        Right retour -> Right (Just (toGazInfosTechniques retour))
  }
  where
    pceText (Pce t) = t
