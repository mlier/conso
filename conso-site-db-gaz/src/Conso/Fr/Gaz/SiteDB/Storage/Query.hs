{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Conso.Fr.Gaz.SiteDB.Storage.Query
  ( derniereIngestDate
  , derniereInfosContractuelles
  , derniereInfosTechniques
  , detectionTrous
  ) where

import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time
  ( Day, parseTimeM, defaultTimeLocale
  , addDays, addGregorianMonthsRollOver, formatTime )
import           Database.SQLite.Simple

import           Conso.Fr.Gaz.SiteDB.Types


-- | MAX(date_fin) dans gaz_ingestion_log pour un endpoint donné.
-- Retourne Nothing si aucune ingestion n'a encore eu lieu pour cet endpoint.
derniereIngestDate :: Connection -> Text -> IO (Maybe Text)
derniereIngestDate conn endpoint = do
  rows <- query conn
    "SELECT MAX(date_fin) FROM gaz_ingestion_log \
    \WHERE endpoint = ? AND date_fin IS NOT NULL"
    (Only endpoint) :: IO [Only (Maybe Text)]
  return $ case rows of
    [Only mv] -> mv
    _         -> Nothing


-- | Dernières informations contractuelles stockées (tous les champs métier).
-- Utilisé pour détecter si les données ont changé avant d'insérer.
derniereInfosContractuelles :: Connection -> IO (Maybe GazInfosContractuelles)
derniereInfosContractuelles conn = do
  rows <- query_ conn
    "SELECT date_mes, tarif_acheminement, date_publication, conso_journaliere_plafond,\
    \       car_actuelle, car_future, cja, cja_journaliere, cja_mensuelle,\
    \       profil_type_actuel, profil_type_futur,\
    \       date_debut_profil_type_actuel, date_fin_profil_type_actuel,\
    \       modulation_assiette, modulation_n_1, modulation_n_2, modulation_n_3\
    \ FROM gaz_infos_contractuelles ORDER BY id DESC LIMIT 1"
    :: IO [( Maybe Text, Maybe Text, Maybe Text, Maybe Text
           , Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text
           )
           :.
           ( Maybe Text, Maybe Text, Maybe Text, Maybe Text
           , Maybe Text, Maybe Text, Maybe Text, Maybe Text
           )]
  return $ case rows of
    [(mes, tar, pub, plaf, carA, carF, cja, cjaJ, cjaM)
     :.
     (pA, pF, dDP, dFP, mAss, mN1, mN2, mN3)]
      -> Just GazInfosContractuelles
          { icDateMes                   = mes
          , icTarifAcheminement         = tar
          , icDatePublication           = pub
          , icConsoJournalierePlafond   = plaf
          , icCarActuelle               = carA
          , icCarFuture                 = carF
          , icCja                       = cja
          , icCjaJournaliere            = cjaJ
          , icCjaMensuelle              = cjaM
          , icProfilTypeActuel          = pA
          , icProfilTypeFutur           = pF
          , icDateDebutProfilTypeActuel = dDP
          , icDateFinProfilTypeActuel   = dFP
          , icModulationAssiette        = mAss
          , icModulationN1              = mN1
          , icModulationN2              = mN2
          , icModulationN3              = mN3
          }
    _ -> Nothing


-- | Dernières informations techniques stockées (tous les champs métier).
derniereInfosTechniques :: Connection -> IO (Maybe GazInfosTechniques)
derniereInfosTechniques conn = do
  rows <- query_ conn
    "SELECT numero_rue, nom_rue, complement_adresse, code_postal, commune,\
    \       client_sensible_mig, code_calibre, code_debit, code_debit_normalise, frequence,\
    \       matricule_compteur, pression_livraison,\
    \       identifiant_pitd, libelle_pitd,\
    \       regime_propriete_compteur, regime_propriete_convertisseur,\
    \       regime_propriete_enregistreur, regime_propriete_poste\
    \ FROM gaz_infos_techniques ORDER BY id DESC LIMIT 1"
    :: IO [( Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text
           , Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text )
           :.
           ( Maybe Text, Maybe Text
           , Maybe Text, Maybe Text
           , Maybe Text, Maybe Text, Maybe Text, Maybe Text )]
  return $ case rows of
    [(nr, nomR, compl, cp, com, csm, cc, cd, cdn, freq)
     :.
     (mc, pl, ipitd, lpitd, rpc, rpconv, rpenr, rpposte)]
      -> Just GazInfosTechniques
          { itNumeroRue                    = nr
          , itNomRue                       = nomR
          , itComplementAdresse            = compl
          , itCodePostal                   = cp
          , itCommune                      = com
          , itClientSensibleMig            = csm
          , itCodeCalibre                  = cc
          , itCodeDebit                    = cd
          , itCodeDebitNormalise           = cdn
          , itFrequence                    = freq
          , itMatriculeCompteur            = mc
          , itPressionLivraison            = pl
          , itIdentifiantPitd              = ipitd
          , itLibellePitd                  = lpitd
          , itRegimeProprieteCompteur      = rpc
          , itRegimeProprieteConvertisseur = rpconv
          , itRegimeProprieteEnregistreur  = rpenr
          , itRegimeProprietePoste         = rpposte
          }
    _ -> Nothing


-- | Retourne les plages de dates manquantes dans gaz_consos entre deux bornes.
-- Génère la séquence de dates attendues (un enregistrement par jour ou par mois)
-- et la compare aux date_debut effectivement stockées.
detectionTrous :: Connection -> Text -> Text -> PeriodeGaz -> IO [(Text, Text)]
detectionTrous conn dateDebutStr dateFinStr periode = do
  let parseD s = parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack s) :: Maybe Day
  case (parseD dateDebutStr, parseD dateFinStr) of
    (Nothing, _) -> return []
    (_, Nothing) -> return []
    (Just debut, Just fin) -> do
      let fmt       = formatTime defaultTimeLocale "%Y-%m-%d"
          attendues = map (T.pack . fmt) (genererDates periode debut fin)
      rows <- query conn
        "SELECT DISTINCT date_debut FROM gaz_consos \
        \WHERE periode = ? AND date_debut >= ? AND date_debut <= ? \
        \ORDER BY date_debut"
        (periodeGazToText periode, dateDebutStr, dateFinStr)
        :: IO [Only Text]
      let stockees   = map (\(Only d) -> d) rows
          manquantes = filter (`notElem` stockees) attendues
      return (map (\d -> (d, d)) manquantes)


genererDates :: PeriodeGaz -> Day -> Day -> [Day]
genererDates PJournalier debut fin =
  takeWhile (<= fin) $ iterate (addDays 1) debut
genererDates PMensuel debut fin =
  takeWhile (<= fin) $ iterate (addGregorianMonthsRollOver 1) debut
