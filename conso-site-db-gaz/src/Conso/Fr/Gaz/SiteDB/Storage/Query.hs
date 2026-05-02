{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Conso.Fr.Gaz.SiteDB.Storage.Query
  ( derniereIngestDate
  , derniereInfosContractuelles
  , derniereInfosTechniques
  , detectionTrousContinu
  ) where

import           Data.Text                     (Text)
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
    \ FROM gaz_info_contractuelle ORDER BY id DESC LIMIT 1"
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
    \ FROM gaz_info_technique ORDER BY id DESC LIMIT 1"
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


-- | Détecte les ruptures de continuité dans une table gaz (fin != debut_suivant).
-- Le nom de table est contrôlé : toujours l'une des 3 constantes gaz_*.
-- Retourne des paires (YYYY-MM-DD, YYYY-MM-DD) correspondant aux bornes du trou.
detectionTrousContinu :: Connection -> Text -> Text -> Text -> IO [(Text, Text)]
detectionTrousContinu conn dateDebutStr dateFinStr table =
  query conn
    (Query $
      "SELECT SUBSTR(fin,1,10), SUBSTR(next_debut,1,10) \
      \FROM (\
      \  SELECT fin, LEAD(debut) OVER (ORDER BY debut) AS next_debut \
      \  FROM " <> table <> " \
      \  WHERE SUBSTR(debut,1,10) >= ? AND SUBSTR(debut,1,10) <= ?\
      \) WHERE fin != next_debut AND next_debut IS NOT NULL")
    (dateDebutStr, dateFinStr)
