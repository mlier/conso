{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-|
Module      : Conso.Fr.Elec.SiteDB.Ingestion.GapFill
Description : Combler les lacunes élec via les demandes M023

Ce module orchestre le processus en deux étapes :

  1. __Détecter__ les plages manquantes dans la base site (via "Conso.Fr.Elec.SiteDB.Storage.Gaps")
  2. __Demander__ les données manquantes via M023 (fonctions injectées par l'appelant)
  3. __Ingérer__ les fichiers JSON reçus via 'ingestDirectory'

Les fonctions d'envoi M023 sont passées en paramètre sous forme de callbacks
('GapFillCallbacks'). Cela évite une dépendance circulaire entre @conso-site-db-elec@
et @conso-elec-sge@ (qui possède le client SOAP SGE).

Usage typique depuis @conso-elec-sge@ :

@
let callbacks = GapFillCallbacks
      { sendFinesRequest = \\prms d1 d2 ->
          fmap (const "ok") \<$\> commanderMesuresFines sess prms d1 d2
      , sendFactRequest  = \\prms d1 d2 ->
          fmap (const "ok") \<$\> commanderMesuresFact  sess prms d1 d2
      , sendItcRequest   = \\prms ->
          fmap (const "ok") \<$\> commanderItc          sess prms
      }
report <- fillGaps callbacks "~\/.conso" "~\/.conso\/sites" "~\/.conso\/rfiles" prm from to
@
-}
module Conso.Fr.Elec.SiteDB.Ingestion.GapFill
  ( GapFillCallbacks(..)
  , GapFillReport(..)
  , GapRequest(..)
  , fillGaps
  , detectGapsForPrm
  ) where

import           Data.Text              (Text)
import           Data.Either            (rights, lefts)
import           Data.Time              (Day, UTCTime(..), addDays)
import           Control.Concurrent     (threadDelay)

import           Conso.Fr.SiteDB.Types                               (Prm(..))
import           Conso.Fr.SiteDB.Registry                            (openRegistry, lookupByPrm)
import           Conso.Fr.Elec.SiteDB.Types.Common            (PrmId(..), Periode(..), Pas(..))
import           Conso.Fr.Elec.SiteDB.Storage.Connection      (openSiteDbElec)
import           Conso.Fr.Elec.SiteDB.Storage.Gaps
import           Conso.Fr.Elec.SiteDB.Ingestion.FromRfiles    (ingestDirectory, IngestDirResult)

-- | Fonctions d'envoi des demandes M023, fournies par l'appelant.
-- Typiquement des clôtures sur une session SGE du package @conso-elec-sge@.
data GapFillCallbacks = GapFillCallbacks
  { -- | Envoie une demande de mesures fines (R63/R64/R65/R66).
    -- Reçoit : liste de PRM, date début, date fin.
    -- Retourne : @Right affaireId@ ou @Left erreur@.
    sendFinesRequest :: [PrmId] -> Day -> Day -> IO (Either Text Text)
    -- | Envoie une demande de mesures facturantes (R67).
  , sendFactRequest  :: [PrmId] -> Day -> Day -> IO (Either Text Text)
    -- | Envoie une demande d'informations techniques et contractuelles (C68).
  , sendItcRequest   :: [PrmId] -> IO (Either Text Text)
  }

-- | Description d'une lacune nécessitant une demande M023.
data GapRequest
  = GapFines PrmId Day Day  -- ^ Lacune R63/R65/R66 : demande mesures fines
  | GapFact  PrmId Day Day  -- ^ Lacune R67 : demande mesures facturantes
  | GapItc   PrmId          -- ^ Absence C68 : demande infos techniques
  deriving (Show)

-- | Rapport de l'opération de comblement.
data GapFillReport = GapFillReport
  { gfrGapsDetected  :: [GapRequest]    -- ^ Lacunes identifiées
  , gfrRequestsSent  :: [Text]          -- ^ AffaireIds des demandes envoyées
  , gfrRequestErrors :: [Text]          -- ^ Erreurs lors de l'envoi des demandes
  , gfrIngestResults :: [IngestDirResult] -- ^ Résultats d'ingestion post-M023
  } deriving (Show)

-- | Détecte les lacunes pour un PRM sur une période sans envoyer de demandes.
-- Utile pour inspecter l'état de la base avant de décider des demandes à faire.
detectGapsForPrm
  :: FilePath -- ^ Répertoire de configuration (contient @registry.db@)
  -> FilePath -- ^ Répertoire des bases SQLite site
  -> Prm      -- ^ PRM à analyser
  -> Day      -- ^ Début de la période à vérifier
  -> Day      -- ^ Fin de la période à vérifier
  -> IO (Either Text [GapRequest])
detectGapsForPrm configDir siteDbDir prm@(Prm prmText) fromDay toDay = do
  reg <- openRegistry configDir
  mSiteId <- lookupByPrm reg prm
  case mSiteId of
    Nothing     -> return $ Left ("PRM inconnu dans le registre : " <> prmText)
    Just siteId -> do
      conn <- openSiteDbElec siteDbDir siteId
      let prmId   = PrmId prmText
          start   = UTCTime fromDay 0
          end     = UTCTime (addDays 1 toDay) 0
      -- Détection des lacunes dans les courbes de charge (R63 — PA CONS BRUT)
      curveGaps <- detectCurveGaps conn "CONS" "PA" "BRUT" PT30M start end
      -- Détection des lacunes dans les énergies quotidiennes (R65)
      energyGaps <- detectEnergyGaps conn "CONS" fromDay toDay
      let gapRequests =
            [ GapFines prmId (utctDay (periodeDebut g)) (utctDay (periodeFin g))
            | g <- curveGaps ] ++
            [ GapFines prmId d d
            | d <- energyGaps ]
      return $ Right gapRequests

-- | Détecte les lacunes, envoie les demandes M023, puis ingère les fichiers reçus.
-- @waitSeconds@ est le délai d'attente (en secondes) entre l'envoi de la demande
-- et le scan du dossier de sortie (les fichiers M023 peuvent mettre quelques heures).
fillGaps
  :: GapFillCallbacks
  -> FilePath -- ^ Répertoire de configuration (contient @registry.db@)
  -> FilePath -- ^ Répertoire des bases SQLite site
  -> FilePath -- ^ Répertoire où arrivent les fichiers JSON M023
  -> Prm      -- ^ PRM à traiter
  -> Day      -- ^ Début de la période
  -> Day      -- ^ Fin de la période
  -> Int      -- ^ Délai d'attente en secondes avant de scanner le dossier de sortie
  -> IO GapFillReport
fillGaps callbacks configDir siteDbDir outputDir prm fromDay toDay waitSeconds = do
  -- 1. Détection des lacunes
  gapsOrErr <- detectGapsForPrm configDir siteDbDir prm fromDay toDay
  case gapsOrErr of
    Left err -> return $ GapFillReport [] [] [err] []
    Right gaps -> do
      -- 2. Envoi des demandes M023
      (affaireIds, reqErrors) <- sendRequests callbacks gaps
      -- 3. Attendre l'arrivée des fichiers
      if null affaireIds
        then return $ GapFillReport gaps [] reqErrors []
        else do
          threadDelay (waitSeconds * 1_000_000)
          -- 4. Ingestion des fichiers reçus
          ingestResults <- ingestDirectory configDir siteDbDir outputDir
          return $ GapFillReport gaps affaireIds reqErrors ingestResults

-- | Envoie les demandes M023 pour les lacunes détectées.
-- Dédoublonne les périodes (plusieurs lacunes sur la même période = 1 seule demande).
sendRequests :: GapFillCallbacks -> [GapRequest] -> IO ([Text], [Text])
sendRequests callbacks gaps = do
  results <- mapM sendOne gaps
  let successes = rights results
      failures  = lefts  results
  return (successes, failures)
  where
    sendOne (GapFines prmId d1 d2) =
      sendFinesRequest callbacks [prmId] d1 d2
    sendOne (GapFact prmId d1 d2) =
      sendFactRequest callbacks [prmId] d1 d2
    sendOne (GapItc prmId) =
      sendItcRequest callbacks [prmId]
