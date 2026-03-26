{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-|
Module      : Conso.Fr.Elec.Sge
Description : Point d'entrée de la bibliothèque SGE Enedis B2B

= Infrastructure

Ce module re-exporte l'infrastructure SOAP commune :

  * 'SgeEnv', 'Sge', 'Test' — configuration lue depuis
    @~\/.conso\/conso-env.yaml@
  * 'wsRequest' \/ 'wsRequestTest' — envoi d'une requête typée,
    retourne @Either (code, libellé) réponse@
  * 'xmlRequest' \/ 'xmlRequestTest' — variantes retournant le XML brut
  * 'prettyXml' — indentation d'une réponse XML brute
  * 'getEnv', 'getEnvSge' — lecture de la configuration

= Webservices disponibles

Chaque webservice expose les fonctions @initType@, @initTypeTest@ et
@myrequest@ dans son propre module. En raison de ces noms identiques,
les modules doivent être importés qualifiés :

> import qualified Conso.Fr.Elec.Sge.ConsulterMesuresV11 as CM1

== Consultation

  * "Conso.Fr.Elec.Sge.ConsulterMesuresV11" — historique des mesures
    relevées (36 mois max, segments C1–C5 hors C1 HTA)
  * "Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesV3" — mesures détaillées
    (courbe de charge, index, Pmax, énergie) pour C1–C5 et P1–P4 Linky
  * "Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10" —
    données techniques et contractuelles d'un point
  * "Conso.Fr.Elec.Sge.RechercherPointV20" — recherche de point par critères
    (adresse, PRM, domaine de tension…)

== Commandes de mesures

  * "Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10" — souscrire un
    accès aux données de mesures (CDC, IDX, PMAX, ENERGIE) pour les C5
  * "Conso.Fr.Elec.Sge.CommanderTransmissionDonneesInfraJV10" — activer
    la transmission infra-journalière (CDC, IDX, PTD)

== Publication asynchrone M023

  * "Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10" — demande
    d'historique de mesures fines (flux R63 à R66)
  * "Conso.Fr.Elec.Sge.DemandePublicationMesuresFacturantesM23V10" —
    demande d'historique de mesures facturantes
  * "Conso.Fr.Elec.Sge.DemandePublicationInformationsTechniquesContractuellesM23V10" —
    demande d'historique des informations techniques et contractuelles

== Services d'accès aux données (SAD)

  * "Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10" — souscrire un SAD
  * "Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10" — arrêter un SAD
  * "Conso.Fr.Elec.Sge.CommanderRenouvellementServicesAccesDonneesV10" —
    renouveler un SAD
  * "Conso.Fr.Elec.Sge.CommanderModificationOptionsServicesAccesDonneesV10" —
    modifier les options de publication d'un SAD
  * "Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10" — lister les SAD
    actifs d'un PRM

== Webservices obsolètes (supprimés en SGE v26.1)

  * "Conso.Fr.Elec.Sge.CommanderCollectePublicationMesuresV30" —
    remplacé par 'CommanderAccesDonneesMesuresV10'
  * "Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10" —
    remplacé par 'RechercherServicesAccesDonneesV10'
  * "Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10" —
    remplacé par 'CommanderArretServicesAccesDonneesV10'

= Fichiers Rxx (SFTP)

Ce module re-exporte les fonctions de téléchargement et de déchiffrement
des fichiers Rxx déposés par Enedis sur le serveur SFTP :

  * 'getConfig', 'lsRFiles', 'listRFiles', 'loadRFiles' — gestion SFTP
  * 'DecryptConfig', 'decryptDir', 'decryptZipFile' — déchiffrement AES

= Exemple d'utilisation

@
import Conso.Fr.Elec.Sge
import qualified Conso.Fr.Elec.Sge.ConsulterMesuresV11 as CM1

main :: IO ()
main = do
    myType \<- CM1.initType \"12345678901234\" True
    rep \<- wsRequest myType :: IO (Either (String, String) CM1.ConsulterMesuresResponseType)
    print rep
@
-}
module Conso.Fr.Elec.Sge
  ( -- * Configuration
    SgeEnv(..)
  , Sge
  , Test(..)
  , getEnv
  , getEnvSge
    -- * Transport SOAP
  , wsRequest
  , wsRequestTest
  , xmlRequest
  , xmlRequestTest
  , prettyXml
  , getLoginContrat
    -- * Fichiers Rxx — SFTP
  , module Conso.Fr.Elec.Sge.Rfiles.LoadRFiles
    -- * Fichiers Rxx — Déchiffrement
  , module Conso.Fr.Elec.Sge.Rfiles.DecryptRFiles
  ) where

import Conso.Fr.Elec.Sge.Sge
    ( SgeEnv(..), Sge, Test(..)
    , getEnv, getEnvSge
    , wsRequest, wsRequestTest
    , xmlRequest, xmlRequestTest
    , prettyXml, getLoginContrat
    )

import Conso.Fr.Elec.Sge.Rfiles.LoadRFiles
import Conso.Fr.Elec.Sge.Rfiles.DecryptRFiles
