{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SiteDB.Storage.Insert
Description : Insertions SQLite pour tous les types de flux SgeDB

Fournit une fonction d'insertion par type de flux (R63..R67, C68)
et la fonction 'logIngestion' pour tracer chaque ingestion dans @elec_ingestion_log@.

Stratégies d'insertion :

  * @elec_curve_points@, @elec_daily_energy@, @elec_daily_pmax@ — @INSERT OR REPLACE@ (idempotent)
  * @elec_index_values@, @elec_billing_measures@, @elec_prm_info@ — @INSERT@ simple (conserve l'historique)
-}
module Conso.Fr.Elec.SiteDB.Storage.Insert
  ( insertCurvePoints
  , insertIndexValues
  , insertDailyEnergy
  , insertDailyPmax
  , insertBillingMeasures
  , insertPrmInfoIfChanged
  , logIngestion
  , IngestionId
  ) where

import           Database.SQLite.Simple
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (UTCTime, Day)
import           Data.Time.Format       (formatTime, defaultTimeLocale)
import           Data.Time.Calendar     (showGregorian)
import           Conso.Fr.Elec.SiteDB.Types.Common
import           Conso.Fr.Elec.SiteDB.Types.Header  (CodeFlux, codeFluxToText)
import           Conso.Fr.Elec.SiteDB.Types.R63
import           Conso.Fr.Elec.SiteDB.Types.R64
import           Conso.Fr.Elec.SiteDB.Types.R65
import           Conso.Fr.Elec.SiteDB.Types.R66
import           Conso.Fr.Elec.SiteDB.Types.R67
import           Conso.Fr.Elec.SiteDB.Types.C68
import           Conso.Fr.Elec.SiteDB.Storage.Query (queryLatestPrmInfo, PrmInfoRow(..))
import           Control.Monad                     (when)

type IngestionId = Int

-- ---------------------------------------------------------------------------
-- Helpers de formatage

fmtUTC :: UTCTime -> Text
fmtUTC = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

fmtDay :: Day -> Text
fmtDay = T.pack . showGregorian

-- ---------------------------------------------------------------------------
-- Log d'ingestion

-- | Enregistre une ligne dans @elec_ingestion_log@ et retourne son identifiant.
logIngestion
  :: Connection    -- ^ Connexion à la base PRM
  -> CodeFlux      -- ^ Type de flux ingéré
  -> Text          -- ^ Mode de publication (@P@, @Q@, @H@ ou @M@)
  -> Text          -- ^ Identifiant de la demande SGE
  -> Maybe Text    -- ^ Identifiant de publication (R6X-REC uniquement)
  -> Maybe Int     -- ^ Numéro de séquence (optionnel)
  -> UTCTime       -- ^ Horodate d'ingestion (@getCurrentTime@)
  -> Maybe UTCTime -- ^ Début de la période couverte
  -> Maybe UTCTime -- ^ Fin de la période couverte
  -> Maybe Text    -- ^ Nom du fichier source (pour traçabilité)
  -> IO IngestionId
logIngestion conn cf modePub idDem idPub numSeq dateIng deb fin src = do
  execute conn
    "INSERT INTO elec_ingestion_log \
    \ (code_flux, mode_publication, id_demande, id_publication, \
    \  num_sequence, date_ingestion, date_debut_periode, date_fin_periode, \
    \  fichier_source) \
    \ VALUES (?,?,?,?,?,?,?,?,?)"
    ( codeFluxToText cf, modePub, idDem, idPub, numSeq
    , fmtUTC dateIng
    , fmap fmtUTC deb, fmap fmtUTC fin
    , src )
  fmap fromIntegral (lastInsertRowId conn)

-- ---------------------------------------------------------------------------
-- Insertion courbes de charge (R63)

-- | Insère les points de courbe de charge d'une 'MesureR63' dans @elec_curve_points@.
-- Utilise @INSERT OR REPLACE@ : idempotent sur la clé @(etape_metier, grandeur_metier, grandeur_physique, horodate, pas)@.
insertCurvePoints :: Connection -> IngestionId -> MesureR63 -> IO ()
insertCurvePoints conn ingId m =
  mapM_ (insertGrandeur (etapeMetierToText (mr63EtapeMetier m))) (mr63Grandeurs m)
  where
    insertGrandeur em g =
      mapM_ (insertPoint em g) (gr63Points g)
    insertPoint em g p =
      execute conn
        "INSERT OR REPLACE INTO elec_curve_points \
        \ (etape_metier, grandeur_metier, grandeur_physique, unite, \
        \  horodate, valeur, pas, nature, type_completion, iv, ec, ingestion_id) \
        \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?)"
        (( em
        , grandeurMetierToText (gr63GrandeurMetier g)
        , grandeurPhysiqueR63ToText (gr63GrandeurPhysique g)
        , gr63Unite g
        , fmtUTC (pcHorodate p)
        , pcValeur p
        , pasToText (pcPas p)
        , naturePointToText (pcNature p)
        , fmap typeCompletionToText (pcTypeCompletion p)
        , pcIndiceVraisemblance p
        ) :. ( pcEtatComplementaire p
             , ingId ))

-- ---------------------------------------------------------------------------
-- Insertion index (R64) — aplatissement de la hiérarchie

-- | Insère les valeurs d'index d'une 'MesureR64' dans @elec_index_values@.
-- Aplatit la hiérarchie @contexte → grandeur → calendrier → classeTemporelle → valeur@.
-- Utilise @INSERT@ simple (pas d'idempotence car plusieurs relevés peuvent coexister).
insertIndexValues :: Connection -> IngestionId -> MesureR64 -> IO ()
insertIndexValues conn ingId m =
  mapM_ insertCtx (mr64Contextes m)
  where
    insertCtx ctx =
      mapM_ (insertGrandeur ctx) (ctx64Grandeurs ctx)
    insertGrandeur ctx g = do
      mapM_ (insertCal ctx g) (gr64Calendriers g)
      case gr64CadranTotalisateur g of
        Nothing  -> return ()
        Just cdt -> mapM_ (insertTot ctx g cdt) (ctotValeurs cdt)
    insertCal ctx g cal =
      mapM_ (insertClasse ctx g cal) (calClassesTemporelles cal)
    insertClasse ctx g cal ct =
      mapM_ (insertVal ctx g cal ct False) (ctValeurs ct)
    insertTot ctx g cdt vi =
      execute conn
        "INSERT INTO elec_index_values \
        \ (etape_metier, contexte_releve, type_releve, motif_releve, \
        \  grandeur_metier, grandeur_physique, unite, \
        \  id_calendrier, libelle_grille, id_classe_temporelle, \
        \  libelle_classe_temp, code_cadran, is_totalisateur, \
        \  horodate, valeur, iv, ingestion_id) \
        \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        (( etapeMetierToText (ctx64EtapeMetier ctx)
        , contexteReleveToText (ctx64ContexteReleve ctx)
        , typeReleveToText (ctx64TypeReleve ctx)
        , ctx64MotifReleve ctx
        , grandeurMetierToText (gr64GrandeurMetier g)
        , gr64GrandeurPhysique g
        , gr64Unite g
        , Nothing :: Maybe Text
        , Nothing :: Maybe Text
        , Nothing :: Maybe Text
        ) :. ( Nothing :: Maybe Text
             , Just (ctotCodeCadran cdt)
             , 1 :: Int
             , fmtUTC (viHorodate vi)
             , viValeur vi
             , viIndiceVraisemblance vi
             , ingId ))
    insertVal ctx g cal ct _isTot vi =
      execute conn
        "INSERT INTO elec_index_values \
        \ (etape_metier, contexte_releve, type_releve, motif_releve, \
        \  grandeur_metier, grandeur_physique, unite, \
        \  id_calendrier, libelle_grille, id_classe_temporelle, \
        \  libelle_classe_temp, code_cadran, is_totalisateur, \
        \  horodate, valeur, iv, ingestion_id) \
        \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        (( etapeMetierToText (ctx64EtapeMetier ctx)
        , contexteReleveToText (ctx64ContexteReleve ctx)
        , typeReleveToText (ctx64TypeReleve ctx)
        , ctx64MotifReleve ctx
        , grandeurMetierToText (gr64GrandeurMetier g)
        , gr64GrandeurPhysique g
        , gr64Unite g
        , Just (calIdCalendrier cal)
        , Just (calLibelleGrille cal)
        , ctIdClasseTemporelle ct
        ) :. ( ctLibelleClasseTemporelle ct
             , ctCodeCadran ct
             , 0 :: Int
             , fmtUTC (viHorodate vi)
             , viValeur vi
             , viIndiceVraisemblance vi
             , ingId ))

-- ---------------------------------------------------------------------------
-- Insertion énergies quotidiennes (R65)

-- | Insère les énergies journalières d'une 'MesureR65' dans @elec_daily_energy@.
-- Utilise @INSERT OR REPLACE@ : idempotent sur @(grandeur_metier, grandeur_physique, date)@.
insertDailyEnergy :: Connection -> IngestionId -> MesureR65 -> IO ()
insertDailyEnergy conn ingId m =
  mapM_ insertGrandeur (mr65Grandeurs m)
  where
    insertGrandeur g =
      mapM_ (insertPoint g) (gr65Points g)
    insertPoint g p =
      execute conn
        "INSERT OR REPLACE INTO elec_daily_energy \
        \ (etape_metier, grandeur_metier, grandeur_physique, unite, \
        \  mode_calcul, date, valeur, ingestion_id) \
        \ VALUES (?,?,?,?,?,?,?,?)"
        ( etapeMetierToText (mr65EtapeMetier m)
        , grandeurMetierToText (gr65GrandeurMetier g)
        , grandeurPhysiqueEnergieToText (gr65GrandeurPhysique g)
        , gr65Unite g
        , modeCalculToText (mr65ModeCalcul m)
        , fmtDay (peDate p)
        , peValeur p
        , ingId )

-- ---------------------------------------------------------------------------
-- Insertion Pmax quotidiennes (R66)

-- | Insère les Pmax journalières d'une 'MesureR66' dans @elec_daily_pmax@.
-- Utilise @INSERT OR REPLACE@ : idempotent sur @(grandeur_metier, grandeur_physique, horodate)@.
insertDailyPmax :: Connection -> IngestionId -> MesureR66 -> IO ()
insertDailyPmax conn ingId m =
  mapM_ insertGrandeur (mr66Grandeurs m)
  where
    insertGrandeur g =
      mapM_ (insertPoint g) (gr66Points g)
    insertPoint g p =
      execute conn
        "INSERT OR REPLACE INTO elec_daily_pmax \
        \ (etape_metier, grandeur_metier, grandeur_physique, unite, \
        \  horodate, valeur, ingestion_id) \
        \ VALUES (?,?,?,?,?,?,?)"
        ( etapeMetierToText (mr66EtapeMetier m)
        , grandeurMetierToText (gr66GrandeurMetier g)
        , grandeurPhysiquePmaxToText (gr66GrandeurPhysique g)
        , gr66Unite g
        , fmtUTC (ppHorodate p)
        , ppValeur p
        , ingId )

-- ---------------------------------------------------------------------------
-- Insertion mesures facturantes (R67) — INSERT sans REPLACE pour conserver les statuts

-- | Insère les mesures facturantes d'une 'MesureR67' dans @elec_billing_measures@.
-- Utilise @INSERT@ simple (sans @REPLACE@) pour conserver plusieurs relevés
-- avec des statuts différents sur la même période.
insertBillingMeasures :: Connection -> IngestionId -> MesureR67 -> IO ()
insertBillingMeasures conn ingId m =
  mapM_ insertCtx (mr67Contextes m)
  where
    insertCtx ctx =
      mapM_ (insertGrandeur ctx) (ctx67Grandeurs ctx)
    insertGrandeur ctx g =
      mapM_ (insertCal ctx g) (gr67Calendriers g)
    insertCal ctx g cal =
      mapM_ (insertClasse ctx g cal) (cal67ClassesTemporelles cal)
    insertClasse ctx g cal ct =
      mapM_ (insertQuantite ctx g cal ct) (ct67Quantites ct)
    insertQuantite ctx g cal ct q =
      execute conn
        "INSERT INTO elec_billing_measures \
        \ (etape_metier, id_motif_releve, libelle_motif_releve, \
        \  grandeur_metier, grandeur_physique, unite, \
        \  code_grille, libelle_grille, code_calendrier, libelle_calendrier, \
        \  id_classe_temporelle, libelle_classe_temp, \
        \  date_creation, debut, fin, quantite, \
        \  code_nature, libelle_nature, code_statut, libelle_statut, \
        \  ingestion_id) \
        \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        (( etapeMetierToText (ctx67EtapeMetier ctx)
        , ctx67IdMotifReleve ctx
        , ctx67LibelleMotifReleve ctx
        , grandeurMetierToText (gr67GrandeurMetier g)
        , gr67GrandeurPhysique g
        , gr67Unite g
        , cal67CodeGrille cal
        , cal67LibelleGrille cal
        , cal67CodeCalendrier cal
        , cal67LibelleCalendrier cal
        ) :. ( ct67IdClasseTemporelle ct
             , ct67LibelleClasseTemporelle ct
             , fmtUTC (qDateCreation q)
             , fmtDay (qDbtMesure q)
             , fmtDay (qFinMesure q)
             , qQuantite q
             , qCodeNature q
             , qLibelleNature q
             , qCodeStatut q
             , qLibelleStatut q
             ) :. Only ingId)

-- ---------------------------------------------------------------------------
-- Insertion informations techniques et contractuelles (C68)

boolToInt :: Maybe Bool -> Maybe Int
boolToInt = fmap (\b -> if b then 1 else 0)

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Compare ITC fields to a stored PrmInfoRow; returns True if any field changed.
hasC68Changed :: PrmInfoRow -> InfoTechniqueContractuelle -> Bool
hasC68Changed row itc = textFields row /= textFields' itc || intFields row /= intFields' itc
  where
    textFields r =
      [ piSegment r, piEtatContractuel r, piEtatAlimentation r
      , piPuissanceSouscrite r, piDomaineTension r
      , piAdresseNumeroNomVoie r, piAdresseBatiment r, piAdresseEscalierEtage r
      , piAdresseLieuDit r, piAdresseCodePostal r, piAdresseCommune r
      , piNiveauOuvertureServices r, piDateModifFta r
      , piDateAugmentationPuissance r, piDateDiminutionPuissance r
      , piDateMesSoutirage r, piDateMesInjection r, piDatePremierePoseLinky r
      , piTelephoneDepannage r, piAutoConsoCollective r, piAutoConsoIndividuelle r
      , piPuissanceSouscriteUnite r, piFormuleTarifaireCode r, piFormuleTarifaireLibelle r
      , piCodeTarifAcheminement r, piTypeOffre r, piContexteUtilisation r
      , piForfaitValeur r, piForfaitUnite r, piCalendrierTurpeCode r
      , piGroupePeriodeMobile r, piGroupePeriodeMobileDistrib r
      , piDateDebutContrat r, piNatureContrat r, piDateRefusPoseAmm r
      , piCategorieClient r, piTypeResidence r, piReferenceClient r
      , piTitulaireCivilite r, piTitulaireNom r, piTitulairePrenom r
      , piTitulaireDenominationSociale r, piTitulaireNomCommercial r
      , piTitulaireSiren r, piTitulaireSiret r, piTitulaireSecteur r, piTitulaireActiviteNaf r
      , piReferenceContrat r
      , piTensionLivraison r, piPuissanceRaccordSoutirage r, piPuissanceRaccordInjection r
      , piPuissanceLimiteSoutirage r, piTensionContractuelle r, piModeAlimApresCompteur r
      , piZoneQualiteDesserte r, piLongueurLiaisonAerienne r, piLongueurLiaisonSouterraine r
      , piProdAutonomePuissance r, piCoupureLocalisation r, piCoupureRestrictionMotif r
      , piLimiteurPuissance r
      , piTypeComptage r, piModeReleve r, piMediaReleve r, piTensionComptage r, piComptageParticularite r
      , piMatriculeCompteur r, piNumeroSerieCompteur r
      , piPeriodeDeploiementLinky r, piIntensiteNominale r, piPuissanceMaxCompteur r
      , piRegimeProprieteCompteur r, piCompteurSituation r
      , piDisjoncteurCalibre r, piDisjoncteurNature r, piDisjoncteurSituation r
      , piDisjoncteurIntensiteReglage r, piDisjoncteurRegimePropriete r
      , piTcCalibre r, piTcClassePrecision r, piTcCouplage r, piTcPosition r, piTcRegimePropriete r
      , piTtCalibre r, piTtClassePrecision r, piTtCouplage r
      , piRelaisNature r, piRelaisPlageHc r, piRelaisTypeCommande r, piRelaisRegimePropriete r
      , piProductionFiliere r, piProductionTechnologie r
      , fmap tshow (piCoefficientLecture r)
      , fmap tshow (piPertesFer r), fmap tshow (piPertesJoules r), fmap tshow (piPertesReactives r)
      ]
    textFields' t =
      [ segment t, etatContractuel t, etatAlimentation t
      , puissanceSouscrite t, domaineTension t
      , adresseNumeroNomVoie t, adresseBatiment t, adresseEscalierEtage t
      , adresseLieuDit t, adresseCodePostal t, adresseCommune t
      , niveauOuvertureServices t, dateModifFta t
      , dateAugmentationPuissance t, dateDiminutionPuissance t
      , dateMesSoutirage t, dateMesInjection t, datePremierePoseLinky t
      , telephoneDepannage t, autoConsoCollective t, autoConsoIndividuelle t
      , puissanceSouscriteUnite t, formuleTarifaireCode t, formuleTarifaireLibelle t
      , codeTarifAcheminement t, typeOffre t, contexteUtilisation t
      , forfaitValeur t, forfaitUnite t, calendrierTurpeCode t
      , groupePeriodeMobile t, groupePeriodeMobileDistrib t
      , dateDebutContrat t, natureContrat t, dateRefusPoseAmm t
      , categorieClient t, typeResidence t, referenceClient t
      , titulaireCivilite t, titulaireNom t, titulairePrenom t
      , titulaireDenominationSociale t, titulaireNomCommercial t
      , titulaireSiren t, titulaireSiret t, titulaireSecteur t, titulaireActiviteNaf t
      , referenceContrat t
      , tensionLivraison t, puissanceRaccordSoutirage t, puissanceRaccordInjection t
      , puissanceLimiteSoutirage t, tensionContractuelle t, modeAlimApresCompteur t
      , zoneQualiteDesserte t, longueurLiaisonAerienne t, longueurLiaisonSouterraine t
      , prodAutonomePuissance t, coupureLocalisation t, coupureRestrictionMotif t
      , limiteurPuissance t
      , typeComptage t, modeReleve t, mediaReleve t, tensionComptage t, comptageParticularite t
      , matriculeCompteur t, numeroSerieCompteur t
      , periodeDeploiementLinky t, intensiteNominale t, puissanceMaxCompteur t
      , regimeProprieteCompteur t, compteurSituation t
      , disjoncteurCalibre t, disjoncteurNature t, disjoncteurSituation t
      , disjoncteurIntensiteReglage t, disjoncteurRegimePropriete t
      , tcCalibre t, tcClassePrecision t, tcCouplage t, tcPosition t, tcRegimePropriete t
      , ttCalibre t, ttClassePrecision t, ttCouplage t
      , relaisNature t, relaisPlageHc t, relaisTypeCommande t, relaisRegimePropriete t
      , productionFiliere t, productionTechnologie t
      , fmap tshow (coefficientLecture t)
      , fmap tshow (pertesFer t), fmap tshow (pertesJoules t), fmap tshow (pertesReactives t)
      ]
    intFields r =
      [ piTypageSensible r, piTypageAlimComplementaire r, piTypageAlimSecours r
      , piTypageBornePoste r, piTypageBorneFixe r
      , piTypeInjection r, piRefusPoseAmm r
      , piTeleoperable r, piEligiblePeriodeMobile r, piBoitierTelereport r
      , piTicActivee r, piTicActivable r, piTicStandard r
      , piCompteurAccessibilite r
      , piDisjoncteurAccessibilite r
      , piNbFilsBranchement r, piProdAutonomeNb r, piNbFilsCompteur r
      , piDisjoncteurNbPoles r, piNbFilsBranchement r
      ]
    intFields' t =
      [ boolToInt (typageSensible t), boolToInt (typageAlimComplementaire t)
      , boolToInt (typageAlimSecours t)
      , boolToInt (typageBornePoste t), boolToInt (typageBorneFixe t)
      , boolToInt (typeInjection t), boolToInt (refusPoseAmm t)
      , boolToInt (teleoperable t), boolToInt (eligiblePeriodeMobile t), boolToInt (boitierTelereport t)
      , boolToInt (ticActivee t), boolToInt (ticActivable t), boolToInt (ticStandard t)
      , boolToInt (compteurAccessibilite t)
      , boolToInt (disjoncteurAccessibilite t)
      , nbFilsBranchement t, prodAutonomeNb t, nbFilsCompteur t
      , disjoncteurNbPoles t, nbFilsBranchement t
      ]

-- | Insère un C68 uniquement si les données ont changé depuis la dernière ingestion.
insertPrmInfoIfChanged :: Connection -> IngestionId -> UTCTime -> InfoTechniqueContractuelle -> IO ()
insertPrmInfoIfChanged conn ingId dateIng itc = do
  mPrev <- queryLatestPrmInfo conn
  let changed = case mPrev of
        Nothing  -> True
        Just row -> hasC68Changed row itc
  when changed $ do
    execute conn
      "INSERT INTO elec_prm_info \
      \ (segment, etat_contractuel, etat_alimentation, puissance_souscrite, domaine_tension,\
      \  adresse_numero_nom_voie, adresse_batiment, adresse_escalier_etage,\
      \  adresse_lieu_dit, adresse_code_postal, adresse_commune,\
      \  typage_sensible, typage_alimentation_complementaire, typage_alimentation_secours,\
      \  typage_borne_poste, typage_borne_fixe,\
      \  niveau_ouverture_services, date_modif_fta,\
      \  date_augmentation_puissance, date_diminution_puissance,\
      \  date_mes_soutirage, date_mes_injection, date_premiere_pose_linky,\
      \  telephone_depannage, auto_conso_collective, auto_conso_individuelle,\
      \  puissance_souscrite_unite, formule_tarifaire_code, formule_tarifaire_libelle,\
      \  code_tarif_acheminement, type_offre, contexte_utilisation,\
      \  forfait_valeur, forfait_unite, calendrier_turpe_code,\
      \  groupe_periode_mobile, groupe_periode_mobile_distributeur,\
      \  date_debut_contrat, nature_contrat, type_injection,\
      \  refus_pose_amm, date_refus_pose_amm,\
      \  categorie_client, type_residence, reference_client,\
      \  titulaire_civilite, titulaire_nom, titulaire_prenom,\
      \  titulaire_denomination_sociale, titulaire_nom_commercial,\
      \  titulaire_siren, titulaire_siret, titulaire_secteur, titulaire_activite_naf,\
      \  reference_contrat, tension_livraison,\
      \  puissance_raccordement_soutirage, puissance_raccordement_injection,\
      \  puissance_limite_soutirage, tension_contractuelle,\
      \  mode_alimentation_apres_compteur, nb_fils_branchement, zone_qualite_desserte,\
      \  longueur_liaison_aerienne, longueur_liaison_souterraine,\
      \  prod_autonome_nb, prod_autonome_puissance,\
      \  coupure_localisation, coupure_restriction_motif, limiteur_puissance,\
      \  type_comptage, mode_releve, media_releve,\
      \  teleoperable, eligible_periode_mobile, tension_comptage, comptage_particularite,\
      \  boitier_telereport,\
      \  matricule_compteur, numero_serie_compteur,\
      \  tic_activee, tic_activable, tic_standard, periode_deploiement_linky,\
      \  intensite_nominale, puissance_max_compteur, coefficient_lecture,\
      \  nb_fils_compteur, regime_propriete_compteur,\
      \  compteur_accessibilite, compteur_situation,\
      \  disjoncteur_calibre, disjoncteur_nature, disjoncteur_nb_poles,\
      \  disjoncteur_accessibilite, disjoncteur_situation,\
      \  disjoncteur_intensite_reglage, disjoncteur_regime_propriete,\
      \  tc_calibre, tc_classe_precision, tc_couplage, tc_position, tc_regime_propriete,\
      \  tt_calibre, tt_classe_precision, tt_couplage,\
      \  pertes_fer, pertes_joules, pertes_reactives,\
      \  relais_nature, relais_plage_hc, relais_type_commande, relais_regime_propriete,\
      \  production_filiere, production_technologie,\
      \  date_ingestion, ingestion_id) \
      \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,\
      \         ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,\
      \         ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,\
      \         ?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
      -- groups of ≤10 joined with :.
      ( (segment itc, etatContractuel itc, etatAlimentation itc
        , puissanceSouscrite itc, domaineTension itc
        , adresseNumeroNomVoie itc, adresseBatiment itc, adresseEscalierEtage itc
        , adresseLieuDit itc, adresseCodePostal itc)
      :. Only (adresseCommune itc)
      :. (boolToInt (typageSensible itc), boolToInt (typageAlimComplementaire itc)
         , boolToInt (typageAlimSecours itc)
         , boolToInt (typageBornePoste itc), boolToInt (typageBorneFixe itc)
         , niveauOuvertureServices itc, dateModifFta itc
         , dateAugmentationPuissance itc, dateDiminutionPuissance itc
         , dateMesSoutirage itc)
      :. (dateMesInjection itc, datePremierePoseLinky itc
         , telephoneDepannage itc, autoConsoCollective itc, autoConsoIndividuelle itc
         , puissanceSouscriteUnite itc, formuleTarifaireCode itc, formuleTarifaireLibelle itc
         , codeTarifAcheminement itc, typeOffre itc)
      :. (contexteUtilisation itc, forfaitValeur itc, forfaitUnite itc
         , calendrierTurpeCode itc, groupePeriodeMobile itc, groupePeriodeMobileDistrib itc
         , dateDebutContrat itc, natureContrat itc
         , boolToInt (typeInjection itc), boolToInt (refusPoseAmm itc))
      :. (dateRefusPoseAmm itc, categorieClient itc, typeResidence itc, referenceClient itc
         , titulaireCivilite itc, titulaireNom itc, titulairePrenom itc
         , titulaireDenominationSociale itc, titulaireNomCommercial itc, titulaireSiren itc)
      :. (titulaireSiret itc, titulaireSecteur itc, titulaireActiviteNaf itc
         , referenceContrat itc, tensionLivraison itc
         , puissanceRaccordSoutirage itc, puissanceRaccordInjection itc
         , puissanceLimiteSoutirage itc, tensionContractuelle itc, modeAlimApresCompteur itc)
      :. (nbFilsBranchement itc, zoneQualiteDesserte itc
         , longueurLiaisonAerienne itc, longueurLiaisonSouterraine itc
         , prodAutonomeNb itc, prodAutonomePuissance itc
         , coupureLocalisation itc, coupureRestrictionMotif itc, limiteurPuissance itc
         , typeComptage itc)
      :. (modeReleve itc, mediaReleve itc
         , boolToInt (teleoperable itc), boolToInt (eligiblePeriodeMobile itc)
         , tensionComptage itc, comptageParticularite itc
         , boolToInt (boitierTelereport itc)
         , matriculeCompteur itc, numeroSerieCompteur itc
         , boolToInt (ticActivee itc))
      :. (boolToInt (ticActivable itc), boolToInt (ticStandard itc)
         , periodeDeploiementLinky itc, intensiteNominale itc, puissanceMaxCompteur itc
         , coefficientLecture itc, nbFilsCompteur itc, regimeProprieteCompteur itc
         , boolToInt (compteurAccessibilite itc), compteurSituation itc)
      :. (disjoncteurCalibre itc, disjoncteurNature itc, disjoncteurNbPoles itc
         , boolToInt (disjoncteurAccessibilite itc), disjoncteurSituation itc
         , disjoncteurIntensiteReglage itc, disjoncteurRegimePropriete itc
         , tcCalibre itc, tcClassePrecision itc, tcCouplage itc)
      :. (tcPosition itc, tcRegimePropriete itc
         , ttCalibre itc, ttClassePrecision itc, ttCouplage itc
         , pertesFer itc, pertesJoules itc, pertesReactives itc
         , relaisNature itc, relaisPlageHc itc)
      :. (relaisTypeCommande itc, relaisRegimePropriete itc
         , productionFiliere itc, productionTechnologie itc
         , fmtUTC dateIng, ingId)
      )
    newId <- fmap fromIntegral (lastInsertRowId conn)
    insertCalendriers conn newId (calendriersFournisseur itc)
    insertContinuites conn newId (continuiteFourniture itc)
    insertQualites    conn newId (qualiteFourniture itc)

insertCalendriers :: Connection -> Int -> [CalendrierFournisseur] -> IO ()
insertCalendriers conn prmInfoId = mapM_ ins
  where
    ins cf = execute conn
      "INSERT INTO elec_calendriers_fournisseur (prm_info_id, code, periode_mobile_autorisee, profilable) \
      \ VALUES (?,?,?,?)"
      (prmInfoId, cfCode cf, boolToInt (cfPeriodeMobileAutorisee cf), boolToInt (cfProfilable cf))

insertContinuites :: Connection -> Int -> [ContinuiteFourniture] -> IO ()
insertContinuites conn prmInfoId = mapM_ ins
  where
    ins c = execute conn
      "INSERT INTO elec_continuite_fourniture \
      \ (prm_info_id, id_fonctionnel, type, periodicite, date_reference, mois_reference,\
      \  nb_coupures_breves, nb_coupures_longues, nb_total_coupures, type_coupures) \
      \ VALUES (?,?,?,?,?,?,?,?,?,?)"
      ( prmInfoId, contIdFonctionnel c, contType c, contPeriodicite c
      , contDateReference c, contMoisReference c
      , contNbCoupuresBreves c, contNbCoupuresLongues c, contNbTotalCoupures c
      , contTypeCoupures c )

insertQualites :: Connection -> Int -> [QualiteFourniture] -> IO ()
insertQualites conn prmInfoId = mapM_ ins
  where
    ins q = execute conn
      "INSERT INTO elec_qualite_fourniture \
      \ (prm_info_id, id_fonctionnel, periodicite, date_reference,\
      \  nb_creux, profondeur_creux, duree_creux_valeur, duree_creux_unite) \
      \ VALUES (?,?,?,?,?,?,?,?)"
      ( prmInfoId, qualIdFonctionnel q, qualPeriodicite q, qualDateReference q
      , qualNbCreux q, qualProfondeurCreux q, qualDureeValeur q, qualDureeUnite q )
