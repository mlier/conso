{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template d'affichage pour ConsulterDonneesTechniquesContractuellesV10.
module Display.InfoDisplay () where

import           Brick
import           Data.List (intercalate)
import           Text.XML.HaXml.Schema.Schema (SimpleType(simpleTypeText))

import           Display
import           Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10Type
import qualified Conso.Fr.Elec.Sge.EnedisDictionnaireTypeComplexeV50 as Dc


instance Renderable ConsulterDonneesTechniquesContractuellesResponseType where
    toWidget (Left (code, msg)) = renderError code msg
    toWidget (Right resp)       =
        renderPoint (consulterDonneesTechniquesContractuellesResponseType_point resp)


-- ---------------------------------------------------------------------------
-- Helpers

-- | Maybe libellé → String (fallback "—")
mlib :: SimpleType a => Maybe a -> String
mlib = maybe "\x2014" simpleTypeText

-- | Valeur + unité d'une puissance
renderPuissance :: Dc.PuissanceType -> String
renderPuissance p =
    simpleTypeText (Dc.puissanceType_valeur p)
    ++ " "
    ++ simpleTypeText (Dc.puissanceType_unite p)

-- | Décodage des entités HTML (libellés Enedis)
decodeHtml :: String -> String
decodeHtml []           = []
decodeHtml ('&':'l':'t':';':rest)        = '<' : decodeHtml rest
decodeHtml ('&':'g':'t':';':rest)        = '>' : decodeHtml rest
decodeHtml ('&':'a':'m':'p':';':rest)    = '&' : decodeHtml rest
decodeHtml (c:rest)                      = c   : decodeHtml rest


-- ---------------------------------------------------------------------------
-- Point principal

renderPoint :: PointType -> Widget ()
renderPoint pt = vBox
    [ section "Identification"
        [ field      "PRM"                                (simpleTypeText $ pointType_id pt)
        , field      "État contractuel"                   (mlib $ Dc.pointEtatContractuelType_libelle $ pointDonneesGeneralesType_etatContractuel dg)
        , maybeField "Segment clientèle"                  (mlib . Dc.pointSegmentClienteleType_libelle <$> pointDonneesGeneralesType_segment dg)
        , maybeField "Niveau ouverture services"          (simpleTypeText <$> pointDonneesGeneralesType_niveauOuvertureServices dg)
        , maybeField "Date modif. FTA"                    (simpleTypeText <$> pointDonneesGeneralesType_dateDerniereModificationFormuleTarifaireAcheminement dg)
        , maybeField "Date augmentation puissance"        (simpleTypeText <$> pointDonneesGeneralesType_dateDerniereAugmentationPuissanceSouscrite dg)
        , maybeField "Date diminution puissance"          (simpleTypeText <$> pointDonneesGeneralesType_dateDerniereDiminutionPuissanceSouscrite dg)
        ]
    , section "Adresse"
        (renderAdresse $ pointDonneesGeneralesType_adresseInstallation dg)
    , maybe emptyWidget renderAlimentation  (pointType_situationAlimentation pt)
    , maybe emptyWidget renderComptage      (pointType_situationComptage pt)
    , maybe emptyWidget renderContractuel   (pointType_situationContractuelle pt)
    ]
  where
    dg = pointType_donneesGenerales pt


-- ---------------------------------------------------------------------------
-- Adresse

renderAdresse :: AdresseInstallationType -> [Widget ()]
renderAdresse a =
    [ maybeField "Escalier / Étage / Appt" (simpleTypeText <$> adresseInstallationType_escalierEtEtageEtAppartement a)
    , maybeField "Bâtiment"                (simpleTypeText <$> adresseInstallationType_batiment a)
    , maybeField "Voie"                    (simpleTypeText <$> adresseInstallationType_numeroEtNomVoie a)
    , maybeField "Lieu-dit"                (simpleTypeText <$> adresseInstallationType_lieuDit a)
    , maybeField "Code postal"             (simpleTypeText <$> adresseInstallationType_codePostal a)
    , field      "Commune"                 (renderCommune $ adresseInstallationType_commune a)
    ]

renderCommune :: Dc.CommuneFranceType -> String
renderCommune c =
    simpleTypeText (Dc.communeFranceType_libelle c)
    ++ " ("
    ++ simpleTypeText (Dc.communeFranceType_code c)
    ++ ")"

renderDomaineTension :: Dc.DomaineTensionType -> String
renderDomaineTension d =
    decodeHtml (mlib $ Dc.domaineTensionType_libelle d)
    ++ " ("
    ++ simpleTypeText (Dc.domaineTensionType_code d)
    ++ ")"


-- ---------------------------------------------------------------------------
-- Alimentation

renderAlimentation :: SituationAlimentationType -> Widget ()
renderAlimentation sa =
    section "Alimentation" $
    case situationAlimentationType_alimentationPrincipale sa of
        Nothing -> [ustr "(aucune)"]
        Just ap ->
            [ field      "Domaine de tension"              (renderDomaineTension $ alimentationPrincipaleType_domaineTension ap)
            , maybeField "Tension de livraison"            (mlib . Dc.tensionLivraisonType_libelle <$> alimentationPrincipaleType_tensionLivraison ap)
            , maybeField "Mode après compteur"             (mlib . Dc.alimentationModeApresCompteurType_libelle <$> alimentationPrincipaleType_modeApresCompteur ap)
            , maybeField "Puissance raccordement soutirage" (renderPuissance <$> alimentationPrincipaleType_puissanceRaccordementSoutirage ap)
            ]


-- ---------------------------------------------------------------------------
-- Comptage

renderComptage :: SituationComptageType -> Widget ()
renderComptage sc = vBox $
    section "Relève"
        ([ maybeField "Mode de relève"   (mlib . Dc.releveModeType_libelle <$> situationComptageType_modeReleve sc)
         , maybeField "Média de relève"  (mlib . Dc.releveMediaType_libelle <$> situationComptageType_mediaReleve sc)
         , maybeField "Plages HC futures" (mlib . Dc.plageHeuresCreusesType_libelle <$> situationComptageType_futuresPlagesHeuresCreuses sc)
         ]
        ++ maybe [] renderModalitesReleve (situationComptageType_caracteristiquesReleve sc))
    : maybe [] renderDispositif (situationComptageType_dispositifComptage sc)

renderModalitesReleve :: ModalitesReleveType -> [Widget ()]
renderModalitesReleve mr =
    [ maybeField "Mode traitement"    (mlib . Dc.releveTraitementModeType_libelle <$> modalitesReleveType_modeTraitement mr)
    , maybeField "Périodicité"        (mlib . Dc.periodiciteType_libelle <$> modalitesReleveType_periodicite mr)
    , maybeField "Plage de relève"    (mlib . Dc.relevePlageType_libelle <$> modalitesReleveType_plageReleve mr)
    ]

renderDispositif :: DispositifComptageType -> [Widget ()]
renderDispositif dc' =
    section "Dispositif de comptage"
        ([ field "Type de comptage" (mlib $ Dc.structureComptageType_libelle $ dispositifComptageType_typeComptage dc')
         ]
        ++ maybe [] renderDisjoncteur    (dispositifComptageType_disjoncteur dc')
        ++ maybe [] renderRelais         (dispositifComptageType_relais dc')
        ++ maybe [] renderTransfoCourant (dispositifComptageType_transformateurCourant dc')
        ++ maybe [] renderTransfoTension (dispositifComptageType_transformateurTension dc'))
    : renderCompteurs (maybe [] compteursType_compteur $ dispositifComptageType_compteurs dc')

renderDisjoncteur :: DisjoncteurType -> [Widget ()]
renderDisjoncteur d =
    [ maybeField "Disjoncteur calibre" (mlib . Dc.disjoncteurCalibreType_libelle <$> disjoncteurType_calibre d) ]

renderRelais :: RelaisType -> [Widget ()]
renderRelais r =
    [ maybeField "Plages HC" (simpleTypeText <$> relaisType_plageHeuresCreuses r) ]

renderTransfoCourant :: TransformateurCourantType -> [Widget ()]
renderTransfoCourant t =
    [ maybeField "TC calibre"         (mlib . Dc.transformateurCalibreType_libelle <$> transformateurCourantType_calibre t)
    , maybeField "TC couplage"        (mlib . Dc.transformateurCouplageType_libelle <$> transformateurCourantType_couplage t)
    , maybeField "TC classe précision" (mlib . Dc.transformateurPrecisionClasseType_libelle <$> transformateurCourantType_classePrecision t)
    , maybeField "TC position"        (mlib . Dc.transformateurCourantPositionType_libelle <$> transformateurCourantType_position t)
    ]

renderTransfoTension :: TransformateurTensionType -> [Widget ()]
renderTransfoTension t =
    [ maybeField "TT calibre"          (mlib . Dc.transformateurCalibreType_libelle <$> transformateurTensionType_calibre t)
    , maybeField "TT couplage"         (mlib . Dc.transformateurCouplageType_libelle <$> transformateurTensionType_couplage t)
    , maybeField "TT classe précision" (mlib . Dc.transformateurPrecisionClasseType_libelle <$> transformateurTensionType_classePrecision t)
    ]


-- ---------------------------------------------------------------------------
-- Compteurs

renderCompteurs :: [CompteurType] -> [Widget ()]
renderCompteurs [] = []
renderCompteurs cs = zipWith renderCompteur [1 :: Int ..] cs

renderCompteur :: Int -> CompteurType -> Widget ()
renderCompteur n c =
    section ("Compteur " ++ show n) $
        [ maybeField "Matricule"     (simpleTypeText <$> compteurType_matricule c)
        , maybeField "TIC activée"   (simpleTypeText <$> compteurType_ticActivee c)
        , maybeField "TIC standard"  (simpleTypeText <$> compteurType_ticStandard c)
        , maybeField "TIC activable" (simpleTypeText <$> compteurType_ticActivable c)
        , maybeField "Plages HC"     (simpleTypeText <$> compteurType_plagesHeuresCreuses c)
        ]
        ++ maybe [] renderProgrammationHoraire (compteurType_programmationHoraire c)

renderProgrammationHoraire :: ProgrammationHoraireType -> [Widget ()]
renderProgrammationHoraire ph =
    map renderPosteHoraire (programmationHoraireType_programmationPosteHoraire ph)

renderPosteHoraire :: ProgrammationPosteHoraireType -> Widget ()
renderPosteHoraire p =
    let code    = simpleTypeText $ programmationPosteHoraireType_code p
        libelle = mlib (programmationPosteHoraireType_libelle p)
        periodes = mlib (programmationPosteHoraireType_periodesHoraires p)
    in field ("Poste " ++ code ++ " — " ++ libelle) periodes


-- ---------------------------------------------------------------------------
-- Situation contractuelle

renderContractuel :: SituationContractuelleType -> Widget ()
renderContractuel sc =
    section "Situation contractuelle" $
    case situationContractuelleType_structureTarifaire sc of
        Nothing -> [ustr "(aucune structure tarifaire)"]
        Just st ->
            [ maybeField "Calendrier fournisseur"
                         (renderCalendrier <$> structureTarifaireType_calendrierFrn st)
            , maybeField "Formule tarifaire acheminement"
                         (renderFta <$> structureTarifaireType_formuleTarifaireAcheminement st)
            , maybeField "Puissance souscrite max"
                         (renderPuissance <$> structureTarifaireType_puissanceSouscriteMax st)

            ]
            ++ renderDenivele (structureTarifaireType_denivelePuissances st)

renderCalendrier :: Dc.CalendrierType -> String
renderCalendrier c =
    decodeHtml (mlib $ Dc.calendrierType_libelle c)
    ++ " ("
    ++ simpleTypeText (Dc.calendrierType_code c)
    ++ ")"

renderFta :: Dc.AcheminementTarifType -> String
renderFta fta =
    decodeHtml (mlib $ Dc.acheminementTarifType_libelle fta)
    ++ " ("
    ++ simpleTypeText (Dc.acheminementTarifType_code fta)
    ++ ")"

renderDenivele :: Maybe StructureTarifaireDenivelePuissancesType -> [Widget ()]
renderDenivele Nothing   = []
renderDenivele (Just dp) =
    case structureTarifaireDenivelePuissancesType_classesTemporelles dp of
        Nothing  -> []
        Just cts ->
            let classes = denivelePuissancesClassesTemporellesType_classeTemporelle cts
            in if null classes then []
               else [ field "Dénivélé puissances"
                        (intercalate " | " $ map renderClasseTemporelle classes) ]

renderClasseTemporelle :: DenivelePuissancesClasseTemporelleType -> String
renderClasseTemporelle ct =
    simpleTypeText (denivelePuissancesClasseTemporelleType_code ct)
    ++ " "
    ++ mlib (denivelePuissancesClasseTemporelleType_libelle ct)
    ++ " : "
    ++ renderPuissance (denivelePuissancesClasseTemporelleType_puissance ct)
