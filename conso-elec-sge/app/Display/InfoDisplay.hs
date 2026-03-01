{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template d'affichage pour ConsulterDonneesTechniquesContractuellesV10.
module Display.InfoDisplay () where

import           Brick
import           Text.XML.HaXml.Schema.Schema (SimpleType(simpleTypeText))

import           Display
import           Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10Type


instance Renderable ConsulterDonneesTechniquesContractuellesResponseType where
    toWidget (Left (code, msg)) = renderError code msg
    toWidget (Right resp)       =
        renderPoint (consulterDonneesTechniquesContractuellesResponseType_point resp)


renderPoint :: PointType -> Widget ()
renderPoint pt = vBox
    [ section "Identification"
        [ field "PRM"                      (simpleTypeText $ pointType_id pt)
        , field "État contractuel"         (show $ pointDonneesGeneralesType_etatContractuel dg)
        , maybeField "Segment clientèle"   (show <$> pointDonneesGeneralesType_segment dg)
        , maybeField "Niveau de services"  (simpleTypeText <$> pointDonneesGeneralesType_niveauOuvertureServices dg)
        ]
    , section "Adresse"
        (renderAdresse $ pointDonneesGeneralesType_adresseInstallation dg)
    , maybe emptyWidget renderAlimentation  (pointType_situationAlimentation pt)
    , maybe emptyWidget renderComptage      (pointType_situationComptage pt)
    , maybe emptyWidget renderContractuel   (pointType_situationContractuelle pt)
    ]
  where
    dg = pointType_donneesGenerales pt


renderAdresse :: AdresseInstallationType -> [Widget ()]
renderAdresse a =
    [ maybeField "Escalier / Étage / Appt" (simpleTypeText <$> adresseInstallationType_escalierEtEtageEtAppartement a)
    , maybeField "Bâtiment"                (simpleTypeText <$> adresseInstallationType_batiment a)
    , maybeField "Voie"                    (simpleTypeText <$> adresseInstallationType_numeroEtNomVoie a)
    , maybeField "Lieu-dit"                (simpleTypeText <$> adresseInstallationType_lieuDit a)
    , maybeField "Code postal"             (simpleTypeText <$> adresseInstallationType_codePostal a)
    , field      "Commune"                 (show $ adresseInstallationType_commune a)
    ]


renderAlimentation :: SituationAlimentationType -> Widget ()
renderAlimentation sa =
    section "Alimentation" $
    case situationAlimentationType_alimentationPrincipale sa of
        Nothing -> [str "(aucune)"]
        Just ap ->
            [ field      "Domaine de tension"    (show $ alimentationPrincipaleType_domaineTension ap)
            , maybeField "Tension de livraison"  (show <$> alimentationPrincipaleType_tensionLivraison ap)
            , maybeField "Mode après compteur"   (show <$> alimentationPrincipaleType_modeApresCompteur ap)
            , maybeField "Puissance raccordement soutirage"
                         (show <$> alimentationPrincipaleType_puissanceRaccordementSoutirage ap)
            ]


renderComptage :: SituationComptageType -> Widget ()
renderComptage sc =
    section "Comptage" $
    [ maybeField "Mode de relève"  (show <$> situationComptageType_modeReleve sc)
    , maybeField "Média de relève" (show <$> situationComptageType_mediaReleve sc)
    ]
    ++ case situationComptageType_dispositifComptage sc of
        Nothing  -> [str "(pas de dispositif)"]
        Just dc' ->
            field "Type de comptage" (show $ dispositifComptageType_typeComptage dc')
            : renderCompteurs (maybe [] compteursType_compteur $ dispositifComptageType_compteurs dc')


renderCompteurs :: [CompteurType] -> [Widget ()]
renderCompteurs [] = []
renderCompteurs cs = zipWith renderCompteur [1 :: Int ..] cs


renderCompteur :: Int -> CompteurType -> Widget ()
renderCompteur n c =
    section ("Compteur " ++ show n)
        [ maybeField "Matricule"    (simpleTypeText <$> compteurType_matricule c)
        , maybeField "TIC activée"  (show           <$> compteurType_ticActivee c)
        , maybeField "TIC standard" (show           <$> compteurType_ticStandard c)
        , maybeField "TIC activable" (show          <$> compteurType_ticActivable c)
        ]


renderContractuel :: SituationContractuelleType -> Widget ()
renderContractuel sc =
    section "Situation contractuelle" $
    case situationContractuelleType_structureTarifaire sc of
        Nothing -> [str "(aucune structure tarifaire)"]
        Just st ->
            [ maybeField "Formule tarifaire acheminement"
                         (show <$> structureTarifaireType_formuleTarifaireAcheminement st)
            , maybeField "Puissance souscrite max"
                         (show <$> structureTarifaireType_puissanceSouscriteMax st)
            ]
