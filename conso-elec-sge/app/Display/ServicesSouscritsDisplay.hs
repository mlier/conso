{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template d'affichage pour RechercherServicesSouscritsMesuresV10.
module Display.ServicesSouscritsDisplay () where

import           Brick
import           Text.XML.HaXml.Schema.Schema (SimpleType(simpleTypeText))

import           Display
import           Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10Type


instance Renderable RechercherServicesSouscritsMesuresResponseType where
    toWidget (Left (code, msg)) = renderError code msg
    toWidget (Right resp) =
        case rechercherServicesSouscritsMesuresResponseType_servicesSouscritsMesures resp of
            Nothing -> section "Services souscrits" [ustr "(aucun service souscrit)"]
            Just ss -> vBox $ map renderServiceSouscrit
                                   (servicesSouscritsMesuresType_serviceSouscritMesures ss)


renderServiceSouscrit :: ServiceSouscritMesuresType -> Widget ()
renderServiceSouscrit s =
    let typ = serviceSouscritMesuresType_serviceSouscritType s
        titre = simpleTypeText (serviceSouscritMesuresType_serviceSouscritId s)
             ++ " — " ++ simpleTypeText (serviceSouscritType_code typ)
             ++ " — " ++ simpleTypeText (serviceSouscritMesuresType_etatCode s)
    in section titre
        [ field      "Libellé"        (simpleTypeText $ serviceSouscritMesuresType_serviceSouscritLibelle s)
        , maybeField "Libellé type"   (simpleTypeText <$> serviceSouscritType_libelle typ)
        , field      "PRM"            (simpleTypeText $ serviceSouscritMesuresType_pointId s)
        , maybeField "Contrat"        (simpleTypeText <$> serviceSouscritMesuresType_contratId s)
        , maybeField "Libellé contrat" (simpleTypeText <$> serviceSouscritMesuresType_contratLibelle s)
        , field      "Début"          (simpleTypeText $ serviceSouscritMesuresType_dateDebut s)
        , maybeField "Fin"            (simpleTypeText <$> serviceSouscritMesuresType_dateFin s)
        , maybeField "Motif de fin"   (simpleTypeText <$> serviceSouscritMesuresType_motifFinLibelle s)
        , maybeField "Type mesures"   (simpleTypeText <$> serviceSouscritMesuresType_mesuresTypeCode s)
        , maybeField "Pas"            (simpleTypeText <$> serviceSouscritMesuresType_mesuresPas s)
        , maybeField "Corrigées"      (simpleTypeText <$> serviceSouscritMesuresType_mesuresCorrigees s)
        , maybeField "Périodicité"    (simpleTypeText <$> serviceSouscritMesuresType_periodiciteTransmission s)
        , maybeField "Injection"      (simpleTypeText <$> serviceSouscritMesuresType_injection s)
        , maybeField "Soutirage"      (simpleTypeText <$> serviceSouscritMesuresType_soutirage s)
        ]
