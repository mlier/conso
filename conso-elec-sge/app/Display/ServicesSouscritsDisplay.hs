{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template d'affichage pour RechercherServicesSouscritsMesuresV10.
module Display.ServicesSouscritsDisplay () where

import           Brick
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
        titre = sText (serviceSouscritMesuresType_serviceSouscritId s)
             ++ " — " ++ sText (serviceSouscritType_code typ)
             ++ " — " ++ sText (serviceSouscritMesuresType_etatCode s)
    in section titre
        [ field      "Libellé"        (sText $ serviceSouscritMesuresType_serviceSouscritLibelle s)
        , maybeField "Libellé type"   (sText <$> serviceSouscritType_libelle typ)
        , field      "PRM"            (sText $ serviceSouscritMesuresType_pointId s)
        , maybeField "Contrat"        (sText <$> serviceSouscritMesuresType_contratId s)
        , maybeField "Libellé contrat" (sText <$> serviceSouscritMesuresType_contratLibelle s)
        , field      "Début"          (sText $ serviceSouscritMesuresType_dateDebut s)
        , maybeField "Fin"            (sText <$> serviceSouscritMesuresType_dateFin s)
        , maybeField "Motif de fin"   (sText <$> serviceSouscritMesuresType_motifFinLibelle s)
        , maybeField "Type mesures"   (sText <$> serviceSouscritMesuresType_mesuresTypeCode s)
        , maybeField "Pas"            (sText <$> serviceSouscritMesuresType_mesuresPas s)
        , maybeField "Corrigées"      (sText <$> serviceSouscritMesuresType_mesuresCorrigees s)
        , maybeField "Périodicité"    (sText <$> serviceSouscritMesuresType_periodiciteTransmission s)
        , maybeField "Injection"      (sText <$> serviceSouscritMesuresType_injection s)
        , maybeField "Soutirage"      (sText <$> serviceSouscritMesuresType_soutirage s)
        ]
