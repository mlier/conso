{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template d'affichage pour RechercherServicesAccesDonneesV10.
module Display.AcsServicesDisplay () where

import           Brick
import           Data.List (intercalate)
import           Text.XML.HaXml.Schema.Schema (SimpleType(simpleTypeText))

import           Display
import           Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10Type


instance Renderable RechercherServicesAccesDonneesReponseType where
    toWidget (Left (code, msg)) = renderError code msg
    toWidget (Right resp) =
        case rechercherServicesAccesDonneesReponseType_servicesSouscrits resp of
            Nothing -> section "Services AccesDonnees" [ustr "(aucun service souscrit)"]
            Just ss -> vBox $ map renderService
                                   (servicesSouscritsType_serviceSouscrit ss)


renderService :: ServiceSouscritType -> Widget ()
renderService s =
    let sid    = simpleTypeText (serviceSouscritType_serviceSouscritId s)
        code   = simpleTypeText (serviceSouscritType_serviceSouscritCode s)
        etats  = map simpleTypeText (serviceSouscritType_etatCode s)
        etat   = intercalate ", " etats
        titre  = sid ++ " — " ++ code ++ " — " ++ etat
        opts   = maybe [] optionsPublicationType_optionPublication
                          (serviceSouscritType_optionsPublication s)
        secFn  = if "ACTIF" `elem` etats then sectionActif else sectionTermine
    in secFn titre $
        [ field      "Libellé"         (simpleTypeText $ serviceSouscritType_serviceSouscritLibelle s)
        , field      "PRM"             (simpleTypeText $ serviceSouscritType_pointId s)
        , maybeField "Contrat"         (simpleTypeText <$> serviceSouscritType_contratId s)
        , maybeField "Libellé contrat" (simpleTypeText <$> serviceSouscritType_contratLibelle s)
        , field      "Début"           (intercalate ", " $ map simpleTypeText $ serviceSouscritType_dateDebut s)
        , maybeField "Fin"             (simpleTypeText <$> serviceSouscritType_dateFin s)
        , maybeField "Motif de fin"    (simpleTypeText <$> serviceSouscritType_motifFinLibelle s)
        , maybeField "Type mesures"    (simpleTypeText <$> serviceSouscritType_mesuresTypeCode s)
        , maybeField "Pas"             (simpleTypeText <$> serviceSouscritType_mesuresPas s)
        , field      "Injection"       (intercalate ", " $ map simpleTypeText $ serviceSouscritType_injection s)
        , field      "Soutirage"       (intercalate ", " $ map simpleTypeText $ serviceSouscritType_soutirage s)
        ] ++ map renderOption opts

renderOption :: OptionPublicationType -> Widget ()
renderOption opt = vBox
    [ maybeField "  Option — Corrigées"    (simpleTypeText <$> optionPublicationType_mesuresCorrigees opt)
    , field      "  Option — Périodicité"  (simpleTypeText $ optionPublicationType_periodiciteTransmission opt)
    , field      "  Option — Début"        (simpleTypeText $ optionPublicationType_dateDebut opt)
    , maybeField "  Option — Fin"          (simpleTypeText <$> optionPublicationType_dateFin opt)
    ]
