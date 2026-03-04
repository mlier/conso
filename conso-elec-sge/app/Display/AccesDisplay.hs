{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template d'affichage pour CommanderAccesDonneesMesuresV10.
module Display.AccesDisplay () where

import           Brick
import           Text.XML.HaXml.Schema.Schema (SimpleType(simpleTypeText))

import           Display
import           Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10Type


instance Renderable CommanderAccesDonneesMesuresResponseType where
    toWidget (Left (code, msg)) = renderError code msg
    toWidget (Right resp) =
        section "Accès données mesures" $
            [ field      "Identifiant d'affaire" (simpleTypeText $ commanderAccesDonneesMesuresResponseType_affaireId resp)
            , maybeField "Service souscrit"      (simpleTypeText <$> commanderAccesDonneesMesuresResponseType_serviceSouscritId resp)
            ] ++ maybe [] renderPrestations (commanderAccesDonneesMesuresResponseType_prestations resp)


renderPrestations :: PrestationsType -> [Widget ()]
renderPrestations p = renderPrestation (prestationsType_prestation p)


renderPrestation :: PrestationType -> [Widget ()]
renderPrestation p =
    [ field      "Prestation (fiche)"   (simpleTypeText $ prestationFicheType_code $ prestationType_fiche p)
    , maybeField "Prestation (libellé)" (simpleTypeText <$> prestationFicheType_libelle (prestationType_fiche p))
    , maybeField "Option"               (simpleTypeText . prestationOptionType_code <$> prestationType_option p)
    , maybeField "Cas"                  (simpleTypeText . prestationCasType_code    <$> prestationType_cas   p)
    ]
