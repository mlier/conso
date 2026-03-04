{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template d'affichage pour CommanderArretServiceSouscritMesuresV10.
module Display.ArretDisplay () where

import           Text.XML.HaXml.Schema.Schema (SimpleType(simpleTypeText))

import           Display
import           Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10Type


instance Renderable CommanderArretServiceSouscritMesuresResponseType where
    toWidget (Left (code, msg)) = renderError code msg
    toWidget (Right resp) =
        section "Arrêt service souscrit commandé" $
            case commanderArretServiceSouscritMesuresResponseType_affaireId resp of
                Nothing  -> [ustr "Demande prise en compte."]
                Just aid -> [field "Identifiant d'affaire" (simpleTypeText aid)]
