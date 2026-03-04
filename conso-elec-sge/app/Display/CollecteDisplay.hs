{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template d'affichage pour CommanderCollectePublicationMesuresV30.
module Display.CollecteDisplay () where

import           Brick
import           Text.XML.HaXml.Schema.Schema (SimpleType(simpleTypeText))

import           Display
import           Conso.Fr.Elec.Sge.CommanderCollectePublicationMesuresV30Type
import qualified Conso.Fr.Elec.Sge.EnedisDictionnaireTypeComplexeV50 as Dc


instance Renderable CommanderCollectePublicationMesuresResponseType where
    toWidget (Left (code, msg)) = renderError code msg
    toWidget (Right resp) =
        section "Collecte/publication mesures commandée" $
            [ maybeField "Identifiant d'affaire" (simpleTypeText <$> commanderCollectePublicationMesuresResponseType_affaireId resp)
            , maybeField "Service souscrit"       (simpleTypeText <$> commanderCollectePublicationMesuresResponseType_serviceSouscritId resp)
            ] ++ maybe [] renderPrestations (commanderCollectePublicationMesuresResponseType_prestations resp)


renderPrestations :: PrestationsType -> [Widget ()]
renderPrestations p = concatMap renderPrestation (prestationsType_prestation p)


renderPrestation :: PrestationType -> [Widget ()]
renderPrestation p =
    [ field      "Prestation (rang)"    (simpleTypeText $ prestationType_rang p)
    , field      "Prestation (fiche)"   (simpleTypeText $ Dc.prestationFicheType_code $ prestationType_fiche p)
    , maybeField "Prestation (libellé)" (simpleTypeText <$> Dc.prestationFicheType_libelle (prestationType_fiche p))
    , maybeField "Option (code)"        (simpleTypeText . Dc.prestationOptionType_code <$> prestationType_option p)
    , maybeField "Option (libellé)"     (simpleTypeText <$> (prestationType_option p >>= Dc.prestationOptionType_libelle))
    ]
