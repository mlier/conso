{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template d'affichage pour RechercherPointV20.
module Display.RechercheDisplay () where

import           Data.List (intercalate)

import           Brick
import           Text.XML.HaXml.Schema.Schema (SimpleType(simpleTypeText))

import           Display
import           Conso.Fr.Elec.Sge.RechercherPointV20Type
import           Conso.Fr.Elec.Sge.EnedisDictionnaireTypeComplexeV50
    ( AdresseAfnorType(..)
    , PointEtatContractuelType(..)
    , StructureComptageType(..) )
import           Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 (Chaine255Type)


instance Renderable RechercherPointResponseType where
    toWidget (Left (code, msg)) = renderError code msg
    toWidget (Right resp) =
        case rechercherPointResponseType_points resp of
            Nothing     -> ustr "Aucun point trouvé."
            Just points -> vBox $ zipWith renderPoint [1..] (pointsType_point points)


renderPoint :: Int -> PointType -> Widget ()
renderPoint n pt = vBox
    [ section ("Point " ++ show n ++ " — PRM " ++ simpleTypeText (pointType_id pt))
        [ field      "État contractuel"  (renderEtat (pointType_etatContractuel pt))
        , maybeField "Type de comptage"  (renderComptage <$> pointType_typeComptage pt)
        , maybeField "Nom client"        (simpleTypeText <$> pointType_nomClientFinalOuDenominationSociale pt)
        ]
    , section "Adresse" (renderAdresseAfnor (pointType_adresseInstallationNormalisee pt))
    , renderListe "Matricule(s)"    (pointType_matricule   pt)
    , renderListe "Numéro(s) série" (pointType_numeroSerie pt)
    ]


renderAdresseAfnor :: AdresseAfnorType -> [Widget ()]
renderAdresseAfnor a =
    [ maybeField "Ligne 1"  (simpleTypeText <$> adresseAfnorType_ligne1 a)
    , maybeField "Ligne 2"  (simpleTypeText <$> adresseAfnorType_ligne2 a)
    , maybeField "Ligne 3"  (simpleTypeText <$> adresseAfnorType_ligne3 a)
    , maybeField "Ligne 4"  (simpleTypeText <$> adresseAfnorType_ligne4 a)
    , maybeField "Ligne 5"  (simpleTypeText <$> adresseAfnorType_ligne5 a)
    , field      "Commune"  (simpleTypeText (adresseAfnorType_ligne6 a))
    , maybeField "Pays"     (simpleTypeText <$> adresseAfnorType_ligne7 a)
    ]


renderEtat :: PointEtatContractuelType -> String
renderEtat e = simpleTypeText (pointEtatContractuelType_code e)
    ++ maybe "" ((" - " ++) . simpleTypeText) (pointEtatContractuelType_libelle e)


renderComptage :: StructureComptageType -> String
renderComptage sc = simpleTypeText (structureComptageType_code sc)
    ++ maybe "" ((" - " ++) . simpleTypeText) (structureComptageType_libelle sc)


renderListe :: String -> [Chaine255Type] -> Widget ()
renderListe _   []  = emptyWidget
renderListe lbl xs  = field lbl (intercalate ", " (map simpleTypeText xs))
