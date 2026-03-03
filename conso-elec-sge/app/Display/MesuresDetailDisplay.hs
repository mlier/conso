{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template d'affichage pour ConsulterMesuresDetailleesV3.
module Display.MesuresDetailDisplay () where

import           Brick
import           Text.XML.HaXml.Schema.Schema (SimpleType(simpleTypeText))

import           Display
import           Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesCommunV12Type


instance Renderable ConsulterMesuresDetailleesV3ResponseType where
    toWidget (Left (code, msg)) = renderError code msg
    toWidget (Right resp)       = vBox $
        [ section "Informations" $
            [ maybeField "PRM"            (simpleTypeText <$> consulterMesuresDetailleesV3ResponseType_pointId resp)
            , maybeField "Corrigées"      (simpleTypeText <$> consulterMesuresDetailleesV3ResponseType_mesuresCorrigees resp)
            , maybeField "Pas"            (simpleTypeText <$> consulterMesuresDetailleesV3ResponseType_pas resp)
            , maybeField "Mode de calcul" (simpleTypeText <$> consulterMesuresDetailleesV3ResponseType_modeCalcul resp)
            , maybeField "Type de valeur" (simpleTypeText <$> consulterMesuresDetailleesV3ResponseType_typeValeur resp)
            ] ++ maybe [] renderPeriode (consulterMesuresDetailleesV3ResponseType_periode resp)
        ]
        ++ map renderGrandeur  (consulterMesuresDetailleesV3ResponseType_grandeur  resp)
        ++ map renderContexte  (consulterMesuresDetailleesV3ResponseType_contexte  resp)


renderPeriode :: PeriodeType -> [Widget ()]
renderPeriode p =
    [ field "Début" (simpleTypeText $ periodeType_dateDebut p)
    , field "Fin"   (simpleTypeText $ periodeType_dateFin   p)
    ]


renderGrandeur :: GrandeurInstantanees -> Widget ()
renderGrandeur g =
    let titre = simpleTypeText (grandeurInstantanees_grandeurMetier   g)
             ++ " (" ++ simpleTypeText (grandeurInstantanees_grandeurPhysique g) ++ ")"
             ++ " [" ++ simpleTypeText (grandeurInstantanees_unite             g) ++ "]"
        rows  = case grandeurInstantanees_points g of
                  [] -> [ustr "(aucune donnée)"]
                  pts -> pointsHeader : map renderPoint pts
    in section titre rows


pointsHeader :: Widget ()
pointsHeader =
    withAttr labelAttr $ ustr $
    padTo 25 "Horodatage" ++ " | " ++ padTo 12 "Valeur" ++ " | " ++
    padTo 5 "Pas" ++ " | N | iv"


renderPoint :: Points -> Widget ()
renderPoint p = ustr $
    padTo 25 (simpleTypeText $ points_d p) ++ " | " ++
    padTo 12 (simpleTypeText $ points_v p) ++ " | " ++
    padTo 5  (maybe ""  simpleTypeText (points_p p)) ++ " | " ++
    maybe " " simpleTypeText (points_n  p)           ++ " | " ++
    maybe ""  simpleTypeText (points_iv p)


renderContexte :: Contexte -> Widget ()
renderContexte c =
    section (simpleTypeText (contexte_typeReleve c)
             ++ " — " ++ simpleTypeText (contexte_etapeMetier c)) $
    [ field      "Contexte de relevé" (simpleTypeText $ contexte_contexteReleve c)
    , maybeField "Motif de relevé"    (simpleTypeText <$> contexte_motifReleve c)
    ] ++ map renderGrandeurType (contexte_grandeur c)


renderGrandeurType :: GrandeurType -> Widget ()
renderGrandeurType g =
    section (simpleTypeText (grandeurType_grandeurMetier g)
             ++ " [" ++ simpleTypeText (grandeurType_unite g) ++ "]") $
    map renderCalendrier (grandeurType_calendrier g)


renderCalendrier :: Calendrier -> Widget ()
renderCalendrier c =
    section (simpleTypeText (calendrier_libelleCalendrier c)) $
    concatMap renderClasseTemporelle (calendrier_classeTemporelle c)


renderClasseTemporelle :: ClasseTemporelle -> [Widget ()]
renderClasseTemporelle ct =
    let titre = maybe "(sans libellé)" simpleTypeText (classeTemporelle_libelleClasseTemporelle ct)
        rows  = map renderValeur (classeTemporelle_valeur ct)
    in [section titre (if null rows then [ustr "(vide)"] else rows)]


renderValeur :: Valeur -> Widget ()
renderValeur v = ustr $
    padTo 25 (simpleTypeText $ valeur_d  v) ++ " | " ++
    padTo 12 (simpleTypeText $ valeur_v  v) ++ " | iv=" ++
    simpleTypeText (valeur_iv v)


padTo :: Int -> String -> String
padTo n s = take n (s ++ repeat ' ')
