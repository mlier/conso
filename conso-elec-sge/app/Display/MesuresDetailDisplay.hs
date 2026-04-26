{-# OPTIONS_GHC -Wno-orphans #-}

module Display.MesuresDetailDisplay () where

import           Brick
import           Data.List       (nub, sortBy)
import qualified Data.Map.Strict as Map
import           Text.XML.HaXml.Schema.Schema (SimpleType(simpleTypeText))

import           Display
import           Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesCommunV12Type


-- ---------------------------------------------------------------------------
-- Instance principale

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


-- ---------------------------------------------------------------------------
-- Période

renderPeriode :: PeriodeType -> [Widget ()]
renderPeriode p =
    [ field "Début" (simpleTypeText $ periodeType_dateDebut p)
    , field "Fin"   (simpleTypeText $ periodeType_dateFin   p)
    ]


-- ---------------------------------------------------------------------------
-- GrandeurInstantanees (courbe de charge — points horodatés, inchangé)

renderGrandeur :: GrandeurInstantanees -> Widget ()
renderGrandeur g =
    let titre = labelGrandeurMetier   (simpleTypeText (grandeurInstantanees_grandeurMetier   g))
             ++ " — " ++ labelGrandeurPhysique (simpleTypeText (grandeurInstantanees_grandeurPhysique g))
             ++ " (" ++ simpleTypeText (grandeurInstantanees_unite g) ++ ")"
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


-- ---------------------------------------------------------------------------
-- Contexte → GrandeurType en tableau croisé

renderContexte :: Contexte -> Widget ()
renderContexte c = vBox $ map renderGrandeurType (contexte_grandeur c)

renderGrandeurType :: GrandeurType -> Widget ()
renderGrandeurType g =
    let title  = labelGrandeurMetier (simpleTypeText (grandeurType_grandeurMetier g))
              ++ " — " ++ labelGrandeurPhysique (simpleTypeText (grandeurType_grandeurPhysique g))
              ++ " (" ++ simpleTypeText (grandeurType_unite g) ++ ")"
        infos  = collectColInfos g
        cols   = map ciKey infos
        cts    = map ciCT  infos
        rows   = sortedDates cts
        pivot  = buildPivot cols cts
        colWs  = map (dynColWidth pivot rows) (zip cols infos)
    in section title $
        renderLegende infos
        : renderCalRow infos colWs
        : renderHeader cols colWs
        : renderSep colWs
        : map (renderPivotRow cols colWs pivot) rows


labelGrandeurMetier :: String -> String
labelGrandeurMetier "CONS" = "Consommation"
labelGrandeurMetier "PROD" = "Production"
labelGrandeurMetier s      = s

labelGrandeurPhysique :: String -> String
labelGrandeurPhysique "PA"  = "Puissance Active"
labelGrandeurPhysique "PRI" = "Puissance Réactive Inductive"
labelGrandeurPhysique "PRC" = "Puissance Réactive Capacitive"
labelGrandeurPhysique "E"   = "Tension"
labelGrandeurPhysique "EA"  = "Énergie Active"
labelGrandeurPhysique "ER"  = "Énergie Réactive"
labelGrandeurPhysique "ERC" = "Énergie Réactive Capacitive"
labelGrandeurPhysique "ERI" = "Énergie Réactive Inductive"
labelGrandeurPhysique "DD"  = "Durée de Dépassement"
labelGrandeurPhysique "DE"  = "Dépassement Énergétique"
labelGrandeurPhysique "DQ"  = "Dépassement Quadratique"
labelGrandeurPhysique "PMA" = "Puissance Maximale Atteinte"
labelGrandeurPhysique "TF"  = "Temps de Fonctionnement"
labelGrandeurPhysique s     = s


-- ---------------------------------------------------------------------------
-- ColInfo : métadonnées d'une colonne

data ColInfo = ColInfo
    { ciKey     :: String   -- "HCB", "HPB"…
    , ciLibelle :: String   -- "Heures Creuses Saison Basse"
    , ciCalId   :: String   -- "DI000003"
    , ciCalLib  :: String   -- "Avec différenciation…"
    , ciCT      :: ClasseTemporelle
    }

collectColInfos :: GrandeurType -> [ColInfo]
collectColInfos g =
    [ ColInfo { ciKey     = ctKey ct
              , ciLibelle = maybe "" simpleTypeText (classeTemporelle_libelleClasseTemporelle ct)
              , ciCalId   = simpleTypeText (calendrier_idCalendrier    cal)
              , ciCalLib  = simpleTypeText (calendrier_libelleCalendrier cal)
              , ciCT      = ct
              }
    | cal <- grandeurType_calendrier g
    , ct  <- calendrier_classeTemporelle cal
    ]

ctKey :: ClasseTemporelle -> String
ctKey ct = case classeTemporelle_idClasseTemporelle ct of
    Just x  -> simpleTypeText x
    Nothing -> maybe "?" simpleTypeText (classeTemporelle_codeCadran ct)


-- ---------------------------------------------------------------------------
-- Pivot

type PivotMap = Map.Map String (Map.Map String String)

buildPivot :: [String] -> [ClasseTemporelle] -> PivotMap
buildPivot keys cts = foldr insertCT Map.empty (zip keys cts)
  where
    insertCT (key, ct) acc = foldr (insertVal key) acc (classeTemporelle_valeur ct)
    insertVal key v acc =
        let d   = simpleTypeText (valeur_d v)
            val = simpleTypeText (valeur_v v)
        in Map.insertWith Map.union d (Map.singleton key val) acc

sortedDates :: [ClasseTemporelle] -> [String]
sortedDates cts =
    sortBy compare $ nub
    [ simpleTypeText (valeur_d v)
    | ct <- cts, v <- classeTemporelle_valeur ct ]

dynColWidth :: PivotMap -> [String] -> (String, ColInfo) -> Int
dynColWidth pivot rows (key, ci) =
    maximum $ length key
            : length (ciCalId ci)
            : [ maybe 0 length (Map.lookup key cells)
              | d <- rows, Just cells <- [Map.lookup d pivot] ]


-- ---------------------------------------------------------------------------
-- Rendu du tableau

dateW :: Int
dateW = 22

renderLegende :: [ColInfo] -> Widget ()
renderLegende infos =
    section "Légende" $ map legendeLine infos
  where
    legendeLine ci =
        ustr $ rpad 10 (ciKey ci)
            ++ rpad 45 (ciLibelle ci)
            ++ "  [" ++ ciCalId ci ++ " : " ++ ciCalLib ci ++ "]"

renderCalRow :: [ColInfo] -> [Int] -> Widget ()
renderCalRow infos colWs =
    withAttr sectionAttr $ ustr $
    rpad dateW "" ++
    concatMap (\(ci, w) -> " | " ++ rpad w (ciCalId ci)) (zip infos colWs)

renderHeader :: [String] -> [Int] -> Widget ()
renderHeader cols colWs =
    withAttr labelAttr $ ustr $
    rpad dateW "Date" ++
    concatMap (\(c, w) -> " | " ++ rpad w c) (zip cols colWs)

renderSep :: [Int] -> Widget ()
renderSep colWs =
    ustr $ replicate dateW '-' ++
    concatMap (\w -> "-+-" ++ replicate w '-') colWs

renderPivotRow :: [String] -> [Int] -> PivotMap -> String -> Widget ()
renderPivotRow cols colWs pivot d =
    let cells = maybe Map.empty id (Map.lookup d pivot)
    in ustr $
       rpad dateW d ++
       concatMap (\(c, w) ->
           " | " ++ lpad w (maybe "" id (Map.lookup c cells))
       ) (zip cols colWs)


-- ---------------------------------------------------------------------------
-- Utilitaires

padTo :: Int -> String -> String
padTo n s = take n (s ++ repeat ' ')

rpad :: Int -> String -> String
rpad = padTo

lpad :: Int -> String -> String
lpad n s = let s' = take n s
           in replicate (n - length s') ' ' ++ s'
