{-# OPTIONS_GHC -Wno-orphans #-}

module Display.MesuresDisplay () where

import           Brick
import           Data.List       (nub, sortBy)
import           Data.Ord        (comparing)
import qualified Data.Map.Strict as Map
import qualified Data.Ord        as Ord
import           Text.XML.HaXml.Schema.Schema (SimpleType(simpleTypeText))

import           Display
import           Conso.Fr.Elec.Sge.ConsulterMesuresV11Type
import           Conso.Fr.Elec.Sge.EnedisDictionnaireTypeComplexeV50
    ( grandeurPhysiqueType_code
    , classeTemporelleType_code, classeTemporelleType_libelle
    , calendrierType_code,       calendrierType_libelle )

type ColKey   = String
type RowKey   = (String, String)
type PivotMap = Map.Map RowKey (Map.Map ColKey String)

data GrilleLabel = TURPE | FRN deriving (Show)

instance Renderable ConsulterMesuresResponseType where
    toWidget (Left (code, msg)) = renderError code msg
    toWidget (Right resp) =
        let turpe = maybe [] seriesMesuresDateesType_serie
                        (consulterMesuresResponseType_seriesMesuresDateesGrilleTurpe resp)
            frn   = maybe [] seriesMesuresDateesType_serie
                        (consulterMesuresResponseType_seriesMesuresDateesGrilleFrn resp)
            labeled = map (\s -> (TURPE, s)) turpe ++ map (\s -> (FRN, s)) frn
        in if null labeled
           then ustr "(aucune donnée)"
           else vBox
               [ renderLegende labeled
               , renderTableau labeled
               ]

renderTableau :: [(GrilleLabel, SerieMesuresDateesType)] -> Widget ()
renderTableau labeled =
    let series = map snd labeled
        cols   = map colKey series
        calCodes = map serieCalCode series
        rows   = sortedRows series
        pivot  = buildPivot cols series
        colWs  = map (dynColWidth pivot rows) (zip3 cols calCodes series)
    in section "Mesures" $
        renderCalRow calCodes colWs
        : renderHeader cols colWs
        : renderSep colWs
        : map (renderRow cols colWs pivot) rows

colKey :: SerieMesuresDateesType -> ColKey
colKey s = case serieMesuresDateesType_classeTemporelle s of
    Just ct -> simpleTypeText (classeTemporelleType_code ct)
    Nothing -> simpleTypeText (grandeurPhysiqueType_code
                   (serieMesuresDateesType_grandeurPhysique s))

serieCalCode :: SerieMesuresDateesType -> String
serieCalCode s = case serieMesuresDateesType_calendrier s of
    Nothing  -> ""
    Just cal -> simpleTypeText (calendrierType_code cal)

sortedRows :: [SerieMesuresDateesType] -> [RowKey]
sortedRows series =
    sortBy (comparing (Ord.Down . fst)) $ nub
    [ ( simpleTypeText (mesureDateeType_dateDebut m)
      , simpleTypeText (mesureDateeType_dateFin   m) )
    | s      <- series
    , Just md <- [serieMesuresDateesType_mesuresDatees s]
    , m      <- mesuresDateesType_mesure md
    ]

buildPivot :: [ColKey] -> [SerieMesuresDateesType] -> PivotMap
buildPivot keys series = foldr insertSerie Map.empty (zip keys series)
  where
    insertSerie (key, s) acc =
        case serieMesuresDateesType_mesuresDatees s of
            Nothing -> acc
            Just md -> foldr (insertMesure key) acc (mesuresDateesType_mesure md)
    insertMesure key m acc =
        let rk  = ( simpleTypeText (mesureDateeType_dateDebut m)
                  , simpleTypeText (mesureDateeType_dateFin   m) )
            val = simpleTypeText (mesureDateeType_valeur m)
        in Map.insertWith Map.union rk (Map.singleton key val) acc

dynColWidth :: PivotMap -> [RowKey] -> (ColKey, String, SerieMesuresDateesType) -> Int
dynColWidth pivot rows (key, calCode, _) =
    maximum $ length key
            : length calCode
            : [ maybe 0 length (Map.lookup key cells)
              | rk <- rows, Just cells <- [Map.lookup rk pivot] ]

periodeW :: Int
periodeW = 23

renderCalRow :: [String] -> [Int] -> Widget ()
renderCalRow calCodes colWs =
    withAttr sectionAttr $ ustr $
    rpad periodeW "" ++
    concatMap (\(c, w) -> " | " ++ rpad w c) (zip calCodes colWs)

renderHeader :: [ColKey] -> [Int] -> Widget ()
renderHeader cols colWs =
    withAttr labelAttr $ ustr $
    rpad periodeW "Période" ++
    concatMap (\(c, w) -> " | " ++ rpad w c) (zip cols colWs)

renderSep :: [Int] -> Widget ()
renderSep colWs =
    ustr $ replicate periodeW '-' ++
    concatMap (\w -> "-+-" ++ replicate w '-') colWs

renderRow :: [ColKey] -> [Int] -> PivotMap -> RowKey -> Widget ()
renderRow cols colWs pivot (d, f) =
    let cells = maybe Map.empty id (Map.lookup (d, f) pivot)
    in ustr $
       rpad periodeW (d ++ "\x2192" ++ f) ++
       concatMap (\(c, w) ->
           " | " ++ lpad w (maybe "" id (Map.lookup c cells))
       ) (zip cols colWs)

renderLegende :: [(GrilleLabel, SerieMesuresDateesType)] -> Widget ()
renderLegende labeled =
    section "Légende" $ map legendeLine labeled
  where
    legendeLine (grille, s) =
        let key    = colKey s
            ctDesc = case serieMesuresDateesType_classeTemporelle s of
                Nothing -> simpleTypeText (grandeurPhysiqueType_code
                               (serieMesuresDateesType_grandeurPhysique s))
                Just ct -> maybe key simpleTypeText (classeTemporelleType_libelle ct)
            calDesc = case serieMesuresDateesType_calendrier s of
                Nothing  -> ""
                Just cal ->
                    let code = simpleTypeText (calendrierType_code cal)
                        lib  = maybe "" simpleTypeText (calendrierType_libelle cal)
                    in "  [" ++ code ++ " : " ++ lib ++ "]"
            unite = simpleTypeText (serieMesuresDateesType_unite s)
        in ustr $ rpad 5 key ++ " " ++ rpad 5 (show grille)
                ++ "  " ++ ctDesc
                ++ "  (" ++ unite ++ ")"
                ++ calDesc

rpad :: Int -> String -> String
rpad n s = take n (s ++ repeat ' ')

lpad :: Int -> String -> String
lpad n s = let s' = take n s
           in replicate (n - length s') ' ' ++ s'
