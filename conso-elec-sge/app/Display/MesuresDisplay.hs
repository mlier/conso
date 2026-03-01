{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template d'affichage pour ConsulterMesuresV11.
module Display.MesuresDisplay () where

import           Brick
import           Text.XML.HaXml.Schema.Schema (SimpleType(simpleTypeText))

import           Display
import           Conso.Fr.Elec.Sge.ConsulterMesuresV11Type


instance Renderable ConsulterMesuresResponseType where
    toWidget (Left (code, msg)) = renderError code msg
    toWidget (Right resp)       = vBox
        [ maybe emptyWidget (renderSeries "Grille TURPE")
            (consulterMesuresResponseType_seriesMesuresDateesGrilleTurpe resp)
        , maybe emptyWidget (renderSeries "Grille FRN")
            (consulterMesuresResponseType_seriesMesuresDateesGrilleFrn resp)
        ]


renderSeries :: String -> SeriesMesuresDateesType -> Widget ()
renderSeries titre sm =
    section titre $
    map renderSerie (seriesMesuresDateesType_serie sm)


renderSerie :: SerieMesuresDateesType -> Widget ()
renderSerie s =
    let grandeur = show  $ serieMesuresDateesType_grandeurPhysique s
        unite    = simpleTypeText $ serieMesuresDateesType_unite s
        title    = grandeur ++ " (" ++ unite ++ ")"
        rows     = case serieMesuresDateesType_mesuresDatees s of
            Nothing -> [str "(aucune mesure)"]
            Just md -> tableHeader : map renderMesure (mesuresDateesType_mesure md)
    in section title rows


tableHeader :: Widget ()
tableHeader =
    withAttr labelAttr $ str $
    padTo 12 "Début" ++ " | " ++ padTo 12 "Fin" ++ " | " ++ padTo 12 "Valeur" ++ " | Nature"


renderMesure :: MesureDateeType -> Widget ()
renderMesure m = str $
    padTo 12 (simpleTypeText $ mesureDateeType_dateDebut m) ++ " | " ++
    padTo 12 (simpleTypeText $ mesureDateeType_dateFin   m) ++ " | " ++
    padTo 12 (simpleTypeText $ mesureDateeType_valeur    m) ++ " | " ++
    show (mesureDateeType_nature m)


padTo :: Int -> String -> String
padTo n s = take n (s ++ repeat ' ')
