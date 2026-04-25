{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Display.ConsoDisplay
  ( ConsosPubliees(..)
  , ConsosInfos(..)
  ) where

import           Brick
import           Data.List              ( intersperse )
import           Data.Maybe             ( listToMaybe )
import qualified Data.Text              as T
import           Text.Printf            ( printf )

import           Display
import           Conso.Fr.Gaz.Adict.Types


-- ---------------------------------------------------------------------------
-- Newtypes pour distinguer les deux commandes

newtype ConsosPubliees = ConsosPubliees [ConsoRestit]
newtype ConsosInfos    = ConsosInfos    [ConsoRestit]


-- ---------------------------------------------------------------------------
-- Helpers tableau

data ColAlign = AlignLeft | AlignRight

data Col a = Col
    { colHeader :: String
    , colWidth  :: Int
    , colAlign  :: ColAlign
    , colGet    :: a -> String
    }

tableCell :: Int -> ColAlign -> String -> Widget ()
tableCell w AlignLeft  s = ustr $ take w $ s ++ repeat ' '
tableCell w AlignRight s = ustr $ replicate (max 0 (w - length s)) ' ' ++ take w s

sep :: Widget ()
sep = ustr " │ "

fmt2 :: Maybe Double -> String
fmt2 = maybe "" (printf "%.2f")

fmtInt :: Maybe Double -> String
fmtInt = maybe "" (show . (round :: Double -> Int))

sepWidth :: [Col a] -> Int
sepWidth cols = sum (map colWidth cols) + 3 * (length cols - 1)

renderTableHeader :: [Col a] -> Widget ()
renderTableHeader cols = vBox
    [ withAttr labelAttr $ hBox $ intersperse sep
        [ tableCell (colWidth c) AlignLeft (colHeader c) | c <- cols ]
    , ustr $ replicate (sepWidth cols) '─'
    ]

renderTableRow :: [Col a] -> a -> Widget ()
renderTableRow cols x = hBox $ intersperse sep
    [ tableCell (colWidth c) (colAlign c) (colGet c x) | c <- cols ]


-- ---------------------------------------------------------------------------
-- Accesseurs communs ConsoRestit

getC :: (Consommation -> Maybe T.Text) -> ConsoRestit -> String
getC f cr = maybe "" (maybe "" T.unpack . f) (cr_consommation cr)

getN :: (Consommation -> Maybe Double) -> ConsoRestit -> String
getN f cr = fmt2 (cr_consommation cr >>= f)

getIdxDebut :: ConsoRestit -> String
getIdxDebut cr = fmtInt (cr_releve_debut cr >>= rd_index_brut_debut >>= valeur_index)

getIdxFin :: ConsoRestit -> String
getIdxFin cr = fmtInt (cr_releve_fin cr >>= rf_index_brut_fin >>= valeur_index)

getCoeff :: ConsoRestit -> String
getCoeff cr = fmt2 (cr_consommation cr >>= coeff_calcul >>= coeff_conversion)

getPcs :: ConsoRestit -> String
getPcs cr = fmt2 (cr_consommation cr >>= coeff_calcul >>= valeur_pcs)


-- ---------------------------------------------------------------------------
-- Colonnes communes (sans Journée)

commonCols :: [Col ConsoRestit]
commonCols =
    [ Col "Début"         25 AlignLeft  (getC date_debut_consommation)
    , Col "Fin"           25 AlignLeft  (getC date_fin_consommation)
    , Col "Énergie kWh"   11 AlignRight (getN energie)
    , Col "Vol brut"       8 AlignRight (getN volume_brut)
    , Col "Vol converti"  12 AlignRight (getN volume_converti)
    , Col "Idx déb"        7 AlignRight getIdxDebut
    , Col "Idx fin"        7 AlignRight getIdxFin
    , Col "Coeff conv"    10 AlignRight getCoeff
    , Col "PCS"            5 AlignRight getPcs
    , Col "Qualif."        8 AlignLeft  (getC type_qualif_conso)
    , Col "Statut"        12 AlignLeft  (getC statut_conso)
    ]

publieesCols :: [Col ConsoRestit]
publieesCols = commonCols

infosCols :: [Col ConsoRestit]
infosCols = Col "Journée" 10 AlignLeft (getC journee_gaziere) : commonCols


-- ---------------------------------------------------------------------------
-- Helpers affichage

consoHeader :: [Col ConsoRestit] -> [ConsoRestit] -> Widget ()
consoHeader cols lst = vBox
    [ field "PCE"     (maybe "-" (T.unpack . id_pce) (listToMaybe lst >>= cr_pce))
    , field "Période" (maybe "-" T.unpack (listToMaybe lst >>= cr_periode >>= valeur))
    , renderTableHeader cols
    ]

consoBody :: [Col ConsoRestit] -> [ConsoRestit] -> Widget ()
consoBody cols lst = vBox $
    map (renderTableRow cols) lst
    ++ concatMap renderStatutRow lst
  where
    renderStatutRow cr = case cr_statut_restitution cr of
        Nothing -> []
        Just s  -> [renderStatut (Just s)]


-- ---------------------------------------------------------------------------
-- Instances Renderable

instance Renderable ConsosPubliees where
    toHeader (Left  _)                    = emptyWidget
    toHeader (Right (ConsosPubliees []))  = ustr "Aucune consommation publiée."
    toHeader (Right (ConsosPubliees lst)) = consoHeader publieesCols lst

    toWidget (Left  err)                    = renderError err
    toWidget (Right (ConsosPubliees []))    = emptyWidget
    toWidget (Right (ConsosPubliees lst))   = consoBody publieesCols lst


instance Renderable ConsosInfos where
    toHeader (Left  _)                  = emptyWidget
    toHeader (Right (ConsosInfos []))   = ustr "Aucune consommation informative."
    toHeader (Right (ConsosInfos lst))  = consoHeader infosCols lst

    toWidget (Left  err)                  = renderError err
    toWidget (Right (ConsosInfos []))     = emptyWidget
    toWidget (Right (ConsosInfos lst))    = consoBody infosCols lst


-- ---------------------------------------------------------------------------
-- InjectionRestit (inchangé)

instance Renderable [InjectionRestit] where
    toHeader (Left  _)   = emptyWidget
    toHeader (Right [])  = ustr "Aucune injection."
    toHeader (Right lst) = vBox
        [ field "PCE"     (maybe "-" (T.unpack . id_pce) (listToMaybe lst >>= ir_pce))
        , field "Période" (maybe "-" T.unpack (listToMaybe lst >>= ir_periode >>= valeur))
        , renderTableHeader injCols
        ]

    toWidget (Left  err) = renderError err
    toWidget (Right [])  = emptyWidget
    toWidget (Right lst) = vBox $
        map (renderTableRow injCols) lst
        ++ concatMap renderStatutRow lst
      where
        renderStatutRow ir = case ir_statut_restitution ir of
            Nothing -> []
            Just s  -> [renderStatut (Just s)]

injCols :: [Col InjectionRestit]
injCols =
    [ Col "Début"         25 AlignLeft  (getCI date_debut_injection)
    , Col "Fin"           25 AlignLeft  (getCI date_fin_injection)
    , Col "Énergie kWh"   11 AlignRight (getNI inj_energie)
    , Col "Vol brut"       8 AlignRight (getNI inj_volume_brut)
    , Col "Vol converti"  12 AlignRight (getNI inj_volume_converti)
    , Col "Qualif."        8 AlignLeft  (getCI type_qualif_injection)
    , Col "Statut"        12 AlignLeft  (getCI statut_injection)
    ]
  where
    getCI f ir = maybe "" (maybe "" T.unpack . f) (ir_injection ir)
    getNI f ir = fmt2 (ir_injection ir >>= f)


-- ---------------------------------------------------------------------------
-- Helpers communs

renderStatut :: Maybe StatutRestitution -> Widget ()
renderStatut Nothing  = emptyWidget
renderStatut (Just s) = case (sr_code s, sr_message s) of
    (Just c, Just m) -> withAttr errorAttr $ ustr $ T.unpack c <> " — " <> T.unpack m
    (Just c, _)      -> withAttr errorAttr $ ustr $ T.unpack c
    _                -> emptyWidget
