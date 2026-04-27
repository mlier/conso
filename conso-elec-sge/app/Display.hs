module Display
  ( Renderable(..)
  , renderApp
  , ustr
  , sText
  , field
  , maybeField
  , section
  , sectionActif
  , sectionTermine
  , renderError
  , errorAttr
  , sectionAttr
  , labelAttr
  , theMap
  ) where

import           Brick
import           Brick.Widgets.Border (borderWithLabel, border)
import qualified Graphics.Vty         as V
import qualified Data.Text            as T
import           Text.XML.HaXml.Schema.Schema (SimpleType, simpleTypeText)


-- | Typeclass associant chaque type de réponse à un widget brick.
--   Une instance = un "template" de mise en page par webservice.
class Renderable a where
    toWidget :: Either (String, String) a -> Widget ()


-- | Lance l'affichage TUI. Quitter avec 'q' ou Escape. Défiler avec ↑/↓.
-- | Comme 'str' mais utilise 'txt' (text-width) qui calcule la largeur
--   d'affichage Unicode via ses propres tables, indépendamment de la locale C.
ustr :: String -> Widget n
ustr = txt . T.pack

-- | Like 'simpleTypeText' but resolves XML character entities (&amp; &lt; etc.).
sText :: SimpleType a => a -> String
sText x = T.unpack
    . T.replace (T.pack "&amp;")  (T.pack "&")
    . T.replace (T.pack "&lt;")   (T.pack "<")
    . T.replace (T.pack "&gt;")   (T.pack ">")
    . T.replace (T.pack "&quot;") (T.pack "\"")
    . T.replace (T.pack "&apos;") (T.pack "'")
    $ T.pack (simpleTypeText x)

renderApp :: Renderable a => Either (String, String) a -> IO ()
renderApp x = do
    let theApp = App
            { appDraw         = const [viewport () Vertical (toWidget x)]
            , appChooseCursor = neverShowCursor
            , appHandleEvent  = handleKey
            , appStartEvent   = return ()
            , appAttrMap      = const theMap
            }
    _ <- defaultMain theApp ()
    return ()

handleKey :: BrickEvent () () -> EventM () () ()
handleKey (VtyEvent (V.EvKey V.KEsc        [])) = halt
handleKey (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleKey (VtyEvent (V.EvKey V.KUp         [])) = vScrollBy (viewportScroll ()) (-1)
handleKey (VtyEvent (V.EvKey V.KDown       [])) = vScrollBy (viewportScroll ()) 1
handleKey _                                      = return ()


-- ---------------------------------------------------------------------------
-- Attributs de style

errorAttr, sectionAttr, labelAttr :: AttrName
errorAttr   = attrName "error"
sectionAttr = attrName "section"
labelAttr   = attrName "label"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (errorAttr,   V.withForeColor V.defAttr V.red   `V.withStyle` V.bold)
    , (sectionAttr, V.withForeColor V.defAttr V.cyan  `V.withStyle` V.bold)
    , (labelAttr,   V.withForeColor V.defAttr V.yellow)
    ]


-- ---------------------------------------------------------------------------
-- Widgets helpers réutilisables

-- | Ligne "label : valeur"
field :: String -> String -> Widget ()
field lbl val =
    withAttr labelAttr (ustr lbl) <+> ustr (" : " ++ val)

-- | Ligne optionnelle — absente si Nothing
maybeField :: String -> Maybe String -> Widget ()
maybeField lbl = maybe emptyWidget (field lbl)

-- | Bloc avec titre encadré
section :: String -> [Widget ()] -> Widget ()
section title rows =
    borderWithLabel (withAttr sectionAttr $ ustr (" " ++ title ++ " ")) $
    padRight Max $ vBox rows

-- | Bloc encadré avec couleur personnalisée (cadre + titre), contenu étendu à pleine largeur.
sectionColored :: V.Color -> String -> [Widget ()] -> Widget ()
sectionColored color title rows =
    updateAttrMap (applyAttrMappings
        [ (attrName "border", V.withForeColor V.defAttr color `V.withStyle` V.bold)
        , (sectionAttr,       V.withForeColor V.defAttr color `V.withStyle` V.bold)
        ]) $
    borderWithLabel (withAttr sectionAttr $ ustr (" " ++ title ++ " ")) $
    padRight Max $ vBox rows

-- | Cadre vert — service actif.
sectionActif :: String -> [Widget ()] -> Widget ()
sectionActif = sectionColored V.green

-- | Cadre rouge — service terminé ou résilié.
sectionTermine :: String -> [Widget ()] -> Widget ()
sectionTermine = sectionColored V.red

-- | Affichage d'erreur SGT
renderError :: String -> String -> Widget ()
renderError code msg =
    border $ withAttr errorAttr $ vBox
        [ ustr ("Erreur " ++ code)
        , ustr msg
        ]
