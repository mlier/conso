module Display
  ( Renderable(..)
  , renderApp
  , field
  , maybeField
  , section
  , renderError
  , errorAttr
  , sectionAttr
  , labelAttr
  , theMap
  ) where

import           Brick
import           Brick.Widgets.Border (borderWithLabel, border)
import qualified Graphics.Vty         as V


-- | Typeclass associant chaque type de réponse à un widget brick.
--   Une instance = un "template" de mise en page par webservice.
class Renderable a where
    toWidget :: Either (String, String) a -> Widget ()


-- | Lance l'affichage TUI. Quitter avec 'q' ou Escape. Défiler avec ↑/↓.
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
    withAttr labelAttr (str lbl) <+> str (" : " ++ val)

-- | Ligne optionnelle — absente si Nothing
maybeField :: String -> Maybe String -> Widget ()
maybeField lbl = maybe emptyWidget (field lbl)

-- | Bloc avec titre encadré
section :: String -> [Widget ()] -> Widget ()
section title rows =
    borderWithLabel (withAttr sectionAttr $ str (" " ++ title ++ " ")) $
    vBox rows

-- | Affichage d'erreur SGT
renderError :: String -> String -> Widget ()
renderError code msg =
    border $ withAttr errorAttr $ vBox
        [ str ("Erreur " ++ code)
        , str msg
        ]
