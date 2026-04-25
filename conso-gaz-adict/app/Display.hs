{-# LANGUAGE OverloadedStrings #-}
module Display
  ( Renderable(..)
  , renderApp
  , ustr
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
import           Brick.Widgets.Border   ( borderWithLabel, border )
import qualified Graphics.Vty           as V
import qualified Data.Text              as T

import           Conso.Fr.Gaz.Adict.Adict ( AdictError(..) )


-- | Typeclass associant chaque type de réponse à un widget Brick.
--   'toHeader' retourne la partie fixe (hors viewport) ; défaut : vide.
class Renderable a where
    toWidget :: Either AdictError a -> Widget ()
    toHeader :: Either AdictError a -> Widget ()
    toHeader _ = emptyWidget


-- | Lance l'affichage TUI Brick.  Quitter avec q ou Esc, défiler avec ↑/↓.
renderApp :: Renderable a => Either AdictError a -> IO ()
renderApp x = do
    let theApp = App
            { appDraw         = const [vBox [toHeader x, viewport () Vertical (toWidget x)]]
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
-- Widgets helpers

-- | Comme 'str' mais utilise 'txt' pour la largeur Unicode correcte.
ustr :: String -> Widget n
ustr = txt . T.pack

-- | Ligne "label : valeur".
field :: String -> String -> Widget ()
field lbl val =
    withAttr labelAttr (ustr lbl) <+> ustr (" : " ++ val)

-- | Ligne optionnelle — absente si Nothing.
maybeField :: String -> Maybe String -> Widget ()
maybeField lbl = maybe emptyWidget (field lbl)

-- | Bloc avec titre encadré.
section :: String -> [Widget ()] -> Widget ()
section title rows =
    borderWithLabel (withAttr sectionAttr $ ustr (" " ++ title ++ " ")) $
    padRight Max $ vBox rows

-- | Affichage d'erreur ADICT.
renderError :: AdictError -> Widget ()
renderError err =
    border $ withAttr errorAttr $ vBox
        [ ustr "Erreur ADICT"
        , case err of
            FunctionalError code msg -> ustr $ "Erreur fonctionnelle " ++ T.unpack code ++ " : " ++ T.unpack msg
            HttpError status msg     -> ustr $ "Erreur HTTP " ++ show status ++ " : " ++ T.unpack msg
            ParseError msg           -> ustr $ "Erreur de décodage : " ++ T.unpack msg
            AuthError msg            -> ustr $ "Erreur d'authentification : " ++ T.unpack msg
            NetworkError msg         -> ustr $ "Erreur réseau : " ++ T.unpack msg
        ]
