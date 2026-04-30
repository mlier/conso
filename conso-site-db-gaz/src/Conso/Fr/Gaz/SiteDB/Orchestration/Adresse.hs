{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.SiteDB.Orchestration.Adresse
  ( codePostalPce
  ) where

import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)

import Conso.Fr.Gaz.Adict.Adict (AdictSession)
import Conso.Fr.Gaz.Adict.DonneesTechniques (consulterDonneesTechniques)
import Conso.Fr.Gaz.Adict.Types
  ( RetourDonneesTechniques(..), DonneesTechniques(..), SituationCompteurDetail(..) )

import Conso.Fr.SiteDB.Orchestration.Types (GetCodePostal)


-- | Retourne le code postal d'un PCE via ADICT DonneesTechniques.
codePostalPce :: AdictSession -> GetCodePostal
codePostalPce session pce = do
  logV True $ "ADICT DonneesTechniques → PCE " <> T.unpack pce
  result <- consulterDonneesTechniques session pce
  case result of
    Left err ->
      let msg = show err
      in return (Left msg)
    Right rdt ->
      case rdt_donnees rdt >>= dt_situation_compteur >>= scd_code_postal of
        Nothing -> return (Left "code postal absent dans la réponse ADICT")
        Just cp -> return (Right cp)

logV :: Bool -> String -> IO ()
logV True  msg = hPutStrLn stderr $ "[verbose] " <> msg
logV False _   = return ()
