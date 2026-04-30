{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.SiteDB.Orchestration.Adresses
  ( verifierCoherence
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)

import Conso.Fr.SiteDB.Orchestration.Types (GetCodePostal, VerifAdresse(..))

-- | Vérifie que deux points de livraison ont le même code postal.
-- Les fonctions de récupération sont fournies par les extensions (codePostalPrm, codePostalPce).
verifierCoherence :: Bool -> GetCodePostal -> GetCodePostal -> Text -> Text -> IO VerifAdresse
verifierCoherence verbose getPrm getPce prmId pceId = do
  logV verbose $ "Vérification adresses PRM=" <> T.unpack prmId <> " PCE=" <> T.unpack pceId
  eCpPrm <- getPrm prmId
  eCpPce <- getPce pceId
  return $ case (eCpPrm, eCpPce) of
    (Left e, _) -> VerifImpossible $ "SGE: " <> e
    (_, Left e) -> VerifImpossible $ "ADICT: " <> e
    (Right cpP, Right cpC)
      | T.strip cpP == T.strip cpC -> CodePostauxIdentiques
      | otherwise                  -> Mismatch cpP cpC

logV :: Bool -> String -> IO ()
logV True  msg = hPutStrLn stderr $ "[verbose] " <> msg
logV False _   = return ()
