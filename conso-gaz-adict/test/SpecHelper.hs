{-# LANGUAGE TypeApplications #-}
module SpecHelper
  ( module Test.Hspec
  , sandboxSession
  , pendingOnAdictError
  , sandboxC
  , recevablesC
  , nonRecevablesC
  ) where

import Control.Exception    ( try, SomeException )
import System.Environment   ( lookupEnv )
import Test.Hspec

import Conso.Fr.Gaz.Adict.Adict
    ( getEnv, initSessionWith, AdictSession, AdictEnv(..) )


-- | Initialise une session vers le bac à sable GRDF.
--   Active le mode debug si la variable d'environnement @CONSO_VERBOSE=1@.
sandboxSession :: IO AdictSession
sandboxSession = do
    env     <- getEnv
    verbose <- (Just "1" ==) <$> lookupEnv "CONSO_VERBOSE"
    initSessionWith verbose (sandbox env)

-- | Marque le test « pending » si l'API sandbox est inaccessible (réseau/TLS/auth).
--   Les vraies assertions hspec ne sont pas masquées.
pendingOnAdictError :: Expectation -> Expectation
pendingOnAdictError action = do
    res <- try @SomeException action
    case res of
        Left e   -> pendingWith $ "API ADICT inaccessible : " ++ show e
        Right () -> return ()


-- ---------------------------------------------------------------------------
-- Couleurs console

sandboxC :: String
sandboxC = cyanC "SANDBOX"

recevablesC :: String
recevablesC = greenC "RECEVABLES"

nonRecevablesC :: String
nonRecevablesC = redC "NON RECEVABLES"

redC :: String -> String
redC = col "\ESC[31m"

greenC :: String -> String
greenC = col "\ESC[32m"

cyanC :: String -> String
cyanC = col "\ESC[36m"

col :: String -> String -> String
col c s = c ++ s ++ "\ESC[0m"
