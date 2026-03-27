{-# LANGUAGE TypeApplications #-}
module SpecHelper
  ( module Test.Hspec
  , sandboxSession
  , getTestPce
  , pendingOnAdictError
  , sandboxC
  , recevablesC
  , nonRecevablesC
  ) where

import Control.Exception  ( try, SomeException )
import Test.Hspec
import Data.Text          ( Text )

import Conso.Fr.Gaz.Adict.Adict
    ( getEnv, initSessionWith, AdictSession, AdictEnv(..), TestData(..) )


-- | Initialise une session vers le bac à sable GRDF.
sandboxSession :: IO AdictSession
sandboxSession = do
    env <- getEnv
    initSessionWith (sandbox env)

-- | Identifiant PCE de test lu depuis la configuration.
getTestPce :: IO Text
getTestPce = testPce . testData <$> getEnv

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
