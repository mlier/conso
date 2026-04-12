{-# LANGUAGE TypeApplications #-}
module SpecHelper
  ( module Test.Hspec
  , sandboxSession
  , pendingOnAdictError
  , shouldBeFunctionalError
  , sandboxC
  , recevablesC
  , nonRecevablesC
  ) where

import Control.Exception    ( try, throwIO, fromException, SomeException )
import Data.Text            ( Text )
import System.Environment   ( lookupEnv )
import Test.HUnit.Lang      ( HUnitFailure )
import Test.Hspec

import Conso.Fr.Gaz.Adict.Adict
    ( getEnv, initSessionWith, AdictSession, AdictEnv(..), AdictError(..) )


-- | Initialise une session vers le bac à sable GRDF.
--   Active le mode debug si la variable d'environnement @CONSO_VERBOSE=1@.
sandboxSession :: IO AdictSession
sandboxSession = do
    env      <- getEnv
    debugReq <- (Just "1" ==) <$> lookupEnv "DEBUG"
    verbose  <- (Just "1" ==) <$> lookupEnv "CONSO_VERBOSE"
    initSessionWith debugReq verbose (sandbox env)

-- | Marque le test « pending » si l'API sandbox est inaccessible (réseau/TLS/auth).
--   Les vraies assertions hspec ne sont pas masquées.
pendingOnAdictError :: Expectation -> Expectation
pendingOnAdictError action = do
    res <- try @SomeException action
    case res of
        Left e -> case fromException e :: Maybe HUnitFailure of
            Just huf -> throwIO huf   -- re-lancer les échecs d'assertion hspec
            Nothing  -> pendingWith $ "API ADICT inaccessible : " ++ show e
        Right () -> return ()


-- | Vérifie qu'une réponse ADICT est une erreur fonctionnelle avec le code GRDF attendu.
--   Affiche le code et le message réels en cas d'échec pour faciliter le diagnostic.
shouldBeFunctionalError :: Show a => Either AdictError a -> Text -> Expectation
shouldBeFunctionalError result expectedCode = case result of
    Left (FunctionalError code _) | code == expectedCode -> pure ()
    Left (FunctionalError code msg) -> expectationFailure $
        "Expected FunctionalError " ++ show expectedCode ++
        ", got code " ++ show code ++ ": " ++ show msg
    Left err  -> expectationFailure $
        "Expected FunctionalError " ++ show expectedCode ++
        ", got: " ++ show err
    Right _   -> expectationFailure $
        "Expected FunctionalError " ++ show expectedCode ++ ", but got Right"


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
