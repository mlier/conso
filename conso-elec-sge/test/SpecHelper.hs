{-# LANGUAGE TypeApplications #-}

module SpecHelper (
  module Test.Hspec, wsRequest, wsRequestTest,
  testPointId, testNomClient,
  productionC, homologationC, recevablesC, nonRecevablesC,
  pendingOnNetworkError,
  shouldHaveCode
) where

import Test.Hspec
import Control.Exception (try)
import Network.HTTP.Client (HttpException)

import Conso.Fr.Elec.Sge.Sge
    ( getEnv,
      wsRequest,
      wsRequestTest,
      SgeEnv(test),
      Test(nomClientFinalOuDenominationSociale, pointId) )

import qualified Data.Text as T


testPointId :: IO String
testPointId = do
    T.unpack . pointId . test <$> getEnv

testNomClient:: IO String
testNomClient = do
    T.unpack . nomClientFinalOuDenominationSociale . test <$> getEnv


-- | Exécute un test d'homologation et le marque « pending » si le serveur
--   est inaccessible (erreur réseau / TLS). Les échecs d'assertions (shouldBe,
--   shouldSatisfy, etc.) restent des vraies erreurs et ne sont pas masqués.
pendingOnNetworkError :: Expectation -> Expectation
pendingOnNetworkError action = do
    res <- try @HttpException action
    case res of
        Left e   -> pendingWith $ "Serveur d'homologation inaccessible (réseau/TLS) : " ++ show e
        Right () -> return ()


productionC :: String
productionC = magentaC "PRODUCTION"

homologationC :: String
homologationC = magentaC "HOMOLOGATION"

recevablesC :: String
recevablesC = greenC "RECEVABLES"

nonRecevablesC :: String
nonRecevablesC = redC "NON RECEVABLES"


redC :: String -> String
redC = col "\ESC[31m"

greenC :: String -> String
greenC = col "\ESC[32m"

magentaC :: String -> String
magentaC = col "\ESC[35m"

cyanC :: String -> String
cyanC = col "\ESC[36m"

col :: String -> String -> String
col c s = c ++ s ++ "\ESC[0m"


-- | Vérifie le code d'erreur d'une réponse Left.
--   En cas d'échec, affiche aussi le libellé SGE pour faciliter le diagnostic.
shouldHaveCode ::  Show a => Either (String, String) a -> String -> Expectation
shouldHaveCode (Right _) _ =
    expectationFailure "Expected Left (code d'erreur), got Right (succès inattendu)"
shouldHaveCode (Left (code, label)) expected
    | code == expected = return ()
    | otherwise = expectationFailure $
        "expected: " ++ show expected ++
        "\n but got: " ++ show code ++
        " (" ++ label ++ ")"
