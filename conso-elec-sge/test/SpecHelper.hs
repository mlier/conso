{-# LANGUAGE TypeApplications #-}

module SpecHelper (
  module Test.Hspec, wsRequest, wsRequestTest,
  testPointId, testNomClient,
  productionC, homologationC, recevablesC, nonRecevablesC,
  pendingOnNetworkError,
  shouldHaveCode,
  cleanupServices
) where

import Control.Exception (try)
import Control.Monad (unless)
import Network.HTTP.Client (HttpException)
import Test.Hspec
import qualified Data.Text as T
import Text.XML.HaXml.Schema.Schema (SimpleType(simpleTypeText))

import Conso.Fr.Elec.Sge.Sge
    ( getEnv,
      wsRequest,
      wsRequestTest,
      SgeEnv(test),
      Test(nomClientFinalOuDenominationSociale, pointId) )

import qualified Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10 as RSA
import           Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10Type
    ( RechercherServicesAccesDonneesReponseType
    , rechercherServicesAccesDonneesReponseType_servicesSouscrits
    , servicesSouscritsType_serviceSouscrit
    , serviceSouscritType_serviceSouscritId
    , serviceSouscritType_etatCode
    , serviceSouscritType_soutirage
    , serviceSouscritType_injection )
import qualified Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10 as ASAD
import           Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10 ( Sens(..) )
import           Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10Type
    ( CommanderArretServicesAccesDonneesResponseType )


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


-- | Arrête tous les services actifs sur le PRM avant un test.
--   Effectue une recherche RSA, filtre les services ACTIF, et les arrête via ASAD
--   en séparant les sens SOUTIRAGE et INJECTION.
cleanupServices :: String -> IO ()
cleanupServices prm = do
    rsaType <- RSA.initTypeTest prm
    rep     <- wsRequestTest rsaType
                   :: IO (Either (String, String) RechercherServicesAccesDonneesReponseType)
    case rep of
        Left  _    -> return ()
        Right resp -> do
            let services = maybe [] servicesSouscritsType_serviceSouscrit
                               (rechercherServicesAccesDonneesReponseType_servicesSouscrits resp)
            let actifs   = filter isActif services
            let sidsSOUT = map getSid $ filter isSoutirage actifs
            let sidsINJ  = map getSid $ filter isInjection actifs
            unless (null sidsSOUT) $ stopBySens prm SensSOUTIRAGE sidsSOUT
            unless (null sidsINJ)  $ stopBySens prm SensINJECTION sidsINJ
  where
    isActif s   = "ACTIF" `elem` map simpleTypeText (serviceSouscritType_etatCode s)
    getSid      = simpleTypeText . serviceSouscritType_serviceSouscritId
    isSoutirage = not . null . serviceSouscritType_soutirage
    isInjection = not . null . serviceSouscritType_injection

stopBySens :: String -> Sens -> [String] -> IO ()
stopBySens prm sens sids = do
    asadType <- ASAD.initTypeTest prm sens sids
    _        <- wsRequestTest asadType
                    :: IO (Either (String, String) CommanderArretServicesAccesDonneesResponseType)
    return ()


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
