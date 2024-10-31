
module SpecHelper (
  module Test.Hspec, wsRequest, wsRequestTest, 
  testPointId, testNomClient, 
  productionC, homologationC, recevablesC, nonRecevablesC
) where

import Test.Hspec
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
