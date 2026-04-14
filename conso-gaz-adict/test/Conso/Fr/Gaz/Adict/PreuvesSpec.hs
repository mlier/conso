{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.PreuvesSpec where

import SpecHelper
import Data.Either ( isRight )
import qualified Data.ByteString as BS

import Conso.Fr.Gaz.Adict.Preuves ( soumettrePrevue )
import TestData


spec :: Spec
spec = do
    describe sandboxC $ do
        describe recevablesC $ do
            it "GDA-R33 - Soumettre une preuve (statut Preuve en cours de vérification, JDD 33)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    let tmpPath = "/tmp/adict-test-preuve.pdf"
                    BS.writeFile tmpPath minimalPdf
                    rep <- soumettrePrevue session uuidPreuvePassant1 tmpPath
                    rep `shouldSatisfy` isRight

            it "GDA-R34 - Soumettre une preuve (statut Preuve en attente, JDD 34)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    let tmpPath = "/tmp/adict-test-preuve.pdf"
                    BS.writeFile tmpPath minimalPdf
                    rep <- soumettrePrevue session uuidPreuvePassant2 tmpPath
                    rep `shouldSatisfy` isRight

        describe nonRecevablesC $ do
            it "GDA-NR35 - Preuve taille supérieure à 4 Mo (JDD 35)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    let tmpPath = "/tmp/adict-test-preuve.pdf"
                    BS.writeFile tmpPath minimalPdf
                    rep <- soumettrePrevue session uuidPreuveNR35 tmpPath
                    rep `shouldBeFunctionalError` "2000000005"

            it "GDA-NR36 - Format de preuve non autorisé (JDD 36)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    let tmpPath = "/tmp/adict-test-preuve.pdf"
                    BS.writeFile tmpPath minimalPdf
                    rep <- soumettrePrevue session uuidPreuveNR36 tmpPath
                    rep `shouldBeFunctionalError` "2000000002"


-- | Contenu PDF minimal valide (header uniquement).
minimalPdf :: BS.ByteString
minimalPdf = "%PDF-1.0\n1 0 obj<</Type /Catalog>>endobj\n%%EOF\n"


main :: IO ()
main = hspec spec
