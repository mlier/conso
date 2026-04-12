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
            it "GDA-R3 - Soumettre une preuve (PUT /droit_acces/{id}/preuves, JDD 33)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    let tmpPath = "/tmp/adict-test-preuve.pdf"
                    BS.writeFile tmpPath minimalPdf
                    rep <- soumettrePrevue session uuidPreuvePassant1 tmpPath
                    rep `shouldSatisfy` isRight


-- | Contenu PDF minimal valide (header uniquement).
minimalPdf :: BS.ByteString
minimalPdf = "%PDF-1.0\n1 0 obj<</Type /Catalog>>endobj\n%%EOF\n"


main :: IO ()
main = hspec spec
