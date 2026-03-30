{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.InjectionsPublieesSpec where

import SpecHelper
import Data.Either ( isRight )

import Conso.Fr.Gaz.Adict.InjectionsPubliees ( consulterInjectionsPubliees )
import Conso.Fr.Gaz.Adict.ConsosPubliees     ( PeriodeParam(..) )


spec :: Spec
spec = do
    describe sandboxC $ do
        describe recevablesC $ do
            it "INJ-R1 - Consulter les injections publiées (période)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    pce     <- getTestPce
                    rep     <- consulterInjectionsPubliees session pce (ByPeriode "2024")
                    rep `shouldSatisfy` isRight


main :: IO ()
main = hspec spec
