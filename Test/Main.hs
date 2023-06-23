module Main where

import Test.Hspec
import IHP.Prelude

main :: IO ()
main = hspec do
    tests

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
    describe "Test" do
        it "should work" $ withContext do
            True `shouldBe` True