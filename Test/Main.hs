module Teat.Main where

import Test.Hspec
import IHP.Prelude

import Test.LandingPageSpec

main :: IO ()
main = hspec do
    Test.LandingPageSpec.tests