module Main where

import Test.Hspec
import IHP.Prelude
import IHP.FrameworkConfig
import IHP.Test.Mocking
import IHP.HaskellSupport
import IHP.ModelSupport
import Config

import Generated.Types
import Web.Routes
import Web.Types
import Web.FrontController
import Network.Wai
import IHP.ControllerPrelude
import IHP.ViewPrelude
import Network.HTTP.Types.Status

main :: IO ()
main = hspec do
    tests

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
    describe "Test" do
        it "should work" $ withContext do
            True `shouldBe` True

        itShouldAllowNewAndCreate ParagraphCta NewParagraphCtaAction CreateParagraphCtaAction
        itShouldAllowNewAndCreate ParagraphQuote NewParagraphQuoteAction CreateParagraphQuoteAction




itShouldAllowNewAndCreate ::
    forall record newAction createAction application.
    ( Controller createAction
    , Typeable createAction
    , Typeable application
    , ?modelContext :: ModelContext
    , Typeable record
    , Record record
    , CanCreate record
    , SetField "landingPageId" record  (Id' "landing_pages")
    )
    => record -> newAction -> createAction -> SpecWith (MockContext application)
itShouldAllowNewAndCreate record newAction createAction =
     it "should allow new and create" $ withContext do

        -- Create a Landing page.
        landingPage <- newRecord @LandingPage |> createRecord

        -- Create an existing record.
        existingRecord <- newRecord @record
            |> set #landingPageId landingPage.id
            |> createRecord

        let actions =
                [ (newAction landingPage.id, [])
                , (createAction, [("landingPageId", cs $ show landingPage.id)])
                ]

        forEach actions (\(action, params) -> do
            response <- callActionWithParams action params

            response `responseStatusShouldBe` status200
            )
