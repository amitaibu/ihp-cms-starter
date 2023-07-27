module Test.LandingPageSpec where


import IHP.Prelude

import IHP.FrameworkConfig
import IHP.Test.Mocking
import IHP.HaskellSupport
import IHP.ModelSupport
import Test.Hspec
import Config

import Generated.Types
import Web.Routes
import Web.Types
import Web.FrontController
import Network.Wai
import IHP.ControllerPrelude
import IHP.ViewPrelude hiding (query)
import Data.Text as Text
import Network.HTTP.Types.Status
import Network.HTTP.Client
import qualified Network.Wreq as Wreq
import Control.Lens ((^.))


tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
    describe "LandingPage" do
        it "should send an email on each page view" $ withContext do
            -- Get random title.
            title <- generateAuthenticationToken

            -- Create a Landing page.
            landingPage <- newRecord @LandingPage
                |> set #title title
                |> createRecord


            response <-
                callAction $ ShowLandingPageAction landingPage.id

            -- Assert email was sent, and caught by Mailhog.
            documentBody <- do
                response <- Wreq.get "http://0.0.0.0:8025/api/v1/messages"
                pure (response ^. Wreq.responseBody)

            cs documentBody `shouldContain` ("Landing page " <> cs landingPage.title)
