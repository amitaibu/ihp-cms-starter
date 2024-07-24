module Test.UsersSpec where

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
import IHP.ViewPrelude
import Network.HTTP.Types.Status

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
    describe "Users" do
        it "should show users list on users page" $ withContext do
            user <- newRecord @User
                |> set #email "test@example.com"
                |> createRecord

            response <- withUser user do
                callAction UsersAction

            response `responseBodyShouldContain` "test@example.com"

