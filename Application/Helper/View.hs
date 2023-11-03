module Application.Helper.View where

import IHP.ViewPrelude
import "cryptonite" Crypto.PubKey.RSA.PKCS15 as RSA
import "cryptonite" Crypto.Hash.Algorithms as Hash.Algorithms
import Data.ByteString.Base64 as Base64
import Application.Helper.Controller

-- Here you can add functions which are available in all your views

-- | Sign the image URL to prevent tampering.
signImageUrl :: (?context::ControllerContext) => Text -> Int -> Int -> Text
signImageUrl imageUrl width height= case RSA.sign Nothing (Just Hash.Algorithms.SHA256) rsaPrivateKey (cs $ imageUrl <> size) of
    Left msg -> error $ "Cannot sign image URL, private key is invalid:" <> show msg
    Right signature -> signature |> Base64.encode |> cs
    where
        size = show width <> "x" <> show height