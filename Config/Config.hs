module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FileStorage.Config
import IHP.FrameworkConfig
import Web.View.CustomCSSFramework
import Crypto.PubKey.RSA as RSA

newtype RsaPublicAndPrivateKeys = RsaPublicAndPrivateKeys (RSA.PublicKey, RSA.PrivateKey)


config :: ConfigBuilder
config = do
    -- See https://ihp.digitallyinduced.com/Guide/config.html
    -- for what you can do here
    option customTailwind

    -- Private and public keys to sign and verify image style URLs.
    (publicKey, privateKey) <- liftIO $ liftIO $ RSA.generate 300 65537
    option $ RsaPublicAndPrivateKeys (publicKey, privateKey)

    -- Static directory.
    initStaticDirStorage