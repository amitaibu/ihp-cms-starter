module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FileStorage.Config
import IHP.FrameworkConfig ( ConfigBuilder, option )
import Web.View.CustomCSSFramework
import Crypto.PubKey.RSA as RSA
import Control.Exception (catch)
import qualified Data.ByteString as BS
import Web.JWT

data RsaPublicAndPrivateKeys = RsaPublicAndPrivateKeys { publicKey :: RSA.PublicKey, privateKey :: RSA.PrivateKey }

config :: ConfigBuilder
config = do
    -- See https://ihp.digitallyinduced.com/Guide/config.html
    -- for what you can do here
    option customTailwind

    -- Static directory.
    initStaticDirStorage

    -- Private and public keys to sign and verify image style URLs.
    (publicKey, privateKey) <- liftIO $ liftIO $ RSA.generate 300 65537
    option $ RsaPublicAndPrivateKeys publicKey privateKey

        -- Private and public keys to sign and verify image style URLs.
    privateKeyContent <- liftIO $ readRsaKeyFromFile "./Config/id_rsa"
    publicKeyContent <- liftIO $ readRsaKeyFromFile "./Config/id_rsa.pub"

    case (readRsaSecret privateKeyContent, readRsaPublicKey publicKeyContent) of
        (Just privateKey, Just publicKey) -> option $ RsaPublicAndPrivateKeys publicKey privateKey
        _ -> error "Failed to read RSA keys, please execute from the root of your project: ssh-keygen -t rsa -f ./Config/id_rsa"


readRsaKeyFromFile :: FilePath -> IO BS.ByteString
readRsaKeyFromFile path = do
    catch (BS.readFile path) handleException
  where
    handleException :: IOError -> IO BS.ByteString
    handleException _ = return BS.empty