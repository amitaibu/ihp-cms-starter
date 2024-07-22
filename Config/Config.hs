module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FileStorage.Config
import IHP.FrameworkConfig ( ConfigBuilder, option )
import Web.View.CustomCSSFramework
import IHP.EnvVar
import "cryptonite" Crypto.PubKey.RSA as RSA
import Control.Exception (catch)
import qualified Data.ByteString as BS
import Web.JWT
import qualified IHP.Log as Log
import IHP.Log.Types
import Database.Bloodhound (Server(..))

data RsaKeys = RsaKeys { publicKey :: RSA.PublicKey, privateKey :: RSA.PrivateKey }

config :: ConfigBuilder
config = do
    -- See https://ihp.digitallyinduced.com/Guide/config.html
    -- for what you can do here
    option customTailwind

    -- Static directory.
    initStaticDirStorage

    -- Private and public keys to sign and verify image style URLs.
    privateKeyContent <- liftIO $ readRsaKeyFromFile "./Config/jwtRS256.key"
    publicKeyContent <- liftIO $ readRsaKeyFromFile "./Config/jwtRS256.key.pub"

    case (readRsaSecret privateKeyContent, readRsaPublicKey publicKeyContent) of
        (Just privateKey, Just publicKey) -> option $ RsaKeys publicKey privateKey
        _ -> error "Failed to read RSA keys, please execute from the root of your project: ssh-keygen -t rsa -b 4096 -m PEM -f ./Config/jwtRS256.key && openssl rsa -in ./Config/jwtRS256.key -pubout -outform PEM -out ./Config/jwtRS256.key.pub"

    -- Elasticsearch configuration
    esHost <- env @Text "ELASTICSEARCH_HOST"
    esPort <- env @Int "ELASTICSEARCH_PORT"
    let esServer = Server $ esHost <> ":" <> show esPort

    liftIO $ putStrLn $ "Elasticsearch Server: " <> show esServer

    option esServer

    -- Less verbose logs.
    logger <- liftIO $ newLogger def
      { level = Error
      , formatter = withTimeFormatter
      }
    option logger


readRsaKeyFromFile :: FilePath -> IO BS.ByteString
readRsaKeyFromFile path = do
    catch (BS.readFile path) handleException
  where
    handleException :: IOError -> IO BS.ByteString
    handleException _ = return BS.empty