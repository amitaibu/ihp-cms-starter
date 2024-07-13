module Web.Controller.ImageStyle where

import Web.Controller.Prelude
import IHP.ControllerSupport
import System.Directory (doesFileExist)
import qualified Data.Text as Text

-- Imports for the signed token.
import "cryptonite" Crypto.PubKey.RSA.PKCS15 as RSA
import "cryptonite" Crypto.Hash.Algorithms as Hash.Algorithms
import Config
import Data.ByteString.Base64 as Base64
import qualified Data.UUID as UUID (fromText)

instance Controller ImageStyleController where
    action RenderImageStyleAction { width, height, originalImagePath, signed } = do
        let size = show width <> "x" <> show height

        -- Verify the token, and deny or allow access based on the result.
        -- Also, deny access if there's `../` in the path, to prevent traversal attacks.

        accessDeniedUnless (rsaSignatureMatches (originalImagePath <> size) signed && not (Text.isInfixOf "../" originalImagePath))

        -- Get the original image directory and UUID from the path.
        let (originalImageDirectory, uuid) = extractDirectoryAndUUID originalImagePath

        let imageStylePathDirectory = originalImageDirectory <> "/imageStyles/" <> size
        let imageStylePath = imageStylePathDirectory <> "/" <> uuid

        fileExists <- doesFileExist (cs $ storagePrefix <> imageStylePath)

        if fileExists
            then do
                -- Image style found.
                renderFile (cs $ storagePrefix <> imageStylePath) "application/jpg"
            else do
                -- Image style not found, so create it.
                let options :: StoreFileOptions = def
                        { directory = imageStylePathDirectory
                        , preprocess = applyImageMagick "jpg" ["-resize", cs size <> "^", "-gravity", "center", "-extent", cs size, "-quality", "85%", "-strip"]
                        , fileName = UUID.fromText uuid
                        }

                storedFile <- storeFileFromPath (cs $ storagePrefix <> originalImageDirectory <> "/" <> uuid) options

                renderFile (cs $ storagePrefix <> storedFile.path) "application/jpg"

-- | Extracts the directory and UUID from a path like "pictures/8ed22caa-11ea-4c45-a05e-91a51e72558d"
extractDirectoryAndUUID :: (?context :: context, ConfigProvider context) => Text -> (Text, Text)
extractDirectoryAndUUID inputText =
    case reverse parts of
        uuid : pathSegments -> (Text.intercalate "/" (reverse pathSegments), uuid)
        _ -> ("", "")
    where
        frameworkConfig = ?context.frameworkConfig
        trimmedText = Text.replace (frameworkConfig.baseUrl <> "/") "" inputText
        parts = Text.splitOn "/" trimmedText