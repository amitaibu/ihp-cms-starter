module Web.Controller.ImageStyle where

import Web.Controller.Prelude
import IHP.ControllerSupport
import System.Directory (doesFileExist)
import qualified Data.Text as T
import qualified Data.UUID as UUID


instance Controller ImageStyleController where
    action RenderImageStyleAction { width, height, originalImagePath } = do
        -- Get the original image directory and UUID from the path.
        let (originalImageDirectory, uuid) = extractDirectoryAndUUID originalImagePath

        let size = show width <> "x" <> show height
        let imageStylePathDirectory = originalImageDirectory <> "/imageStyles/" <> size
        let imageStylePath = imageStylePathDirectory <> "/" <> uuid

        -- @todo: How to get rid of the "static/" prefix?
        fileExists <- doesFileExist (cs $ "static/" <> imageStylePath)

        if fileExists
            then do
                -- Image style found.
                renderFile (cs $ "static/" <> imageStylePath) "application/jpg"
            else do
                -- Image style not found, so create it.
                let options :: StoreFileOptions = def
                        { directory = imageStylePathDirectory
                        , preprocess = applyImageMagick "jpg" ["-resize", cs size <> "^", "-gravity", "center", "-extent", cs size, "-quality", "85%", "-strip"]
                        , fileName = UUID.fromText uuid
                        }

                storedFile <- storeFileFromPath (cs $ "static/" <> originalImageDirectory <> "/" <> uuid) options

                renderFile (cs $ "static/" <> storedFile.path) "application/jpg"


extractDirectoryAndUUID :: (?context :: context, ConfigProvider context) => Text -> (Text, Text)
extractDirectoryAndUUID inputText =
    case reverse parts of
        uuid : pathSegments -> (T.intercalate "/" (reverse pathSegments), uuid)
        _ -> ("", "")
    where
        frameworkConfig = ?context.frameworkConfig
        trimmedText = T.replace (frameworkConfig.baseUrl <> "/") "" inputText
        parts = T.splitOn "/" trimmedText