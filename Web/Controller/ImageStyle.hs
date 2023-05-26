module Web.Controller.ImageStyle where

import Web.Controller.Prelude
import IHP.ControllerSupport
import System.Directory (doesFileExist)


instance Controller ImageStyleController where
    action RenderImageStyleAction { width, height, path, uuid } = do
        -- @todo: Check if the processed image already exists
        let size = show width <> "x" <> show height
        let originalImagePAth = path <> "/" <> show uuid
        let imageStylePathDirectory = path <> "/imageStyles/" <> show width <> "x" <> show height


        let options :: StoreFileOptions = def
                { directory = path <> "/imageStyles/" <> show width <> "x" <> show height
                , preprocess = applyImageMagick "jpg" ["-resize", cs size <> "^", "-gravity", "north", "-extent", cs size, "-quality", "85%", "-strip"]
                }

        storedFile <- storeFileFromPath originalImagePAth options

        renderFile (cs storedFile.path) "application/jpg"