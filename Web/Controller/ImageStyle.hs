module Web.Controller.ImageStyle where

import Web.Controller.Prelude
import Web.View.ImageStyle.Index
import Web.View.ImageStyle.New
import Web.View.ImageStyle.Edit
import Web.View.ImageStyle.Show

instance Controller ImageStyleController where
    action ImageStylesAction = do
        imageStyle <- query @ImageStyle |> fetch
        render IndexView { .. }

    action NewImageStyleAction = do
        let imageStyle = newRecord
        render NewView { .. }

    action ShowImageStyleAction { imageStyleId } = do
        imageStyle <- fetch imageStyleId
        render ShowView { .. }

    action EditImageStyleAction { imageStyleId } = do
        imageStyle <- fetch imageStyleId
        render EditView { .. }

    action UpdateImageStyleAction { imageStyleId } = do
        imageStyle <- fetch imageStyleId
        imageStyle
            |> buildImageStyle
            |> ifValid \case
                Left imageStyle -> render EditView { .. }
                Right imageStyle -> do
                    imageStyle <- imageStyle |> updateRecord
                    setSuccessMessage "ImageStyle updated"
                    redirectTo EditImageStyleAction { .. }

    action CreateImageStyleAction = do
        let imageStyle = newRecord @ImageStyle
        imageStyle
            |> buildImageStyle
            |> ifValid \case
                Left imageStyle -> render NewView { .. } 
                Right imageStyle -> do
                    imageStyle <- imageStyle |> createRecord
                    setSuccessMessage "ImageStyle created"
                    redirectTo ImageStylesAction

    action DeleteImageStyleAction { imageStyleId } = do
        imageStyle <- fetch imageStyleId
        deleteRecord imageStyle
        setSuccessMessage "ImageStyle deleted"
        redirectTo ImageStylesAction

buildImageStyle imageStyle = imageStyle
    |> fill @'[]
