module Web.Controller.ParagraphHeroImages where

import Web.Controller.Prelude
import Web.View.ParagraphHeroImages.New
import Web.View.ParagraphHeroImages.Edit

instance Controller ParagraphHeroImagesController where
    action NewParagraphHeroImageAction { .. } = do
        weight <- getParagraphsCount landingPageId
        let paragraphHeroImage = newRecord
                |> set #landingPageId landingPageId
                |> set #weight weight

        let formStatus = FormStatusNotSubmitted

        render NewView { .. }

    action EditParagraphHeroImageAction { paragraphHeroImageId } = do
        paragraphHeroImage <- fetch paragraphHeroImageId
        -- Get from the session, if the form was submitted successfully.
        formStatus <- getAndClearFormStatus
        render EditView { .. }

    action UpdateParagraphHeroImageAction { paragraphHeroImageId } = do
        let uploadImage = uploadToStorageWithOptions $ def
                { preprocess = applyImageMagick "jpg" ["-resize", "1024x1024^", "-gravity", "north", "-extent", "1024x1024", "-quality", "85%", "-strip"] }

        formStatus <- getAndClearFormStatus

        paragraphHeroImage <- fetch paragraphHeroImageId
        paragraphHeroImage
            |> uploadImage #imageUrl
            >>= buildParagraphHeroImage
            >>= ifValid \case
                Left paragraphHeroImage -> do
                    setFormStatus FormStatusError
                    render EditView { .. }
                Right paragraphHeroImage -> do
                    paragraphHeroImage <- paragraphHeroImage |> updateRecord
                    setSuccessMessage "Hero Image updated"
                    -- We don't setFormStatus, since we redirect to a new page.
                    redirectTo EditLandingPageAction { landingPageId = paragraphHeroImage.landingPageId }

    action CreateParagraphHeroImageAction = do
        let uploadImage = uploadToStorageWithOptions $ def
                { preprocess = applyImageMagick "jpg" ["-resize", "1024x1024^", "-gravity", "north", "-extent", "1024x1024", "-quality", "85%", "-strip"] }

        let paragraphHeroImage = newRecord @ParagraphHeroImage
        let formStatus = FormStatusNotSubmitted

        paragraphHeroImage
            |> uploadImage #imageUrl
            >>= buildParagraphHeroImage
            >>= ifValid \case
                Left paragraphHeroImage -> do
                    setFormStatus FormStatusError
                    render NewView { .. }
                Right paragraphHeroImage -> do
                    paragraphHeroImage <- paragraphHeroImage |> createRecord
                    setSuccessMessage "Hero Image created"
                    -- We don't setFormStatus, since we redirect to a new page.
                    redirectTo EditLandingPageAction { landingPageId = paragraphHeroImage.landingPageId }

    action DeleteParagraphHeroImageAction { paragraphHeroImageId } = do
        paragraphHeroImage <- fetch paragraphHeroImageId
        deleteRecord paragraphHeroImage
        setSuccessMessage "Hero Image deleted"
        redirectTo EditLandingPageAction { landingPageId = paragraphHeroImage.landingPageId }

buildParagraphHeroImage paragraphHeroImage = paragraphHeroImage
    |> fill @["landingPageId", "weight", "title", "subtitle"]
    |> validateField #title nonEmpty
    |> validateField #imageUrl nonEmpty
    |> return

