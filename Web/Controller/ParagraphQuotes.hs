module Web.Controller.ParagraphQuotes where

import Web.Controller.Prelude
import Web.View.ParagraphQuotes.New
import Web.View.ParagraphQuotes.Edit

instance Controller ParagraphQuotesController where
    action NewParagraphQuoteAction { .. } = do
        weight <- getParagraphsCount landingPageId
        let paragraphQuote = newRecord
                |> set #landingPageId landingPageId
                |> set #weight weight

        let formStatus = FormStatusNotSubmitted

        render NewView { .. }

    action EditParagraphQuoteAction { paragraphQuoteId } = do
        paragraphQuote <- fetch paragraphQuoteId
        -- Get from the session, if the form was submitted successfully.
        formStatus <- getAndClearFormStatus
        render EditView { .. }

    action UpdateParagraphQuoteAction { paragraphQuoteId } = do
        let uploadImage = uploadToStorageWithOptions $ def
                { preprocess = applyImageMagick "jpg" ["-resize", "1024x1024^", "-gravity", "north", "-extent", "1024x1024", "-quality", "85%", "-strip"] }

        formStatus <- getAndClearFormStatus

        paragraphQuote <- fetch paragraphQuoteId
        paragraphQuote
            |> uploadImage #imageUrl
            >>= buildParagraphQuote
            >>= ifValid \case
                Left paragraphQuote -> do
                    setFormStatus FormStatusError
                    render EditView { .. }
                Right paragraphQuote -> do
                    paragraphQuote <- paragraphQuote |> updateRecord
                    setSuccessMessage "Quote updated"
                    setFormStatus FormStatusSuccess
                    redirectTo EditLandingPageAction { landingPageId = paragraphQuote.landingPageId }

    action CreateParagraphQuoteAction = do
        let uploadImage = uploadToStorageWithOptions $ def
                { preprocess = applyImageMagick "jpg" ["-resize", "1024x1024^", "-gravity", "north", "-extent", "1024x1024", "-quality", "85%", "-strip"] }

        let paragraphQuote = newRecord @ParagraphQuote
        let formStatus = FormStatusNotSubmitted

        paragraphQuote
            |> uploadImage #imageUrl
            >>= buildParagraphQuote
            >>= ifValid \case
                Left paragraphQuote -> do
                    setFormStatus FormStatusError
                    render NewView { .. }
                Right paragraphQuote -> do
                    paragraphQuote <- paragraphQuote |> createRecord
                    setSuccessMessage "Quote created"
                    setFormStatus FormStatusSuccess
                    redirectTo EditLandingPageAction { landingPageId = paragraphQuote.landingPageId }

    action DeleteParagraphQuoteAction { paragraphQuoteId } = do
        paragraphQuote <- fetch paragraphQuoteId
        deleteRecord paragraphQuote
        setSuccessMessage "Quote deleted"
        redirectTo EditLandingPageAction { landingPageId = paragraphQuote.landingPageId }

buildParagraphQuote paragraphQuote = paragraphQuote
    |> fill @["landingPageId", "weight", "subtitle", "body"]
    |> validateField #subtitle nonEmpty
    |> validateField #body nonEmpty
    |> validateField #imageUrl nonEmpty
    |> return

