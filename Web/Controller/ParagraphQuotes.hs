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

        render NewView { .. }

    action EditParagraphQuoteAction { paragraphQuoteId } = do
        paragraphQuote <- fetch paragraphQuoteId
        render EditView { .. }

    action UpdateParagraphQuoteAction { paragraphQuoteId } = do
        let uploadImage = uploadToStorageWithOptions $ def
                { preprocess = applyImageMagick "jpg" ["-resize", "1024x1024^", "-gravity", "north", "-extent", "1024x1024", "-quality", "85%", "-strip"] }


        paragraphQuote <- fetch paragraphQuoteId
        paragraphQuote
            |> uploadImage #imageUrl
            >>= buildParagraphQuote
            >>= ifValid \case
                Left paragraphQuote -> render EditView { .. }
                Right paragraphQuote -> do
                    paragraphQuote <- paragraphQuote |> updateRecord
                    setSuccessMessage "Quote updated"
                    redirectTo EditLandingPageAction { landingPageId = paragraphQuote.landingPageId }

    action CreateParagraphQuoteAction = do
        let uploadImage = uploadToStorageWithOptions $ def
                { preprocess = applyImageMagick "jpg" ["-resize", "1024x1024^", "-gravity", "north", "-extent", "1024x1024", "-quality", "85%", "-strip"] }

        let paragraphQuote = newRecord @ParagraphQuote

        paragraphQuote
            |> uploadImage #imageUrl
            >>= buildParagraphQuote
            >>= ifValid \case
                Left paragraphQuote -> render NewView { .. }
                Right paragraphQuote -> do
                    paragraphQuote <- paragraphQuote |> createRecord
                    setSuccessMessage "Quote created"
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

