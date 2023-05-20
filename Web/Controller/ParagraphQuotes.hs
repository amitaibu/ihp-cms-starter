module Web.Controller.ParagraphQuotes where

import Web.Controller.Prelude
import Web.View.ParagraphQuotes.Index
import Web.View.ParagraphQuotes.New
import Web.View.ParagraphQuotes.Edit
import Web.View.ParagraphQuotes.Show

instance Controller ParagraphQuotesController where
    action ParagraphQuotesAction = do
        paragraphQuotes <- query @ParagraphQuote |> fetch
        render IndexView { .. }

    action NewParagraphQuoteAction { landingPageId } = do
        weight <- getParagraphsCount landingPageId
        let paragraphQuote = newRecord
                |> set #landingPageId landingPageId
                |> set #weight weight

        render NewView { .. }

    action ShowParagraphQuoteAction { paragraphQuoteId } = do
        paragraphQuote <- fetch paragraphQuoteId
        render ShowView { .. }

    action EditParagraphQuoteAction { paragraphQuoteId } = do
        paragraphQuote <- fetch paragraphQuoteId
        render EditView { .. }

    action UpdateParagraphQuoteAction { paragraphQuoteId } = do
        paragraphQuote <- fetch paragraphQuoteId
        paragraphQuote
            |> buildParagraphQuote
            |> ifValid \case
                Left paragraphQuote -> render EditView { .. }
                Right paragraphQuote -> do
                    paragraphQuote <- paragraphQuote |> updateRecord
                    setSuccessMessage "ParagraphQuote updated"
                    redirectTo EditLandingPageAction { landingPageId = paragraphQuote.landingPageId }

    action CreateParagraphQuoteAction = do
        let paragraphQuote = newRecord @ParagraphQuote
        paragraphQuote
            |> buildParagraphQuote
            |> ifValid \case
                Left paragraphQuote -> render NewView { .. }
                Right paragraphQuote -> do
                    paragraphQuote <- paragraphQuote |> createRecord
                    setSuccessMessage "ParagraphQuote created"
                    redirectTo EditLandingPageAction { landingPageId = paragraphQuote.landingPageId }

    action DeleteParagraphQuoteAction { paragraphQuoteId } = do
        paragraphQuote <- fetch paragraphQuoteId
        deleteRecord paragraphQuote
        setSuccessMessage "ParagraphQuote deleted"
        redirectTo EditLandingPageAction { landingPageId = paragraphQuote.landingPageId }

buildParagraphQuote paragraphQuote = paragraphQuote
    |> fill @["subtitle", "landingPageId", "weight"]
