module Application.Helper.Controller where

import IHP.ControllerPrelude
import Generated.Types
import Web.Types

-- Here you can add functions which are available in all your controllers

fetchLandingPageWithParagraphs :: (?modelContext :: ModelContext) => Id LandingPage -> IO LandingPageWithParagraphs
fetchLandingPageWithParagraphs landingPageId = do
    landingPage <- fetch landingPageId
    paragraphCtas <- query @ParagraphCta
        |> filterWhere (#landingPageId, landingPageId)
        |> fetch

    paragraphQuotes <- query @ParagraphQuote
        |> filterWhere (#landingPageId, landingPageId)
        |> fetch

    paragraphCtaRefLandingPages <- query @LandingPage
        |> filterWhereIn (#id, map (get #refLandingPageId) paragraphCtas)
        |> fetch

    return $ LandingPageWithParagraphs
        { landingPage = landingPage
        , paragraphCtas = paragraphCtas
        , paragraphQuotes = paragraphQuotes
        , paragraphCtaRefLandingPages = paragraphCtaRefLandingPages
        }

getParagraphsCount :: (?modelContext::ModelContext) => Id LandingPage -> IO Int
getParagraphsCount landingPageId = do
    landingPageWithParagraphs <- fetchLandingPageWithParagraphs landingPageId
    pure $ length landingPageWithParagraphs.paragraphCtas
                    + length landingPageWithParagraphs.paragraphQuotes
                    + 1
