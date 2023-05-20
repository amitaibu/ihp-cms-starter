module Application.Helper.Controller where

import IHP.ControllerPrelude
import Generated.Types
import Web.Types

-- Here you can add functions which are available in all your controllers

fetchLandingPageWithRecords :: (?modelContext :: ModelContext) => Id LandingPage -> IO LandingPageWithRecords
fetchLandingPageWithRecords landingPageId = do
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

    return $ LandingPageWithRecords
        { landingPageWithRecordsLandingPage = landingPage
        , landingPageWithRecordsParagraphCtas = paragraphCtas
        , landingPageWithRecordsParagraphQuotes = paragraphQuotes
        , landingPageWithRecordsParagraphCtaRefLandingPages = paragraphCtaRefLandingPages
        }

getParagraphsCount :: (?modelContext::ModelContext) => Id LandingPage -> IO Int
getParagraphsCount landingPageId = do
    landingPageWithRecords <- fetchLandingPageWithRecords landingPageId
    pure $ length landingPageWithRecords.landingPageWithRecordsParagraphCtas
                    + length landingPageWithRecords.landingPageWithRecordsParagraphQuotes
                    + 1
