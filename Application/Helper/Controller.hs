module Application.Helper.Controller where

import IHP.ControllerPrelude
import Generated.Types

-- Here you can add functions which are available in all your controllers

fetchLandingPageWithParagraphs :: (?modelContext :: ModelContext) => Id LandingPage -> IO (Include' ["paragraphCtasLandingPages", "paragraphQuotes"] LandingPage)
fetchLandingPageWithParagraphs landingPageId = do
    fetch landingPageId
        >>= pure . modify #paragraphCtasLandingPages (orderByDesc #weight)
        >>= pure . modify #paragraphQuotes (orderByDesc #weight)
        >>= fetchRelated #paragraphCtasLandingPages
        >>= fetchRelated #paragraphQuotes
        >>= (\landingPage -> do
                paragraphCtasLandingPages <- query @ParagraphCta
                    |> filterWhereIn (#id, ids landingPage.paragraphCtasLandingPages)
                    |> fetch
                    >>= collectionFetchRelated #refLandingPageId

                pure $ updateLandingPage landingPage paragraphCtasLandingPages
            )
        where
            updateLandingPage :: Include' ["paragraphCtasLandingPages", "paragraphQuotes"] LandingPage -> [Include "refLandingPageId" ParagraphCta] -> Include' ["paragraphCtasLandingPages", "paragraphQuotes"] LandingPage
            updateLandingPage landingPage paragraphCtasLandingPages =
                landingPage
                    |> updateField @"paragraphCtasLandingPages" paragraphCtasLandingPages


getParagraphsCount :: (?modelContext::ModelContext) => Id LandingPage -> IO Int
getParagraphsCount landingPageId = do
    landingPage <- fetchLandingPageWithParagraphs landingPageId
    pure $ length landingPage.paragraphCtasLandingPages
                    + length landingPage.paragraphQuotes
                    + 1
