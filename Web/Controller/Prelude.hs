module Web.Controller.Prelude
( module Web.Types
, module Application.Helper.Controller
, module IHP.ControllerPrelude
, module Generated.Types
, fetchLandingPageWithParagraphs
, getParagraphsCount
)
where

import Web.Types
import Application.Helper.Controller
import IHP.ControllerPrelude
import Generated.Types
import Web.Routes

-- @todo: Is this the correct place for helper functions?
fetchLandingPageWithParagraphs :: (?modelContext :: ModelContext) => Id LandingPage -> IO (Include' ["paragraphCtasLandingPages", "paragraphQuotes"] LandingPage)
fetchLandingPageWithParagraphs landingPageId = do
    fetch landingPageId
        >>= pure . modify #paragraphCtasLandingPages (orderByDesc #weight)
        >>= pure . modify #paragraphQuotes (orderByDesc #weight)
        >>= fetchRelated #paragraphCtasLandingPages
        >>= fetchRelated #paragraphQuotes


getParagraphsCount :: (?modelContext::ModelContext) => Id LandingPage -> IO Int
getParagraphsCount landingPageId = do
    landingPage <- fetchLandingPageWithParagraphs landingPageId
    pure $ length landingPage.paragraphCtasLandingPages
                    + length landingPage.paragraphQuotes
                    + 1
