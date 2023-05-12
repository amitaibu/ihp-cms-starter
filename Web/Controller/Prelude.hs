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
fetchLandingPageWithParagraphs landingPageId = do
    fetch landingPageId
        >>= pure . modify #paragraphCtas (orderByDesc #weight)
        >>= pure . modify #paragraphQuotes (orderByDesc #weight)
        >>= fetchRelated #paragraphCtas
        >>= fetchRelated #paragraphQuotes


getParagraphsCount landingPageId = do
    landingPage <- fetchLandingPageWithParagraphs landingPageId
    pure $ length landingPage.paragraphCtas
                    + length landingPage.paragraphQuotes
                    + 1
