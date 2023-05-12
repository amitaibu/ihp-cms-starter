module Web.Controller.LandingPages where

import Web.Controller.Prelude
import Web.View.LandingPages.Index
import Web.View.LandingPages.New
import Web.View.LandingPages.Edit
import Web.View.LandingPages.Show

instance Controller LandingPagesController where
    action LandingPagesAction = do
        landingPages <- query @LandingPage |> fetch
        render IndexView { .. }

    action NewLandingPageAction = do
        let landingPage = newRecord
        render NewView { .. }

    action ShowLandingPageAction { landingPageId } = do
        landingPage <- fetchLandingPageWithParagraphs landingPageId

        render ShowView { .. }

    action EditLandingPageAction { landingPageId } = do
        landingPage <- fetchLandingPageWithParagraphs landingPageId

        render EditView { .. }

    action UpdateLandingPageAction { landingPageId } = do
        landingPage <- fetch landingPageId
        landingPage
            |> buildLandingPage
            |> ifValid \case
                Left landingPage -> do

                    -- Fetch the paragraphs again, because we need to show the validation errors.
                    landingPage <- fetchLandingPageWithParagraphs landingPageId

                    render EditView { .. }
                Right landingPage -> do
                    landingPage <- landingPage |> updateRecord
                    setSuccessMessage "LandingPage updated"
                    redirectTo EditLandingPageAction { .. }

    action CreateLandingPageAction = do
        let landingPage = newRecord @LandingPage
        landingPage
            |> buildLandingPage
            |> ifValid \case
                Left landingPage -> render NewView { .. }
                Right landingPage -> do
                    landingPage <- landingPage |> createRecord
                    setSuccessMessage "LandingPage created"
                    -- After we create the Landing page, we can start adding Paragraphs to it.
                    redirectTo EditLandingPageAction { landingPageId = landingPage.id }

    action DeleteLandingPageAction { landingPageId } = do
        landingPage <- fetch landingPageId
        deleteRecord landingPage
        setSuccessMessage "LandingPage deleted"
        redirectTo LandingPagesAction

buildLandingPage landingPage = landingPage
    |> fill @'["title", "slug"]

fetchLandingPageWithParagraphs landingPageId = do
    fetch landingPageId
        >>= pure . modify #paragraphCtas (orderByDesc #weight)
        >>= pure . modify #paragraphQuotes (orderByDesc #weight)
        >>= fetchRelated #paragraphCtas
        >>= fetchRelated #paragraphQuotes