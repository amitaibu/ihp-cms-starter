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

                    -- After we update the Landing page, we can set the order of the paragraphs.
                    let params = paramListOrNothing @UUID "paragraphId"

                    case catMaybes params of
                        [] -> do
                            -- No paragraphs to update.
                            pure ()

                        uuids -> do
                            -- We need to update the weight of the paragraphs,
                            -- So load them.
                            landingPage <- fetchLandingPageWithParagraphs landingPageId

                            forEachWithIndex uuids (\(weight, uuid) -> do
                                    -- We need to find the paragraph by uuid.
                                    -- @todo: Something like this, but should work :)
                                    -- Also, there's probably a nicer way to write this.
                                    let mparagraph = find (\p -> p.id == packId uuid) landingPage.paragraphCtas
                                            |> \case
                                                Just paragraph -> Just paragraph
                                                Nothing -> find (\p -> p.id == packId uuid) landingPage.paragraphQuotes


                                    case mparagraph of
                                        Nothing -> pure ()
                                        Just paragraph ->
                                            if paragraph.weight /= weight
                                                then do
                                                    -- Set new weight.
                                                    paragraph <- paragraph
                                                        |> set #weight weight
                                                        |> updateRecord

                                                    pure ()

                                                else
                                                    -- Weight hasn't changed.
                                                    pure ()
                                )


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

