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
                    landingPageWithParagraphs <- fetchLandingPageWithParagraphs landingPageId

                    render EditView { landingPage = landingPage |> set1 landingPageWithParagraphs |> set2 landingPageWithParagraphs }
                    where
                        set1 :: Include' ["paragraphCtas", "paragraphQuotes"] LandingPage -> LandingPage -> Include "paragraphCtas" LandingPage
                        set1 landingPageWithParagraphs landingPage = landingPage
                            |> updateField @"paragraphCtas" landingPageWithParagraphs.paragraphCtas

                        set2 :: Include' ["paragraphCtas", "paragraphQuotes"] LandingPage -> Include "paragraphCtas" LandingPage -> Include' ["paragraphCtas", "paragraphQuotes"] LandingPage
                        set2 landingPageWithParagraphs landingPage  = landingPage
                            |> updateField @"paragraphQuotes" landingPageWithParagraphs.paragraphQuotes



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

                            -- Iterate over all paragraphs, and update the weight.
                            forEach landingPage.paragraphCtas updateParagraph
                            forEach landingPage.paragraphQuotes updateParagraph

                            where
                                updateParagraph :: forall record.
                                        ( HasField "id" record (Id record)
                                        , SetField "weight" record Int
                                        , CanUpdate record
                                        , PrimaryKey (GetTableName record) ~ UUID
                                        , ?modelContext :: ModelContext
                                        ) => record -> IO ()
                                updateParagraph paragraph = do
                                    let uuid :: UUID = unpackId paragraph.id
                                    let weight = elemIndex uuid uuids |> fromMaybe 0

                                    paragraph
                                        |> set #weight weight
                                        |> updateRecord


                                    pure ()

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
    |> validateField #title nonEmpty
    |> validateField #slug nonEmpty

