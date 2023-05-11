module Web.Controller.LandingPages where

import Web.Controller.Prelude
import Web.View.LandingPages.Index
import Web.View.LandingPages.New
import Web.View.LandingPages.Edit
import Web.View.LandingPages.Show
import qualified Web.View.ParagraphCtas.Show as ParagraphCtas
import qualified Web.View.ParagraphQuotes.Show as ParagraphQuotes

instance Controller LandingPagesController where
    action LandingPagesAction = do
        landingPages <- query @LandingPage |> fetch
        render IndexView { .. }

    action NewLandingPageAction = do
        let landingPage = newRecord
        render NewView { .. }

    action ShowLandingPageAction { landingPageId } = do
        landingPage <- fetch landingPageId

        -- Get all the Paragraphs for this Landing Page.
        -- As we won't have a lot of Paragraphs, we can just fetch them all.
        paragraphCtas <- query @ParagraphCta
            |> filterWhere (#landingPageId, landingPageId)
            |> orderByAsc #weight
            |> fetch

        paragraphQuotes <- query @ParagraphQuote
            |> filterWhere (#landingPageId, landingPageId)
            |> orderByAsc #weight
            |> fetch


        render ShowView { .. }
        where
            -- We will render each paragraph with it's own renderer, and have a tuple
            -- with the weight, and the `hsx` result. Then we can sort the list by weight.
            paragraphCtas' = paragraphCtas
                    |> fmap (\(paragraph :: ParagraphCta) -> (paragraph.weight, ParagraphCtas.renderParagraph paragraph.title))

            paragraphQuotes' = paragraphQuotes
                    |> fmap (\(paragraph :: ParagraphQuote) -> (paragraph.weight, ParagraphQuotes.renderParagraph paragraph.title))

            -- @todo: Is there a better way, so render happens inside the Show instead of here.
            -- Currently without rendering, we can't place all in a single List, as they have different
            -- types.

            -- allParagraphsRendered = paragraphCtas' ++ paragraphQuotes'
                -- Sort the paragraphs by weight.
                -- |> sortOn fst
                -- Get the `hsx` result.
                -- |> fmap snd

    action EditLandingPageAction { landingPageId } = do
        landingPage <- fetch landingPageId
        render EditView { .. }

    action UpdateLandingPageAction { landingPageId } = do
        landingPage <- fetch landingPageId
        landingPage
            |> buildLandingPage
            |> ifValid \case
                Left landingPage -> render EditView { .. }
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

merge [] ys = ys
merge (x:xs) ys = x:merge ys xs