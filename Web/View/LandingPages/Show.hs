module Web.View.LandingPages.Show where
import Web.View.Prelude
import Text.Blaze.Html
import qualified Web.View.ParagraphCtas.Show as ParagraphCtas
import qualified Web.View.ParagraphQuotes.Show as ParagraphQuotes
import Web.Types
import Web.Element.ElementBuild (buildButtonPrimary)

data ShowView = ShowView { landingPageWithRecords :: LandingPageWithRecords }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <div class="flex flex-row gap-2 items-baseline">
            <h1 class="text-3xl">{landingPage.title}</h1>
            <a href={EditLandingPageAction landingPage.id} class="text-blue-500 text-sm hover:underline hover:text-blue-600">(Edit)</a>
        </div>

        {orderAndRenderParagraphs landingPageWithRecords }

    |]
        where
            landingPage = landingPageWithRecords.landingPageWithRecordsLandingPage

            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "LandingPages" LandingPagesAction
                            , breadcrumbText "Show LandingPage"
                            ]


orderAndRenderParagraphs :: (?context::ControllerContext) => LandingPageWithRecords -> Text.Blaze.Html.Html
orderAndRenderParagraphs landingPageWithRecords =
    [hsx|{forEach allSorted (\rendered -> rendered)}|]
    where
        paragraphCtas = landingPageWithRecords.landingPageWithRecordsParagraphCtas
        paragraphQuotes = landingPageWithRecords.landingPageWithRecordsParagraphQuotes
        paragraphCtaRefLandingPages = landingPageWithRecords.landingPageWithRecordsParagraphCtaRefLandingPages

        ctas = paragraphCtas
            |> fmap (\paragraph ->
                let
                    -- Get the referenced Landing page out of the ParagraphCTA, through the `ParagraphCtaRefLandingPageId`
                    -- property.
                    refLandingPageButton =
                            paragraphCtaRefLandingPages
                                -- Get the referenced Landing page
                                |> filter (\paragraphCtaRefLandingPage -> paragraphCtaRefLandingPage.id == paragraph.refLandingPageId)
                                |> head
                                -- Get the button from the referenced Landing page.
                                |> maybe mempty (\landingPage -> buildButtonPrimary (pathTo $ ShowLandingPageAction landingPage.id) landingPage.title)

                in
                ( paragraph.weight
                , ParagraphCtas.renderParagraph paragraph.title paragraph.body refLandingPageButton
                )

            )

        quotes = paragraphQuotes
            |> fmap (\paragraph -> (paragraph.weight, ParagraphQuotes.renderParagraph paragraph.title))

        allSorted = ctas ++ quotes
            |> sortOn fst
            |> fmap snd
