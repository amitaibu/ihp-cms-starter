module Web.View.LandingPages.Show where
import Web.View.Prelude
import Text.Blaze.Html
import qualified Web.View.ParagraphCtas.Show as ParagraphCtas
import qualified Web.View.ParagraphQuotes.Show as ParagraphQuotes
import Web.Types

data ShowView = ShowView { landingPageWithRecords :: LandingPageWithRecords }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <div class="flex flex-row gap-2 items-baseline">
            <h1 class="text-3xl">{landingPage.title}</h1>
            <a href={EditLandingPageAction landingPage.id} class="text-blue-500 text-sm hover:underline hover:text-blue-600">(Edit)</a>
        </div>

        {orderAndRenderParagraphs paragraphCtas paragraphQuotes}

    |]
        where
            landingPage = landingPageWithRecords.landingPageWithRecordsLandingPage
            paragraphCtas = landingPageWithRecords.landingPageWithRecordsParagraphCtas
            paragraphQuotes = landingPageWithRecords.landingPageWithRecordsParagraphQuotes

            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "LandingPages" LandingPagesAction
                            , breadcrumbText "Show LandingPage"
                            ]


orderAndRenderParagraphs :: (?context::ControllerContext) => [ParagraphCta] -> [ParagraphQuote] -> Text.Blaze.Html.Html
orderAndRenderParagraphs ctas quotes =
    [hsx|{forEach allSorted (\rendered -> rendered)}|]
    where
        ctas' = ctas
            |> fmap (\paragraph ->
                ( paragraph.weight
                , ParagraphCtas.renderParagraph paragraph.title paragraph.body
                )
            )

        quotes' = quotes
            |> fmap (\paragraph -> (paragraph.weight, ParagraphQuotes.renderParagraph paragraph.title))

        allSorted = ctas' ++ quotes'
            |> sortOn fst
            |> fmap snd
