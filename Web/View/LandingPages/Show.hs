module Web.View.LandingPages.Show where
import Web.View.Prelude
import Text.Blaze.Html
import qualified Web.View.ParagraphCtas.Show as ParagraphCtas
import qualified Web.View.ParagraphQuotes.Show as ParagraphQuotes

data ShowView = ShowView
    {   landingPage :: Include' ["paragraphCtas", "paragraphQuotes"] LandingPage
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <div class="flex flex-row gap-2 items-baseline">
            <h1 class="text-3xl">{landingPage.title}</h1>
            <a href={EditLandingPageAction landingPage.id} class="text-blue-500 text-sm hover:underline hover:text-blue-600">(Edit)</a>
        </div>

        {orderAndRenderParagraphs landingPage.paragraphCtas landingPage.paragraphQuotes}

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "LandingPages" LandingPagesAction
                            , breadcrumbText "Show LandingPage"
                            ]


-- orderAndRenderParagraphs :: [ParagraphCta] -> [ParagraphQuote] -> Text.Blaze.Html.Html
orderAndRenderParagraphs ctas quotes =
    [hsx|{forEach sorted (\tuple -> snd tuple)}|]
    where
        ctas' = fmap (\paragraph -> (paragraph.weight, ParagraphCtas.renderParagraph paragraph.title)) ctas
        quotes' = fmap (\paragraph -> (paragraph.weight, ParagraphQuotes.renderParagraph paragraph.title)) quotes
        all = ctas' ++ quotes'
        sorted = sortOn fst all
