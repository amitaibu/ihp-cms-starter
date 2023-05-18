module Web.View.LandingPages.Show where
import Web.View.Prelude
import Text.Blaze.Html
import qualified Web.View.ParagraphCtas.Show as ParagraphCtas
import qualified Web.View.ParagraphQuotes.Show as ParagraphQuotes

data ShowView = ShowView
    {   landingPage :: Include' ["paragraphCtasLandingPages", "paragraphQuotes"] LandingPage
    }

instance View ShowView where
    html ShowView { .. } = [hsx|


    |]
        where
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
