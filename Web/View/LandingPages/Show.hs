module Web.View.LandingPages.Show where

import Web.Element.Cta
import Web.Element.ElementWrap
import Web.Element.Link
import Web.Element.Types
import Web.Element.Quote
import Web.Types
import qualified Web.View.ParagraphQuotes.Show as ParagraphQuotes
import Web.View.Prelude

data ShowView = ShowView { landingPageWithRecords :: LandingPageWithRecords }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {header}

        {orderAndRenderParagraphs landingPageWithRecords }

    |]
        where
            landingPage = landingPageWithRecords.landingPage

            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "LandingPages" LandingPagesAction
                            , breadcrumbText "Show LandingPage"
                            ]

            header = [hsx|
                    {breadcrumb}
                    {titleAndEdit}
                |]
                    |> wrapVerticalSpacing AlignNone
                    |> wrapContainerWide


            titleAndEdit =
                [ cs landingPage.title |> wrapHeaderTag 1
                , renderLinkAction (EditLandingPageAction landingPage.id) "Edit"
                ]
                    |> mconcat
                    |> wrapHorizontalSpacingTiny AlignBaseline


orderAndRenderParagraphs :: (?context::ControllerContext) => LandingPageWithRecords -> Html
orderAndRenderParagraphs landingPageWithRecords =
    ctas ++ quotes
        -- Order by weight.
        |> sortOn fst
        |> fmap snd
        |> mconcat
    where

        ctas = landingPageWithRecords.paragraphCtas
            |> fmap (\paragraph ->
                ( paragraph.weight
                , renderParagraphCta paragraph
                )
            )

        quotes = landingPageWithRecords.paragraphQuotes
            |> fmap (\paragraph ->
                ( paragraph.weight
                , renderParagraphQuote paragraph
                )
            )

renderParagraphCta :: ParagraphCta -> Html
renderParagraphCta paragraphCta =
    RenderCta
        { title = paragraphCta.title
        , body = paragraphCta.body
        , button = RenderButton
            { text = "Read More"
            , url = pathTo $ ShowLandingPageAction paragraphCta.landingPageId
            , isPrimary = False
            }
        }
        |> Web.Element.Cta.render

renderParagraphQuote :: ParagraphQuote -> Html
renderParagraphQuote paragraphQuote =
    RenderQuote
        { body = paragraphQuote.body
        , subtitle = paragraphQuote.subtitle
        , imageUrl = paragraphQuote.imageUrl |> fromMaybe ""
        }
        |> Web.Element.Quote.render