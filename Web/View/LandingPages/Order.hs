module Web.View.LandingPages.Order where

import Web.Element.Cta
import Web.Element.ElementWrap
import Web.Element.Link
import Web.Element.Types
import Web.Element.Quote
import Web.Types
import Web.View.Prelude

data OrderParagraphsView = OrderParagraphsView { landingPageWithRecords :: LandingPageWithRecords }

instance View OrderParagraphsView where
    html OrderParagraphsView { .. } = [hsx|
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
                , cs paragraph.title
                )
            )

        quotes = landingPageWithRecords.paragraphQuotes
            |> fmap (\paragraph ->
                ( paragraph.weight
                , cs paragraph.body
                )
            )
