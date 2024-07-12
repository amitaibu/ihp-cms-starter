module Web.View.LandingPages.Show where
import Web.Element.ElementWrap
import Web.Element.Link
import Web.Element.Types
import Web.Types
import qualified Web.View.ParagraphCtas.Show as ParagraphCtas
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
            |> sortOn fst
            |> fmap snd
            |> mconcat
    where
        paragraphCtas = landingPageWithRecords.paragraphCtas
        paragraphQuotes = landingPageWithRecords.paragraphQuotes
        paragraphCtaRefLandingPages = landingPageWithRecords.paragraphCtaRefLandingPages

        -- @todos:
        ctas = []
        -- ctas = paragraphCtas
        --     |> fmap (\paragraph ->
        --         let
        --             -- Get the referenced Landing page out of the ParagraphCTA, through the `ParagraphCtaRefLandingPageId`
        --             -- property.
        --             refLandingPageButton =
        --                     paragraphCtaRefLandingPages
        --                         -- Get the referenced Landing page
        --                         |> filter (\paragraphCtaRefLandingPage -> paragraphCtaRefLandingPage.id == paragraph.refLandingPageId)
        --                         |> head
        --                         -- Get the button from the referenced Landing page.
        --                         |> maybe "" (\landingPage -> buildButtonPrimary (pathTo $ ShowLandingPageAction landingPage.id) landingPage.title)

        --         in
        --         ( paragraph.weight
        --         , ParagraphCtas.renderParagraph paragraph.title paragraph.body refLandingPageButton
        --         )

        --     )

        quotes = paragraphQuotes
            |> fmap (\paragraph ->
                ( paragraph.weight
                , case paragraph.imageUrl of
                    Just imageUrl -> ParagraphQuotes.renderParagraph paragraph.body paragraph.subtitle imageUrl
                    Nothing -> ""
                ))

