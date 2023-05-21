module Web.View.LandingPages.Show where
import Web.View.Prelude
import qualified Web.View.ParagraphCtas.Show as ParagraphCtas
import qualified Web.View.ParagraphQuotes.Show as ParagraphQuotes
import Web.Types
import Web.Element.Types
import Web.Element.ElementBuild
import Web.Element.ElementWrap

data ShowView = ShowView { landingPageWithRecords :: LandingPageWithRecords }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div class="container-wide w-full flex flex-col gap-4">
            {header}
        </div>

        {orderAndRenderParagraphs landingPageWithRecords }

    |]
        where
            landingPage = landingPageWithRecords.landingPage



            header = [hsx|
                    {breadcrumb}
                    {titleAndEdit}
                |]
                    |> wrapVerticalSpacing AlignNone
                    |> wrapContainerWide

            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "LandingPages" LandingPagesAction
                            , breadcrumbText "Show LandingPage"
                            ]

            titleAndEdit =
                [ cs landingPage.title |> wrapHeaderTag 1
                , buildLink (EditLandingPageAction landingPage.id) "Edit"
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
            |> fmap (\paragraph ->
                ( paragraph.weight
                , case paragraph.imageUrl of
                    Just imageUrl -> ParagraphQuotes.renderParagraph paragraph.body paragraph.subtitle imageUrl
                    Nothing -> mempty
                ))

