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
    html ShowView { .. } = do
        paragraphs <- orderAndRenderParagraphs landingPageWithRecords
        [hsx|
            {header}

            { paragraphs }

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
                , buildLink (EditLandingPageAction landingPage.id) "Edit"
                ]
                    |> mconcat
                    |> wrapHorizontalSpacingTiny AlignBaseline


-- orderAndRenderParagraphs :: (?context::ControllerContext) => LandingPageWithRecords -> IO Html
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
                , pure $ ParagraphCtas.renderParagraph paragraph.title paragraph.body refLandingPageButton
                )

            )

        quotes = paragraphQuotes
            |> fmap (\paragraph ->
                ( paragraph.weight
                , ParagraphQuotes.renderParagraph paragraph.body paragraph.subtitle (fromMaybe "" paragraph.imageUrl)
                ))

