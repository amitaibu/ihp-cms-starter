module Web.View.LandingPages.Edit where
import Web.Controller.Prelude
import Web.Element.Button
import Web.Element.ElementWrap
import Web.Element.Link
import Web.Element.Types
import Web.View.Prelude

data EditView = EditView { landingPageWithRecords :: LandingPageWithRecords }

instance View EditView where
    html EditView { .. } = [hsx|
        {body}
    |]
        where
            body =
                [ header
                , renderForm landingPage paragraphCtas paragraphQuotes
                ]
                |> mconcat
                |> wrapVerticalSpacing AlignNone
                |> wrapContainerWide

            landingPage = landingPageWithRecords.landingPage
            paragraphCtas = landingPageWithRecords.paragraphCtas
            paragraphQuotes = landingPageWithRecords.paragraphQuotes

            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "LandingPages" LandingPagesAction
                , breadcrumbText "Edit LandingPage"
                ]

            header =
                [ breadcrumb
                , titleAndEdit
                ]
                |> mconcat
                |> wrapVerticalSpacing AlignNone


            titleAndEdit =
                [ [hsx|Edit {landingPage.title}|] |> wrapHeaderTag 1
                , renderLinkAction (ShowLandingPageAction landingPage.id) "Show"
                ]
                |> mconcat
                |> wrapHorizontalSpacingTiny AlignBaseline

renderForm :: LandingPage -> [ParagraphCta] -> [ParagraphQuote] -> Html
renderForm landingPage paragraphCtas paragraphQuotes = formFor landingPage [hsx|
    {body}
|]

    where
        body :: (?formContext :: FormContext LandingPage) => Html
        body = [hsx|
            {(textField #title)}

            <div class="border p-4">
                {paragraphs}
            </div>

            {submitButton {label = "Save Landing page"}}
        |]
            |> wrapVerticalSpacing AlignNone

        paragraphs =
            [   addParagraphs
            ,   [hsx|
                    <ul class="js-sortable">
                        {orderAndRenderParagraphs paragraphCtas paragraphQuotes}
                    </ul>
                |]
            ]
                |> mconcat
                |> wrapVerticalSpacing AlignNone


        addParagraphs =
            [   cs ("Paragraphs" :: Text) |> wrapHeaderTag 3
            ,   paragraphButtons
            ]
                |> mconcat
                |> wrapVerticalSpacing AlignNone

        paragraphButtons =
            [   renderButtonAction (NewParagraphCtaAction landingPage.id) "New CTA"
            ,   renderButtonAction (NewParagraphQuoteAction landingPage.id) "New Quote"
            ]
                |> mconcat
                |> wrapHorizontalSpacing AlignCenter

orderAndRenderParagraphs :: [ParagraphCta] -> [ParagraphQuote] -> Html
orderAndRenderParagraphs paragraphCtas paragraphQuotes =
    ctas
        ++ quotes
        |> sortOn fst
        |> fmap snd
        |> mconcat

    where
        ctas = paragraphCtas
            |> fmap (\paragraph -> (paragraph.weight, paragraphTitleAndOps EditParagraphCtaAction DeleteParagraphCtaAction paragraph.id paragraph.title ("CTA" :: Text)))

        quotes = paragraphQuotes
            |> fmap (\paragraph -> (paragraph.weight, paragraphTitleAndOps EditParagraphQuoteAction DeleteParagraphQuoteAction paragraph.id paragraph.subtitle ("Quote" :: Text)))



        -- Show the paragraph title and the operations to perform on it.
        paragraphTitleAndOps :: (Show (PrimaryKey record), HasPath controller) => (Id' record -> controller) -> (Id' record -> controller) -> Id' record -> Text -> Text -> Html
        paragraphTitleAndOps editAction deleteAction id title type_  =
            [hsx|
                <li>{body}</li>
            |]
            where
                body =
                    [hsx|
                        {sortableHandle}

                        <input type="hidden" name="paragraphId" value={show id} />

                        {title} <span class="text-sm text-gray-600">({type_})</span>
                        {operations}
                    |]
                        |> wrapHorizontalSpacingTiny AlignCenter

                operations =
                    [   renderLinkAction (editAction id) "Edit"
                    ,   renderLinkDeleteAction (deleteAction id)
                    ]
                        |> mconcat
                        |> wrapHorizontalSpacingTiny AlignNone



        sortableHandle =
            if
                length ctas + length quotes > 1
            then
                [hsx|
                    <div class="sortable-handle">
                        <div class="sortable-handle">
                            <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="w-4 h-4 text-gray-200 hover:text-gray-500">
                                <path stroke-linecap="round" stroke-linejoin="round" d="M3 7.5L7.5 3m0 0L12 7.5M7.5 3v13.5m13.5 0L16.5 21m0 0L12 16.5m4.5 4.5V7.5" />
                            </svg>
                        </div>
                    </div>
                |]
            else
                mempty
