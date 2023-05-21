module Web.View.LandingPages.Edit where
import Web.View.Prelude
import Web.Controller.Prelude
import Web.Element.Types
import Web.Element.ElementBuild
import Web.Element.ElementWrap

data EditView = EditView { landingPageWithRecords :: LandingPageWithRecords }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <div class="flex flex-row gap-2">
            <h1>Edit {landingPage.title}</h1>
            <div><a href={ShowLandingPageAction landingPage.id} class="text-blue-500 text-sm hover:underline">(Show)</a></div>
        </div>
        {renderForm landingPage paragraphCtas paragraphQuotes}

        <!-- jsDelivr :: Sortable :: Latest (https://www.jsdelivr.com/package/npm/sortablejs) -->
        <script src="https://cdn.jsdelivr.net/npm/sortablejs@latest/Sortable.min.js"></script>
        <script src={assetPath "/sortable.js"}></script>
    |]
        where
            landingPage = landingPageWithRecords.landingPage
            paragraphCtas = landingPageWithRecords.paragraphCtas
            paragraphQuotes = landingPageWithRecords.paragraphQuotes

            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "LandingPages" LandingPagesAction
                , breadcrumbText "Edit LandingPage"
                ]

renderForm :: LandingPage -> [ParagraphCta] -> [ParagraphQuote] -> Html
renderForm landingPage paragraphCtas paragraphQuotes = formFor landingPage [hsx|

    <div class="container-wide flex flex-col gap-y-4">
        {(textField #title)}

        <div class="flex flex-col gap-y-4 border p-4">
            <h3 class="text-xl">Paragraphs</h3>
            <ul class="flex flex-row gap-4">
                <li><a href={pathTo $ NewParagraphCtaAction landingPage.id } class="inline-block border border-gray-500 rounded-lg px-4 py-2">+ CTA</a></li>
                <li><a href={pathTo $ NewParagraphQuoteAction landingPage.id } class="inline-block border border-gray-500 rounded-lg px-4 py-2">+ Quote</a></li>
            </ul>

            <ul class="js-sortable">
                {orderAndRenderParagraphs paragraphCtas paragraphQuotes}
            </ul>
        </div>

        {submitButton {label = "Save Landing page"}}
    </div>
|]

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
                <li class="flex flex-row gap-2 items-baseline">

                    {sortableHandle}

                    <input type="hidden" name="paragraphId" value={show id} />

                    {title} <span class="text-sm text-gray-600">({type_})</span>
                    {operations}
                </li>
            |]
            where
                operations =
                    [ buildLink (editAction id) "Edit"
                    , buildLinkDeleteAction (deleteAction id)
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