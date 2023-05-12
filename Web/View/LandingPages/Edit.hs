module Web.View.LandingPages.Edit where
import Web.View.Prelude


data EditView = EditView { landingPage :: Include' ["paragraphCtas", "paragraphQuotes"] LandingPage }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <div class="flex flex-row gap-2">
            <h1>Edit {landingPage.title}</h1>
            <div><a href={ShowLandingPageAction landingPage.id} class="text-blue-500 text-sm hover:underline">(Show)</a></div>
        </div>
        {renderForm landingPage}

        <!-- jsDelivr :: Sortable :: Latest (https://www.jsdelivr.com/package/npm/sortablejs) -->
        <script src="https://cdn.jsdelivr.net/npm/sortablejs@latest/Sortable.min.js"></script>
        <script src={assetPath "/sortable.js"}></script>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "LandingPages" LandingPagesAction
                , breadcrumbText "Edit LandingPage"
                ]

renderForm :: Include' ["paragraphCtas", "paragraphQuotes"] LandingPage -> Html
renderForm landingPage = formFor landingPage [hsx|

    <div class="flex flex-col gap-y-4">
        {(textField #title)}
        {(textField #slug)}

        <ul class="flex flex-row gap-4">
            <li><a href={pathTo $ NewParagraphCtaAction landingPage.id } class="inline-block border border-gray-500 rounded-lg px-4 py-2">+ CTA</a></li>
            <li><a href={pathTo $ NewParagraphQuoteAction landingPage.id } class="inline-block border border-gray-500 rounded-lg px-4 py-2">+ Quote</a></li>
        </ul>

        <ul id="sortable">
            {orderAndRenderParagraphs landingPage.paragraphCtas landingPage.paragraphQuotes}
        </ul>

        <div>{submitButton}</div>
    </div>

|]

-- orderAndRenderParagraphs :: (?context::ControllerContext) => [ParagraphCta] -> [ParagraphQuote] -> Text.Blaze.Html.Html
orderAndRenderParagraphs ctas quotes =
    [hsx|{forEach allSorted (\rendered -> rendered)}|]
    where
        ctas' = ctas
            |> fmap (\paragraph -> (paragraph.weight, paragraphTitleAndOps EditParagraphCtaAction paragraph.id paragraph.title ("CTA" :: Text)))

        quotes' = quotes
            |> fmap (\paragraph -> (paragraph.weight, paragraphTitleAndOps EditParagraphQuoteAction paragraph.id paragraph.title ("Quote" :: Text)))

        allSorted = ctas' ++ quotes'
            |> sortOn fst
            |> fmap snd

        paragraphTitleAndOps action id title type_  =
            -- Show the paragraph title and the operations to perform on it.
            [hsx|
                <li class="flex flex-row gap-2 items-baseline">

                    <div class="sortable-handle">
                        <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="w-4 h-4 text-gray-200 hover:text-gray-500">
                            <path stroke-linecap="round" stroke-linejoin="round" d="M3 7.5L7.5 3m0 0L12 7.5M7.5 3v13.5m13.5 0L16.5 21m0 0L12 16.5m4.5 4.5V7.5" />
                        </svg>
                    </div>

                    <input type="hidden" name="paragraphId" value={show id} />

                    {title} <span class="text-sm text-gray-600">({type_})</span>
                    <div>
                        <a href={pathTo $ action id} class="text-blue-500 text-sm hover:underline">Edit</a>
                    </div>
                </li>
            |]