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
                        <svg xmlns="http://www.w3.org/2000/svg" class="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 11.5V14m0-2.5v-6a1.5 1.5 0 113 0m-3 6a1.5 1.5 0 00-3 0v2a7.5 7.5 0 0015 0v-5a1.5 1.5 0 00-3 0m-6-3V11m0-5.5v-1a1.5 1.5 0 013 0v1m0 0V11m0-5.5a1.5 1.5 0 013 0v3m0 0V11" />
                        </svg>
                    </div>

                    <input type="hidden" name="paragraphId" value={show id} />

                    {title} <span class="text-sm text-gray-600">({type_})</span>
                    <div>
                        <a href={pathTo $ action id} class="text-blue-500 text-sm hover:underline">Edit</a>
                    </div>
                </li>
            |]