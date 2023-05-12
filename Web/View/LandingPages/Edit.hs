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

        {orderAndRenderParagraphs landingPage.paragraphCtas landingPage.paragraphQuotes}

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
                    <input type="hidden" name="paragraphId" value={show id} />
                    {title} <span class="text-sm text-gray-600">({type_})</span>
                    <div>
                        <a href={pathTo $ action id} class="text-blue-500 text-sm hover:underline">Edit</a>
                    </div>
                </li>
            |]