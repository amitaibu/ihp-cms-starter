module Web.View.ParagraphQuotes.Show where
import Web.View.Prelude

data ShowView = ShowView { paragraphQuote :: ParagraphQuote }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show ParagraphQuote</h1>
        <p>{paragraphQuote}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "ParagraphQuotes" ParagraphQuotesAction
                            , breadcrumbText "Show ParagraphQuote"
                            ]

renderParagraph :: Text -> Html
renderParagraph title =
    [hsx|
    <div>
        <h1 class="text-3xl font-bold">Quote: {title}</h1>
    </div>
    |]