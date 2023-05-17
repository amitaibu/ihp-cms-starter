module Web.View.ParagraphQuotes.New where
import Web.View.Prelude

data NewView = NewView { paragraphQuote :: ParagraphQuote }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New ParagraphQuote</h1>
        {renderForm paragraphQuote}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ParagraphQuotes" ParagraphQuotesAction
                , breadcrumbText "New ParagraphQuote"
                ]

renderForm :: ParagraphQuote -> Html
renderForm paragraphQuote = formFor paragraphQuote [hsx|
    {(hiddenField #landingPageId)}
    {(hiddenField #weight)}
    {(textField #title) {required = True}}
    {submitButton}

|]