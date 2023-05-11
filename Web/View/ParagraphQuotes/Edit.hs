module Web.View.ParagraphQuotes.Edit where
import Web.View.Prelude

data EditView = EditView { paragraphQuote :: ParagraphQuote }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit ParagraphQuote</h1>
        {renderForm paragraphQuote}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ParagraphQuotes" ParagraphQuotesAction
                , breadcrumbText "Edit ParagraphQuote"
                ]

renderForm :: ParagraphQuote -> Html
renderForm paragraphQuote = formFor paragraphQuote [hsx|
    {(hiddenField #landingPageId)}
    {(hiddenField #weight)}
    {(textField #title)}
    {submitButton}

|]