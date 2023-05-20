module Web.View.ParagraphQuotes.New where
import Web.View.Prelude
import Web.View.ParagraphQuotes.Form


data NewView = NewView { paragraphQuote :: ParagraphQuote }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New ParagraphQuote</h1>
        {renderForm paragraphQuote False}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ParagraphQuotes" ParagraphQuotesAction
                , breadcrumbText "New ParagraphQuote"
                ]

