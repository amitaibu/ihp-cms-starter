module Web.View.ParagraphQuotes.New where
import Web.View.Prelude
import Web.View.ParagraphQuotes.Form


data NewView = NewView
    { paragraphQuote :: ParagraphQuote
    , formStatus :: FormStatus
    }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New ParagraphQuote</h1>
        {renderForm paragraphQuote True formStatus}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbText "New Quote"
                ]

