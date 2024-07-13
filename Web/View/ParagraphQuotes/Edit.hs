module Web.View.ParagraphQuotes.Edit where
import Web.View.Prelude
import Web.View.ParagraphQuotes.Form

data EditView = EditView
    { paragraphQuote :: ParagraphQuote
    , formStatus :: FormStatus
    }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit ParagraphQuote</h1>
        {renderForm paragraphQuote False formStatus}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbText "Edit Quote"
                ]
