module Web.View.ParagraphQuotes.Edit where
import Web.View.Prelude
import Web.View.ParagraphQuotes.Form

data EditView = EditView { paragraphQuote :: ParagraphQuote }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit ParagraphQuote</h1>
        {renderForm paragraphQuote False}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbText "Edit Quote"
                ]
