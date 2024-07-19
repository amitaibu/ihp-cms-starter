module Web.View.ParagraphQuotes.Edit where
import Web.View.Prelude
import Web.View.ParagraphQuotes.Form

data EditView = EditView
    { paragraphQuote :: ParagraphQuote
    , formStatus :: FormStatus
    , landingPage :: LandingPage
    }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit ParagraphQuote</h1>
        {renderForm paragraphQuote False formStatus}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbText "Landing Page"
                , breadcrumbLink (cs landingPage.title) (EditLandingPageAction landingPage.id)
                , breadcrumbText "Edit Quote"
                ]