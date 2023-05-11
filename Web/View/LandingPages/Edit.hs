module Web.View.LandingPages.Edit where
import Web.View.Prelude

data EditView = EditView { landingPage :: LandingPage }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit LandingPage</h1>
        {renderForm landingPage}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "LandingPages" LandingPagesAction
                , breadcrumbText "Edit LandingPage"
                ]

renderForm :: LandingPage -> Html
renderForm landingPage = formFor landingPage [hsx|
    
    {submitButton}

|]