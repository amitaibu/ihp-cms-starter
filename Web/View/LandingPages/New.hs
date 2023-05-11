module Web.View.LandingPages.New where
import Web.View.Prelude

data NewView = NewView { landingPage :: LandingPage }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New LandingPage</h1>
        {renderForm landingPage}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "LandingPages" LandingPagesAction
                , breadcrumbText "New LandingPage"
                ]

renderForm :: LandingPage -> Html
renderForm landingPage = formFor landingPage [hsx|
    
    {submitButton}

|]