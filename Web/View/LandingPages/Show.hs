module Web.View.LandingPages.Show where
import Web.View.Prelude

data ShowView = ShowView { landingPage :: LandingPage }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show LandingPage</h1>
        <p>{landingPage}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "LandingPages" LandingPagesAction
                            , breadcrumbText "Show LandingPage"
                            ]