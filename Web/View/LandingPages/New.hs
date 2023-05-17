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

    <div class="flex flex-col gap-y-4">
        {(textField #title)}
        {(textField #slug) {helpText = "This will be used in the URL. It should be unique."}}

        {submitButton {label = "Save Landing page"}}
    </div>

|]