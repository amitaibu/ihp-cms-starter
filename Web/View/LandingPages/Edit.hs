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

    {(textField #title)}
    {(textField #slug)}

    <ul class="flex flex-col gap-y-2">
        <li><a href={pathTo $ NewParagraphCtasAction landingPage.id } class="inline-block btn btn-primary mb-4">+ CTA</a></li>
    </ul>

    {submitButton}

|]