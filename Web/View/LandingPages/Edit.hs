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

    <div class="flex flex-col gap-y-4">
        {(textField #title)}
        {(textField #slug)}

        <ul class="flex flex-row gap-4">
            <li><a href={pathTo $ NewParagraphCtaAction landingPage.id } class="inline-block border border-gray-500 rounded-lg px-4 py-2">+ CTA</a></li>
            <li><a href={pathTo $ NewParagraphQuoteAction landingPage.id } class="inline-block border border-gray-500 rounded-lg px-4 py-2">+ Quote</a></li>
        </ul>

        <div>{submitButton}</div>
    </div>

|]