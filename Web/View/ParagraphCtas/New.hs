module Web.View.ParagraphCtas.New where
import Web.View.Prelude

data NewView = NewView { paragraphCta :: ParagraphCta }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New ParagraphCta</h1>
        {renderForm paragraphCta}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ParagraphCta" ParagraphCtaAction
                , breadcrumbText "New ParagraphCta"
                ]

renderForm :: ParagraphCta -> Html
renderForm paragraphCta = formFor paragraphCta [hsx|
    {(hiddenField #landingPageId)}
    {(hiddenField #weight)}
    {(textField #title)}
    {(textareaField #body)}
    {submitButton}

|]