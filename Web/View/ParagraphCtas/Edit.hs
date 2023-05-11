module Web.View.ParagraphCtas.Edit where
import Web.View.Prelude

data EditView = EditView { paragraphCta :: ParagraphCta }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit ParagraphCta</h1>
        {renderForm paragraphCta}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ParagraphCta" ParagraphCtaAction
                , breadcrumbText "Edit ParagraphCta"
                ]

renderForm :: ParagraphCta -> Html
renderForm paragraphCta = formFor paragraphCta [hsx|
    {(hiddenField #landingPageId)}
    {(hiddenField #weight)}
    {(textField #title)}
    {submitButton}

|]