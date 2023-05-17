module Web.View.ParagraphCtas.Edit where
import Web.View.Prelude
import Web.Element.Types
import Web.Element.ElementWrap


data EditView = EditView
    { paragraphCta :: ParagraphCta
    , landingPages :: [LandingPage]
    }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit ParagraphCta</h1>
        {renderForm paragraphCta landingPages}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ParagraphCta" ParagraphCtaAction
                , breadcrumbText "Edit ParagraphCta"
                ]

renderForm :: ParagraphCta -> [LandingPage] -> Html
renderForm paragraphCta landingPages = formFor paragraphCta [hsx|
    {(hiddenField #landingPageId)}
    {(hiddenField #weight)}
    {visibleForm paragraphCta landingPages}

|]
    where
            visibleForm paragraphCta landingPages =
                [hsx|
                    {(textField #title) {required = True}}
                    {(textareaField #body) {required = True}}
                    {submitButton}
                |]
                |> wrapContainerVerticalSpacing AlignNone
                |> wrapContainerWide

