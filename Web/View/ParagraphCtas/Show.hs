module Web.View.ParagraphCtas.Show where
import Web.View.Prelude
import Web.Element.ElementWrap

data ShowView = ShowView { paragraphCta :: ParagraphCta }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show ParagraphCta</h1>
        <p>{paragraphCta}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "ParagraphCta" ParagraphCtaAction
                            , breadcrumbText "Show ParagraphCta"
                            ]



renderParagraph :: Text -> Text -> Html
renderParagraph title body =
    titleWrapped ++ bodyWrapper
    |> wrapContainerVerticalSpacing
    where
        titleWrapped = cs title
            |> wrapHeaderTag 1

        bodyWrapper = cs body
            |> wrapProse