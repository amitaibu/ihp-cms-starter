module Web.View.ParagraphCtas.Show where
import Web.View.Prelude
import Web.Element.ElementWrap
import Web.Element.InnerElementLayout

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
    |> wrapContainerVerticalSpacing AlignCenter
    |> buildInnerElementLayout
    |> wrapContainerNarrow
    |> wrapBackgroundColor Gray100
    where
        titleWrapped = cs title
            |> wrapHeaderTag 1

        bodyWrapper = cs body
            |> wrapProse