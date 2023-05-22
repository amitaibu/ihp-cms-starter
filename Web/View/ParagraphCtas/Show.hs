module Web.View.ParagraphCtas.Show where
import Web.View.Prelude

import Web.Element.Types
import Web.Element.ElementWrap
import Web.Element.InnerElementLayout
import Web.Element.ElementBuild (buildButtonPrimary)

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



renderParagraph :: Text -> Text -> Html -> Html
renderParagraph title body button =
    titleWrapped
        ++ bodyWrapped
        ++ button
        |> wrapVerticalSpacing AlignCenter
        |> buildInnerElementLayout Gray100
        |> wrapContainerNarrow
    where
        titleWrapped = cs title
            |> wrapHeaderTag 1

        bodyWrapped = body
            |> preEscapedToHtml
            |> wrapProse