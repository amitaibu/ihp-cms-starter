module Web.Element.Cta where

import Web.View.Prelude

import Web.Element.Types
import Web.Element.ElementBuild
import Web.Element.ElementWrap


renderSingle :: RenderCta -> Html
renderSingle record@RenderCta {title, body, link} =
    titleWrapped
        ++ bodyWrapped
        ++ button
        |> wrapVerticalSpacing AlignCenter
        |> \e -> [hsx|<div class={classes'}>{e}</div>|]
        |> wrapContainerNarrow
    where
        classes' = classes
            [ (getBackgroundColor Gray100, True)
            , "relative rounded-lg border border-gray-300 w-full h-full overflow-hidden p-6 md:px-8"
            ]

        titleWrapped = cs title
            |> wrapHeaderTag 1

        bodyWrapped = body
            |> preEscapedToHtml
            |> wrapProse

        button = buildButtonPrimary (link.url) (link.text)




