module Web.Element.Cta where

import Web.View.Prelude

import Web.Element.ElementWrap
import Web.Element.Types


render :: RenderCta -> Html
render record@RenderCta {title, body} =
    titleWrapped
        ++ bodyWrapped
        -- ++ button
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
            |> renderMarkdown
            |> wrapProse



