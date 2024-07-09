module Web.Element.Quote where

import Web.View.Prelude

import Web.Element.ElementWrap
import Web.Element.Types


render :: RenderQuote -> Html
render record@RenderCta {body, subtitle, imageUrl} =
     quotationSign
        ++ bodyWrapped
        ++ titleWrapped
        |> wrapVerticalSpacing AlignNone
        |> buildElementLayoutSplitImageAndContent (pathTo $ RenderImageStyleAction 400 200 imageUrl signed)
    where
        -- Sign the image URL to prevent tampering.
        signed = signImageUrl imageUrl 400 200

        bodyWrapped = body
            |> preEscapedToHtml
            |> wrapTextResponsiveFontSize TextSize2xl

        titleWrapped = cs subtitle
            |> wrapTextResponsiveFontSize TextSizeSm




