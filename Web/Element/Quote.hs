module Web.Element.Quote where
import Web.View.Prelude

import Application.Helper.Icons
import Web.Element.ElementWrap
import Web.Element.Types


render :: RenderQuote -> Html
render record@RenderQuote {body, subtitle, imageUrl} =
     quotationSign
        ++ bodyWrapped
        ++ titleWrapped
        |> wrapVerticalSpacing AlignNone
        |> renderImageAndContent (pathTo $ RenderImageStyleAction 400 200 imageUrl signed)
    where
        -- Sign the image URL to prevent tampering.
        signed = signImageUrl imageUrl 400 200

        bodyWrapped = body
            |> preEscapedToHtml
            |> wrapTextResponsiveFontSize TextSize2xl

        titleWrapped = cs subtitle
            |> wrapTextResponsiveFontSize TextSizeSm


renderImageAndContent :: Text -> Html -> Html
renderImageAndContent imageUrl items =
    -- We use grid and row/col start to position both the image and the text on the same cell.
    [hsx|
        <div class="flex flex-col sm:grid sm:grid-rows-1 md:grid-cols-2 gap-2 md:gap-8 lg:gap-10 overflow-hidden bg-gray-50">

            <div class="w-full grid grid-rows-1">
                <figure class="row-start-1 col-start-1 child-object-cover h-full">
                    {image}
                </figure>
            </div>
            <div class="pt-5 pb-8 px-5 lg:py-8 lg:max-w-lg my-auto">
                {items}
            </div>
        </div>
    |]
    where
        image = [hsx|<img src={imageUrl} class="w-full h-full" />|]


