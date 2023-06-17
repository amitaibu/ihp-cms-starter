module Web.Element.InnerElementLayout where

import Web.View.Prelude

import Web.Element.Types


getInnerElementBaseClasses :: Text
getInnerElementBaseClasses = "relative rounded-lg border border-gray-300 w-full h-full overflow-hidden"

buildInnerElementLayout :: Color -> Html -> Html
buildInnerElementLayout color element =
    [hsx|
        <div class={classes'}>
            {element}
        </div>
    |]
    where
        classes' = classes
            [ (getInnerElementBaseClasses, True)
            , (getBackgroundColor color, True)
            , "p-6 md:px-8"
            ]


buildElementLayoutSplitImageAndContent :: Text -> Html -> Html
buildElementLayoutSplitImageAndContent imageUrl items =
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
