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
