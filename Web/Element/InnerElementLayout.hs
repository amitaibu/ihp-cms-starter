module Web.Element.InnerElementLayout where

import Web.View.Prelude


getInnerElementBaseClasses :: Text
getInnerElementBaseClasses = "relative rounded-lg border border-gray-300 w-full h-full overflow-hidden"

buildInnerElementLayout :: Html -> Html
buildInnerElementLayout element =
    [hsx|
        <div class="{ getInnerElementBaseClasses } p-6 md:px-8">
            { element }
        </div>

    |]
