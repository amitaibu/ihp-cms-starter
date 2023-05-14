module Web.Element.ElementBuild where

import Web.Controller.Prelude
import Web.View.Prelude

{-| Build a link, by getting the action and text.
-}
buildLink :: (HasPath controller) => controller -> Text -> Bool ->  Html
buildLink action text isDeleteAction =
    [hsx|
        <a href={pathTo action} class={classes'}>{text}</a>
    |]
    where
        classes' = classes
            [ "text-blue-500 text-sm hover:underline"
            , ("js-delete ", isDeleteAction)
            ]