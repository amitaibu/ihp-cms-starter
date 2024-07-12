module Web.Element.Link where

import Web.Controller.Prelude
import Web.Element.Types
import Web.View.Prelude

-- RenderLink

render :: RenderLink ->  Html
render record@RenderLink {text, url}  =
    [hsx|
        <a href={url} class={classes'}>{text}</a>
    |]
    where
        classes' = classes
            [ "text-blue-500 text-sm hover:underline"
            ]


-- Link

{-| Build a link, by getting the action and text.
-}
renderLinkAction :: (HasPath controller) => controller -> Text ->  Html
renderLinkAction action text  =
    renderLinkHelper action text False


{-| Build a link for a delete action, by getting the action and text.
-}
renderLinkDeleteAction :: (HasPath controller) => controller ->  Html
renderLinkDeleteAction action  =
    renderLinkHelper action "Delete" True


{-| Helper function to build a link.
-}
renderLinkHelper :: (HasPath controller) => controller -> Text -> Bool ->  Html
renderLinkHelper action text isDeleteAction =
    [hsx|
        <a href={pathTo action} class={classes'}>{text}</a>
    |]
    where
        classes' = classes
            [ "text-blue-500 text-sm hover:underline"
            , ("js-delete ", isDeleteAction)
            ]
