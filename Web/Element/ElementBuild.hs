module Web.Element.ElementBuild where

import Web.Controller.Prelude
import Web.View.Prelude

-- Button

buildButton :: Text -> Text ->  Html
buildButton url text  =
    buildButtonHelper url text False


buildButtonPrimary :: Text -> Text ->  Html
buildButtonPrimary url text  =
    buildButtonHelper url text True


buildButtonHelper :: Text -> Text ->  Bool -> Html
buildButtonHelper url text isPrimary  =
    [hsx|
        <div class="inline-block button-wrapper">
            <a class={classes'} href={url}>
                <div>{text}</div>
            </a>
        </div>

    |]
    where classes' = classes
            [ "flex flex-row items-center gap-x-4 rounded-lg px-6 py-3 text-xl border-2 border-blue-500 hover:bg-blue-600 w-fit"
            , ("bg-blue-500 text-white", isPrimary)
            , ("text-blue-900 bg-white hover:text-white", not isPrimary)
            ]



-- Link

{-| Build a link, by getting the action and text.
-}
buildLink :: (HasPath controller) => controller -> Text ->  Html
buildLink action text  =
    buildLinkHelper action text False


{-| Build a link for a delete action, by getting the action and text.
-}
buildLinkDeleteAction :: (HasPath controller) => controller ->  Html
buildLinkDeleteAction action  =
    buildLinkHelper action "Delete" True



{-| Helper function to build a link.
-}
buildLinkHelper :: (HasPath controller) => controller -> Text -> Bool ->  Html
buildLinkHelper action text isDeleteAction =
    [hsx|
        <a href={pathTo action} class={classes'}>{text}</a>
    |]
    where
        classes' = classes
            [ "text-blue-500 text-sm hover:underline"
            , ("js-delete ", isDeleteAction)
            ]
