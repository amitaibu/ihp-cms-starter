module Web.Element.Button where

import Web.Controller.Prelude
import Web.Element.Types
import Web.View.Prelude

-- Button

renderButton :: Text -> Text ->  Html
renderButton url text  =
    renderButtonHelper url text False


renderButtonPrimary :: Text -> Text ->  Html
renderButtonPrimary url text  =
    renderButtonHelper url text True


renderButtonHelper :: Text -> Text ->  Bool -> Html
renderButtonHelper url text isPrimary  =
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

