module Web.Element.PageTitle where

import Web.View.Prelude

import Application.Helper.Icons
import Web.Element.ElementWrap
import Web.Element.Types


render :: Text -> Html
render text =
    text
        |> cs
        |> wrapHeaderTag 1
        |> wrapProse


