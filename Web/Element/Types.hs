module Web.Element.Types where
import Web.View.Prelude

data Color = ColorNone | Gray100 | Gray500 deriving (Eq, Show)

data Align = AlignNone | AlignStart | AlignCenter | AlignEnd deriving (Eq, Show)


-- @todo: Move to Utility.hs?
getBackgroundColor :: Color -> Text
getBackgroundColor color =
    case color of
        ColorNone -> "bg-transparent"
        Gray100 -> "bg-gray-100"
        Gray500 -> "bg-gray-500"