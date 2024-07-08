module Web.Element.Types where
import Web.View.Prelude

data Align = AlignNone | AlignStart | AlignCenter | AlignEnd | AlignBaseline deriving (Eq, Show)

data Justify = JustifyStart | JustifyCenter | JustifyEnd | JustifyBetween deriving (Eq, Show)

data FontWeight = FontWeightLight | FontWeightNormal | FontWeightBold deriving (Eq, Show)

data Color
    = ColorTransparent
    | BlueGray100
    | Blue100
    | Gray300
    | Gray500
    | Gray900
    | Green600
    | Orange100
    | Red600
    | TppBlue
    | TppPurple
    | White
    deriving (Eq, Show)

data TextSize = TextSizeXs | TextSizeSm | TextSizeBase | TextSizeLg | TextSizeXl | TextSize2xl | TextSize3xl deriving (Eq, Show)

data LinkType = LinkTypeNoDelete | LinkTypeDeleteWithConfirmation | LinkTypeDeleteWithoutConfirmation deriving (Eq, Show)

data RoundedCorner = RoundedCornerNone | RoundedCornerLarge | RoundedCornerFull deriving (Eq, Show)

-- | The padding around the background color.
data BackgroundColorPadding
    = BackgroundColorPaddingCard -- Used for example for comments.
    | BackgroundColorPaddingSection -- Used for example for the footer.
    deriving (Eq, Show)

data Width = WidthLg | WidthXl | Width2xl | Width3xl deriving (Eq, Show)