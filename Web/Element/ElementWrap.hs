module Web.Element.ElementWrap where

import Web.View.Prelude
import Text.Blaze.Internal
import Web.Element.Types



-- Containers


wrapBackgroundColor :: Color -> Html -> Html
wrapBackgroundColor color element =
        case element of
        Empty _ -> mempty
        _ -> [hsx|<div class={getBackgroundColor color}>{element}</div>|]


{- Wrap an element with a wide container -}
wrapContainerWide :: Html -> Html
wrapContainerWide element =
    case element of
        Empty _ -> mempty
        _ -> [hsx|<div class="container-wide w-full">{element}</div>|]


{- Wrap an element with a narrow container -}
wrapContainerNarrow :: Html -> Html
wrapContainerNarrow element =
    case element of
        Empty _ -> mempty
        _ -> [hsx|<div class="container-narrow w-full">{element}</div>|]


-- Spacing

wrapContainerVerticalSpacing :: Align -> Html -> Html
wrapContainerVerticalSpacing align element =
    case element of
        Empty _ -> mempty
        _ -> [hsx|<div class={classes'}>{element}</div>|]
    where
        classes' =
            classes
            [ "flex flex-col gap-y-5 md:gap-y-6"
            , ("items-start", align == AlignStart)
            , ("items-center", align == AlignCenter)
            , ("items-end", align == AlignEnd)
            ]



wrapContainerVerticalSpacingTiny :: Align -> Html -> Html
wrapContainerVerticalSpacingTiny align element =
    case element of
        Empty _ -> mempty
        _ -> [hsx|<div class={classes'}>{element}</div>|]
    where
        classes' =
            classes
            [ "flex flex-col gap-y-2"
            , ("items-start", align == AlignStart)
            , ("items-center", align == AlignCenter)
            , ("items-end", align == AlignEnd)
            ]


-- Text

wrapProse :: Html -> Html
wrapProse element =
    [hsx|
        <div class="prose md:prose-lg lg:prose-xl prose-headings:text-gray-700 max-w-3xl">
            {element}
        </div>
    |]

wrapTextResponsiveFontSize :: Text -> Html -> Html
wrapTextResponsiveFontSize size element =
    [hsx|
        <div class={sizeClasses}>{element}</div>
    |]
    where
        sizeClasses :: Text =
            case size of
                "xs" -> "text-xs"
                "sm" -> "text-xs md:text-sm"
                "base" -> "text-sm md:text-base"
                "lg" -> "md:text-lg"
                "xl" -> "text-lg md:text-xl"
                "2xl" -> "text-xl md:text-2xl"
                "3xl" -> "text-xl md:text-2xl lg:text-3xl"
                _ -> "text-base"

wrapHeaderTag :: Int -> Html -> Html
wrapHeaderTag number element =
    wrapProse html
    where
        html = case number of
            1 -> [hsx|<h1>{element}</h1>|]
            2 -> [hsx|<h2>{element}</h2>|]
            3 -> [hsx|<h3>{element}</h3>|]
            4 -> [hsx|<h4>{element}</h4>|]
            5 -> [hsx|<h5>{element}</h5>|]
            6 -> [hsx|<h6>{element}</h6>|]
            _ -> mempty



