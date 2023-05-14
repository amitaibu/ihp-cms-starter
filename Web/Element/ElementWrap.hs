module Web.Element.ElementWrap where

import Web.View.Prelude
import Text.Blaze.Internal

-- Spacing

wrapContainerVerticalSpacing :: Html -> Html
wrapContainerVerticalSpacing element =
    case element of
        Empty _ -> mempty
        _ -> [hsx|<div class="flex flex-col gap-y-5 md:gap-y-6">{element}</div>|]



wrapContainerVerticalSpacingTiny :: Html -> Html
wrapContainerVerticalSpacingTiny element =
    case element of
        Empty _ -> mempty
        _ -> [hsx|<div class="flex flex-col gap-y-2">{element}</div>|]


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


