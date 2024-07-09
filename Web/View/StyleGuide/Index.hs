module Web.View.StyleGuide.Index where
import Web.View.Prelude
import Web.Element.Types

import Web.Element.Cta
import Web.Element.Quote

data IndexView = IndexView { }

instance View IndexView where
    html IndexView { .. } =
        [ breadcrumb
        , cta
        , quote
        ]
        |> mconcat
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Style Guide" StyleGuideAction
                ]

            cta = RenderCta
                { title = "Hello, World!"
                , body = "This is a test of the __emergency__ broadcast system."
                , link = RenderLink
                    { text = "Click me!"
                    , url = "/"
                    }
                }
                |> Web.Element.Cta.render

            quote = RenderQuote
                { body = "The quick brown fox jumps over the lazy dog."
                , subtitle = "An old proverb."
                , imageUrl = "https://example.com/image.jpg"
                }
                |> Web.Element.Quote.render



