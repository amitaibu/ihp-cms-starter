module Web.View.StyleGuide.Index where
import Web.View.Prelude
import Web.Element.Types
import Web.Element.Cta


data IndexView = IndexView { }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}
        {cta}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Style Guide" StyleGuideAction
                ]

            cta = RenderCta
                { title = "Hello, World!"
                , body = [hsx|This is a test of <strong>the</strong> emergency broadcast system.|]
                , link = RenderLink
                    { text = "Click me!"
                    , url = "/"
                    }
                }
                |> Web.Element.Cta.render


