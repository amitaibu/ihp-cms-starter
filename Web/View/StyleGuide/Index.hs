module Web.View.StyleGuide.Index where
import Web.View.Prelude
import Web.Element.Types
import Data.Text

import Web.Element.Cta
import Web.Element.Quote

data IndexView = IndexView { }

instance View IndexView where
    html IndexView { .. } =
        [ breadcrumb
        , renderTitleAndElement "CTA" cta
        , renderTitleAndElement "Quote" quote
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


renderTitleAndElement :: Text -> Html -> Html
renderTitleAndElement title element =
    renderTitleAndElementHelper title Nothing element

renderTitleAndElementHelper :: Text -> Maybe Text -> Html -> Html
renderTitleAndElementHelper title maybeUrl element = [hsx|
        { header }
        <dd class="container-wide mb-8">
            { element }
        </dd>
    |]


    where
        uniqueId = title |> toLower |> Data.Text.replace " " "-"

        header = [hsx|
            <dt class="container-wide my-6">
                <a class="title-wrapper text-blue-500 text-2xl hover:underline" href={ "#" ++ uniqueId } id={ uniqueId }>{ title }</a>

                { maybeLink }
            </dt>
        |]

        maybeLink = case maybeUrl of
            Just url -> [hsx|
                <div class="ml-4">
                    <a class="text-sm" href={ url } target="_blank">See design</a>
                </div>
            |]
            Nothing -> ""