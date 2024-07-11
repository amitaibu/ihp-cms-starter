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
        , elementsAccordion

        -- Include the JavaScript for the accordion.
        , jsScript
        ]
        |> mconcat
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Style Guide" StyleGuideAction
                ]

            elementsAccordion = [hsx|
                <dl class="accordion">
                    { elements }
                </dl>
            |]

            jsScript = [hsx|
                <script src={assetPath "/styleGuide.js"}></script>
            |]

            -- All the elements to be displayed in the Style Guide.
            elements =
                [ renderTitleAndElementNoContainer "CTA" cta
                , renderTitleAndElementWideContainer "Quote" quote
                ]
                |> mconcat

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
                , imageUrl = "/paragraph_quotes/imageUrl/84fa7d47-002d-4fe5-a86a-df51cc4ec10f"
                }
                |> Web.Element.Quote.render

renderTitleAndElementNoContainer :: Text -> Html -> Html
renderTitleAndElementNoContainer title element =
    renderTitleAndElementHelper title Nothing element Nothing

renderTitleAndElementWideContainer :: Text -> Html -> Html
renderTitleAndElementWideContainer title element =
    renderTitleAndElementHelper title Nothing element (Just "container-wide")

renderTitleAndElementHelper :: Text -> Maybe Text -> Html -> Maybe Text -> Html
renderTitleAndElementHelper title maybeUrl element maybeContainerClass  = [hsx|
        { header }
        <dd class={"mb-8 " ++ maybeContainerClass}>
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