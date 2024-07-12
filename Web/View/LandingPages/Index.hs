module Web.View.LandingPages.Index where
import Web.Element.Button
import Web.Element.ElementWrap
import Web.Element.Link
import Web.Element.Types
import Web.View.Prelude

data IndexView = IndexView { landingPages :: [LandingPage]  }

instance View IndexView where
    html IndexView { .. } =
        [ breadcrumb
        , header
        , body
        ]
        |> mconcat
        |> wrapVerticalSpacing AlignNone
        |> wrapContainerWide
        where
            header =
                [ [hsx|"Landing pages Index"|] |> wrapHeaderTag 1
                , renderButtonAction (NewLandingPageAction) "New Landing page"
                ]
                |> mconcat
                |> wrapHorizontalSpacing AlignNone

            body = forEach landingPages renderLandingPage |> wrapListOl

            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Landing Pages" LandingPagesAction
                ]

renderLandingPage :: LandingPage -> Html
renderLandingPage landingPage =
        [ cs landingPage.title
        , operations
        ]
        |> mconcat
        |> wrapHorizontalSpacingTiny AlignBaseline
    where
        operations =
            [ renderLinkAction (ShowLandingPageAction landingPage.id) "Show"
            , renderLinkAction (EditLandingPageAction landingPage.id) "Edit"
            , renderLinkDeleteAction (DeleteLandingPageAction landingPage.id)
            ]
            |> mconcat
            |> wrapHorizontalSpacingTiny AlignCenter
