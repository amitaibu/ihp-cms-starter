module Web.View.LandingPages.Index where
import Web.View.Prelude
import Web.Element.Types
import Web.Element.ElementBuild
import Web.Element.ElementWrap

data IndexView = IndexView { landingPages :: [LandingPage]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        {header}
        <ol class="list-decimal flex flex-col gap-2">
            {forEach landingPages renderLandingPage}
        </ol>

    |]
        |> wrapVerticalSpacing AlignNone
        |> wrapContainerWide
        where
            header =
                [ [hsx|Landing pages Index|] |> wrapHeaderTag 1
                , buildButton (pathTo NewLandingPageAction) "New Landing page"
                ]
                    |> mconcat
                    |> wrapHorizontalSpacing AlignNone

            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Landing Pages" LandingPagesAction
                ]

renderLandingPage :: LandingPage -> Html
renderLandingPage landingPage = [hsx|
    <li class="flex flex-row gap-2">
        {landingPage.title}
        {operations}
    </li>
|]
    where
        operations =
            [ buildLink (ShowLandingPageAction landingPage.id) "Show"
            , buildLink (EditLandingPageAction landingPage.id) "Edit"
            , buildLinkDeleteAction (DeleteLandingPageAction landingPage.id)
            ]
                |> mconcat
                |> wrapHorizontalSpacingTiny AlignCenter