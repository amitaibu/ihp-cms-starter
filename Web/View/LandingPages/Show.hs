module Web.View.LandingPages.Show where
import Web.View.Prelude
import Text.Blaze.Html

data ShowView = ShowView
    {   landingPage :: LandingPage
    -- ,   allParagraphsRendered :: [Text.Blaze.Html.Html]
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <div class="flex flex-row gap-2 items-baseline">
            <h1 class="text-3xl">{landingPage.title}</h1>
            <a href={EditLandingPageAction landingPage.id} class="text-blue-500 text-sm hover:underline hover:text-blue-600">(Edit)</a>
        </div>
        <!-- {allParagraphsRendered} -->

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "LandingPages" LandingPagesAction
                            , breadcrumbText "Show LandingPage"
                            ]