module Web.View.LandingPages.Index where
import Web.View.Prelude

data IndexView = IndexView { landingPages :: [LandingPage]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewLandingPageAction} class="btn btn-primary ms-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>LandingPage</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach landingPages renderLandingPage}</tbody>
            </table>
            
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "LandingPages" LandingPagesAction
                ]

renderLandingPage :: LandingPage -> Html
renderLandingPage landingPage = [hsx|
    <tr>
        <td>{landingPage}</td>
        <td><a href={ShowLandingPageAction landingPage.id}>Show</a></td>
        <td><a href={EditLandingPageAction landingPage.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteLandingPageAction landingPage.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]