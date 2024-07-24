module Web.View.News.Show where
import Web.View.Prelude

data ShowView = ShowView { news :: News }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show News</h1>
        <p>{news}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "News" NewsAction
                            , breadcrumbText "Show News"
                            ]