module Web.View.News.Index where
import Web.View.Prelude

data IndexView = IndexView { news :: [News] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewNewsAction} class="btn btn-primary ms-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>News</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach news renderNews}</tbody>
            </table>
            
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "News" NewsAction
                ]

renderNews :: News -> Html
renderNews news = [hsx|
    <tr>
        <td>{news}</td>
        <td><a href={ShowNewsAction news.id}>Show</a></td>
        <td><a href={EditNewsAction news.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteNewsAction news.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]