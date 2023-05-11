module Web.View.Articles.Index where
import Web.View.Prelude

data IndexView = IndexView { articles :: [Article]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewArticleAction} class="btn btn-primary ms-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Article</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach articles renderArticle}</tbody>
            </table>

        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Articles" ArticlesAction
                ]

renderArticle :: Article -> Html
renderArticle article = [hsx|
    <tr>
        <td>
            <div class="flex flex-row gap-2">
                <img src={article.imageUrl} alt="Article Image" class="w-16 h-16" />
                {article.title}
            </div>
        </td>
        <td><a href={ShowArticleAction article.id}>Show</a></td>
        <td><a href={EditArticleAction article.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteArticleAction article.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]