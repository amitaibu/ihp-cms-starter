module Web.View.ParagraphFeaturedArticles.Index where
import Web.View.Prelude

data IndexView = IndexView { paragraphFeaturedArticles :: [ParagraphFeaturedArticle]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewParagraphFeaturedArticleAction} class="btn btn-primary ms-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>ParagraphFeaturedArticle</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach paragraphFeaturedArticles renderParagraphFeaturedArticle}</tbody>
            </table>
            
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ParagraphFeaturedArticles" ParagraphFeaturedArticlesAction
                ]

renderParagraphFeaturedArticle :: ParagraphFeaturedArticle -> Html
renderParagraphFeaturedArticle paragraphFeaturedArticle = [hsx|
    <tr>
        <td>{paragraphFeaturedArticle}</td>
        <td><a href={ShowParagraphFeaturedArticleAction paragraphFeaturedArticle.id}>Show</a></td>
        <td><a href={EditParagraphFeaturedArticleAction paragraphFeaturedArticle.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteParagraphFeaturedArticleAction paragraphFeaturedArticle.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]