module Web.View.Articles.Show where
import Web.View.Prelude

data ShowView = ShowView { article :: Article }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <div class="flex flex-row gap-2 items-baseline">
            <h1 class="text-3xl">{article.title}</h1>
            <a href={EditArticleAction article.id} class="text-blue-500 text-sm hover:underline hover:text-blue-600">(Edit)</a>
        </div>
        <img src={article.imageUrl} alt="Article Image" />
        <div>{article}</div>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Articles" ArticlesAction
                            , breadcrumbText "Show Article"
                            ]