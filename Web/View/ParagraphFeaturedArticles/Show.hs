module Web.View.ParagraphFeaturedArticles.Show where
import Web.View.Prelude

data ShowView = ShowView { paragraphFeaturedArticle :: ParagraphFeaturedArticle }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show ParagraphFeaturedArticle</h1>
        <p>{paragraphFeaturedArticle}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "ParagraphFeaturedArticles" ParagraphFeaturedArticlesAction
                            , breadcrumbText "Show ParagraphFeaturedArticle"
                            ]