module Web.View.ParagraphFeaturedArticles.New where
import Web.View.Prelude

data NewView = NewView { paragraphFeaturedArticle :: ParagraphFeaturedArticle }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New ParagraphFeaturedArticle</h1>
        {renderForm paragraphFeaturedArticle}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ParagraphFeaturedArticles" ParagraphFeaturedArticlesAction
                , breadcrumbText "New ParagraphFeaturedArticle"
                ]

renderForm :: ParagraphFeaturedArticle -> Html
renderForm paragraphFeaturedArticle = formFor paragraphFeaturedArticle [hsx|
    {(textField #landingPageId)}
    {(textField #weight)}
    {submitButton}

|]