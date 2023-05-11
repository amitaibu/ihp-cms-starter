module Web.View.ParagraphFeaturedArticles.Edit where
import Web.View.Prelude

data EditView = EditView { paragraphFeaturedArticle :: ParagraphFeaturedArticle }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit ParagraphFeaturedArticle</h1>
        {renderForm paragraphFeaturedArticle}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ParagraphFeaturedArticles" ParagraphFeaturedArticlesAction
                , breadcrumbText "Edit ParagraphFeaturedArticle"
                ]

renderForm :: ParagraphFeaturedArticle -> Html
renderForm paragraphFeaturedArticle = formFor paragraphFeaturedArticle [hsx|
    {(textField #landingPageId)}
    {(textField #weight)}
    {submitButton}

|]