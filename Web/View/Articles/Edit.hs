module Web.View.Articles.Edit where
import Web.View.Prelude

data EditView = EditView { article :: Article }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Article</h1>
        {renderForm article}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Articles" ArticlesAction
                , breadcrumbText "Edit Article"
                ]

renderForm :: Article -> Html
renderForm article = formFor article [hsx|
    {(textField #title)}

    <input
        type="file"
        name="imageUrl"
        class="form-control-file"
        accept="image/*"
        data-preview="#imageUrlPreview"
    />

    <img id="imageUrlPreview" src={article.imageUrl}/>

    {submitButton}

|]