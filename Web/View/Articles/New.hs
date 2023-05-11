module Web.View.Articles.New where
import Web.View.Prelude

data NewView = NewView { article :: Article }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Article</h1>
        {renderForm article}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Articles" ArticlesAction
                , breadcrumbText "New Article"
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

    <img id="imageUrlPreview"/>


    {submitButton}

|]