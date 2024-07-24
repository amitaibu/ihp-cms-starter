module Web.View.News.Edit where
import Web.View.Prelude

data EditView = EditView { news :: News }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit News</h1>
        {renderForm news}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "News" NewsAction
                , breadcrumbText "Edit News"
                ]

renderForm :: News -> Html
renderForm news = formFor news [hsx|
    {(textField #title)}
    {(textareaField #body)}
    {submitButton}

|]