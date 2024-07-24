module Web.View.News.New where
import Web.View.Prelude

data NewView = NewView { news :: News }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New News</h1>
        {renderForm news}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "News" NewsAction
                , breadcrumbText "New News"
                ]

renderForm :: News -> Html
renderForm news = formFor news [hsx|
    {(textField #title)}
    {(textareaField #body)}
    {submitButton}

|]