module Web.View.ImageStyle.New where
import Web.View.Prelude

data NewView = NewView { imageStyle :: ImageStyle }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New ImageStyle</h1>
        {renderForm imageStyle}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ImageStyles" ImageStylesAction
                , breadcrumbText "New ImageStyle"
                ]

renderForm :: ImageStyle -> Html
renderForm imageStyle = formFor imageStyle [hsx|
    
    {submitButton}

|]