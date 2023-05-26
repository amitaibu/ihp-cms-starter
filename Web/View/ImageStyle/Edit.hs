module Web.View.ImageStyle.Edit where
import Web.View.Prelude

data EditView = EditView { imageStyle :: ImageStyle }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit ImageStyle</h1>
        {renderForm imageStyle}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ImageStyles" ImageStylesAction
                , breadcrumbText "Edit ImageStyle"
                ]

renderForm :: ImageStyle -> Html
renderForm imageStyle = formFor imageStyle [hsx|
    
    {submitButton}

|]