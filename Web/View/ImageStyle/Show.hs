module Web.View.ImageStyle.Show where
import Web.View.Prelude

data ShowView = ShowView { imageStyle :: ImageStyle }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show ImageStyle</h1>
        <p>{imageStyle}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "ImageStyles" ImageStylesAction
                            , breadcrumbText "Show ImageStyle"
                            ]