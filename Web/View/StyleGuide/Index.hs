module Web.View.StyleGuide.Index where
import Web.View.Prelude

data IndexView = IndexView { }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Style Guide" StyleGuideAction
                ]
