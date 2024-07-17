module Web.View.ParagraphHeroImages.New where
import Web.View.Prelude
import Web.View.ParagraphHeroImages.Form


data NewView = NewView
    { paragraphHeroImage :: ParagraphHeroImage
    , formStatus :: FormStatus
    }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Paragraph Hero Image</h1>
        {renderForm paragraphHeroImage True formStatus}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbText "New Hero Image"
                ]

