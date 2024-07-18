module Web.View.ParagraphHeroImages.Edit where
import Web.View.Prelude
import Web.View.ParagraphHeroImages.Form

data EditView = EditView
    { paragraphHeroImage :: ParagraphHeroImage
    , formStatus :: FormStatus
    }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Paragraph Hero Image</h1>
        {renderForm paragraphHeroImage True formStatus}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbText "Edit Hero Image"
                ]
