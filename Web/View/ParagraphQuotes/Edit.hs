module Web.View.ParagraphQuotes.Edit where
import Web.View.Prelude
import Web.Element.Types
import Web.Element.ElementWrap

data EditView = EditView { paragraphQuote :: ParagraphQuote }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit ParagraphQuote</h1>
        {renderForm paragraphQuote}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ParagraphQuotes" ParagraphQuotesAction
                , breadcrumbText "Edit ParagraphQuote"
                ]

renderForm :: ParagraphQuote -> Html
renderForm paragraphQuote = formFor paragraphQuote [hsx|
    {(hiddenField #landingPageId)}
    {(hiddenField #weight)}
    {visibleForm paragraphQuote}
    |]
    where
            visibleForm paragraphQuote =
                [hsx|
                    {(textField #subtitle) {required = True}}

                    <input
                        type="file"
                        name="imageUrl"
                        class="form-control-file"
                        accept="image/*"
                        data-preview="#imageUrlPreview"
                        required="required"
                    />

                    <img id="imageUrlPreview"/>

                    {submitButton}
                |]
                |> wrapContainerVerticalSpacing AlignNone
                |> wrapContainerWide