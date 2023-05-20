module Web.View.ParagraphQuotes.New where
import Web.View.Prelude
import Web.Element.Types
import Web.Element.ElementWrap

data NewView = NewView { paragraphQuote :: ParagraphQuote }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New ParagraphQuote</h1>
        {renderForm paragraphQuote}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ParagraphQuotes" ParagraphQuotesAction
                , breadcrumbText "New ParagraphQuote"
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