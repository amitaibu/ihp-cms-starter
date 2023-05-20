module Web.View.ParagraphQuotes.Form where

import Web.View.Prelude
import Web.Element.Types
import Web.Element.ElementWrap

-- @todo: Add `required={isImageRequired}` to form.
renderForm :: ParagraphQuote -> Bool -> Html
renderForm paragraphQuote isImageRequired = formFor paragraphQuote [hsx|
    {(hiddenField #landingPageId)}
    {(hiddenField #weight)}
    {visibleForm paragraphQuote}
    |]
    where
            visibleForm paragraphQuote =
                [hsx|
                    {(textareaField #body) {required = True}}
                    {(textField #subtitle) {required = True}}

                    <input
                        type="file"
                        name="imageUrl"
                        class="form-control-file"
                        accept="image/*"
                        data-preview="#imageUrlPreview"
                    />

                    <img id="imageUrlPreview"/>

                    {submitButton}
                |]
                |> wrapContainerVerticalSpacing AlignNone
                |> wrapContainerWide