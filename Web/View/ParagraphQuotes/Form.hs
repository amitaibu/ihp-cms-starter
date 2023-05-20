module Web.View.ParagraphQuotes.Form where

import Web.View.Prelude
import Web.Element.Types
import Web.Element.ElementWrap

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
                        required={isImageRequired}
                    />

                    <img id="imageUrlPreview" src={paragraphQuote.imageUrl} class={imageClasses} />

                    {submitButton}
                |]
                |> wrapContainerVerticalSpacing AlignNone
                |> wrapContainerWide

            imageClasses = classes [("w-20 h-20", isJust paragraphQuote.imageUrl)]

