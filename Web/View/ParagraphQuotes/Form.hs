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
            imageClasses = classes [("w-20 h-20", isJust paragraphQuote.imageUrl)]

            visibleForm :: (?formContext :: FormContext ParagraphQuote) => ParagraphQuote -> Html
            visibleForm paragraphQuote =
                [hsx|
                    {(textareaField #body) {required = True}}
                    {(textField #subtitle) {required = True}}

                    {(fileField #imageUrl) {required = True, additionalAttributes = [("accept", "image/*"), ("data-preview", "#imageUrlPreview")]}}

                    <img id="imageUrlPreview" src={paragraphQuote.imageUrl} class={imageClasses} />

                    {submitButton}
                |]
                |> wrapContainerVerticalSpacing AlignNone
                |> wrapContainerWide



