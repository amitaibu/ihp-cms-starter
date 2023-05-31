module Web.View.ParagraphQuotes.Show where
import Web.View.Prelude
import Web.Element.Types
import Web.Element.ElementWrap
import Web.Element.InnerElementLayout
import Crypto.PubKey.RSA.PKCS15 as RSA
import Crypto.Hash.Algorithms as Hash.Algorithms
import qualified Config
import IHP.ControllerSupport

data ShowView = ShowView { paragraphQuote :: ParagraphQuote }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show ParagraphQuote</h1>
        <p>{paragraphQuote}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "ParagraphQuotes" ParagraphQuotesAction
                            , breadcrumbText "Show ParagraphQuote"
                            ]

renderParagraph :: Text -> Text -> Text -> Html
renderParagraph body subtitle imageUrl =
    quotationSign
        ++ bodyWrapped
        ++ titleWrapped
        |> wrapVerticalSpacing AlignNone
        |> buildElementLayoutSplitImageAndContent (pathTo $ RenderImageStyleAction 400 200 imageUrl (cs signed))
    where

        -- Sign the image URL to prevent tampering.
        Config.PublicAndPrivateKeys (_, privateKey) = getAppConfig @Config.PublicAndPrivateKeys
        signed = case RSA.sign Nothing (Just Hash.Algorithms.SHA256) privateKey (cs $ imageUrl <> "400x200") of
            Left _ -> error $ "Cannot sign image URL. Private key is invalid." <> show privateKey
            Right signature -> signature

        -- https://iconmonstr.com/quote-3-svg/
        quotationSign = [hsx|
            <svg xmlns="http://www.w3.org/2000/svg" width="48" height="48" viewBox="0 0 24 24" class="fill-gray-300">
                <path d="M9.983 3v7.391c0 5.704-3.731 9.57-8.983 10.609l-.995-2.151c2.432-.917 3.995-3.638 3.995-5.849h-4v-10h9.983zm14.017 0v7.391c0 5.704-3.748 9.571-9 10.609l-.996-2.151c2.433-.917 3.996-3.638 3.996-5.849h-3.983v-10h9.983z"/>
            </svg>

        |]
        bodyWrapped = body
            |> preEscapedToHtml
            |> wrapTextResponsiveFontSize TextSize2xl

        titleWrapped = cs subtitle
            |> wrapTextResponsiveFontSize TextSizeSm

