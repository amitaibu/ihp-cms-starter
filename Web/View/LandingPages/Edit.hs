module Web.View.LandingPages.Edit where
import Web.Controller.Prelude
import Web.Element.Button
import Web.Element.ElementWrap
import Web.Element.Link
import Web.Element.SubmitButton
import Web.Element.Types
import Web.View.Prelude

data EditView = EditView
    { landingPageWithRecords :: LandingPageWithRecords
    , formStatus :: FormStatus
    }


instance View EditView where
    html EditView { .. } =
        [ header
        , renderForm landingPage paragraphCtas paragraphQuotes formStatus
        ]
        |> mconcat
        |> wrapVerticalSpacing AlignNone
        |> wrapContainerWide
        where
            landingPage = landingPageWithRecords.landingPage
            paragraphCtas = landingPageWithRecords.paragraphCtas
            paragraphQuotes = landingPageWithRecords.paragraphQuotes

            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Landing Pages" LandingPagesAction
                , breadcrumbText "Edit Landing Page"
                ]

            header =
                [ breadcrumb
                , titleAndEdit
                ]
                |> mconcat
                |> wrapVerticalSpacing AlignNone


            titleAndEdit =
                [ [hsx|Edit {landingPage.title}|] |> wrapHeaderTag 1
                , renderLinkAction (ShowLandingPageAction landingPage.id) "Show"
                ]
                |> mconcat
                |> wrapHorizontalSpacingTiny AlignBaseline

renderForm :: LandingPage -> [ParagraphCta] -> [ParagraphQuote] -> FormStatus -> Html
renderForm landingPage paragraphCtas paragraphQuotes formStatus = formFor landingPage body
    where
        body :: (?formContext :: FormContext LandingPage) => Html
        body = [hsx|
            {(textField #title)}

            <div class="border p-4">
                {paragraphs}
            </div>

            { renderSubmitButtonwithFormStatus
                (submitButton {label = "Save Landing page"})
                formStatus
            }
        |]
            |> wrapVerticalSpacing AlignNone

        paragraphs =
            [ addParagraphs
            , orderAndRenderParagraphs paragraphCtas paragraphQuotes |> wrapListOl
            , renderLinkAction (ShowOrderLandingPageParagraphsAction landingPage.id) "Re-Order"
            ]
                |> mconcat
                |> wrapVerticalSpacing AlignNone




        addParagraphs =
            [   cs ("Paragraphs" :: Text) |> wrapHeaderTag 3
            ,   paragraphButtons
            ]
                |> mconcat
                |> wrapVerticalSpacing AlignNone

        paragraphButtons =
            [   renderButtonAction (NewParagraphCtaAction landingPage.id) "New CTA"
            ,   renderButtonAction (NewParagraphQuoteAction landingPage.id) "New Quote"
            ]
                |> mconcat
                |> wrapHorizontalSpacing AlignCenter

orderAndRenderParagraphs :: [ParagraphCta] -> [ParagraphQuote] -> Html
orderAndRenderParagraphs paragraphCtas paragraphQuotes =
    ctas
        ++ quotes
        |> sortOn fst
        |> fmap snd
        |> mconcat

    where
        ctas = paragraphCtas
            |> fmap (\paragraph -> (paragraph.weight, paragraphTitleAndOps EditParagraphCtaAction DeleteParagraphCtaAction paragraph.id paragraph.title ("CTA" :: Text)))

        quotes = paragraphQuotes
            |> fmap (\paragraph -> (paragraph.weight, paragraphTitleAndOps EditParagraphQuoteAction DeleteParagraphQuoteAction paragraph.id paragraph.subtitle ("Quote" :: Text)))



        -- Show the paragraph title and the operations to perform on it.
        paragraphTitleAndOps :: (Show (PrimaryKey record), HasPath controller) => (Id' record -> controller) -> (Id' record -> controller) -> Id' record -> Text -> Text -> Html
        paragraphTitleAndOps editAction deleteAction id title type_  =
            [hsx|
                <li class="">{body}</li>
            |]
            where
                body =
                    [hsx|
                        {title} <span class="text-sm text-gray-600">({type_})</span>
                        {operations}
                    |]
                        |> wrapHorizontalSpacingTiny AlignCenter

                operations =
                    [   renderLinkAction (editAction id) "Edit"
                    ,   renderLinkDeleteAction (deleteAction id)
                    ]
                        |> mconcat
                        |> wrapHorizontalSpacingTiny AlignNone




