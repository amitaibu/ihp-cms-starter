module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types
import IHP.LoginSupport.Types

-- Custom types

{-| The result of fetching Landing page with all its paragraphs.

@see `fetchLandingPageWithRecords`
-}
data LandingPageWithRecords = LandingPageWithRecords
    { landingPage :: !LandingPage
    , paragraphCtas :: ![ParagraphCta]
    , paragraphQuotes :: ![ParagraphQuote]
    , paragraphCtaRefLandingPages :: ![LandingPage]
    } deriving (Show)

--

data WebApplication = WebApplication deriving (Eq, Show)

data ParagraphQuotesController
    = ParagraphQuotesAction
    | NewParagraphQuoteAction { landingPageId :: !(Id LandingPage) }
    | ShowParagraphQuoteAction { paragraphQuoteId :: !(Id ParagraphQuote) }
    | CreateParagraphQuoteAction
    | EditParagraphQuoteAction { paragraphQuoteId :: !(Id ParagraphQuote) }
    | UpdateParagraphQuoteAction { paragraphQuoteId :: !(Id ParagraphQuote) }
    | DeleteParagraphQuoteAction { paragraphQuoteId :: !(Id ParagraphQuote) }
    deriving (Eq, Show, Data)

data ParagraphCtasController
    = ParagraphCtaAction
    | NewParagraphCtaAction { landingPageId :: !(Id LandingPage) }
    | ShowParagraphCtaAction { paragraphCtaId :: !(Id ParagraphCta) }
    | CreateParagraphCtaAction
    | EditParagraphCtaAction { paragraphCtaId :: !(Id ParagraphCta) }
    | UpdateParagraphCtaAction { paragraphCtaId :: !(Id ParagraphCta) }
    | DeleteParagraphCtaAction { paragraphCtaId :: !(Id ParagraphCta) }
    deriving (Eq, Show, Data)

data LandingPagesController
    = LandingPagesAction
    | NewLandingPageAction
    | ShowLandingPageAction { landingPageId :: !(Id LandingPage) }
    | CreateLandingPageAction
    | EditLandingPageAction { landingPageId :: !(Id LandingPage) }
    | UpdateLandingPageAction { landingPageId :: !(Id LandingPage) }
    | DeleteLandingPageAction { landingPageId :: !(Id LandingPage) }
    deriving (Eq, Show, Data)



data ImageStyleController
    = RenderImageStyleAction { width :: !Int, height :: !Int, originalImagePath :: !Text, signed :: !Text }
    deriving (Eq, Show, Data)

instance HasNewSessionUrl User where
    newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User

data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    deriving (Eq, Show, Data)
data UsersController
    = UsersAction
    | NewUserAction
    | ShowUserAction { userId :: !(Id User) }
    | CreateUserAction
    | EditUserAction { userId :: !(Id User) }
    | UpdateUserAction { userId :: !(Id User) }
    | DeleteUserAction { userId :: !(Id User) }
    deriving (Eq, Show, Data)


newtype LandingPageWrapper = LandingPageWrapper LandingPage

instance CanCreate LandingPageWrapper where
    create :: (?modelContext :: ModelContext) => LandingPageWrapper -> IO LandingPageWrapper
    create (LandingPageWrapper model) = do
        model <- create model
        -- Auto create the first paragraph
        paragraphCta <- newRecord @ParagraphCta
            |> set #landingPageId model.id
            |> set #title "Auto created Title"
            |> set #body "Auto created Body"
            |> set #refLandingPageId model.id
            |> set #weight 0
            |> createRecord

        pure $ LandingPageWrapper model

    createMany [] = pure []
    createMany models = do
        let innerModels = fmap (\(LandingPageWrapper model) -> model) models
        landingPages <- createMany innerModels

        -- Auto create the first paragraph for each model.
        forEach landingPages (\model -> do
            newRecord @ParagraphCta
                |> set #landingPageId model.id
                |> set #title "Auto created Title"
                |> set #body "Auto created Body"
                |> set #refLandingPageId model.id
                |> set #weight 0
                |> createRecord

            pure ()
            )

        pure $ fmap (\model -> LandingPageWrapper model) landingPages


