module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

-- Custom types

{-| The result of fetching Landing page with all its paragraphs.

@see `fetchLandingPageWithRecords`
-}
data LandingPageWithRecords = LandingPageWithRecords
    { landingPageWithRecordsLandingPage :: !LandingPage
    , landingPageWithRecordsParagraphCtas :: ![ParagraphCta]
    , landingPageWithRecordsParagraphQuotes :: ![ParagraphQuote]
    , landingPageWithRecordsParagraphCtaRefLandingPages :: ![LandingPage]
    } deriving (Show)

--

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data ArticlesController
    = ArticlesAction
    | NewArticleAction
    | ShowArticleAction { articleId :: !(Id Article) }
    | CreateArticleAction
    | EditArticleAction { articleId :: !(Id Article) }
    | UpdateArticleAction { articleId :: !(Id Article) }
    | DeleteArticleAction { articleId :: !(Id Article) }
    deriving (Eq, Show, Data)

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


