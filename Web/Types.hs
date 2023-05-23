module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

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


{-| The result of fetching a `Post` with all the nested records.

@see `fetchPostWithRecords`
-}
data PostWithRecords = PostWithRecords
    { post :: !Post
    , comments :: ![Comment]
    , commentUsers :: ![User]
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



data PostsController
    = PostsAction
    | NewPostAction
    | ShowPostAction { postId :: !(Id Post) }
    | CreatePostAction
    | EditPostAction { postId :: !(Id Post) }
    | UpdatePostAction { postId :: !(Id Post) }
    | DeletePostAction { postId :: !(Id Post) }
    deriving (Eq, Show, Data)
