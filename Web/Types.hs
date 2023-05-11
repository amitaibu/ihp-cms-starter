module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

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
    | NewParagraphQuoteAction
    | ShowParagraphQuoteAction { paragraphQuoteId :: !(Id ParagraphQuote) }
    | CreateParagraphQuoteAction
    | EditParagraphQuoteAction { paragraphQuoteId :: !(Id ParagraphQuote) }
    | UpdateParagraphQuoteAction { paragraphQuoteId :: !(Id ParagraphQuote) }
    | DeleteParagraphQuoteAction { paragraphQuoteId :: !(Id ParagraphQuote) }
    deriving (Eq, Show, Data)

data ParagraphFeaturedArticlesController
    = ParagraphFeaturedArticlesAction
    | NewParagraphFeaturedArticleAction
    | ShowParagraphFeaturedArticleAction { paragraphFeaturedArticleId :: !(Id ParagraphFeaturedArticle) }
    | CreateParagraphFeaturedArticleAction
    | EditParagraphFeaturedArticleAction { paragraphFeaturedArticleId :: !(Id ParagraphFeaturedArticle) }
    | UpdateParagraphFeaturedArticleAction { paragraphFeaturedArticleId :: !(Id ParagraphFeaturedArticle) }
    | DeleteParagraphFeaturedArticleAction { paragraphFeaturedArticleId :: !(Id ParagraphFeaturedArticle) }
    deriving (Eq, Show, Data)

data ParagraphCtasController
    = ParagraphCtasAction
    | NewParagraphCtasAction
    | ShowParagraphCtasAction { paragraphCtaId :: !(Id ParagraphCta) }
    | CreateParagraphCtasAction
    | EditParagraphCtasAction { paragraphCtaId :: !(Id ParagraphCta) }
    | UpdateParagraphCtasAction { paragraphCtaId :: !(Id ParagraphCta) }
    | DeleteParagraphCtasAction { paragraphCtaId :: !(Id ParagraphCta) }
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
