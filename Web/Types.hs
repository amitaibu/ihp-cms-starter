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
