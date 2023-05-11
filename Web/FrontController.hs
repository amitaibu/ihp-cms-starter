module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.ParagraphCtas
import Web.Controller.ParagraphFeaturedArticles
import Web.Controller.ParagraphQuotes
import Web.Controller.Articles
import Web.Controller.Static

instance FrontController WebApplication where
    controllers = 
        [ startPage WelcomeAction
        -- Generator Marker
        , parseRoute @ParagraphCtasController
        , parseRoute @ParagraphFeaturedArticlesController
        , parseRoute @ParagraphQuotesController
        , parseRoute @ArticlesController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
