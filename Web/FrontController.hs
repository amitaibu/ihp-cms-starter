module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Companies
import Web.Controller.ImageStyle
import Web.Controller.LandingPages
import Web.Controller.ParagraphCtas
import Web.Controller.ParagraphQuotes

instance FrontController WebApplication where
    controllers =
        [ startPage LandingPagesAction
        -- Generator Marker
        , parseRoute @CompaniesController
        , parseRoute @ImageStyleController
        , parseRoute @LandingPagesController
        , parseRoute @ParagraphCtasController
        , parseRoute @ParagraphQuotesController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
