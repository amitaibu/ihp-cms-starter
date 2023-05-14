module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

-- Generator Marker
instance AutoRoute StaticController
instance AutoRoute ArticlesController


instance AutoRoute ParagraphQuotesController


instance AutoRoute ParagraphFeaturedArticlesController


instance AutoRoute ParagraphCtasController




instance CanRoute LandingPagesController where
    parseRoute' = do
        string "/landing-pages/"
        slug <- remainingText
        pure ShowLandingPageAction { slug = slug }

-- instance HasPath LandingPagesController where
--     pathTo ShowLandingPageAction { slug = slug } = "/landing-pages/" <> slug


instance AutoRoute LandingPagesController