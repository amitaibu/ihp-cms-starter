module Application.Helper.Elasticsearch where

import IHP.Prelude
import IHP.ModelSupport
import Database.Bloodhound
import Network.HTTP.Client
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson
import IHP.ControllerPrelude

import Generated.Types

-- Make News an instance of ToJSON to allow serialization
instance ToJSON News where
    toJSON news = object
        [ "id" .= (show news.id)
        , "title" .= news.title
        , "body" .= news.body
        -- Add other fields as necessary
        ]

-- Initialize Elasticsearch connection
initES :: (?context :: ControllerContext) => IO BHEnv
initES = do
    let server = ?context.frameworkConfig.esServer
    manager <- newManager defaultManagerSettings
    return $ mkBHEnv server manager

-- Index a news item in Elasticsearch
indexNews :: (?modelContext :: ModelContext, ?context :: ControllerContext) => News -> IO (Either BHError Value)
indexNews news = do
    bhenv <- initES
    let indexName = IndexName "news_index"
        docId = DocId $ T.pack $ "news_" <> show news.id
    runBH bhenv $ indexDocument indexName (MappingName "document") (toJSON news) docId

-- Search for news
searchNews :: (?modelContext :: ModelContext, ?context :: ControllerContext) => Text -> IO [SearchResult Value]
searchNews query = do
    bhenv <- initES
    let indexName = IndexName "news_index"
        searchQuery = MultiMatchQuery ["title", "body"] (TE.encodeUtf8 query)
        search = mkSearch (Just searchQuery) Nothing
    result <- runBH bhenv $ searchByIndex indexName search
    case result of
        Left err -> do
            putStrLn $ "Error: " ++ show err
            return []
        Right searchResult -> return $ hits $ searchHits searchResult

-- Helper function to use in your controllers
searchNewsHandler :: (?modelContext :: ModelContext, ?context :: ControllerContext) => Text -> IO [News]
searchNewsHandler query = do
    results <- searchNews query
    return $ mapMaybe (decode . encode . sourceAsJSON) results