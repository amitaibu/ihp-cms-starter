module Application.Helper.Elasticsearch where

import IHP.Prelude
import IHP.ModelSupport
import Database.Bloodhound
import Network.HTTP.Client (Manager, Response(..))
import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (ToJSON, FromJSON, Value, toJSON, parseJSON, withObject, (.:), decode, object, (.=), eitherDecode)
import IHP.ControllerSupport
import qualified Data.Text as T
import Prelude (read)

import Generated.Types

instance ToJSON News where
    toJSON News {..} =
        object
            [ "type" .= ("news" :: Text)
            , "title" .= title
            , "body" .= body
            ]

-- Index a news item in Elasticsearch
indexNews :: (?context :: ControllerContext) => News -> IO ()
indexNews news = do
    let
        (esServer, esManager) = getAppConfig @(Server, Manager)
        indexName = IndexName "news"
        docId = DocId (show news.id)
        document = toJSON news
        settings = defaultIndexDocumentSettings

    -- Execute the index request
    response <- runBH (mkBHEnv esServer esManager) $ indexDocument indexName settings document docId
    pure ()

-- Delete a news item from Elasticsearch
deleteIndexNews :: (?context :: ControllerContext) => Id News -> IO ()
deleteIndexNews newsId = do
    let
        (esServer, esManager) = getAppConfig @(Server, Manager)
        indexName = IndexName "news"
        docId = DocId (show newsId)

    -- Execute the delete request
    response <- runBH (mkBHEnv esServer esManager) $ deleteDocument indexName docId
    pure ()

-- Search for news items in Elasticsearch
searchNews :: (?context :: ControllerContext) => Text -> IO [Id News]
searchNews queryText = do
    -- Execute the search request
    response <- runBH (mkBHEnv esServer esManager) $ searchByIndex indexName (mkSearch (Just query) Nothing)
    -- Parse the response
    result <- parseEsResponse response
    case result of
        Left esError -> do
            -- Handle the error (log it, return empty list, or throw an exception)
            liftIO $ putStrLn $ "Error occurred: " ++ show esError
            return []
        Right (searchResult :: SearchResult Value) -> do
            -- Extract the News IDs from the search result

            let newsIds :: [Id News] = map (textToId . unDocId . hitDocId) $ hits $ searchHits searchResult
            liftIO $ putStrLn $ "newsIds: " ++ show newsIds
            return newsIds

    -- Parse result and extract the News ids. The News IDs is the Doc
    pure []
    where
        (esServer, esManager) = getAppConfig @(Server, Manager)
        indexName = IndexName "news"
        -- @todo: Search all fields
        query = QueryMultiMatchQuery $ mkMultiMatchQuery [(FieldName "title"), (FieldName "body")]  (QueryString queryText)

        unDocId :: DocId -> Text
        unDocId (DocId t) = t


