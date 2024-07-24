module Application.Helper.Elasticsearch where

import IHP.Prelude
import Database.Bloodhound
import Network.HTTP.Client (Manager, Response(..))
import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (ToJSON, FromJSON, Value, toJSON, parseJSON, withObject, (.:), decode, object, (.=), eitherDecode)
import IHP.ControllerSupport
import qualified Data.Text as T

import Generated.Types

instance ToJSON News where
    toJSON News {..} =
        object
            [ "title" .= title
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
    result <- runBH (mkBHEnv esServer esManager) $ searchByIndex indexName (mkSearch (Just query) Nothing)
    -- Parse result and extract the News ids. The News IDs is the Doc ID.

    pure []
    where
        (esServer, esManager) = getAppConfig @(Server, Manager)
        indexName = IndexName "news"
        query = QueryMatchQuery $ mkMatchQuery (FieldName "_all") (QueryString queryText)


