module Application.Helper.Elasticsearch where

import IHP.Prelude
import Database.Bloodhound
import Network.HTTP.Client (Manager)
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (ToJSON, toJSON, object, (.=))
import IHP.ControllerSupport




import Generated.Types


instance ToJSON News where
    toJSON News  {..} =
        object ["title" .= title, "body" .= body]

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
    -- _ <- getResponse response
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
    -- _ <- getResponse response
    pure ()