module Web.Controller.News where

import Web.Controller.Prelude
import Web.Controller.Prelude
import Web.View.News.Index
import Web.View.News.New
import Web.View.News.Edit
import Web.View.News.Show
import Application.Helper.Elasticsearch

instance Controller NewsController where
    action NewsAction = do
        news <- query @News |> fetch
        render IndexView { .. }

    action NewNewsAction = do
        let news = newRecord
        render NewView { .. }

    action ShowNewsAction { .. } = do
        news <- fetch newsId
        render ShowView { .. }

    action EditNewsAction { .. } = do
        news <- fetch newsId
        render EditView { .. }

    action UpdateNewsAction { .. } = do
        news <- fetch newsId
        news
            |> buildNews
            |> ifValid \case
                Left news -> render EditView { .. }
                Right news -> do
                    news <- news |> updateRecord
                    indexNews news  -- Index updated news in Elasticsearch
                    setSuccessMessage "News updated"
                    redirectTo NewsAction

    action CreateNewsAction = do
        let news = newRecord @News
        news
            |> buildNews
            |> ifValid \case
                Left news -> render NewView { .. }
                Right news -> do
                    news <- news |> createRecord
                    indexNews news  -- Index new news in Elasticsearch
                    setSuccessMessage "News created"
                    redirectTo NewsAction

    action DeleteNewsAction { newsId } = do
        news <- fetch newsId
        deleteRecord news
        deleteIndexNews newsId
        setSuccessMessage "News deleted"
        redirectTo NewsAction

    action SearchNewsAction = do
        undefined

buildNews news = news
    |> fill @["title", "body"]