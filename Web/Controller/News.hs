module Web.Controller.News where

import Web.Controller.Prelude
import Web.View.News.Index
import Web.View.News.New
import Web.View.News.Edit
import Web.View.News.Show

instance Controller NewsController where
    action NewsAction = do
        news <- query @News |> fetch
        render IndexView { .. }

    action NewNewsAction = do
        let news = newRecord
        render NewView { .. }

    action ShowNewsAction { newsId } = do
        news <- fetch newsId
        render ShowView { .. }

    action EditNewsAction { newsId } = do
        news <- fetch newsId
        render EditView { .. }

    action UpdateNewsAction { newsId } = do
        news <- fetch newsId
        news
            |> buildNews
            |> ifValid \case
                Left news -> render EditView { .. }
                Right news -> do
                    news <- news |> updateRecord
                    setSuccessMessage "News updated"
                    redirectTo EditNewsAction { .. }

    action CreateNewsAction = do
        let news = newRecord @News
        news
            |> buildNews
            |> ifValid \case
                Left news -> render NewView { .. } 
                Right news -> do
                    news <- news |> createRecord
                    setSuccessMessage "News created"
                    redirectTo NewsAction

    action DeleteNewsAction { newsId } = do
        news <- fetch newsId
        deleteRecord news
        setSuccessMessage "News deleted"
        redirectTo NewsAction

buildNews news = news
    |> fill @'[]
