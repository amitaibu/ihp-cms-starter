module Web.Controller.Articles where

import Web.Controller.Prelude
import Web.View.Articles.Index
import Web.View.Articles.New
import Web.View.Articles.Edit
import Web.View.Articles.Show

instance Controller ArticlesController where
    action ArticlesAction = do
        articles <- query @Article |> fetch
        render IndexView { .. }

    action NewArticleAction = do
        let article = newRecord
        render NewView { .. }

    action ShowArticleAction { articleId } = do
        article <- fetch articleId
        render ShowView { .. }

    action EditArticleAction { articleId } = do
        article <- fetch articleId
        render EditView { .. }

    action UpdateArticleAction { articleId } = do
        article <- fetch articleId
        article
            |> buildArticle
            |> ifValid \case
                Left article -> render EditView { .. }
                Right article -> do
                    article <- article |> updateRecord
                    setSuccessMessage "Article updated"
                    redirectTo EditArticleAction { .. }

    action CreateArticleAction = do
        let article = newRecord @Article
        article
            |> buildArticle
            |> ifValid \case
                Left article -> render NewView { .. } 
                Right article -> do
                    article <- article |> createRecord
                    setSuccessMessage "Article created"
                    redirectTo ArticlesAction

    action DeleteArticleAction { articleId } = do
        article <- fetch articleId
        deleteRecord article
        setSuccessMessage "Article deleted"
        redirectTo ArticlesAction

buildArticle article = article
    |> fill @'["title"]
