module Web.Controller.ParagraphFeaturedArticles where

import Web.Controller.Prelude
import Web.View.ParagraphFeaturedArticles.Index
import Web.View.ParagraphFeaturedArticles.New
import Web.View.ParagraphFeaturedArticles.Edit
import Web.View.ParagraphFeaturedArticles.Show

instance Controller ParagraphFeaturedArticlesController where
    action ParagraphFeaturedArticlesAction = do
        paragraphFeaturedArticles <- query @ParagraphFeaturedArticle |> fetch
        render IndexView { .. }

    action NewParagraphFeaturedArticleAction = do
        let paragraphFeaturedArticle = newRecord
        render NewView { .. }

    action ShowParagraphFeaturedArticleAction { paragraphFeaturedArticleId } = do
        paragraphFeaturedArticle <- fetch paragraphFeaturedArticleId
        render ShowView { .. }

    action EditParagraphFeaturedArticleAction { paragraphFeaturedArticleId } = do
        paragraphFeaturedArticle <- fetch paragraphFeaturedArticleId
        render EditView { .. }

    action UpdateParagraphFeaturedArticleAction { paragraphFeaturedArticleId } = do
        paragraphFeaturedArticle <- fetch paragraphFeaturedArticleId
        paragraphFeaturedArticle
            |> buildParagraphFeaturedArticle
            |> ifValid \case
                Left paragraphFeaturedArticle -> render EditView { .. }
                Right paragraphFeaturedArticle -> do
                    paragraphFeaturedArticle <- paragraphFeaturedArticle |> updateRecord
                    setSuccessMessage "ParagraphFeaturedArticle updated"
                    redirectTo EditParagraphFeaturedArticleAction { .. }

    action CreateParagraphFeaturedArticleAction = do
        let paragraphFeaturedArticle = newRecord @ParagraphFeaturedArticle
        paragraphFeaturedArticle
            |> buildParagraphFeaturedArticle
            |> ifValid \case
                Left paragraphFeaturedArticle -> render NewView { .. } 
                Right paragraphFeaturedArticle -> do
                    paragraphFeaturedArticle <- paragraphFeaturedArticle |> createRecord
                    setSuccessMessage "ParagraphFeaturedArticle created"
                    redirectTo ParagraphFeaturedArticlesAction

    action DeleteParagraphFeaturedArticleAction { paragraphFeaturedArticleId } = do
        paragraphFeaturedArticle <- fetch paragraphFeaturedArticleId
        deleteRecord paragraphFeaturedArticle
        setSuccessMessage "ParagraphFeaturedArticle deleted"
        redirectTo ParagraphFeaturedArticlesAction

buildParagraphFeaturedArticle paragraphFeaturedArticle = paragraphFeaturedArticle
    |> fill @["landingPageId", "weight"]
