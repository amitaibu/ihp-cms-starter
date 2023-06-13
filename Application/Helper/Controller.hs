module Application.Helper.Controller where

import IHP.ControllerPrelude
import Generated.Types
import Web.Types

-- Here you can add functions which are available in all your controllers

fetchCompanyWithRecords :: (?modelContext :: ModelContext) => Id Company -> IO CompanyWithRecords
fetchCompanyWithRecords companyId = do
    company <- fetch companyId
    uploadedFile <- fetch company.uploadedFileId
    pure $ CompanyWithRecords { .. }


getTemporaryDownloadUrlFromFile ::
    ( ?context :: context
    , ConfigProvider context
    , HasField "signedUrlExpiredAt" record UTCTime
    , HasField "path" record Text
    , HasField "signedUrl" record Text
    , SetField "signedUrlExpiredAt" record UTCTime
    , SetField "path" record Text
    , SetField "signedUrl" record Text
    ) => record  -> IO record
getTemporaryDownloadUrlFromFile record = do
    now <- getCurrentTime
    let diff = diffUTCTime now record.signedUrlExpiredAt
    if diff < 0
        then do
            temporaryDownloadUrl <- createTemporaryDownloadUrlFromPath record.path
            record
                |> set #signedUrl (temporaryDownloadUrl |> get #url)
                |> set #signedUrlExpiredAt (temporaryDownloadUrl |> get #expiredAt)
                |> pure

        else
            pure record




fetchLandingPageWithRecords :: (?modelContext :: ModelContext) => Id LandingPage -> IO LandingPageWithRecords
fetchLandingPageWithRecords landingPageId = do

    landingPage <- fetch landingPageId
    paragraphCtas <- fetch landingPage.paragraphCtasRefLandingPages

    -- Landing pages referenced by ParagraphCta.refLandingPageId
    paragraphCtaRefLandingPages <- query @LandingPage
        |> filterWhereIn (#id, map (get #refLandingPageId) paragraphCtas)
        |> fetch

    paragraphQuotes <- fetch landingPage.paragraphQuotes

    return $ LandingPageWithRecords { .. }

getParagraphsCount :: (?modelContext::ModelContext) => Id LandingPage -> IO Int
getParagraphsCount landingPageId = do
    landingPageWithRecords <- fetchLandingPageWithRecords landingPageId

    pure $ length landingPageWithRecords.paragraphCtas
                    + length landingPageWithRecords.paragraphQuotes
                    + 1
