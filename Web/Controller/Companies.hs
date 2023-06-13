module Web.Controller.Companies where

import Web.Controller.Prelude
import Web.View.Companies.Index
import Web.View.Companies.New
import Web.View.Companies.Edit
import Web.View.Companies.Show

-- @todo: Add to Guide
import qualified Network.Wai.Parse as Wai


instance Controller CompaniesController where
    action CompaniesAction = do
        companies <- query @Company |> fetch
        render IndexView { .. }

    action NewCompanyAction = do
        let company = newRecord
        render NewView { .. }

    action ShowCompanyAction { companyId } = do
        company <- fetch companyId
        render ShowView { .. }

    action EditCompanyAction { companyId } = do
        company <- fetch companyId
        render EditView { .. }

    action UpdateCompanyAction { companyId } = do
        company <- fetch companyId
        company
            |> buildCompany
            |> ifValid \case
                Left company -> render EditView { .. }
                Right company -> do
                    company <- company |> updateRecord
                    setSuccessMessage "Company updated"
                    redirectTo EditCompanyAction { .. }

    action CreateCompanyAction = do
        uploadedFile <- withTransaction do
            uploadedFile <- newRecord @UploadedFile |> createRecord

            let file = fileOrNothing "uploadedFile" |> fromMaybe (error "no file given")

            let options :: StoreFileOptions = def
                    { directory = "uploaded_files"
                    , contentDisposition = contentDispositionAttachmentAndFileName
                    }

            storedFile <- storeFileWithOptions file options

            signedUrl <- createTemporaryDownloadUrl storedFile

            uploadedFile
                |> set #signedUrl (signedUrl |> get #url)
                |> set #signedUrlExpiredAt (signedUrl |> get #expiredAt)
                |> set #path (storedFile |> get #path)
                |> set #fileName (cs $ get #fileName file)
                |> set #contentType (cs $ Wai.fileContentType file)
                |> updateRecord

        let company = newRecord @Company
        company
            |> buildCompany
            |> set #uploadedFileId uploadedFile.id
            |> ifValid \case
                Left company -> render NewView { .. }
                Right company -> do
                    company <- company |> createRecord
                    setSuccessMessage "Company created"
                    redirectTo ShowCompanyAction { companyId = company.id }

    action DeleteCompanyAction { companyId } = do
        company <- fetch companyId
        deleteRecord company
        setSuccessMessage "Company deleted"
        redirectTo CompaniesAction

buildCompany company = company
    |> fill @'["name"]
