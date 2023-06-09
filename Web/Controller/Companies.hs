module Web.Controller.Companies where

import Web.Controller.Prelude
import Web.View.Companies.Index
import Web.View.Companies.New
import Web.View.Companies.Edit
import Web.View.Companies.Show

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
        let company = newRecord @Company
        company
            |> buildCompany
            |> ifValid \case
                Left company -> render NewView { .. } 
                Right company -> do
                    company <- company |> createRecord
                    setSuccessMessage "Company created"
                    redirectTo CompaniesAction

    action DeleteCompanyAction { companyId } = do
        company <- fetch companyId
        deleteRecord company
        setSuccessMessage "Company deleted"
        redirectTo CompaniesAction

buildCompany company = company
    |> fill @["title", "uploadedFileId"]
