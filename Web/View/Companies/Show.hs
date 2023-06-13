module Web.View.Companies.Show where
import Web.View.Prelude

data ShowView = ShowView { companyWithRecords :: CompanyWithRecords }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Company</h1>
        <p>{company.name}</p>
        <img src={companyWithRecords.uploadedFile.signedUrl} />

    |]
        where
            company = companyWithRecords.company
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Companies" CompaniesAction
                            , breadcrumbText "Show Company"
                            ]