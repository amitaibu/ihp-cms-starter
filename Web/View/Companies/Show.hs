module Web.View.Companies.Show where
import Web.View.Prelude

data ShowView = ShowView { company :: Company }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Company</h1>
        <p>{company}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Companies" CompaniesAction
                            , breadcrumbText "Show Company"
                            ]