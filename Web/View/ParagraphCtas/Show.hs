module Web.View.ParagraphCtas.Show where
import Web.View.Prelude

data ShowView = ShowView { paragraphCta :: ParagraphCta }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show ParagraphCta</h1>
        <p>{paragraphCta}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "ParagraphCta" ParagraphCtaAction
                            , breadcrumbText "Show ParagraphCta"
                            ]