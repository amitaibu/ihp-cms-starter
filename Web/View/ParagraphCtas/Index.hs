module Web.View.ParagraphCtas.Index where
import Web.View.Prelude

data IndexView = IndexView { paragraphCtas :: [ParagraphCta]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index</h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>ParagraphCta</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach paragraphCtas renderParagraphCta}</tbody>
            </table>

        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ParagraphCta" ParagraphCtasAction
                ]

renderParagraphCta :: ParagraphCta -> Html
renderParagraphCta paragraphCta = [hsx|
    <tr>
        <td>{paragraphCta}</td>
        <td><a href={ShowParagraphCtasAction paragraphCta.id}>Show</a></td>
        <td><a href={EditParagraphCtasAction paragraphCta.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteParagraphCtasAction paragraphCta.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]