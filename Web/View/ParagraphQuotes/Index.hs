module Web.View.ParagraphQuotes.Index where
import Web.View.Prelude

data IndexView = IndexView { paragraphQuotes :: [ParagraphQuote]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index</h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>ParagraphQuote</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach paragraphQuotes renderParagraphQuote}</tbody>
            </table>

        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ParagraphQuotes" ParagraphQuotesAction
                ]

renderParagraphQuote :: ParagraphQuote -> Html
renderParagraphQuote paragraphQuote = [hsx|
    <tr>
        <td>{paragraphQuote.title}</td>
        <td><a href={ShowParagraphQuoteAction paragraphQuote.id}>Show</a></td>
        <td><a href={EditParagraphQuoteAction paragraphQuote.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteParagraphQuoteAction paragraphQuote.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]