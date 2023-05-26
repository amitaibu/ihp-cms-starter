module Web.View.ImageStyle.Index where
import Web.View.Prelude

data IndexView = IndexView { imageStyle :: [ImageStyle]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewImageStyleAction} class="btn btn-primary ms-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>ImageStyle</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach imageStyle renderImageStyle}</tbody>
            </table>
            
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "ImageStyles" ImageStylesAction
                ]

renderImageStyle :: ImageStyle -> Html
renderImageStyle imageStyle = [hsx|
    <tr>
        <td>{imageStyle}</td>
        <td><a href={ShowImageStyleAction imageStyle.id}>Show</a></td>
        <td><a href={EditImageStyleAction imageStyle.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteImageStyleAction imageStyle.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]