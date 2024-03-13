module Web.View.Posts.Index where
import Web.View.Prelude
import Data.Aeson (encode, Value)

data IndexView = IndexView
    { posts :: [Post]
    , postsJson :: Data.Aeson.Value
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewPostAction} class="btn btn-primary ms-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Post</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach posts renderPost}</tbody>
            </table>

            {- Pass the encoded JSON to the JS script -}
            <script data-posts={encode postsJson}>
                // Parse the encoded JSON.
                console.log(JSON.parse(document.currentScript.dataset.posts));
            </script>

        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Posts" PostsAction
                ]

renderPost :: Post -> Html
renderPost post = [hsx|
    <tr>
        <td>{post}</td>
        <td><a href={ShowPostAction post.id}>Show</a></td>
        <td><a href={EditPostAction post.id} class="text-muted">Edit</a></td>
        <td><a href={DeletePostAction post.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]