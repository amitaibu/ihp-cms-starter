module Web.View.Posts.Show where
import Web.View.Prelude

-- We are now getting the `PostWithRecords` not only the `Post`.
data ShowView = ShowView { postWithRecords :: PostWithRecords }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <h1>Show Post</h1>
        <p>{post.title}</p>
        <h2>Comments</h2>
        {forEach comments (\comment -> showComment comment commentUsers)}
    |]
        where
            -- Extract the records out of the `postWithRecords` variable.
            post = postWithRecords.post
            comments = postWithRecords.comments
            commentUsers = postWithRecords.commentUsers

showComment :: Comment -> [User] -> Html
showComment comment commentUsers =
    [hsx|
        {comment.body} authored by {authorName}
    |]
    where
        authorName = commentUsers
            |> filter (\user -> user.id == comment.userId)
            -- There should be a single user.
            |> head
            -- Get the user's name. As `head` returns a Maybe value we need to use `maybe mempty` which means
            -- if no user found, don't show anything.
            |> maybe mempty (get #name)