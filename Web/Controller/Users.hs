module Web.Controller.Users where

import Web.Controller.Prelude
import Web.View.Users.Index
import Web.View.Users.New
import Web.View.Users.Edit
import Web.View.Users.Show

instance Controller UsersController where
    action UsersAction = do
        users <- query @User |> fetch
        render IndexView { .. }

    action NewUserAction = do
        let user = newRecord
        render NewView { .. }

    action ShowUserAction { userId } = do
        user <- fetch userId
        render ShowView { .. }

    action EditUserAction { userId } = do
        user <- fetch userId

        -- Empty the password hash.
        render EditView { user = user |> set #passwordHash "" }

    action UpdateUserAction { userId } = do
        user <- fetch userId
        let originalpasswordHash = user.passwordHash
        user
            |> buildUser
            |> ifValid \case
                Left user -> render EditView { .. }
                Right user -> do
                    -- If the password hash is empty, then the user did not
                    -- change the password. So, we set the password hash to
                    -- the original password hash.
                    hashed <-
                        if user.passwordHash == ""
                            then hashPassword user.passwordHash
                            else pure originalpasswordHash

                    user <- user
                        |> set #passwordHash hashed
                        |> updateRecord
                    setSuccessMessage "User updated"
                    redirectTo EditUserAction { .. }

    action CreateUserAction = do
        let user = newRecord @User
        user
            |> fill @["email", "passwordHash"]
            |> validateField #email isEmail
            |> validateField #passwordHash nonEmpty
            |> ifValid \case
                Left user -> render NewView { .. }
                Right user -> do
                    hashed <- hashPassword user.passwordHash
                    user <- user
                        |> set #passwordHash hashed
                        |> createRecord
                    setSuccessMessage "You have registered successfully"
                    redirectTo UsersAction

    action DeleteUserAction { userId } = do
        user <- fetch userId
        deleteRecord user
        setSuccessMessage "User deleted"
        redirectTo UsersAction

buildUser user = user
    |> fill @["email", "passwordHash", "failedLoginAttempts"]
