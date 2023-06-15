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
        let originalPasswordHash = user.passwordHash
        -- The value from the password confirmation input field.
        let passwordConfirmation = param @Text "passwordConfirmation"
        user
            |> fill @["email", "passwordHash"]
            -- We only validate the email field isn't empty, as the password
            -- can remain empty. We ensure that the error message doesn't include
            -- the entered password.
            |> validateField #passwordHash (isEqual passwordConfirmation |> withCustomErrorMessage "Passwords don't match")
            |> validateField #passwordHash nonEmpty
            |> validateField #email isEmail
            -- After this validation, since it's operation on the IO, we'll need to use >>=.
            |> validateIsUnique #email
            >>= ifValid \case
                Left user -> render EditView { .. }
                Right user -> do
                    -- If the password hash is empty, then the user did not
                    -- change the password. So, we set the password hash to
                    -- the original password hash.
                    hashed <-
                        if user.passwordHash == ""
                            then pure originalPasswordHash
                            else hashPassword user.passwordHash

                    user <- user
                        |> set #passwordHash hashed
                        |> updateRecord
                    setSuccessMessage "User updated"
                    redirectTo EditUserAction { .. }

    action CreateUserAction = do
        let user = newRecord @User
        -- The value from the password confirmation input field.
        let passwordConfirmation = param @Text "passwordConfirmation"
        user
            |> fill @["email", "passwordHash"]
            -- We ensure that the error message doesn't include
            -- the entered password.
            |> validateField #passwordHash (isEqual passwordConfirmation |> withCustomErrorMessage "Passwords don't match")
            |> validateField #passwordHash nonEmpty
            |> validateField #email isEmail
            -- After this validation, since it's operation on the IO, we'll need to use >>=.
            |> validateIsUnique #email
            >>= ifValid \case
                Left user -> render NewView { .. }
                Right user -> do
                    hashed <- hashPassword user.passwordHash
                    user <- user
                        |> set #passwordHash hashed
                        |> createRecord
                    setSuccessMessage "You have registered successfully"
                    redirectToPath "/"

    action DeleteUserAction { userId } = do
        user <- fetch userId
        deleteRecord user
        setSuccessMessage "User deleted"
        redirectTo UsersAction

buildUser user = user
    |> fill @["email", "passwordHash"]
