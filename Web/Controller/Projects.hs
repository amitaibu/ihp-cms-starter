module Web.Controller.Projects where

import Web.Controller.Prelude
import Web.View.Projects.Index
import Web.View.Projects.New
import Database.PostgreSQL.Simple.ToField
import Data.ByteString.Builder (byteString, char8)


instance ToField (ProjectType, Text) where
    toField = serializeTuple


serializeTuple :: (ProjectType, Text) -> Action
serializeTuple (projectType, participants) = Many
    [ Plain (byteString "(")
    , toField projectType
    , Plain (char8 ',')
    , toField participants
    , Plain (char8 ')')
    ]

instance Controller ProjectsController where
    action ProjectsAction = do
        projects <- query @Project |> fetch

        -- Fetch only specific projects by a pair of values.
        -- In this case we really fetch all, but show how we pair the values we want to
        -- query by.
        let pairs = projects |> fmap (\project -> (project.projectType, project.participants))

        -- Use a parameterized query to handle list of tuples
        projectsQuery :: [Project] <- sqlQuery "SELECT * FROM projects WHERE (project_type, participants) IN ?" (Only $ In pairs)


        render IndexView { .. }

    action NewProjectAction = do
        let project = newRecord
        render NewView { .. }

    action CreateProjectAction = do
        let project = newRecord @Project
        project
            |> buildProject
            |> ifValid \case
                Left project -> render NewView { .. }
                Right project -> do
                    project <- project |> createRecord
                    setSuccessMessage "Project created"
                    redirectTo ProjectsAction

buildProject project = project
    |> fill @'["projectType", "participants"]
