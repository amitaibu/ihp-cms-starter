module Web.Controller.Projects where

import Web.Controller.Prelude
import Web.View.Projects.Index
import Web.View.Projects.New

instance Controller ProjectsController where
    action ProjectsAction = do
        projects <- query @Project |> fetch
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
