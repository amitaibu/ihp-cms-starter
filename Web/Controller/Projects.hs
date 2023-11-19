module Web.Controller.Projects where

import Web.Controller.Prelude
import Web.View.Projects.Index
import Web.View.Projects.New


instance Controller ProjectsController where
    action ProjectsAction = do
        let values :: [(ProjectType, Int)] = [(ProjectTypeOngoing, 3), (ProjectTypeNotStarted, 2)]

            valuePairToCondition :: (ProjectType, Int) -> (QueryBuilder "projects" -> QueryBuilder "projects")
            valuePairToCondition (projectType, participants) subQuery =
                subQuery
                    |> queryOr
                        (\qb -> qb |> filterWhere (#projectType, projectType))
                        (\qb -> qb |> filterWhere (#participants, participants))

            conditions = foldl' (\queryBuilder condition -> condition queryBuilder) (query @Project) (map valuePairToCondition values)

        projects <- fetch conditions
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
