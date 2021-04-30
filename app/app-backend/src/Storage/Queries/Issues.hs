module Storage.Queries.Issues (insertIssue) where

import qualified App.Types as App
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.DB.Types as DB
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Issue as Issue
import qualified Database.Beam as B
import EulerHS.Prelude
import qualified Types.Storage.DB as DB

getDbTable :: App.Flow (B.DatabaseEntity be DB.AppDb (B.TableEntity Issue.IssueT))
getDbTable =
  DB._issues . DB.appDb <$> getSchemaName

insertIssue :: Issue.Issue -> App.Flow ()
insertIssue Issue.Issue {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Issue.Issue {..})
