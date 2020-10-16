module Storage.Queries.Issues (insertIssue) where

import qualified App.Types as App
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as Queries
import qualified Beckn.Types.Storage.Issue as Issue
import qualified Beckn.Utils.Common as Utils
import qualified Database.Beam as B
import EulerHS.Prelude
import qualified Types.Storage.DB as DB

getDbTable :: App.Flow (B.DatabaseEntity be DB.AppDb (B.TableEntity Issue.IssueT))
getDbTable =
  DB._issues . DB.appDb <$> Utils.getSchemaName

insertIssue :: Issue.Issue -> App.Flow ()
insertIssue Issue.Issue {..} = do
  dbTable <- getDbTable
  Queries.createOne dbTable (Storage.insertExpression Issue.Issue {..})
    >>= either Queries.throwDBError pure
