module Storage.Queries.Issues (insertIssue) where

import qualified Beckn.Storage.Common as Storage
import Beckn.Storage.DB.Config
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Issue as Issue
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Issue.IssueT))
getDbTable =
  DB.issues . DB.appDb <$> getSchemaName

insertIssue :: HasFlowDBEnv m r => Issue.Issue -> m ()
insertIssue Issue.Issue {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Issue.Issue {..})
