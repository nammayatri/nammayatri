module Storage.Queries.Issues (insertIssue) where

import qualified Beckn.Storage.Common as Storage
import Beckn.Storage.DB.Config
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Schema
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Issue as Issue

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Issue.IssueT))
getDbTable =
  DB.issues . DB.appDb <$> getSchemaName

insertIssue :: DBFlow m r => Issue.Issue -> m ()
insertIssue Issue.Issue {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertValue Issue.Issue {..})
