module Storage.Queries.CancellationReason where

import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Schema
import Beckn.Utils.Common
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.CancellationReason as SCR
import qualified Types.Storage.DB as DB

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity SCR.CancellationReasonT))
getDbTable =
  DB.cancellationReason . DB.appDb <$> getSchemaName

findAll :: DBFlow m r => m [SCR.CancellationReason]
findAll = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate _ = B.val_ True