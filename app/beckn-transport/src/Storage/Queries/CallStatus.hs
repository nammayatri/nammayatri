module Storage.Queries.CallStatus where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.CallStatus as Storage
import qualified Types.Storage.DB as DB

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.CallStatusT))
getDbTable =
  DB.callStatus . DB.transporterDb <$> getSchemaName

create :: DBFlow m r => Storage.CallStatus -> m ()
create callStatus = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertValue callStatus)

findById :: DBFlow m r => Id Storage.CallStatus -> m (Maybe Storage.CallStatus)
findById csId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.CallStatus {..} =
      B.val_ csId B.==. id
