module Storage.Queries.RiderDetails where

import Beckn.External.Encryption
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.RiderDetails as Storage

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.RiderDetailsT))
getDbTable =
  DB.riderDetails . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.RiderDetails -> m ()
createFlow = DB.runSqlDB . create

create :: Storage.RiderDetails -> DB.SqlDB ()
create riderDetails = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue riderDetails)

findById ::
  DBFlow m r =>
  Id Storage.RiderDetails ->
  m (Maybe Storage.RiderDetails)
findById rdId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.RiderDetails {..} = id ==. B.val_ rdId

findByMobileNumber ::
  (DBFlow m r, EncFlow m r) =>
  Text ->
  m (Maybe Storage.RiderDetails)
findByMobileNumber mobileNumber_ = do
  dbTable <- getDbTable
  mobileNumberDbHash <- getDbHash mobileNumber_
  DB.findOne dbTable (predicate mobileNumberDbHash)
  where
    predicate mobileNumberDbHash Storage.RiderDetails {..} = mobileNumber.hash ==. B.val_ mobileNumberDbHash