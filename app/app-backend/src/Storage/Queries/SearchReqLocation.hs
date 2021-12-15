module Storage.Queries.SearchReqLocation where

import qualified Beckn.Storage.Common as Storage
import Beckn.Storage.DB.Config
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id, state)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.SearchReqLocation as Storage

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.SearchReqLocationT))
getDbTable =
  DB.searchReqLocation . DB.appDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.SearchReqLocation -> m ()
createFlow = do
  DB.runSqlDB . create

create :: Storage.SearchReqLocation -> DB.SqlDB ()
create Storage.SearchReqLocation {..} = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue Storage.SearchReqLocation {..})

findLocationById ::
  DBFlow m r =>
  Id Storage.SearchReqLocation ->
  m (Maybe Storage.SearchReqLocation)
findLocationById locId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.SearchReqLocation {..} = id ==. B.val_ locId

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findAllByIds :: DBFlow m r => [Id Storage.SearchReqLocation] -> m [Storage.SearchReqLocation]
findAllByIds locIds = do
  dbTable <- getDbTable
  DB.findAll dbTable identity (predicate locIds)
  where
    predicate locationIds Storage.SearchReqLocation {..} =
      B.in_ id (B.val_ <$> locationIds)
