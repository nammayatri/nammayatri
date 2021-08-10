{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.SearchReqLocation where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id, state)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.SearchReqLocation as Storage

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.SearchReqLocationT))
getDbTable =
  DB.searchReqLocation . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.SearchReqLocation -> m ()
createFlow =
  DB.runSqlDB . create

create :: Storage.SearchReqLocation -> DB.SqlDB ()
create location = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue location)

findLocationById ::
  DBFlow m r =>
  Id Storage.SearchReqLocation ->
  m (Maybe Storage.SearchReqLocation)
findLocationById locId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.SearchReqLocation {..} = id ==. B.val_ locId

findAllByLocIds :: DBFlow m r => [Id Storage.SearchReqLocation] -> [Id Storage.SearchReqLocation] -> m [Storage.SearchReqLocation]
findAllByLocIds fromIds toIds = do
  dbTable <- getDbTable
  DB.findAll dbTable identity (predicate fromIds toIds)
  where
    predicate pFromIds pToIds Storage.SearchReqLocation {..} =
      B.in_ id (B.val_ <$> pFromIds)
        ||. B.in_ id (B.val_ <$> pToIds)
