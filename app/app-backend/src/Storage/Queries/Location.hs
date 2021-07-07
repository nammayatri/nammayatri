module Storage.Queries.Location where

import qualified Beckn.Storage.Common as Storage
import Beckn.Storage.DB.Config
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((&&.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id, state)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Location as Storage

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.LocationT))
getDbTable =
  DB.location . DB.appDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.Location -> m ()
createFlow = do
  DB.runSqlDB . create

create :: Storage.Location -> DB.SqlDB ()
create Storage.Location {..} = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertExpression Storage.Location {..})

findLocationById ::
  DBFlow m r =>
  Id Storage.Location ->
  m (Maybe Storage.Location)
findLocationById locId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Location {..} = id ==. B.val_ locId

findAllWithLimitOffsetWhere ::
  DBFlow m r =>
  [Text] ->
  [Text] ->
  [Text] ->
  [Text] ->
  [Text] ->
  Maybe Int ->
  Maybe Int ->
  m [Storage.Location]
findAllWithLimitOffsetWhere pins cities states districts wards mlimit moffset = do
  dbTable <- getDbTable
  DB.findAll
    dbTable
    (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc)
    predicate
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.Location {..} = B.desc_ createdAt
    predicate Storage.Location {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ pincode `B.in_` (B.val_ . Just <$> pins) ||. complementVal pins,
          city `B.in_` (B.val_ . Just <$> cities) ||. complementVal cities,
          state `B.in_` (B.val_ . Just <$> states) ||. complementVal states,
          district `B.in_` (B.val_ . Just <$> districts) ||. complementVal districts,
          ward `B.in_` (B.val_ . Just <$> wards) ||. complementVal wards
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findAllByIds :: DBFlow m r => [Id Storage.Location] -> m [Storage.Location]
findAllByIds locIds = do
  dbTable <- getDbTable
  DB.findAll dbTable identity (predicate locIds)
  where
    predicate locationIds Storage.Location {..} =
      B.in_ id (B.val_ <$> locationIds)
