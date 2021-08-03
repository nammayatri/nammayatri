module Storage.Queries.SearchRequest where

import qualified Beckn.Storage.Common as Storage
import Beckn.Storage.DB.Config
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Data.Time (UTCTime)
import Database.Beam ((&&.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Person as Person
import qualified Types.Storage.SearchReqLocation as Loc
import qualified Types.Storage.SearchRequest as Storage

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.SearchRequestT))
getDbTable =
  DB.searchRequest . DB.appDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.SearchRequest -> m ()
createFlow = DB.runSqlDB . create

create :: Storage.SearchRequest -> DB.SqlDB ()
create searchRequest = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue searchRequest)

findAllByPersonIdLimitOffset ::
  DBFlow m r =>
  Id Person.Person ->
  Maybe Integer ->
  Maybe Integer ->
  m [Storage.SearchRequest]
findAllByPersonIdLimitOffset personId mlimit moffset = do
  dbTable <- getDbTable
  let limit = fromMaybe 100 mlimit
      offset = fromMaybe 0 moffset
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.SearchRequest {..} = B.desc_ createdAt
    predicate Storage.SearchRequest {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ requestorId ==. B.val_ personId
        ]

findById :: DBFlow m r => Id Storage.SearchRequest -> m (Maybe Storage.SearchRequest)
findById searchRequestId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.SearchRequest {..} = id ==. B.val_ searchRequestId

findByPersonId :: DBFlow m r => Id Person.Person -> Id Storage.SearchRequest -> m (Maybe Storage.SearchRequest)
findByPersonId personId searchRequestId = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate personId)
  where
    predicate personId_ Storage.SearchRequest {..} =
      id ==. B.val_ searchRequestId &&. requestorId ==. B.val_ personId_

findAllByIds :: DBFlow m r => [Id Storage.SearchRequest] -> m [Storage.SearchRequest]
findAllByIds searchRequestIds = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.SearchRequest {..} = id `B.in_` (B.val_ <$> searchRequestIds)

findAllByPerson :: DBFlow m r => Id Person.Person -> m [Storage.SearchRequest]
findAllByPerson perId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.SearchRequest {..} = requestorId ==. B.val_ perId

findAllExpired :: DBFlow m r => Maybe UTCTime -> Maybe UTCTime -> m [Storage.SearchRequest]
findAllExpired maybeFrom maybeTo = do
  dbTable <- getDbTable
  (now :: UTCTime) <- getCurrentTime
  DB.findAll dbTable identity (predicate now)
  where
    predicate now Storage.SearchRequest {..} =
      foldl
        (&&.)
        (B.val_ True)
        ( [ validTill B.<=. B.val_ now
          ]
            <> maybe [] (\from -> [createdAt B.>=. B.val_ from]) maybeFrom
            <> maybe [] (\to -> [createdAt B.<=. B.val_ to]) maybeTo
        )

findAllWithLimitOffsetWhere ::
  DBFlow m r =>
  [Id Loc.SearchReqLocation] ->
  [Id Loc.SearchReqLocation] ->
  [Storage.VehicleVariant] ->
  Maybe Int ->
  Maybe Int ->
  m [Storage.SearchRequest]
findAllWithLimitOffsetWhere fromLocationIds toLocationIds variants mlimit moffset = do
  dbTable <- getDbTable
  DB.findAll
    dbTable
    (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc)
    predicate
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.SearchRequest {..} = B.desc_ createdAt
    predicate Storage.SearchRequest {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ fromLocationId `B.in_` (B.val_ <$> fromLocationIds) ||. complementVal fromLocationIds,
          toLocationId `B.in_` (B.val_ <$> toLocationIds) ||. complementVal toLocationIds,
          vehicleVariant `B.in_` (B.val_ <$> variants) ||. complementVal variants
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False