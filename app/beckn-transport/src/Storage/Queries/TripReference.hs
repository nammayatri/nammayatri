module Storage.Queries.TripReference where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.DB.Types as DB
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Data.Time (UTCTime)
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.TripReference as Storage
import Beckn.Types.Common

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.TripReferenceT))
getDbTable =
  DB._tripReference . DB.transporterDb <$> getSchemaName

create :: Storage.TripReference -> Flow ()
create Storage.TripReference {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.TripReference {..})

findTripReferenceById ::
  Id Storage.TripReference -> Flow (Maybe Storage.TripReference)
findTripReferenceById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.TripReference {..} = _id ==. B.val_ id

listTripReferences :: Maybe Int -> Maybe Int -> [Storage.Status] -> Flow [Storage.TripReference]
listTripReferences mlimit moffset status = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable predicate limit offset orderByDesc
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.TripReference {..} = B.desc_ _createdAt
    predicate Storage.TripReference {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _status `B.in_` (B.val_ <$> status) ||. complementVal status
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

update ::
  Id Storage.TripReference ->
  Storage.Status ->
  Flow ()
update id status = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate tripId Storage.TripReference {..} = _id ==. B.val_ tripId
    setClause newStatus currTime Storage.TripReference {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ newStatus
        ]
