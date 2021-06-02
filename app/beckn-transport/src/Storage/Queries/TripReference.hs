module Storage.Queries.TripReference where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Data.Time (UTCTime)
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.TripReference as Storage

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.TripReferenceT))
getDbTable =
  DB.tripReference . DB.transporterDb <$> getSchemaName

create :: Storage.TripReference -> Flow ()
create Storage.TripReference {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.TripReference {..})

findTripReferenceById ::
  Id Storage.TripReference -> Flow (Maybe Storage.TripReference)
findTripReferenceById trId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.TripReference {..} = id ==. B.val_ trId

listTripReferences :: Maybe Int -> Maybe Int -> [Storage.Status] -> Flow [Storage.TripReference]
listTripReferences mlimit moffset status_ = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.TripReference {..} = B.desc_ createdAt
    predicate Storage.TripReference {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ status `B.in_` (B.val_ <$> status_) ||. complementVal status_
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

update ::
  Id Storage.TripReference ->
  Storage.Status ->
  Flow ()
update trId status_ = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause status_ currTime)
    (predicate trId)
  where
    predicate tripId Storage.TripReference {..} = id ==. B.val_ tripId
    setClause newStatus currTime Storage.TripReference {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ newStatus
        ]
