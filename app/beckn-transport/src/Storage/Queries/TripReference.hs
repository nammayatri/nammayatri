module Storage.Queries.TripReference where

import App.Types
import Beckn.Utils.Extra
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.TripReference as Storage

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.TripReferenceT)
dbTable = DB._tripReference DB.transporterDb

create :: Storage.TripReference -> Flow ()
create Storage.TripReference {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.TripReference {..})
    >>= either DB.throwDBError pure

findTripReferenceById ::
  TripReferenceId -> Flow (Maybe Storage.TripReference)
findTripReferenceById id =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.TripReference {..} = _id ==. B.val_ id

listTripReferences :: Maybe Int -> Maybe Int -> [Storage.Status] -> Flow [Storage.TripReference]
listTripReferences mlimit moffset status =
  DB.findAllWithLimitOffsetWhere dbTable (predicate status) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.TripReference {..} = B.desc_ _createdAt
    predicate status Storage.TripReference {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _status `B.in_` (B.val_ <$> status) ||. complementVal status
        ]

complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

update ::
  TripReferenceId ->
  Storage.Status ->
  Flow (T.DBResult ())
update id status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate id Storage.TripReference {..} = _id ==. B.val_ id
    setClause status currTime Storage.TripReference {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]
