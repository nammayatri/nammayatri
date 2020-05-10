module Storage.Queries.TripReference where

import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.TripReference as Storage

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.TripReferenceT)
dbTable = DB._tripReference DB.transporterDb

create :: Storage.TripReference -> L.Flow ()
create Storage.TripReference {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.TripReference {..})
    >>= either DB.throwDBError pure

findTripReferenceById ::
  TripReferenceId -> L.Flow (Maybe Storage.TripReference)
findTripReferenceById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.TripReference {..} = (_id ==. B.val_ id)

listTripReferences :: Maybe Int -> Maybe Int -> [Storage.Status] -> L.Flow [Storage.TripReference]
listTripReferences mlimit moffset status =
  DB.findAllWithLimitOffsetWhere dbTable (predicate status) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = (toInteger $ fromMaybe 100 mlimit)
    offset = (toInteger $ fromMaybe 0 moffset)
    orderByDesc Storage.TripReference {..} = B.desc_ _createdAt
    predicate status Storage.TripReference {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _status `B.in_` (B.val_ <$> status) ||. complementVal status
        ]

complementVal l
  | (null l) = B.val_ True
  | otherwise = B.val_ False

update ::
  TripReferenceId ->
  Storage.Status ->
  L.Flow (T.DBResult ())
update id status = do
  (currTime :: LocalTime) <- getCurrTime
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
