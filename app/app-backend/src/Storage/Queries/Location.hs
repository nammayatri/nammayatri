module Storage.Queries.Location where

import Beckn.Types.App
import qualified Beckn.Types.Storage.Location as Storage
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries as DB
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.LocationT)
dbTable = DB._location DB.appDb

create :: Storage.Location -> L.Flow ()
create Storage.Location {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Location {..})
    >>= either DB.throwDBError pure

findLocationById ::
  LocationId -> L.Flow (Maybe Storage.Location)
findLocationById id =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Location {..} = _id ==. B.val_ id

findAllWithLimitOffsetWhere :: [Text] -> [Text] -> [Text] -> [Text] -> [Text] -> Maybe Int -> Maybe Int -> L.Flow [Storage.Location]
findAllWithLimitOffsetWhere pins cities states districts wards mlimit moffset =
  DB.findAllWithLimitOffsetWhere
    dbTable
    (predicate pins cities states districts wards)
    limit
    offset
    orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.Location {..} = B.desc_ _createdAt
    predicate pins cities states districts wards Storage.Location {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _pincode `B.in_` (B.val_ . Just <$> pins) ||. complementVal pins,
          _city `B.in_` (B.val_ . Just <$> cities) ||. complementVal cities,
          _state `B.in_` (B.val_ . Just <$> states) ||. complementVal states,
          _district `B.in_` (B.val_ . Just <$> districts) ||. complementVal districts,
          _ward `B.in_` (B.val_ . Just <$> wards) ||. complementVal wards
        ]

complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findAllByIds :: [Text] -> L.Flow [Storage.Location]
findAllByIds locIds =
  DB.findAllOrErr dbTable (pred (LocationId <$> locIds))
  where
    pred locIds Storage.Location {..} =
      B.in_ _id (B.val_ <$> locIds)
