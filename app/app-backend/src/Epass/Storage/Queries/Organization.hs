module Epass.Storage.Queries.Organization where

import qualified Beckn.Types.Storage.Location as BTL
import Beckn.Utils.Extra (getCurrentTimeUTC)
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.DB as DB
import qualified Epass.Types.Storage.Organization as Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB

dbTable :: B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.OrganizationT)
dbTable = DB._organization DB.becknDb

create :: Storage.Organization -> L.Flow ()
create Storage.Organization {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Organization {..})
    >>= either DB.throwDBError pure

findOrganizationById ::
  OrganizationId -> L.Flow (Maybe Storage.Organization)
findOrganizationById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Organization {..} = (_id ==. B.val_ id)

listOrganizations :: Maybe Int -> Maybe Int -> [BTL.LocationType] -> [Int] -> [Text] -> [Text] -> [Text] -> [Text] -> [Storage.Status] -> Maybe Bool -> L.Flow [Storage.Organization]
listOrganizations mlimit moffset locationTypes pincodes cities districts wards states statuses verifiedM =
  DB.findAllWithLimitOffsetWhere dbTable (predicate locationTypes pincodes cities districts wards states statuses verifiedM) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = (toInteger $ fromMaybe 100 mlimit)
    offset = (toInteger $ fromMaybe 0 moffset)
    orderByDesc Storage.Organization {..} = B.desc_ _createdAt
    predicate locationTypes pincodes cities districts wards states statuses verifiedM Storage.Organization {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _status `B.in_` (B.val_ <$> statuses) ||. complementVal statuses,
          _locationType `B.in_` ((B.val_ . Just) <$> locationTypes) ||. complementVal locationTypes,
          _pincode `B.in_` (B.val_ <$> pincodes) ||. complementVal pincodes,
          _city `B.in_` ((B.val_) <$> cities) ||. complementVal cities,
          _state `B.in_` ((B.val_) <$> states) ||. complementVal states,
          _district `B.in_` ((B.val_ . Just) <$> districts) ||. complementVal districts,
          _ward `B.in_` ((B.val_ . Just) <$> wards) ||. complementVal wards,
          maybe (B.val_ True) ((_verified ==.) . B.val_) verifiedM
        ]

complementVal l
  | (null l) = B.val_ True
  | otherwise = B.val_ False

update ::
  OrganizationId ->
  Storage.Status ->
  L.Flow (T.DBResult ())
update id status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate id Storage.Organization {..} = _id ==. B.val_ id
    setClause status currTime Storage.Organization {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]
