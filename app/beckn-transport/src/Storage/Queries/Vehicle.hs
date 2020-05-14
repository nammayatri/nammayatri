module Storage.Queries.Vehicle where

import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Beckn.Types.App
import qualified Types.Storage.DB as DB
import qualified Beckn.Types.Storage.Vehicle as Storage

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.VehicleT)
dbTable = DB._vehicle DB.transporterDb

create :: Storage.Vehicle -> L.Flow ()
create Storage.Vehicle {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Vehicle {..})
    >>= either DB.throwDBError pure

findVehicleById ::
  VehicleId -> L.Flow (Maybe Storage.Vehicle)
findVehicleById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Vehicle {..} = (_id ==. B.val_ id)

findAllWithLimitOffsetByOrgIds :: Maybe Integer -> Maybe Integer -> [Text] -> L.Flow [Storage.Vehicle]
findAllWithLimitOffsetByOrgIds mlimit moffset orgIds = do
    DB.findAllWithLimitOffsetWhere dbTable (predicate orgIds) limit offset orderByDesc
      >>= either DB.throwDBError pure
  where
    orderByDesc Storage.Vehicle {..} = B.desc_ _createdAt
    limit = (toInteger $ fromMaybe 100 mlimit)
    offset = (toInteger $ fromMaybe 0 moffset)
    predicate orgIds Storage.Vehicle {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _organizationId `B.in_` ((\x -> B.val_ x) <$> orgIds) ||. complementVal orgIds
        ]

findAllByOrgIds ::[Text] -> L.Flow [Storage.Vehicle]
findAllByOrgIds orgIds = do
    DB.findAllOrErr dbTable (predicate orgIds)
  where
    predicate orgIds Storage.Vehicle {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _organizationId `B.in_` ((\x -> B.val_ x) <$> orgIds) ||. complementVal orgIds
        ]

complementVal l
  | (null l) = B.val_ True
  | otherwise = B.val_ False
