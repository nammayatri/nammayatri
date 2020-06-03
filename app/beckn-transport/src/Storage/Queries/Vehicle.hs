{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.Vehicle where

import Beckn.Types.App
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Vehicle as Storage
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import qualified Types.Storage.DB as DB

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

findByIdAndOrgId ::
  VehicleId -> Text -> L.Flow Storage.Vehicle
findByIdAndOrgId id orgId = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "INVALID_VEHICLE_ID"
  where
    predicate Storage.Vehicle {..} = (_id ==. B.val_ id &&. _organizationId ==. B.val_ orgId)

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

findAllByOrgIds :: [Text] -> L.Flow [Storage.Vehicle]
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

updateVehicleRec :: Storage.Vehicle -> L.Flow ()
updateVehicleRec vehicle = do
  DB.update dbTable (setClause vehicle) (predicate $ vehicle ^. #_id)
    >>= either DB.throwDBError pure
  where
    setClause vehicle Storage.Vehicle {..} =
      mconcat
        [ _capacity <-. B.val_ (Storage._capacity vehicle),
          _category <-. B.val_ (Storage._category vehicle),
          _make <-. B.val_ (Storage._make vehicle),
          _model <-. B.val_ (Storage._model vehicle),
          _size <-. B.val_ (Storage._size vehicle),
          _variant <-. B.val_ (Storage._variant vehicle),
          _color <-. B.val_ (Storage._color vehicle),
          _energyType <-. B.val_ (Storage._energyType vehicle),
          _registrationCategory <-. B.val_ (Storage._registrationCategory vehicle),
          _updatedAt <-. B.val_ (Storage._updatedAt vehicle)
        ]
    predicate id Storage.Vehicle {..} = _id ==. B.val_ id
