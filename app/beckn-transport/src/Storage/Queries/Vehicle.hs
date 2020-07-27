{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.Vehicle where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.Vehicle as Storage
import Beckn.Utils.Common
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.VehicleT)
dbTable = DB._vehicle DB.transporterDb

create :: Storage.Vehicle -> Flow ()
create Storage.Vehicle {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Vehicle {..})
    >>= either DB.throwDBError pure

findVehicleById ::
  VehicleId -> Flow (Maybe Storage.Vehicle)
findVehicleById id =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Vehicle {..} = _id ==. B.val_ id

findByIdAndOrgId ::
  VehicleId -> Text -> Flow Storage.Vehicle
findByIdAndOrgId id orgId =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "INVALID_VEHICLE_ID"
  where
    predicate Storage.Vehicle {..} = _id ==. B.val_ id &&. _organizationId ==. B.val_ orgId

findAllWithLimitOffsetByOrgIds :: Maybe Integer -> Maybe Integer -> [Text] -> Flow [Storage.Vehicle]
findAllWithLimitOffsetByOrgIds mlimit moffset orgIds =
  DB.findAllWithLimitOffsetWhere dbTable (predicate orgIds) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.Vehicle {..} = B.desc_ _createdAt
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    predicate orgIds Storage.Vehicle {..} =
      foldl
        (&&.)
        (B.val_ True)
        [_organizationId `B.in_` (B.val_ <$> orgIds) ||. complementVal orgIds]

findAllByOrgIds :: [Text] -> Flow [Storage.Vehicle]
findAllByOrgIds orgIds =
  DB.findAllOrErr dbTable (predicate orgIds)
  where
    predicate orgIds Storage.Vehicle {..} =
      foldl
        (&&.)
        (B.val_ True)
        [_organizationId `B.in_` (B.val_ <$> orgIds) ||. complementVal orgIds]

complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

updateVehicleRec :: Storage.Vehicle -> Flow ()
updateVehicleRec vehicle =
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

deleteById :: VehicleId -> Flow ()
deleteById id =
  DB.delete dbTable (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.Vehicle {..} = _id ==. B.val_ id

findByAnyOf :: Maybe Text -> Maybe Text -> Flow (Maybe Storage.Vehicle)
findByAnyOf registrationNoM vehicleIdM =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Vehicle {..} =
      (B.val_ (isNothing vehicleIdM) ||. _id ==. B.val_ (VehicleId (fromMaybe "DONT_MATCH" vehicleIdM)))
        &&. (B.val_ (isNothing registrationNoM) ||. _registrationNo ==. B.val_ (fromMaybe "DONT_MATCH" registrationNoM))

findAllByVariantCatOrgId :: Maybe Storage.Variant -> Maybe Storage.Category -> Maybe Storage.EnergyType -> Integer -> Integer -> Text -> Flow [Storage.Vehicle]
findAllByVariantCatOrgId variantM categoryM energyTypeM limit offset orgId =
  DB.findAllWithLimitOffsetWhere dbTable predicate limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.Vehicle {..} = B.desc_ _createdAt
    predicate Storage.Vehicle {..} =
      _organizationId ==. B.val_ orgId
        &&. (B.val_ (isNothing variantM) ||. _variant ==. B.val_ variantM)
        &&. (B.val_ (isNothing categoryM) ||. _category ==. B.val_ categoryM)
        &&. (B.val_ (isNothing energyTypeM) ||. _energyType ==. B.val_ energyTypeM)

findByIds :: [VehicleId] -> Flow [Storage.Vehicle]
findByIds ids =
  DB.findAllOrErr dbTable predicate
  where
    predicate Storage.Vehicle {..} = B.in_ _id (B.val_ <$> ids)
