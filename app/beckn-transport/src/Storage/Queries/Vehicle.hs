{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.Vehicle where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Vehicle as Storage
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Error
import qualified Types.Storage.DB as DB
import Utils.Common

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.VehicleT))
getDbTable =
  DB.vehicle . DB.transporterDb <$> getSchemaName

create :: Storage.Vehicle -> Flow ()
create Storage.Vehicle {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.Vehicle {..})

findVehicleById ::
  Id Storage.Vehicle -> Flow (Maybe Storage.Vehicle)
findVehicleById vid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Vehicle {..} = id ==. B.val_ vid

findByIdAndOrgId ::
  Id Storage.Vehicle -> Text -> Flow Storage.Vehicle
findByIdAndOrgId vid orgId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= fromMaybeM VehicleNotFound
  where
    predicate Storage.Vehicle {..} = id ==. B.val_ vid &&. organizationId ==. B.val_ orgId

findAllWithLimitOffsetByOrgIds :: Maybe Integer -> Maybe Integer -> [Text] -> Flow [Storage.Vehicle]
findAllWithLimitOffsetByOrgIds mlimit moffset orgIds = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.Vehicle {..} = B.desc_ createdAt
    limit = fromMaybe 100 mlimit
    offset = fromMaybe 0 moffset
    predicate Storage.Vehicle {..} =
      foldl
        (&&.)
        (B.val_ True)
        [organizationId `B.in_` (B.val_ <$> orgIds) ||. complementVal orgIds]

findAllByOrgIds :: [Text] -> Flow [Storage.Vehicle]
findAllByOrgIds orgIds = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Vehicle {..} =
      foldl
        (&&.)
        (B.val_ True)
        [organizationId `B.in_` (B.val_ <$> orgIds) ||. complementVal orgIds]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

updateVehicleRec :: Storage.Vehicle -> Flow ()
updateVehicleRec vehicle = do
  dbTable <- getDbTable
  DB.update dbTable (setClause vehicle) (predicate $ vehicle ^. #id)
  where
    setClause pVehicle Storage.Vehicle {..} =
      mconcat
        [ capacity <-. B.val_ (Storage.capacity pVehicle),
          category <-. B.val_ (Storage.category pVehicle),
          make <-. B.val_ (Storage.make pVehicle),
          model <-. B.val_ (Storage.model pVehicle),
          size <-. B.val_ (Storage.size pVehicle),
          variant <-. B.val_ (Storage.variant pVehicle),
          color <-. B.val_ (Storage.color pVehicle),
          energyType <-. B.val_ (Storage.energyType pVehicle),
          registrationCategory <-. B.val_ (Storage.registrationCategory pVehicle),
          updatedAt <-. B.val_ (Storage.updatedAt pVehicle)
        ]
    predicate vid Storage.Vehicle {..} = id ==. B.val_ vid

deleteById :: Id Storage.Vehicle -> Flow ()
deleteById vehId = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate vehId)
  where
    predicate vid Storage.Vehicle {..} = id ==. B.val_ vid

findByAnyOf :: Maybe Text -> Maybe (Id Storage.Vehicle) -> Flow (Maybe Storage.Vehicle)
findByAnyOf registrationNoM vehicleIdM = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Vehicle {..} =
      (B.val_ (isNothing vehicleIdM) ||. id ==. B.val_ (fromMaybe "DONT_MATCH" vehicleIdM))
        &&. (B.val_ (isNothing registrationNoM) ||. registrationNo ==. B.val_ (fromMaybe "DONT_MATCH" registrationNoM))

findAllByVariantCatOrgId :: Maybe Storage.Variant -> Maybe Storage.Category -> Maybe Storage.EnergyType -> Integer -> Integer -> Text -> Flow [Storage.Vehicle]
findAllByVariantCatOrgId variantM categoryM energyTypeM limit offset orgId = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.Vehicle {..} = B.desc_ createdAt
    predicate Storage.Vehicle {..} =
      organizationId ==. B.val_ orgId
        &&. (B.val_ (isNothing variantM) ||. variant ==. B.val_ variantM)
        &&. (B.val_ (isNothing categoryM) ||. category ==. B.val_ categoryM)
        &&. (B.val_ (isNothing energyTypeM) ||. energyType ==. B.val_ energyTypeM)

findByIds :: [Id Storage.Vehicle] -> Flow [Storage.Vehicle]
findByIds ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Vehicle {..} = B.in_ id (B.val_ <$> ids)

findByRegistrationNo ::
  Text -> Flow (Maybe Storage.Vehicle)
findByRegistrationNo registrationNo_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Vehicle {..} = registrationNo ==. B.val_ registrationNo_
