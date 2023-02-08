module Storage.Queries.Vehicle where

import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.Vehicle

create :: Vehicle -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id Person ->
  m (Maybe Vehicle)
findById = Esq.findById

updateVehicleRec :: Vehicle -> SqlDB ()
updateVehicleRec vehicle = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ VehicleCapacity =. val vehicle.capacity,
        VehicleCategory =. val vehicle.category,
        VehicleMake =. val vehicle.make,
        VehicleModel =. val vehicle.model,
        VehicleSize =. val vehicle.size,
        VehicleVariant =. val vehicle.variant,
        VehicleColor =. val vehicle.color,
        VehicleEnergyType =. val vehicle.energyType,
        VehicleRegistrationNo =. val vehicle.registrationNo,
        VehicleRegistrationCategory =. val vehicle.registrationCategory,
        VehicleUpdatedAt =. val now
      ]
    where_ $ tbl ^. VehicleTId ==. val (toKey vehicle.driverId)

deleteById :: Id Person -> SqlDB ()
deleteById = Esq.deleteByKey @VehicleT

findByAnyOf :: Transactionable m => Maybe Text -> Maybe (Id Person) -> m (Maybe Vehicle)
findByAnyOf registrationNoM vehicleIdM =
  Esq.findOne $ do
    vehicle <- from $ table @VehicleT
    where_ $
      whenJust_ vehicleIdM (\vehicleId -> vehicle ^. VehicleTId ==. val (toKey vehicleId))
        &&. whenJust_ registrationNoM (\regNum -> vehicle ^. VehicleRegistrationNo ==. val regNum)
    return vehicle

findAllByVariantRegNumMerchantId ::
  Transactionable m =>
  Maybe Variant ->
  Maybe Text ->
  Integer ->
  Integer ->
  Id Merchant ->
  m [Vehicle]
findAllByVariantRegNumMerchantId variantM mbRegNum limit' offset' merchantId = do
  let limitVal = fromIntegral limit'
      offsetVal = fromIntegral offset'
  Esq.findAll $ do
    vehicle <- from $ table @VehicleT
    where_ $
      vehicle ^. VehicleMerchantId ==. val (toKey merchantId)
        &&. whenJust_ variantM (\variant -> vehicle ^. VehicleVariant ==. val variant)
        &&. whenJust_ mbRegNum (\regNum -> vehicle ^. VehicleRegistrationNo `ilike` (%) ++. val regNum ++. (%))
    orderBy [desc $ vehicle ^. VehicleCreatedAt]
    limit limitVal
    offset offsetVal
    return vehicle

findByRegistrationNo ::
  Transactionable m =>
  Text ->
  m (Maybe Vehicle)
findByRegistrationNo registrationNo =
  Esq.findOne $ do
    vehicle <- from $ table @VehicleT
    where_ $ vehicle ^. VehicleRegistrationNo ==. val registrationNo
    return vehicle
