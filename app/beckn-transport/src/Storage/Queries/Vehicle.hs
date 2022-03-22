module Storage.Queries.Vehicle where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization
import Domain.Types.Vehicle
import Storage.Tabular.Vehicle
import Utils.Common

create :: Vehicle -> SqlDB ()
create = Esq.create'

findById ::
  Transactionable m =>
  Id Vehicle ->
  m (Maybe Vehicle)
findById = Esq.findById

findByIdAndOrgId ::
  Transactionable m =>
  Id Vehicle ->
  Id Organization ->
  m (Maybe Vehicle)
findByIdAndOrgId vid orgId =
  Esq.findOne $ do
    vehicle <- from $ table @VehicleT
    where_ $
      vehicle ^. VehicleTId ==. val (toKey vid)
        &&. vehicle ^. VehicleOrganizationId ==. val (toKey orgId)
    return vehicle

updateVehicleRec :: Vehicle -> SqlDB ()
updateVehicleRec vehicle = do
  now <- getCurrentTime
  Esq.update' $ \tbl -> do
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
        VehicleRegistrationCategory =. val vehicle.registrationCategory,
        VehicleUpdatedAt =. val now
      ]
    where_ $ tbl ^. VehicleTId ==. val (toKey vehicle.id)

deleteById :: Id Vehicle -> SqlDB ()
deleteById = Esq.deleteByKey' @VehicleT

findByAnyOf :: Transactionable m => Maybe Text -> Maybe (Id Vehicle) -> m (Maybe Vehicle)
findByAnyOf registrationNoM vehicleIdM =
  Esq.findOne $ do
    vehicle <- from $ table @VehicleT
    where_ $
      whenJust_ vehicleIdM (\vehicleId -> vehicle ^. VehicleTId ==. val (toKey vehicleId))
        &&. whenJust_ registrationNoM (\regNum -> vehicle ^. VehicleRegistrationNo ==. val regNum)
    return vehicle

findAllByVariantCatOrgId ::
  Transactionable m =>
  Maybe Variant ->
  Maybe Category ->
  Maybe EnergyType ->
  Maybe Text ->
  Integer ->
  Integer ->
  Id Organization ->
  m [Vehicle]
findAllByVariantCatOrgId variantM categoryM energyTypeM mbRegNum limit' offset' orgId = do
  let limitVal = fromIntegral limit'
      offsetVal = fromIntegral offset'
  Esq.findAll $ do
    vehicle <- from $ table @VehicleT
    where_ $
      vehicle ^. VehicleOrganizationId ==. val (toKey orgId)
        &&. whenJust_ variantM (\variant -> vehicle ^. VehicleVariant ==. val variant)
        &&. whenJust_ categoryM (\category -> vehicle ^. VehicleCategory ==. val (Just category))
        &&. whenJust_ energyTypeM (\energyType -> vehicle ^. VehicleEnergyType ==. val (Just energyType))
        &&. whenJust_ mbRegNum (\regNum -> vehicle ^. VehicleRegistrationNo ==. val regNum)
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
