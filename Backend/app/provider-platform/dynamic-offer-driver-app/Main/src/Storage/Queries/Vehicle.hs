{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Vehicle where

import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.Vehicle
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.Vehicle

create :: Vehicle -> SqlDB m ()
create = Esq.create

upsert :: Vehicle -> SqlDB m ()
upsert a@Vehicle {..} =
  Esq.upsert
    a
    [ VehicleDriverId =. val (toKey driverId),
      VehicleCapacity =. val capacity,
      VehicleCategory =. val category,
      VehicleMake =. val make,
      VehicleModel =. val model,
      VehicleSize =. val size,
      VehicleVariant =. val variant,
      VehicleColor =. val color,
      VehicleEnergyType =. val energyType,
      VehicleRegistrationCategory =. val registrationCategory,
      VehicleUpdatedAt =. val updatedAt
    ]

findById ::
  forall m ma.
  Transactionable ma m =>
  Proxy ma ->
  Id Person ->
  m (Maybe Vehicle)
findById _ = Esq.findById @m @ma

updateVehicleRec :: Vehicle -> SqlDB m ()
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

deleteById :: Id Person -> SqlDB m ()
deleteById = Esq.deleteByKey @VehicleT

findByAnyOf :: forall m ma. Transactionable ma m => Maybe Text -> Maybe (Id Person) -> Proxy ma -> m (Maybe Vehicle)
findByAnyOf registrationNoM vehicleIdM _ =
  Esq.findOne @m @ma $ do
    vehicle <- from $ table @VehicleT
    where_ $
      whenJust_ vehicleIdM (\vehicleId -> vehicle ^. VehicleTId ==. val (toKey vehicleId))
        &&. whenJust_ registrationNoM (\regNum -> vehicle ^. VehicleRegistrationNo ==. val regNum)
    return vehicle

findAllByVariantRegNumMerchantId ::
  forall m ma.
  Transactionable ma m =>
  Maybe Variant.Variant ->
  Maybe Text ->
  Integer ->
  Integer ->
  Id Merchant ->
  Proxy ma ->
  m [Vehicle]
findAllByVariantRegNumMerchantId variantM mbRegNum limit' offset' merchantId _ = do
  let limitVal = fromIntegral limit'
      offsetVal = fromIntegral offset'
  Esq.findAll @m @ma $ do
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
  forall m ma.
  Transactionable ma m =>
  Text ->
  Proxy ma ->
  m (Maybe Vehicle)
findByRegistrationNo registrationNo _ =
  Esq.findOne @m @ma $ do
    vehicle <- from $ table @VehicleT
    where_ $ vehicle ^. VehicleRegistrationNo ==. val registrationNo
    return vehicle
