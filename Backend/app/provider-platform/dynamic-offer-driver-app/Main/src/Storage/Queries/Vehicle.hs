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
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Utils (setMeshConfig)
import Sequelize as Se
import qualified Storage.Beam.Vehicle as BeamV

create :: L.MonadFlow m => Vehicle -> m (MeshResult ())
create vehicle = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamV.VehicleT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainVehicleToBeam vehicle)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

upsert :: L.MonadFlow m => Vehicle -> m ()
upsert a@Vehicle {..} = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamV.VehicleT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      res <- either (pure Nothing) (transformBeamVehicleToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamV.driverId $ Se.Eq (getId a.driverId)]
      if isJust res
        then
          void $
            KV.updateWoReturningWithKVConnector
              dbCOnf'
              updatedMeshConfig
              [ Se.Set BeamV.capacity capacity,
                Se.Set BeamV.category category,
                Se.Set BeamV.make make,
                Se.Set BeamV.model model,
                Se.Set BeamV.size size,
                Se.Set BeamV.variant variant,
                Se.Set BeamV.color color,
                Se.Set BeamV.energyType energyType,
                Se.Set BeamV.registrationCategory registrationCategory,
                Se.Set BeamV.updatedAt updatedAt
              ]
              [Se.Is BeamV.driverId (Se.Eq $ getId a.driverId)]
        else void $ KV.createWoReturingKVConnector dbCOnf' updatedMeshConfig (transformDomainVehicleToBeam a)
    Nothing -> pure ()

findById :: (MonadFlow m) => Id Person -> m (Maybe Vehicle)
findById (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamV.VehicleT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamVehicleToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamV.driverId $ Se.Eq driverId]
    Nothing -> pure Nothing

updateVehicleRec :: (L.MonadFlow m, MonadTime m) => Vehicle -> m (MeshResult ())
updateVehicleRec vehicle = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamV.VehicleT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamV.capacity vehicle.capacity,
          Se.Set BeamV.category vehicle.category,
          Se.Set BeamV.make vehicle.make,
          Se.Set BeamV.model vehicle.model,
          Se.Set BeamV.size vehicle.size,
          Se.Set BeamV.variant vehicle.variant,
          Se.Set BeamV.color vehicle.color,
          Se.Set BeamV.energyType vehicle.energyType,
          Se.Set BeamV.registrationNo vehicle.registrationNo,
          Se.Set BeamV.registrationCategory vehicle.registrationCategory,
          Se.Set BeamV.updatedAt now
        ]
        [Se.Is BeamV.driverId (Se.Eq $ getId vehicle.driverId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

deleteById :: L.MonadFlow m => Id Person -> m ()
deleteById (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamV.VehicleT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.Is BeamV.driverId (Se.Eq driverId)]
    Nothing -> pure ()

findByAnyOf :: L.MonadFlow m => Maybe Text -> Maybe (Id Person) -> m (Maybe Vehicle)
findByAnyOf registrationNoM vehicleIdM = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamV.VehicleT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' ->
      either (pure Nothing) (transformBeamVehicleToDomain <$>)
        <$> KV.findWithKVConnector
          dbCOnf'
          updatedMeshConfig
          -- [Se.And [whenJust vehicleIdM (\vehicleId -> Se.Is BeamV.driverId $ Se.Eq vehicleId), whenJust registrationNoM (\regNum -> Se.Is BeamV.registrationNo $ Se.Eq regNum)]]
          [ Se.And
              ( []
                  <> if isJust vehicleIdM
                    then [Se.Is BeamV.driverId $ Se.Eq (getId (fromJust vehicleIdM))]
                    else
                      []
                        <> ([Se.Is BeamV.registrationNo $ Se.Eq (fromJust registrationNoM) | isJust registrationNoM])
              )
          ]
    Nothing -> pure Nothing

-- findAllByVariantRegNumMerchantId ::
--   Transactionable m =>
--   Maybe Variant.Variant ->
--   Maybe Text ->
--   Integer ->
--   Integer ->
--   Id Merchant ->
--   m [Vehicle]
-- findAllByVariantRegNumMerchantId variantM mbRegNum limit' offset' merchantId = do
--   let limitVal = fromIntegral limit'
--       offsetVal = fromIntegral offset'
--   Esq.findAll $ do
--     vehicle <- from $ table @VehicleT
--     where_ $
--       vehicle ^. VehicleMerchantId ==. val (toKey merchantId)
--         &&. whenJust_ variantM (\variant -> vehicle ^. VehicleVariant ==. val variant)
--         &&. whenJust_ mbRegNum (\regNum -> vehicle ^. VehicleRegistrationNo `ilike` (%) ++. val regNum ++. (%))
--     orderBy [desc $ vehicle ^. VehicleCreatedAt]
--     limit limitVal
--     offset offsetVal
--     return vehicle

findAllByVariantRegNumMerchantId :: L.MonadFlow m => Maybe Variant.Variant -> Maybe Text -> Integer -> Integer -> Id Merchant -> m [Vehicle]
findAllByVariantRegNumMerchantId variantM mbRegNum limit' offset' (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamV.VehicleT
  let updatedMeshConfig = setMeshConfig modelName
  let limitVal = fromIntegral limit'
      offsetVal = fromIntegral offset'
  case dbConf of
    Just dbCOnf' ->
      either (pure []) (transformBeamVehicleToDomain <$>)
        <$> KV.findAllWithOptionsKVConnector
          dbCOnf'
          updatedMeshConfig
          [Se.And ([Se.Is BeamV.merchantId $ Se.Eq merchantId] <> if isJust variantM then [Se.Is BeamV.variant $ Se.Eq (fromJust variantM)] else [] <> ([Se.Is BeamV.registrationNo $ Se.Eq (fromJust mbRegNum) | isJust mbRegNum]))]
          (Se.Desc BeamV.createdAt)
          (Just limitVal)
          (Just offsetVal)
    Nothing -> pure []

findByRegistrationNo :: (MonadFlow m) => Text -> m (Maybe Vehicle)
findByRegistrationNo registrationNo = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamV.VehicleT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamVehicleToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamV.registrationNo $ Se.Eq registrationNo]
    Nothing -> pure Nothing

transformBeamVehicleToDomain :: BeamV.Vehicle -> Vehicle
transformBeamVehicleToDomain BeamV.VehicleT {..} = do
  Vehicle
    { driverId = Id driverId,
      merchantId = Id merchantId,
      variant = variant,
      model = model,
      color = color,
      registrationNo = registrationNo,
      capacity = capacity,
      category = category,
      make = make,
      size = size,
      energyType = energyType,
      registrationCategory = registrationCategory,
      vehicleClass = vehicleClass,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainVehicleToBeam :: Vehicle -> BeamV.Vehicle
transformDomainVehicleToBeam Vehicle {..} =
  BeamV.VehicleT
    { BeamV.driverId = getId driverId,
      BeamV.merchantId = getId merchantId,
      BeamV.variant = variant,
      BeamV.model = model,
      BeamV.color = color,
      BeamV.registrationNo = registrationNo,
      BeamV.capacity = capacity,
      BeamV.category = category,
      BeamV.make = make,
      BeamV.size = size,
      BeamV.energyType = energyType,
      BeamV.registrationCategory = registrationCategory,
      BeamV.vehicleClass = vehicleClass,
      BeamV.createdAt = createdAt,
      BeamV.updatedAt = updatedAt
    }
