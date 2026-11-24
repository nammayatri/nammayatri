{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TowingRequest where

import qualified Data.Aeson
import qualified Domain.Types.TowingRequest
import qualified Domain.Types.VehicleViolation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TowingRequest as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TowingRequest.TowingRequest -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.TowingRequest.TowingRequest] -> m ()
createMany = traverse_ create

findAllByViolationId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.VehicleViolation.VehicleViolation -> m [Domain.Types.TowingRequest.TowingRequest]
findAllByViolationId violationId = do findAllWithKV [Se.Is Beam.violationId $ Se.Eq (Kernel.Types.Id.getId violationId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.TowingRequest.TowingRequest -> m (Maybe Domain.Types.TowingRequest.TowingRequest)
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TowingRequest.TowingStatus -> (Kernel.Types.Id.Id Domain.Types.TowingRequest.TowingRequest -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.TowingRequest.TowingRequest -> m (Maybe Domain.Types.TowingRequest.TowingRequest)
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TowingRequest.TowingRequest -> m ()
updateByPrimaryKey (Domain.Types.TowingRequest.TowingRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.agencyEntityId agencyEntityId,
      Se.Set Beam.agentId agentId,
      Se.Set Beam.currency currency,
      Se.Set Beam.evidence (Data.Aeson.toJSON evidence),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.notes notes,
      Se.Set Beam.parkingFine parkingFine,
      Se.Set Beam.paymentMethod paymentMethod,
      Se.Set Beam.paymentOrderId (Kernel.Types.Id.getId <$> paymentOrderId),
      Se.Set Beam.paymentReceivedAt paymentReceivedAt,
      Se.Set Beam.paymentReceivedBy paymentReceivedBy,
      Se.Set Beam.paymentStatus paymentStatus,
      Se.Set Beam.pickupLat pickupLat,
      Se.Set Beam.pickupLon pickupLon,
      Se.Set Beam.releasedToIdNumberEncrypted (releasedToIdNumber <&> unEncrypted . Kernel.External.Encryption.encrypted),
      Se.Set Beam.releasedToIdNumberHash (releasedToIdNumber <&> Kernel.External.Encryption.hash),
      Se.Set Beam.releasedToIdType releasedToIdType,
      Se.Set Beam.releasedToName releasedToName,
      Se.Set Beam.releasedToPhoneEncrypted (releasedToPhone <&> unEncrypted . Kernel.External.Encryption.encrypted),
      Se.Set Beam.releasedToPhoneHash (releasedToPhone <&> Kernel.External.Encryption.hash),
      Se.Set Beam.releasedToRelation releasedToRelation,
      Se.Set Beam.requestNumber requestNumber,
      Se.Set Beam.route route,
      Se.Set Beam.status status,
      Se.Set Beam.storageFeePerDay storageFeePerDay,
      Se.Set Beam.ticketServiceId (Kernel.Types.Id.getId <$> ticketServiceId),
      Se.Set Beam.totalFee totalFee,
      Se.Set Beam.towingFee towingFee,
      Se.Set Beam.towingVehicleId (Kernel.Types.Id.getId <$> towingVehicleId),
      Se.Set Beam.vehicleNumber vehicleNumber,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.violationId (Kernel.Types.Id.getId violationId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.TowingRequest Domain.Types.TowingRequest.TowingRequest where
  fromTType' (Beam.TowingRequestT {..}) = do
    pure $
      Just
        Domain.Types.TowingRequest.TowingRequest
          { agencyEntityId = agencyEntityId,
            agentId = agentId,
            currency = currency,
            evidence = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> x; Data.Aeson.Error _ -> []) evidence,
            id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            notes = notes,
            parkingFine = parkingFine,
            paymentMethod = paymentMethod,
            paymentOrderId = Kernel.Types.Id.Id <$> paymentOrderId,
            paymentReceivedAt = paymentReceivedAt,
            paymentReceivedBy = paymentReceivedBy,
            paymentStatus = paymentStatus,
            pickupLat = pickupLat,
            pickupLon = pickupLon,
            releasedToIdNumber = EncryptedHashed <$> (Encrypted <$> releasedToIdNumberEncrypted) <*> releasedToIdNumberHash,
            releasedToIdType = releasedToIdType,
            releasedToName = releasedToName,
            releasedToPhone = EncryptedHashed <$> (Encrypted <$> releasedToPhoneEncrypted) <*> releasedToPhoneHash,
            releasedToRelation = releasedToRelation,
            requestNumber = requestNumber,
            route = route,
            status = status,
            storageFeePerDay = storageFeePerDay,
            ticketServiceId = Kernel.Types.Id.Id <$> ticketServiceId,
            totalFee = totalFee,
            towingFee = towingFee,
            towingVehicleId = Kernel.Types.Id.Id <$> towingVehicleId,
            vehicleNumber = vehicleNumber,
            vehicleType = vehicleType,
            violationId = Kernel.Types.Id.Id violationId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TowingRequest Domain.Types.TowingRequest.TowingRequest where
  toTType' (Domain.Types.TowingRequest.TowingRequest {..}) = do
    Beam.TowingRequestT
      { Beam.agencyEntityId = agencyEntityId,
        Beam.agentId = agentId,
        Beam.currency = currency,
        Beam.evidence = Data.Aeson.toJSON evidence,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.notes = notes,
        Beam.parkingFine = parkingFine,
        Beam.paymentMethod = paymentMethod,
        Beam.paymentOrderId = Kernel.Types.Id.getId <$> paymentOrderId,
        Beam.paymentReceivedAt = paymentReceivedAt,
        Beam.paymentReceivedBy = paymentReceivedBy,
        Beam.paymentStatus = paymentStatus,
        Beam.pickupLat = pickupLat,
        Beam.pickupLon = pickupLon,
        Beam.releasedToIdNumberEncrypted = releasedToIdNumber <&> unEncrypted . Kernel.External.Encryption.encrypted,
        Beam.releasedToIdNumberHash = releasedToIdNumber <&> Kernel.External.Encryption.hash,
        Beam.releasedToIdType = releasedToIdType,
        Beam.releasedToName = releasedToName,
        Beam.releasedToPhoneEncrypted = releasedToPhone <&> unEncrypted . Kernel.External.Encryption.encrypted,
        Beam.releasedToPhoneHash = releasedToPhone <&> Kernel.External.Encryption.hash,
        Beam.releasedToRelation = releasedToRelation,
        Beam.requestNumber = requestNumber,
        Beam.route = route,
        Beam.status = status,
        Beam.storageFeePerDay = storageFeePerDay,
        Beam.ticketServiceId = Kernel.Types.Id.getId <$> ticketServiceId,
        Beam.totalFee = totalFee,
        Beam.towingFee = towingFee,
        Beam.towingVehicleId = Kernel.Types.Id.getId <$> towingVehicleId,
        Beam.vehicleNumber = vehicleNumber,
        Beam.vehicleType = vehicleType,
        Beam.violationId = Kernel.Types.Id.getId violationId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
