{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SharedEstimate where

import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.SharedEstimate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SharedEstimate as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedEstimate.SharedEstimate -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SharedEstimate.SharedEstimate] -> m ())
createMany = traverse_ create

findActiveEstimates :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedEstimate.EstimateStatus -> m ([Domain.Types.SharedEstimate.SharedEstimate]))
findActiveEstimates status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

findByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedEstimate.EstimateStatus -> m ([Domain.Types.SharedEstimate.SharedEstimate]))
findByStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

findByTransactionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.SharedEstimate.SharedEstimate))
findByTransactionId transactionId = do findOneWithKV [Se.Is Beam.transactionId $ Se.Eq transactionId]

findByVehicleServiceTier :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ServiceTierType.ServiceTierType -> m ([Domain.Types.SharedEstimate.SharedEstimate]))
findByVehicleServiceTier vehicleServiceTier = do findAllWithKV [Se.Is Beam.vehicleServiceTier $ Se.Eq vehicleServiceTier]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedEstimate.EstimateStatus -> Kernel.Types.Id.Id Domain.Types.SharedEstimate.SharedEstimate -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SharedEstimate.SharedEstimate -> m (Maybe Domain.Types.SharedEstimate.SharedEstimate))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedEstimate.SharedEstimate -> m ())
updateByPrimaryKey (Domain.Types.SharedEstimate.SharedEstimate {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.currency currency,
      Se.Set Beam.distanceUnit distanceUnit,
      Se.Set Beam.estimatedDistance estimatedDistance,
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.status status,
      Se.Set Beam.tollNames tollNames,
      Se.Set Beam.totalMaxFare totalMaxFare,
      Se.Set Beam.totalMinFare totalMinFare,
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.tripCategory tripCategory,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleServiceTier vehicleServiceTier
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SharedEstimate Domain.Types.SharedEstimate.SharedEstimate where
  fromTType' (Beam.SharedEstimateT {..}) = do
    pure $
      Just
        Domain.Types.SharedEstimate.SharedEstimate
          { createdAt = createdAt,
            currency = currency,
            distanceUnit = distanceUnit,
            estimatedDistance = estimatedDistance,
            estimatedDuration = estimatedDuration,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            status = status,
            tollNames = tollNames,
            totalMaxFare = totalMaxFare,
            totalMinFare = totalMinFare,
            transactionId = transactionId,
            tripCategory = tripCategory,
            updatedAt = updatedAt,
            validTill = validTill,
            vehicleServiceTier = vehicleServiceTier
          }

instance ToTType' Beam.SharedEstimate Domain.Types.SharedEstimate.SharedEstimate where
  toTType' (Domain.Types.SharedEstimate.SharedEstimate {..}) = do
    Beam.SharedEstimateT
      { Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.distanceUnit = distanceUnit,
        Beam.estimatedDistance = estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.status = status,
        Beam.tollNames = tollNames,
        Beam.totalMaxFare = totalMaxFare,
        Beam.totalMinFare = totalMinFare,
        Beam.transactionId = transactionId,
        Beam.tripCategory = tripCategory,
        Beam.updatedAt = updatedAt,
        Beam.validTill = validTill,
        Beam.vehicleServiceTier = vehicleServiceTier
      }
