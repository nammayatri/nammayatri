{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SharedEstimate where

import qualified Domain.Types.SharedEstimate
import qualified Domain.Types.SharedSearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.SharedEstimate as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedEstimate.SharedEstimate -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SharedEstimate.SharedEstimate] -> m ())
createMany = traverse_ create

findAllBySharedSearchRequestId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.SharedSearchRequest.SharedSearchRequest -> m ([Domain.Types.SharedEstimate.SharedEstimate]))
findAllBySharedSearchRequestId sharedSearchRequestId = do findAllWithKV [Se.Is Beam.sharedSearchRequestId $ Se.Eq (Kernel.Types.Id.getId sharedSearchRequestId)]

findByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedEstimate.EstimateStatus -> m ([Domain.Types.SharedEstimate.SharedEstimate]))
findByStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedEstimate.EstimateStatus -> Kernel.Types.Id.Id Domain.Types.SharedEstimate.SharedEstimate -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SharedEstimate.SharedEstimate -> m (Maybe Domain.Types.SharedEstimate.SharedEstimate))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedEstimate.SharedEstimate -> m ())
updateByPrimaryKey (Domain.Types.SharedEstimate.SharedEstimate {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppSharedEstimateId bppSharedEstimateId,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.estimateIds (Kernel.Types.Id.getId <$> estimateIds),
      Se.Set Beam.distanceUnit ((estimatedDistance <&> (.unit))),
      Se.Set Beam.estimatedDistance ((Kernel.Types.Common.distanceToHighPrecMeters <$> estimatedDistance)),
      Se.Set Beam.estimatedDistanceValue ((Kernel.Types.Common.distanceToHighPrecDistance <$> (estimatedDistance <&> (.unit)) <*> estimatedDistance)),
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.estimatedTotalFare estimatedTotalFare,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.nightShiftCharge nightShiftCharge,
      Se.Set Beam.nightShiftChargeAmount nightShiftChargeAmount,
      Se.Set Beam.nightShiftEnd nightShiftEnd,
      Se.Set Beam.nightShiftStart nightShiftStart,
      Se.Set Beam.oldNightShiftCharge oldNightShiftCharge,
      Se.Set Beam.providerId providerId,
      Se.Set Beam.providerName providerName,
      Se.Set Beam.providerUrl (Kernel.Prelude.showBaseUrl providerUrl),
      Se.Set Beam.serviceTierName serviceTierName,
      Se.Set Beam.sharedSearchRequestId (Kernel.Types.Id.getId sharedSearchRequestId),
      Se.Set Beam.status status,
      Se.Set Beam.totalFareRangeMax totalFareRangeMax,
      Se.Set Beam.totalFareRangeMin totalFareRangeMin,
      Se.Set Beam.tripCategory tripCategory,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleServiceTierSeatingCapacity vehicleServiceTierSeatingCapacity,
      Se.Set Beam.vehicleVariant (vehicleServiceTierType)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SharedEstimate Domain.Types.SharedEstimate.SharedEstimate where
  fromTType' (Beam.SharedEstimateT {..}) = do
    providerUrl' <- Kernel.Prelude.parseBaseUrl providerUrl
    pure $
      Just
        Domain.Types.SharedEstimate.SharedEstimate
          { bppSharedEstimateId = bppSharedEstimateId,
            createdAt = createdAt,
            estimateIds = Kernel.Types.Id.Id <$> estimateIds,
            estimatedDistance = (Kernel.Utils.Common.mkDistanceWithDefault distanceUnit estimatedDistanceValue <$> estimatedDistance),
            estimatedDuration = estimatedDuration,
            estimatedTotalFare = estimatedTotalFare,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            nightShiftCharge = nightShiftCharge,
            nightShiftChargeAmount = nightShiftChargeAmount,
            nightShiftEnd = nightShiftEnd,
            nightShiftStart = nightShiftStart,
            oldNightShiftCharge = oldNightShiftCharge,
            providerId = providerId,
            providerName = providerName,
            providerUrl = providerUrl',
            serviceTierName = serviceTierName,
            sharedSearchRequestId = Kernel.Types.Id.Id sharedSearchRequestId,
            status = status,
            totalFareRangeMax = totalFareRangeMax,
            totalFareRangeMin = totalFareRangeMin,
            tripCategory = tripCategory,
            updatedAt = updatedAt,
            validTill = validTill,
            vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
            vehicleServiceTierType = vehicleVariant
          }

instance ToTType' Beam.SharedEstimate Domain.Types.SharedEstimate.SharedEstimate where
  toTType' (Domain.Types.SharedEstimate.SharedEstimate {..}) = do
    Beam.SharedEstimateT
      { Beam.bppSharedEstimateId = bppSharedEstimateId,
        Beam.createdAt = createdAt,
        Beam.estimateIds = Kernel.Types.Id.getId <$> estimateIds,
        Beam.distanceUnit = (estimatedDistance <&> (.unit)),
        Beam.estimatedDistance = (Kernel.Types.Common.distanceToHighPrecMeters <$> estimatedDistance),
        Beam.estimatedDistanceValue = (Kernel.Types.Common.distanceToHighPrecDistance <$> (estimatedDistance <&> (.unit)) <*> estimatedDistance),
        Beam.estimatedDuration = estimatedDuration,
        Beam.estimatedTotalFare = estimatedTotalFare,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.nightShiftCharge = nightShiftCharge,
        Beam.nightShiftChargeAmount = nightShiftChargeAmount,
        Beam.nightShiftEnd = nightShiftEnd,
        Beam.nightShiftStart = nightShiftStart,
        Beam.oldNightShiftCharge = oldNightShiftCharge,
        Beam.providerId = providerId,
        Beam.providerName = providerName,
        Beam.providerUrl = Kernel.Prelude.showBaseUrl providerUrl,
        Beam.serviceTierName = serviceTierName,
        Beam.sharedSearchRequestId = Kernel.Types.Id.getId sharedSearchRequestId,
        Beam.status = status,
        Beam.totalFareRangeMax = totalFareRangeMax,
        Beam.totalFareRangeMin = totalFareRangeMin,
        Beam.tripCategory = tripCategory,
        Beam.updatedAt = updatedAt,
        Beam.validTill = validTill,
        Beam.vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
        Beam.vehicleVariant = vehicleServiceTierType
      }
