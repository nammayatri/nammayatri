{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Estimate (module Storage.Queries.Estimate, module ReExport) where

import qualified Domain.Types.Estimate
import qualified Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as Beam
import Storage.Queries.EstimateExtra as ReExport
import Storage.Queries.Transformers.Estimate

findAllBySRId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ([Domain.Types.Estimate.Estimate]))
findAllBySRId (Kernel.Types.Id.Id requestId) = do findAllWithKV [Se.Is Beam.requestId $ Se.Eq requestId]

findByBPPEstimateId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Estimate.BPPEstimate -> m (Maybe Domain.Types.Estimate.Estimate))
findByBPPEstimateId (Kernel.Types.Id.Id bppEstimateId) = do findOneWithKV [Se.Is Beam.bppEstimateId $ Se.Eq bppEstimateId]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> m (Maybe Domain.Types.Estimate.Estimate))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Estimate.EstimateStatus -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> m ())
updateStatus status (Kernel.Types.Id.Id id) = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.updatedAt _now, Se.Set Beam.status status] [Se.Is Beam.id $ Se.Eq id]

updateStatusByRequestId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Estimate.EstimateStatus -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ())
updateStatusByRequestId status (Kernel.Types.Id.Id requestId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.updatedAt _now, Se.Set Beam.status status] [Se.Is Beam.requestId $ Se.Eq requestId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> m (Maybe Domain.Types.Estimate.Estimate))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Estimate.Estimate -> m ())
updateByPrimaryKey (Domain.Types.Estimate.Estimate {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppEstimateId (Kernel.Types.Id.getId bppEstimateId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.device device,
      Se.Set Beam.discount (discount <&> (.amount)),
      Se.Set Beam.driversLocation driversLocation,
      Se.Set Beam.distanceUnit (estimatedDistance <&> (.unit)),
      Se.Set Beam.estimatedDistance (Kernel.Types.Common.distanceToHighPrecMeters <$> estimatedDistance),
      Se.Set Beam.estimatedDistanceValue (Kernel.Types.Common.distanceToHighPrecDistance (estimatedDistance <&> (.unit)) <$> estimatedDistance),
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.estimatedFare ((.amount) estimatedFare),
      Se.Set Beam.estimatedTotalFare ((.amount) estimatedTotalFare),
      Se.Set Beam.itemId itemId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.nightShiftCharge (mknightShiftCharge nightShiftInfo),
      Se.Set Beam.nightShiftChargeAmount (mknightShiftChargeAmount nightShiftInfo),
      Se.Set Beam.nightShiftEnd (nightShiftInfo <&> (.nightShiftEnd)),
      Se.Set Beam.nightShiftStart (nightShiftInfo <&> (.nightShiftStart)),
      Se.Set Beam.oldNightShiftCharge ((.oldNightShiftCharge) =<< nightShiftInfo),
      Se.Set Beam.providerCompletedRidesCount providerCompletedRidesCount,
      Se.Set Beam.providerId providerId,
      Se.Set Beam.providerMobileNumber providerMobileNumber,
      Se.Set Beam.providerName providerName,
      Se.Set Beam.providerUrl (Kernel.Prelude.showBaseUrl providerUrl),
      Se.Set Beam.requestId (Kernel.Types.Id.getId requestId),
      Se.Set Beam.serviceTierName serviceTierName,
      Se.Set Beam.serviceTierShortDesc serviceTierShortDesc,
      Se.Set Beam.specialLocationTag specialLocationTag,
      Se.Set Beam.status status,
      Se.Set Beam.currency (Kernel.Prelude.Just $ (.currency) estimatedFare),
      Se.Set Beam.maxTotalFare (mkMaxTotalFare totalFareRange),
      Se.Set Beam.minTotalFare (mkMinTotalFare totalFareRange),
      Se.Set Beam.tripTermsId (Kernel.Types.Id.getId <$> (tripTerms <&> (.id))),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleVariant vehicleServiceTierType,
      Se.Set Beam.waitingChargePerMin ((.waitingChargePerMin) waitingCharges <&> (.amountInt)),
      Se.Set Beam.waitingChargePerMinAmount ((.waitingChargePerMin) waitingCharges <&> (.amount))
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
