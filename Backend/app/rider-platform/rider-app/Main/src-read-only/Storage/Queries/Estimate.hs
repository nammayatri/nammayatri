{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Estimate (module Storage.Queries.Estimate, module ReExport) where

import qualified Domain.Types.Estimate
import qualified Domain.Types.EstimateStatus
import qualified Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as Beam
import Storage.Queries.EstimateExtra as ReExport
import Storage.Queries.Transformers.Estimate

findAllBySRId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m [Domain.Types.Estimate.Estimate])
findAllBySRId requestId = do findAllWithKVAndConditionalDB [Se.Is Beam.requestId $ Se.Eq (Kernel.Types.Id.getId requestId)] Nothing

findByBPPEstimateId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Estimate.BPPEstimate -> m (Maybe Domain.Types.Estimate.Estimate))
findByBPPEstimateId bppEstimateId = do findOneWithKV [Se.Is Beam.bppEstimateId $ Se.Eq (Kernel.Types.Id.getId bppEstimateId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> m (Maybe Domain.Types.Estimate.Estimate))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findBySRIdAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.EstimateStatus.EstimateStatus -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m (Maybe Domain.Types.Estimate.Estimate))
findBySRIdAndStatus status requestId = do findOneWithKV [Se.And [Se.Is Beam.status $ Se.Eq status, Se.Is Beam.requestId $ Se.Eq (Kernel.Types.Id.getId requestId)]]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.EstimateStatus.EstimateStatus -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.updatedAt _now, Se.Set Beam.status status] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> m (Maybe Domain.Types.Estimate.Estimate))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Estimate.Estimate -> m ())
updateByPrimaryKey (Domain.Types.Estimate.Estimate {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.backendAppVersion backendAppVersion,
      Se.Set Beam.backendConfigVersion (fmap Kernel.Utils.Version.versionToText backendConfigVersion),
      Se.Set Beam.boostSearchPreSelectionServiceTierConfig boostSearchPreSelectionServiceTierConfig,
      Se.Set Beam.bppEstimateId (Kernel.Types.Id.getId bppEstimateId),
      Se.Set Beam.clientBundleVersion (fmap Kernel.Utils.Version.versionToText clientBundleVersion),
      Se.Set Beam.clientConfigVersion (fmap Kernel.Utils.Version.versionToText clientConfigVersion),
      Se.Set Beam.clientManufacturer (clientDevice >>= (.deviceManufacturer)),
      Se.Set Beam.clientModelName (clientDevice <&> (.deviceModel)),
      Se.Set Beam.clientOsType (clientDevice <&> (.deviceType)),
      Se.Set Beam.clientOsVersion (clientDevice <&> (.deviceVersion)),
      Se.Set Beam.clientSdkVersion (fmap Kernel.Utils.Version.versionToText clientSdkVersion),
      Se.Set Beam.device device,
      Se.Set Beam.discount (discount <&> (.amount)),
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.driversLocation driversLocation,
      Se.Set Beam.estimateTags estimateTags,
      Se.Set Beam.estimatedDistance (Kernel.Types.Common.distanceToHighPrecMeters <$> estimatedDistance),
      Se.Set Beam.estimatedDistanceValue (Kernel.Types.Common.distanceToHighPrecDistance distanceUnit <$> estimatedDistance),
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.estimatedFare ((.amount) estimatedFare),
      Se.Set Beam.estimatedPickupDuration estimatedPickupDuration,
      Se.Set Beam.estimatedStaticDuration estimatedStaticDuration,
      Se.Set Beam.estimatedTotalFare ((.amount) estimatedTotalFare),
      Se.Set Beam.insuredAmount insuredAmount,
      Se.Set Beam.isAirConditioned isAirConditioned,
      Se.Set Beam.isBlockedRoute isBlockedRoute,
      Se.Set Beam.isCustomerPrefferedSearchRoute isCustomerPrefferedSearchRoute,
      Se.Set Beam.isInsured (Kernel.Prelude.Just isInsured),
      Se.Set Beam.isMultimodalSearch isMultimodalSearch,
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
      Se.Set Beam.qar qar,
      Se.Set Beam.requestId (Kernel.Types.Id.getId requestId),
      Se.Set Beam.serviceTierName serviceTierName,
      Se.Set Beam.serviceTierShortDesc serviceTierShortDesc,
      Se.Set Beam.smartTipReason smartTipReason,
      Se.Set Beam.smartTipSuggestion smartTipSuggestion,
      Se.Set Beam.specialLocationName specialLocationName,
      Se.Set Beam.specialLocationTag specialLocationTag,
      Se.Set Beam.status status,
      Se.Set Beam.tipOptions tipOptions,
      Se.Set Beam.tollCharges (tollChargesInfo <&> ((.amount) . (.tollCharges))),
      Se.Set Beam.tollNames (tollChargesInfo <&> (.tollNames)),
      Se.Set Beam.currency (Kernel.Prelude.Just $ (.currency) estimatedFare),
      Se.Set Beam.maxTotalFare (mkMaxTotalFare totalFareRange),
      Se.Set Beam.minTotalFare (mkMinTotalFare totalFareRange),
      Se.Set Beam.tripCategory tripCategory,
      Se.Set Beam.tripTermsId (Kernel.Types.Id.getId <$> (tripTerms <&> (.id))),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.vehicleIconUrl (Kernel.Prelude.fmap showBaseUrl vehicleIconUrl),
      Se.Set Beam.vehicleServiceTierAirConditioned vehicleServiceTierAirConditioned,
      Se.Set Beam.vehicleServiceTierSeatingCapacity vehicleServiceTierSeatingCapacity,
      Se.Set Beam.vehicleVariant vehicleServiceTierType,
      Se.Set Beam.waitingChargePerMin ((.waitingChargePerMin) waitingCharges <&> (.amountInt)),
      Se.Set Beam.waitingChargePerMinAmount ((.waitingChargePerMin) waitingCharges <&> (.amount))
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
