{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Estimate where

import qualified Domain.Types.Estimate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Estimate as Beam
import qualified Storage.Queries.EstimateBreakup
import Storage.Queries.Transformers.Estimate

instance FromTType' Beam.Estimate Domain.Types.Estimate.Estimate where
  fromTType' (Beam.EstimateT {..}) = do
    estimateBreakupList' <- Storage.Queries.EstimateBreakup.findAllByEstimateIdT (Kernel.Types.Id.Id id)
    providerUrl' <- Kernel.Prelude.parseBaseUrl providerUrl
    tripTerms' <- mKTripTerms tripTermsId
    pure $
      Just
        Domain.Types.Estimate.Estimate
          { bppEstimateId = Kernel.Types.Id.Id bppEstimateId,
            createdAt = createdAt,
            device = device,
            discount = Kernel.Types.Common.mkPrice currency <$> discount,
            driversLocation = driversLocation,
            estimateBreakupList = estimateBreakupList',
            estimatedDistance = Kernel.Types.Common.mkDistanceWithDefault distanceUnit estimatedDistanceValue <$> estimatedDistance,
            estimatedDuration = estimatedDuration,
            estimatedFare = Kernel.Types.Common.mkPrice currency estimatedFare,
            estimatedTotalFare = Kernel.Types.Common.mkPrice currency estimatedTotalFare,
            id = Kernel.Types.Id.Id id,
            itemId = itemId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            nightShiftInfo = mkNightShiftInfo nightShiftCharge nightShiftChargeAmount nightShiftEnd nightShiftStart oldNightShiftCharge currency,
            providerCompletedRidesCount = providerCompletedRidesCount,
            providerId = providerId,
            providerMobileNumber = providerMobileNumber,
            providerName = providerName,
            providerUrl = providerUrl',
            requestId = Kernel.Types.Id.Id requestId,
            serviceTierName = serviceTierName,
            serviceTierShortDesc = serviceTierShortDesc,
            specialLocationTag = specialLocationTag,
            status = status,
            totalFareRange = mkFareRange currency maxTotalFare minTotalFare,
            tripTerms = tripTerms',
            updatedAt = updatedAt,
            validTill = validTill,
            vehicleServiceTierType = vehicleVariant,
            waitingCharges = Domain.Types.Estimate.WaitingCharges $ Kernel.Types.Common.mkPriceWithDefault waitingChargePerMinAmount currency <$> waitingChargePerMin
          }

instance ToTType' Beam.Estimate Domain.Types.Estimate.Estimate where
  toTType' (Domain.Types.Estimate.Estimate {..}) = do
    Beam.EstimateT
      { Beam.bppEstimateId = Kernel.Types.Id.getId bppEstimateId,
        Beam.createdAt = createdAt,
        Beam.device = device,
        Beam.discount = (discount <&> (.amount)),
        Beam.driversLocation = driversLocation,
        Beam.distanceUnit = (estimatedDistance <&> (.unit)),
        Beam.estimatedDistance = (Kernel.Types.Common.distanceToHighPrecMeters <$> estimatedDistance),
        Beam.estimatedDistanceValue = (Kernel.Types.Common.distanceToHighPrecDistance (estimatedDistance <&> (.unit)) <$> estimatedDistance),
        Beam.estimatedDuration = estimatedDuration,
        Beam.estimatedFare = ((.amount) estimatedFare),
        Beam.estimatedTotalFare = ((.amount) estimatedTotalFare),
        Beam.id = Kernel.Types.Id.getId id,
        Beam.itemId = itemId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.nightShiftCharge = (mknightShiftCharge nightShiftInfo),
        Beam.nightShiftChargeAmount = (mknightShiftChargeAmount nightShiftInfo),
        Beam.nightShiftEnd = (nightShiftInfo <&> (.nightShiftEnd)),
        Beam.nightShiftStart = (nightShiftInfo <&> (.nightShiftStart)),
        Beam.oldNightShiftCharge = ((.oldNightShiftCharge) =<< nightShiftInfo),
        Beam.providerCompletedRidesCount = providerCompletedRidesCount,
        Beam.providerId = providerId,
        Beam.providerMobileNumber = providerMobileNumber,
        Beam.providerName = providerName,
        Beam.providerUrl = Kernel.Prelude.showBaseUrl providerUrl,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.serviceTierName = serviceTierName,
        Beam.serviceTierShortDesc = serviceTierShortDesc,
        Beam.specialLocationTag = specialLocationTag,
        Beam.status = status,
        Beam.currency = (Kernel.Prelude.Just $ (.currency) estimatedFare),
        Beam.maxTotalFare = (mkMaxTotalFare totalFareRange),
        Beam.minTotalFare = (mkMinTotalFare totalFareRange),
        Beam.tripTermsId = (Kernel.Types.Id.getId <$> (tripTerms <&> (.id))),
        Beam.updatedAt = updatedAt,
        Beam.validTill = validTill,
        Beam.vehicleVariant = vehicleServiceTierType,
        Beam.waitingChargePerMin = ((.waitingChargePerMin) waitingCharges <&> (.amountInt)),
        Beam.waitingChargePerMinAmount = ((.waitingChargePerMin) waitingCharges <&> (.amount))
      }
