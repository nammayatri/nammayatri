{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Estimate where

import qualified Data.Text
import qualified Domain.Types.Estimate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Storage.Beam.Estimate as Beam
import qualified Storage.Queries.EstimateBreakup
import Storage.Queries.Transformers.Estimate

instance FromTType' Beam.Estimate Domain.Types.Estimate.Estimate where
  fromTType' (Beam.EstimateT {..}) = do
    providerUrl' <- Kernel.Prelude.parseBaseUrl providerUrl
    tripTerms' <- mKTripTerms tripTermsId
    estimateBreakupList' <- Storage.Queries.EstimateBreakup.findAllByEstimateIdT (Kernel.Types.Id.Id id)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    pure $
      Just
        Domain.Types.Estimate.Estimate
          { id = Kernel.Types.Id.Id id,
            requestId = Kernel.Types.Id.Id requestId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            bppEstimateId = Kernel.Types.Id.Id bppEstimateId,
            estimatedFare = Kernel.Types.Common.mkPrice currency estimatedFare,
            discount = Kernel.Types.Common.mkPrice currency <$> discount,
            estimatedTotalFare = Kernel.Types.Common.mkPrice currency estimatedTotalFare,
            totalFareRange = mkFareRange currency maxTotalFare minTotalFare,
            estimatedDuration = estimatedDuration,
            estimatedDistance = Kernel.Types.Common.mkDistanceWithDefault distanceUnit estimatedDistanceValue <$> estimatedDistance,
            estimatedPickupDuration = estimatedPickupDuration,
            device = device,
            providerId = providerId,
            providerUrl = providerUrl',
            providerName = providerName,
            providerMobileNumber = providerMobileNumber,
            providerCompletedRidesCount = providerCompletedRidesCount,
            vehicleServiceTierType = vehicleVariant,
            vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
            vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
            isAirConditioned = isAirConditioned,
            itemId = itemId,
            tripTerms = tripTerms',
            estimateBreakupList = estimateBreakupList',
            nightShiftInfo = mkNightShiftInfo nightShiftCharge nightShiftChargeAmount nightShiftEnd nightShiftStart oldNightShiftCharge currency,
            status = status,
            waitingCharges = Domain.Types.Estimate.WaitingCharges $ Kernel.Types.Common.mkPriceWithDefault waitingChargePerMinAmount currency <$> waitingChargePerMin,
            driversLocation = driversLocation,
            specialLocationTag = specialLocationTag,
            specialLocationName = specialLocationName,
            serviceTierName = serviceTierName,
            serviceTierShortDesc = serviceTierShortDesc,
            clientSdkVersion = clientSdkVersion',
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            backendConfigVersion = backendConfigVersion',
            backendAppVersion = backendAppVersion,
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion,
            tollChargesInfo = mkTollChargesInfo tollCharges tollNames currency,
            isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
            isBlockedRoute = isBlockedRoute,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            updatedAt = updatedAt,
            createdAt = createdAt,
            validTill = validTill
          }

instance ToTType' Beam.Estimate Domain.Types.Estimate.Estimate where
  toTType' (Domain.Types.Estimate.Estimate {..}) = do
    Beam.EstimateT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.bppEstimateId = Kernel.Types.Id.getId bppEstimateId,
        Beam.estimatedFare = (.amount) estimatedFare,
        Beam.discount = discount <&> (.amount),
        Beam.estimatedTotalFare = (.amount) estimatedTotalFare,
        Beam.currency = Kernel.Prelude.Just $ (.currency) estimatedFare,
        Beam.maxTotalFare = mkMaxTotalFare totalFareRange,
        Beam.minTotalFare = mkMinTotalFare totalFareRange,
        Beam.estimatedDuration = estimatedDuration,
        Beam.estimatedDistance = Kernel.Types.Common.distanceToHighPrecMeters <$> estimatedDistance,
        Beam.estimatedDistanceValue = Kernel.Types.Common.distanceToHighPrecDistance distanceUnit <$> estimatedDistance,
        Beam.estimatedPickupDuration = estimatedPickupDuration,
        Beam.device = device,
        Beam.providerId = providerId,
        Beam.providerUrl = Kernel.Prelude.showBaseUrl providerUrl,
        Beam.providerName = providerName,
        Beam.providerMobileNumber = providerMobileNumber,
        Beam.providerCompletedRidesCount = providerCompletedRidesCount,
        Beam.vehicleVariant = vehicleServiceTierType,
        Beam.vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
        Beam.vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
        Beam.isAirConditioned = isAirConditioned,
        Beam.itemId = itemId,
        Beam.tripTermsId = Kernel.Types.Id.getId <$> (tripTerms <&> (.id)),
        Beam.nightShiftCharge = mknightShiftCharge nightShiftInfo,
        Beam.nightShiftChargeAmount = mknightShiftChargeAmount nightShiftInfo,
        Beam.nightShiftEnd = nightShiftInfo <&> (.nightShiftEnd),
        Beam.nightShiftStart = nightShiftInfo <&> (.nightShiftStart),
        Beam.oldNightShiftCharge = (.oldNightShiftCharge) =<< nightShiftInfo,
        Beam.status = status,
        Beam.waitingChargePerMin = (.waitingChargePerMin) waitingCharges <&> (.amountInt),
        Beam.waitingChargePerMinAmount = (.waitingChargePerMin) waitingCharges <&> (.amount),
        Beam.driversLocation = driversLocation,
        Beam.specialLocationTag = specialLocationTag,
        Beam.specialLocationName = specialLocationName,
        Beam.serviceTierName = serviceTierName,
        Beam.serviceTierShortDesc = serviceTierShortDesc,
        Beam.clientSdkVersion = fmap Kernel.Utils.Version.versionToText clientSdkVersion,
        Beam.clientBundleVersion = fmap Kernel.Utils.Version.versionToText clientBundleVersion,
        Beam.clientConfigVersion = fmap Kernel.Utils.Version.versionToText clientConfigVersion,
        Beam.backendConfigVersion = fmap Kernel.Utils.Version.versionToText backendConfigVersion,
        Beam.backendAppVersion = backendAppVersion,
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.tollCharges = tollChargesInfo <&> ((.amount) . (.tollCharges)),
        Beam.tollNames = tollChargesInfo <&> (.tollNames),
        Beam.isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
        Beam.isBlockedRoute = isBlockedRoute,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.updatedAt = updatedAt,
        Beam.createdAt = createdAt,
        Beam.validTill = validTill
      }
