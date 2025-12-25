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
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    estimateBreakupList' <- Storage.Queries.EstimateBreakup.findAllByEstimateIdT (Kernel.Types.Id.Id id)
    providerUrl' <- Kernel.Prelude.parseBaseUrl providerUrl
    tripTerms' <- mKTripTerms tripTermsId
    vehicleIconUrl' <- Kernel.Prelude.maybe (return Kernel.Prelude.Nothing) (Kernel.Prelude.fmap Kernel.Prelude.Just . parseBaseUrl) vehicleIconUrl
    pure $
      Just
        Domain.Types.Estimate.Estimate
          { backendAppVersion = backendAppVersion,
            backendConfigVersion = backendConfigVersion',
            boostSearchPreSelectionServiceTierConfig = boostSearchPreSelectionServiceTierConfig,
            bppEstimateId = Kernel.Types.Id.Id bppEstimateId,
            businessDiscountInfo = mkBusinessDiscountInfo businessDiscount businessDiscountPercentage currency,
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion clientModelName clientManufacturer,
            clientSdkVersion = clientSdkVersion',
            createdAt = createdAt,
            device = device,
            discount = Kernel.Types.Common.mkPrice currency <$> discount,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driversLocation = driversLocation,
            estimateBreakupList = estimateBreakupList',
            estimateTags = estimateTags,
            estimatedDistance = Kernel.Types.Common.mkDistanceWithDefault distanceUnit estimatedDistanceValue <$> estimatedDistance,
            estimatedDuration = estimatedDuration,
            estimatedFare = Kernel.Types.Common.mkPrice currency estimatedFare,
            estimatedPickupDuration = estimatedPickupDuration,
            estimatedStaticDuration = estimatedStaticDuration,
            estimatedTotalFare = Kernel.Types.Common.mkPrice currency estimatedTotalFare,
            id = Kernel.Types.Id.Id id,
            insuredAmount = insuredAmount,
            isAirConditioned = isAirConditioned,
            isBlockedRoute = isBlockedRoute,
            isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
            isInsured = Kernel.Prelude.fromMaybe False isInsured,
            isMultimodalSearch = isMultimodalSearch,
            itemId = itemId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            nightShiftInfo = mkNightShiftInfo nightShiftCharge nightShiftChargeAmount nightShiftEnd nightShiftStart oldNightShiftCharge currency,
            providerCompletedRidesCount = providerCompletedRidesCount,
            providerId = providerId,
            providerMobileNumber = providerMobileNumber,
            providerName = providerName,
            providerUrl = providerUrl',
            qar = qar,
            requestId = Kernel.Types.Id.Id requestId,
            serviceTierName = serviceTierName,
            serviceTierShortDesc = serviceTierShortDesc,
            smartTipReason = smartTipReason,
            smartTipSuggestion = smartTipSuggestion,
            specialLocationName = specialLocationName,
            specialLocationTag = specialLocationTag,
            status = status,
            tipOptions = tipOptions,
            tollChargesInfo = mkTollChargesInfo tollCharges tollNames currency,
            totalFareRange = mkFareRange currency maxTotalFare minTotalFare,
            tripCategory = tripCategory,
            tripTerms = tripTerms',
            updatedAt = updatedAt,
            validTill = validTill,
            vehicleCategory = vehicleCategory,
            vehicleIconUrl = vehicleIconUrl',
            vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
            vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
            vehicleServiceTierType = vehicleVariant,
            waitingCharges = Domain.Types.Estimate.WaitingCharges $ Kernel.Types.Common.mkPriceWithDefault waitingChargePerMinAmount currency <$> waitingChargePerMin
          }

instance ToTType' Beam.Estimate Domain.Types.Estimate.Estimate where
  toTType' (Domain.Types.Estimate.Estimate {..}) = do
    Beam.EstimateT
      { Beam.backendAppVersion = backendAppVersion,
        Beam.backendConfigVersion = fmap Kernel.Utils.Version.versionToText backendConfigVersion,
        Beam.boostSearchPreSelectionServiceTierConfig = boostSearchPreSelectionServiceTierConfig,
        Beam.bppEstimateId = Kernel.Types.Id.getId bppEstimateId,
        Beam.businessDiscount = businessDiscountInfo <&> ((.amount) . (.businessDiscount)),
        Beam.businessDiscountPercentage = businessDiscountInfo <&> (.businessDiscountPercentage),
        Beam.clientBundleVersion = fmap Kernel.Utils.Version.versionToText clientBundleVersion,
        Beam.clientConfigVersion = fmap Kernel.Utils.Version.versionToText clientConfigVersion,
        Beam.clientManufacturer = clientDevice >>= (.deviceManufacturer),
        Beam.clientModelName = clientDevice <&> (.deviceModel),
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.clientSdkVersion = fmap Kernel.Utils.Version.versionToText clientSdkVersion,
        Beam.createdAt = createdAt,
        Beam.device = device,
        Beam.discount = discount <&> (.amount),
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driversLocation = driversLocation,
        Beam.estimateTags = estimateTags,
        Beam.estimatedDistance = Kernel.Types.Common.distanceToHighPrecMeters <$> estimatedDistance,
        Beam.estimatedDistanceValue = Kernel.Types.Common.distanceToHighPrecDistance distanceUnit <$> estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.estimatedFare = (.amount) estimatedFare,
        Beam.estimatedPickupDuration = estimatedPickupDuration,
        Beam.estimatedStaticDuration = estimatedStaticDuration,
        Beam.estimatedTotalFare = (.amount) estimatedTotalFare,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.insuredAmount = insuredAmount,
        Beam.isAirConditioned = isAirConditioned,
        Beam.isBlockedRoute = isBlockedRoute,
        Beam.isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
        Beam.isInsured = Kernel.Prelude.Just isInsured,
        Beam.isMultimodalSearch = isMultimodalSearch,
        Beam.itemId = itemId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.nightShiftCharge = mknightShiftCharge nightShiftInfo,
        Beam.nightShiftChargeAmount = mknightShiftChargeAmount nightShiftInfo,
        Beam.nightShiftEnd = nightShiftInfo <&> (.nightShiftEnd),
        Beam.nightShiftStart = nightShiftInfo <&> (.nightShiftStart),
        Beam.oldNightShiftCharge = (.oldNightShiftCharge) =<< nightShiftInfo,
        Beam.providerCompletedRidesCount = providerCompletedRidesCount,
        Beam.providerId = providerId,
        Beam.providerMobileNumber = providerMobileNumber,
        Beam.providerName = providerName,
        Beam.providerUrl = Kernel.Prelude.showBaseUrl providerUrl,
        Beam.qar = qar,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.serviceTierName = serviceTierName,
        Beam.serviceTierShortDesc = serviceTierShortDesc,
        Beam.smartTipReason = smartTipReason,
        Beam.smartTipSuggestion = smartTipSuggestion,
        Beam.specialLocationName = specialLocationName,
        Beam.specialLocationTag = specialLocationTag,
        Beam.status = status,
        Beam.tipOptions = tipOptions,
        Beam.tollCharges = tollChargesInfo <&> ((.amount) . (.tollCharges)),
        Beam.tollNames = tollChargesInfo <&> (.tollNames),
        Beam.currency = Kernel.Prelude.Just $ (.currency) estimatedFare,
        Beam.maxTotalFare = mkMaxTotalFare totalFareRange,
        Beam.minTotalFare = mkMinTotalFare totalFareRange,
        Beam.tripCategory = tripCategory,
        Beam.tripTermsId = Kernel.Types.Id.getId <$> (tripTerms <&> (.id)),
        Beam.updatedAt = updatedAt,
        Beam.validTill = validTill,
        Beam.vehicleCategory = vehicleCategory,
        Beam.vehicleIconUrl = Kernel.Prelude.fmap showBaseUrl vehicleIconUrl,
        Beam.vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
        Beam.vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
        Beam.vehicleVariant = vehicleServiceTierType,
        Beam.waitingChargePerMin = (.waitingChargePerMin) waitingCharges <&> (.amountInt),
        Beam.waitingChargePerMinAmount = (.waitingChargePerMin) waitingCharges <&> (.amount)
      }
