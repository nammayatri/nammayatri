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
import qualified Kernel.Types.Price
import qualified Kernel.Types.Version
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Storage.Beam.Estimate as Beam
import Storage.Queries.Transformers.Estimate
import qualified Storage.Queries.Transformers.Estimate

instance FromTType' Beam.Estimate Domain.Types.Estimate.Estimate where
  fromTType' (Beam.EstimateT {..}) = do
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    estimateBreakupList' <- Storage.Queries.Transformers.Estimate.loadEstimateBreakupList estimateBreakupListJson (Kernel.Types.Id.Id id)
    providerUrl' <- Kernel.Prelude.parseBaseUrl providerUrl
    vehicleIconUrl' <- Kernel.Prelude.maybe (return Kernel.Prelude.Nothing) (Kernel.Prelude.fmap Kernel.Prelude.Just . parseBaseUrl) vehicleIconUrl
    pure $
      Just
        Domain.Types.Estimate.Estimate
          { area = area,
            backendAppVersion = backendAppVersion,
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
            driverCancellationNotAllowed = driverCancellationNotAllowed,
            driversLocation = driversLocation,
            estimateBreakupList = estimateBreakupList',
            estimatedDistance = Kernel.Types.Common.mkDistanceWithDefault distanceUnit estimatedDistanceValue <$> estimatedDistance,
            estimatedDuration = estimatedDuration,
            estimatedFare = Kernel.Types.Common.mkPrice currency estimatedFare,
            estimatedPickupDuration = estimatedPickupDuration,
            estimatedStaticDuration = estimatedStaticDuration,
            estimatedTotalFare = Kernel.Types.Common.mkPrice currency estimatedTotalFare,
            fareSettlementType = fareSettlementType,
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
            navigationInstruction = navigationInstruction,
            negativeFareSuggestion = negativeFareSuggestion,
            nightShiftInfo = mkNightShiftInfo nightShiftCharge nightShiftChargeAmount nightShiftEnd nightShiftStart oldNightShiftCharge currency,
            personalDiscountInfo = mkPersonalDiscountInfo personalDiscount personalDiscountPercentage currency,
            providerCompletedRidesCount = providerCompletedRidesCount,
            providerId = providerId,
            providerMobileNumber = providerMobileNumber,
            providerName = providerName,
            providerUrl = providerUrl',
            qar = qar,
            requestId = Kernel.Types.Id.Id requestId,
            selectedOfferId = selectedOfferId,
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
            updatedAt = updatedAt,
            validTill = validTill,
            vehicleCategory = vehicleCategory,
            vehicleIconUrl = vehicleIconUrl',
            vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
            vehicleServiceTierLuggageCapacity = vehicleServiceTierLuggageCapacity,
            vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
            vehicleServiceTierType = vehicleVariant,
            waitingCharges = Domain.Types.Estimate.WaitingCharges $ Kernel.Types.Common.mkPriceWithDefault waitingChargePerMinAmount currency <$> waitingChargePerMin
          }

instance ToTType' Beam.Estimate Domain.Types.Estimate.Estimate where
  toTType' (Domain.Types.Estimate.Estimate {..}) = do
    Beam.EstimateT
      { Beam.area = area,
        Beam.backendAppVersion = backendAppVersion,
        Beam.backendConfigVersion = fmap Kernel.Utils.Version.versionToText backendConfigVersion,
        Beam.boostSearchPreSelectionServiceTierConfig = boostSearchPreSelectionServiceTierConfig,
        Beam.bppEstimateId = Kernel.Types.Id.getId bppEstimateId,
        Beam.businessDiscount = businessDiscountInfo <&> (Kernel.Types.Price.getPriceAmount . Domain.Types.Estimate.businessDiscount),
        Beam.businessDiscountPercentage = businessDiscountInfo <&> Domain.Types.Estimate.businessDiscountPercentage,
        Beam.clientBundleVersion = fmap Kernel.Utils.Version.versionToText clientBundleVersion,
        Beam.clientConfigVersion = fmap Kernel.Utils.Version.versionToText clientConfigVersion,
        Beam.clientManufacturer = clientDevice >>= Kernel.Types.Version.deviceManufacturer,
        Beam.clientModelName = clientDevice <&> Kernel.Types.Version.deviceModel,
        Beam.clientOsType = clientDevice <&> Kernel.Types.Version.deviceType,
        Beam.clientOsVersion = clientDevice <&> Kernel.Types.Version.deviceVersion,
        Beam.clientSdkVersion = fmap Kernel.Utils.Version.versionToText clientSdkVersion,
        Beam.createdAt = createdAt,
        Beam.device = device,
        Beam.discount = discount <&> Kernel.Types.Price.getPriceAmount,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverCancellationNotAllowed = driverCancellationNotAllowed,
        Beam.driversLocation = driversLocation,
        Beam.estimateBreakupListJson = Storage.Queries.Transformers.Estimate.encodeEstimateBreakupList estimateBreakupList,
        Beam.estimatedDistance = Kernel.Types.Common.distanceToHighPrecMeters <$> estimatedDistance,
        Beam.estimatedDistanceValue = Kernel.Types.Common.distanceToHighPrecDistance distanceUnit <$> estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.estimatedFare = Kernel.Types.Price.getPriceAmount estimatedFare,
        Beam.estimatedPickupDuration = estimatedPickupDuration,
        Beam.estimatedStaticDuration = estimatedStaticDuration,
        Beam.estimatedTotalFare = Kernel.Types.Price.getPriceAmount estimatedTotalFare,
        Beam.fareSettlementType = fareSettlementType,
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
        Beam.navigationInstruction = navigationInstruction,
        Beam.negativeFareSuggestion = negativeFareSuggestion,
        Beam.nightShiftCharge = mknightShiftCharge nightShiftInfo,
        Beam.nightShiftChargeAmount = mknightShiftChargeAmount nightShiftInfo,
        Beam.nightShiftEnd = nightShiftInfo <&> Domain.Types.Estimate.nightShiftEnd,
        Beam.nightShiftStart = nightShiftInfo <&> Domain.Types.Estimate.nightShiftStart,
        Beam.oldNightShiftCharge = Domain.Types.Estimate.oldNightShiftCharge =<< nightShiftInfo,
        Beam.personalDiscount = personalDiscountInfo <&> (Kernel.Types.Price.getPriceAmount . Domain.Types.Estimate.personalDiscount),
        Beam.personalDiscountPercentage = personalDiscountInfo <&> Domain.Types.Estimate.personalDiscountPercentage,
        Beam.providerCompletedRidesCount = providerCompletedRidesCount,
        Beam.providerId = providerId,
        Beam.providerMobileNumber = providerMobileNumber,
        Beam.providerName = providerName,
        Beam.providerUrl = Kernel.Prelude.showBaseUrl providerUrl,
        Beam.qar = qar,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.selectedOfferId = selectedOfferId,
        Beam.serviceTierName = serviceTierName,
        Beam.serviceTierShortDesc = serviceTierShortDesc,
        Beam.smartTipReason = smartTipReason,
        Beam.smartTipSuggestion = smartTipSuggestion,
        Beam.specialLocationName = specialLocationName,
        Beam.specialLocationTag = specialLocationTag,
        Beam.status = status,
        Beam.tipOptions = tipOptions,
        Beam.tollCharges = tollChargesInfo <&> (Kernel.Types.Price.getPriceAmount . Domain.Types.Estimate.tollCharges),
        Beam.tollNames = tollChargesInfo <&> Domain.Types.Estimate.tollNames,
        Beam.currency = Kernel.Prelude.Just $ Kernel.Types.Price.getPriceCurrency estimatedFare,
        Beam.maxTotalFare = mkMaxTotalFare totalFareRange,
        Beam.minTotalFare = mkMinTotalFare totalFareRange,
        Beam.tripCategory = tripCategory,
        Beam.updatedAt = updatedAt,
        Beam.validTill = validTill,
        Beam.vehicleCategory = vehicleCategory,
        Beam.vehicleIconUrl = Kernel.Prelude.fmap showBaseUrl vehicleIconUrl,
        Beam.vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
        Beam.vehicleServiceTierLuggageCapacity = vehicleServiceTierLuggageCapacity,
        Beam.vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
        Beam.vehicleVariant = vehicleServiceTierType,
        Beam.waitingChargePerMin = Domain.Types.Estimate.waitingChargePerMin waitingCharges <&> Kernel.Types.Price.getPriceAmountInt,
        Beam.waitingChargePerMinAmount = Domain.Types.Estimate.waitingChargePerMin waitingCharges <&> Kernel.Types.Price.getPriceAmount
      }
