{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SearchRequest where

import qualified Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Kernel.Utils.JSON
import qualified Lib.Yudhishthira.Tools.Utils
import qualified Storage.Beam.SearchRequest as Beam
import qualified Storage.CachedQueries.Merchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity
import qualified Storage.Queries.Location
import qualified Storage.Queries.LocationMapping
import qualified Storage.Queries.Transformers.SearchRequest
import qualified Tools.Error

instance FromTType' Beam.SearchRequest Domain.Types.SearchRequest.SearchRequest where
  fromTType' (Beam.SearchRequestT {..}) = do
    now <- Kernel.Types.Common.getCurrentTime
    merchant <- Storage.CachedQueries.Merchant.findById (Kernel.Types.Id.Id providerId) >>= fromMaybeM (Kernel.Types.Error.MerchantNotFound providerId)
    fromLocationMapping <- Storage.Queries.LocationMapping.getLatestStartByEntityId id >>= fromMaybeM (Tools.Error.FromLocationMappingNotFound id)
    mbToLocationMapping <- Storage.Queries.LocationMapping.getLatestEndByEntityId id
    let startTime_ = fromMaybe now startTime
    bapUri' <- Kernel.Prelude.parseBaseUrl bapUri
    fromLocation' <- Storage.Queries.Location.findById ((.locationId) fromLocationMapping) >>= fromMaybeM (Tools.Error.FromLocationNotFound ((.getId) $ (.locationId) fromLocationMapping))
    merchantOperatingCityId' <- Storage.CachedQueries.Merchant.MerchantOperatingCity.getMerchantOpCityId (Kernel.Types.Id.Id <$> merchantOperatingCityId) merchant bapCity
    stops' <- Storage.Queries.Transformers.SearchRequest.getStops id hasStops
    toLocation' <- maybe (pure Nothing) (Storage.Queries.Location.findById . (.locationId)) mbToLocationMapping
    pure $
      Just
        Domain.Types.SearchRequest.SearchRequest
          { area = area,
            autoAssignEnabled = autoAssignEnabled,
            bapCity = bapCity,
            bapCountry = bapCountry,
            bapId = bapId,
            bapUri = bapUri',
            configInExperimentVersions = fromMaybe [] (Kernel.Utils.JSON.valueToMaybe =<< configInExperimentVersions),
            createdAt = createdAt,
            currency = fromMaybe Kernel.Types.Common.INR currency,
            customerCancellationDues = customerCancellationDues,
            customerLanguage = customerLanguage,
            customerNammaTags = Lib.Yudhishthira.Tools.Utils.tagsNameValueFromTType customerNammaTags,
            device = device,
            disabilityTag = disabilityTag,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverDefaultExtraFee = Kernel.Types.Common.mkAmountWithDefault driverDefaultExtraFeeAmount <$> driverDefaultExtraFee,
            driverIdForSearch = Kernel.Types.Id.Id <$> driverIdForSearch,
            dynamicPricingLogicVersion = dynamicPricingLogicVersion,
            estimatedDistance = estimatedDistance,
            estimatedDuration = estimatedDuration,
            fromLocGeohash = fromLocGeohash,
            fromLocation = fromLocation',
            hasStops = hasStops,
            id = Kernel.Types.Id.Id id,
            isAdvanceBookingEnabled = fromMaybe False isAdvanceBookingEnabled,
            isBlockedRoute = isBlockedRoute,
            isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
            isDashboardRequest = fromMaybe False isDashboardRequest,
            isReallocationEnabled = isReallocationEnabled,
            isReserveRide = isReserveRide,
            isScheduled = fromMaybe False isScheduled,
            merchantOperatingCityId = merchantOperatingCityId',
            messageId = messageId,
            parcelQuantity = parcelQuantity,
            parcelType = parcelType,
            pickupZoneGateId = pickupZoneGateId,
            poolingConfigVersion = poolingConfigVersion,
            poolingLogicVersion = poolingLogicVersion,
            preferSafetyPlus = fromMaybe False preferSafetyPlus,
            providerId = Kernel.Types.Id.Id providerId,
            returnTime = returnTime,
            riderId = Kernel.Types.Id.Id <$> riderId,
            roundTrip = roundTrip,
            searchTags = Lib.Yudhishthira.Tools.Utils.tagsNameValueFromTType searchTags,
            specialLocationTag = specialLocationTag,
            startTime = startTime_,
            stops = stops',
            toLocGeohash = toLocGeohash,
            toLocation = toLocation',
            tollCharges = tollCharges,
            tollNames = tollNames,
            transactionId = transactionId,
            tripCategory = tripCategory,
            validTill = fromMaybe (Kernel.Utils.Common.addUTCTime 600 startTime_) validTill
          }

instance ToTType' Beam.SearchRequest Domain.Types.SearchRequest.SearchRequest where
  toTType' (Domain.Types.SearchRequest.SearchRequest {..}) = do
    Beam.SearchRequestT
      { Beam.area = area,
        Beam.autoAssignEnabled = autoAssignEnabled,
        Beam.bapCity = bapCity,
        Beam.bapCountry = bapCountry,
        Beam.bapId = bapId,
        Beam.bapUri = Kernel.Prelude.showBaseUrl bapUri,
        Beam.configInExperimentVersions = Just $ toJSON configInExperimentVersions,
        Beam.createdAt = createdAt,
        Beam.currency = Just currency,
        Beam.customerCancellationDues = customerCancellationDues,
        Beam.customerLanguage = customerLanguage,
        Beam.customerNammaTags = Lib.Yudhishthira.Tools.Utils.tagsNameValueToTType customerNammaTags,
        Beam.device = device,
        Beam.disabilityTag = disabilityTag,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverDefaultExtraFee = roundToIntegral <$> driverDefaultExtraFee,
        Beam.driverDefaultExtraFeeAmount = driverDefaultExtraFee,
        Beam.driverIdForSearch = Kernel.Types.Id.getId <$> driverIdForSearch,
        Beam.dynamicPricingLogicVersion = dynamicPricingLogicVersion,
        Beam.estimatedDistance = estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.fromLocGeohash = fromLocGeohash,
        Beam.fromLocationId = Just $ Kernel.Types.Id.getId ((.id) fromLocation),
        Beam.hasStops = hasStops,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isAdvanceBookingEnabled = Just isAdvanceBookingEnabled,
        Beam.isBlockedRoute = isBlockedRoute,
        Beam.isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
        Beam.isDashboardRequest = Just isDashboardRequest,
        Beam.isReallocationEnabled = isReallocationEnabled,
        Beam.isReserveRide = isReserveRide,
        Beam.isScheduled = Just isScheduled,
        Beam.merchantOperatingCityId = Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.messageId = messageId,
        Beam.parcelQuantity = parcelQuantity,
        Beam.parcelType = parcelType,
        Beam.pickupZoneGateId = pickupZoneGateId,
        Beam.poolingConfigVersion = poolingConfigVersion,
        Beam.poolingLogicVersion = poolingLogicVersion,
        Beam.preferSafetyPlus = Kernel.Prelude.Just preferSafetyPlus,
        Beam.providerId = Kernel.Types.Id.getId providerId,
        Beam.returnTime = returnTime,
        Beam.riderId = Kernel.Types.Id.getId <$> riderId,
        Beam.roundTrip = roundTrip,
        Beam.searchTags = Lib.Yudhishthira.Tools.Utils.tagsNameValueToTType searchTags,
        Beam.specialLocationTag = specialLocationTag,
        Beam.startTime = Just startTime,
        Beam.toLocGeohash = toLocGeohash,
        Beam.toLocationId = Kernel.Types.Id.getId . (.id) <$> toLocation,
        Beam.tollCharges = tollCharges,
        Beam.tollNames = tollNames,
        Beam.transactionId = transactionId,
        Beam.tripCategory = tripCategory,
        Beam.validTill = Just validTill
      }
