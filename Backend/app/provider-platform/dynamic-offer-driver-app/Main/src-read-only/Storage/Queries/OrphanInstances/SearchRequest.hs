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
import qualified Storage.Beam.SearchRequest as Beam
import qualified Storage.CachedQueries.Merchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity
import qualified Storage.Queries.Location
import qualified Storage.Queries.LocationMapping
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
            createdAt = createdAt,
            currency = fromMaybe Kernel.Types.Common.INR currency,
            customerCancellationDues = customerCancellationDues,
            customerLanguage = customerLanguage,
            device = device,
            disabilityTag = disabilityTag,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverDefaultExtraFee = Kernel.Types.Common.mkAmountWithDefault driverDefaultExtraFeeAmount <$> driverDefaultExtraFee,
            estimatedDistance = estimatedDistance,
            estimatedDuration = estimatedDuration,
            fromLocation = fromLocation',
            id = Kernel.Types.Id.Id id,
            isAdvanceBookingEnabled = fromMaybe False isAdvanceBookingEnabled,
            isBlockedRoute = isBlockedRoute,
            isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
            isReallocationEnabled = isReallocationEnabled,
            isScheduled = fromMaybe False isScheduled,
            merchantOperatingCityId = merchantOperatingCityId',
            messageId = messageId,
            pickupZoneGateId = pickupZoneGateId,
            providerId = Kernel.Types.Id.Id providerId,
            returnTime = returnTime,
            riderId = Kernel.Types.Id.Id <$> riderId,
            roundTrip = roundTrip,
            specialLocationTag = specialLocationTag,
            startTime = startTime_,
            toLocation = toLocation',
            tollCharges = tollCharges,
            tollNames = tollNames,
            transactionId = transactionId,
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
        Beam.createdAt = createdAt,
        Beam.currency = Just currency,
        Beam.customerCancellationDues = customerCancellationDues,
        Beam.customerLanguage = customerLanguage,
        Beam.device = device,
        Beam.disabilityTag = disabilityTag,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverDefaultExtraFee = roundToIntegral <$> driverDefaultExtraFee,
        Beam.driverDefaultExtraFeeAmount = driverDefaultExtraFee,
        Beam.estimatedDistance = estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.fromLocationId = Just $ Kernel.Types.Id.getId ((.id) fromLocation),
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isAdvanceBookingEnabled = Just isAdvanceBookingEnabled,
        Beam.isBlockedRoute = isBlockedRoute,
        Beam.isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
        Beam.isReallocationEnabled = isReallocationEnabled,
        Beam.isScheduled = Just isScheduled,
        Beam.merchantOperatingCityId = Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.messageId = messageId,
        Beam.pickupZoneGateId = pickupZoneGateId,
        Beam.providerId = Kernel.Types.Id.getId providerId,
        Beam.returnTime = returnTime,
        Beam.riderId = Kernel.Types.Id.getId <$> riderId,
        Beam.roundTrip = roundTrip,
        Beam.specialLocationTag = specialLocationTag,
        Beam.startTime = Just startTime,
        Beam.toLocationId = Kernel.Types.Id.getId . (.id) <$> toLocation,
        Beam.tollCharges = tollCharges,
        Beam.tollNames = tollNames,
        Beam.transactionId = transactionId,
        Beam.validTill = Just validTill
      }
