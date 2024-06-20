{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Merchant where

import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Storage.Beam.Merchant as Beam
import Storage.Queries.Transformers.Merchant

instance FromTType' Beam.Merchant Domain.Types.Merchant.Merchant where
  fromTType' (Beam.MerchantT {..}) = do
    gatewayUrl' <- Kernel.Prelude.parseBaseUrl gatewayUrl
    registryUrl' <- Kernel.Prelude.parseBaseUrl registryUrl
    driverOfferBaseUrl' <- Kernel.Prelude.parseBaseUrl driverOfferBaseUrl
    pure $
      Just
        Domain.Types.Merchant.Merchant
          { id = Kernel.Types.Id.Id id,
            subscriberId = Kernel.Types.Id.ShortId subscriberId,
            shortId = Kernel.Types.Id.ShortId shortId,
            name = name,
            defaultCity = city,
            defaultState = state,
            country = country,
            geofencingConfig = mkGeofencingConfig destinationRestriction originRestriction,
            gatewayUrl = gatewayUrl',
            registryUrl = registryUrl',
            fallbackShortId = Kernel.Types.Id.ShortId fallbackShortId,
            bapId = bapId,
            bapUniqueKeyId = bapUniqueKeyId,
            driverOfferBaseUrl = driverOfferBaseUrl',
            driverOfferApiKey = driverOfferApiKey,
            driverOfferMerchantId = driverOfferMerchantId,
            geoHashPrecisionValue = geoHashPrecisionValue,
            minimumDriverRatesCount = minimumDriverRatesCount,
            signingPublicKey = signingPublicKey,
            cipherText = cipherText,
            signatureExpiry = signatureExpiry,
            createdAt = createdAt,
            updatedAt = updatedAt,
            isAvoidToll = isAvoidToll,
            aadhaarVerificationTryLimit = aadhaarVerificationTryLimit,
            aadhaarKeyExpiryTime = aadhaarKeyExpiryTime,
            mediaFileSizeUpperLimit = mediaFileSizeUpperLimit,
            mediaFileUrlPattern = mediaFileUrlPattern,
            editPickupDistanceThreshold = Kernel.Utils.Common.mkDistanceWithDefault distanceUnit editPickupDistanceThresholdValue editPickupDistanceThreshold,
            driverDistanceThresholdFromPickup = Kernel.Utils.Common.mkDistanceWithDefault distanceUnit driverDistanceThresholdFromPickupValue driverDistanceThresholdFromPickup,
            numOfAllowedEditPickupLocationAttemptsThreshold = numOfAllowedEditPickupLocationAttemptsThreshold,
            publicMediaFileUrlPattern = publicMediaFileUrlPattern,
            scheduleRideBufferTime = Kernel.Utils.Common.secondsToNominalDiffTime scheduleRideBufferTime,
            fakeOtpMobileNumbers = fakeOtpMobileNumbers,
            fakeOtpEmails = fakeOtpEmails,
            kaptureDisposition = kaptureDisposition,
            arrivedPickupThreshold = maybe (Kernel.Types.Common.Distance 50 Kernel.Types.Common.Meter) (Kernel.Utils.Common.mkDistanceWithDefaultMeters distanceUnit arrivedPickupThresholdValue) arrivedPickupThreshold,
            arrivingPickupThreshold = Kernel.Types.Common.Distance arrivingPickupThreshold (fromMaybe Kernel.Types.Common.Meter distanceUnit),
            driverOnTheWayNotifyExpiry = fromMaybe 3600 driverOnTheWayNotifyExpiry
          }

instance ToTType' Beam.Merchant Domain.Types.Merchant.Merchant where
  toTType' (Domain.Types.Merchant.Merchant {..}) = do
    Beam.MerchantT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.subscriberId = Kernel.Types.Id.getShortId subscriberId,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.name = name,
        Beam.city = defaultCity,
        Beam.state = defaultState,
        Beam.country = country,
        Beam.destinationRestriction = (.destination) geofencingConfig,
        Beam.originRestriction = (.origin) geofencingConfig,
        Beam.gatewayUrl = Kernel.Prelude.showBaseUrl gatewayUrl,
        Beam.registryUrl = Kernel.Prelude.showBaseUrl registryUrl,
        Beam.fallbackShortId = Kernel.Types.Id.getShortId fallbackShortId,
        Beam.bapId = bapId,
        Beam.bapUniqueKeyId = bapUniqueKeyId,
        Beam.driverOfferBaseUrl = Kernel.Prelude.showBaseUrl driverOfferBaseUrl,
        Beam.driverOfferApiKey = driverOfferApiKey,
        Beam.driverOfferMerchantId = driverOfferMerchantId,
        Beam.geoHashPrecisionValue = geoHashPrecisionValue,
        Beam.minimumDriverRatesCount = minimumDriverRatesCount,
        Beam.signingPublicKey = signingPublicKey,
        Beam.cipherText = cipherText,
        Beam.signatureExpiry = signatureExpiry,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt,
        Beam.isAvoidToll = isAvoidToll,
        Beam.aadhaarVerificationTryLimit = aadhaarVerificationTryLimit,
        Beam.aadhaarKeyExpiryTime = aadhaarKeyExpiryTime,
        Beam.mediaFileSizeUpperLimit = mediaFileSizeUpperLimit,
        Beam.mediaFileUrlPattern = mediaFileUrlPattern,
        Beam.distanceUnit = Just $ (.unit) editPickupDistanceThreshold,
        Beam.editPickupDistanceThreshold = Kernel.Utils.Common.distanceToHighPrecMeters editPickupDistanceThreshold,
        Beam.editPickupDistanceThresholdValue = Just $ Kernel.Utils.Common.distanceToHighPrecDistance ((.unit) editPickupDistanceThreshold) editPickupDistanceThreshold,
        Beam.driverDistanceThresholdFromPickup = Kernel.Utils.Common.distanceToHighPrecMeters driverDistanceThresholdFromPickup,
        Beam.driverDistanceThresholdFromPickupValue = Just $ Kernel.Utils.Common.distanceToHighPrecDistance ((.unit) editPickupDistanceThreshold) driverDistanceThresholdFromPickup,
        Beam.numOfAllowedEditPickupLocationAttemptsThreshold = numOfAllowedEditPickupLocationAttemptsThreshold,
        Beam.publicMediaFileUrlPattern = publicMediaFileUrlPattern,
        Beam.scheduleRideBufferTime = Kernel.Utils.Common.nominalDiffTimeToSeconds scheduleRideBufferTime,
        Beam.fakeOtpMobileNumbers = fakeOtpMobileNumbers,
        Beam.fakeOtpEmails = fakeOtpEmails,
        Beam.kaptureDisposition = kaptureDisposition,
        Beam.arrivedPickupThreshold = Just $ Kernel.Utils.Common.distanceToMeters arrivedPickupThreshold,
        Beam.arrivedPickupThresholdValue = Just $ Kernel.Utils.Common.distanceToHighPrecDistance ((.unit) editPickupDistanceThreshold) arrivedPickupThreshold,
        Beam.arrivingPickupThreshold = Kernel.Utils.Common.distanceToHighPrecDistance ((.unit) editPickupDistanceThreshold) arrivingPickupThreshold,
        Beam.driverOnTheWayNotifyExpiry = Just driverOnTheWayNotifyExpiry
      }
