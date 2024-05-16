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
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Storage.Beam.Merchant as Beam
import Storage.Queries.Transformers.Merchant

instance FromTType' Beam.Merchant Domain.Types.Merchant.Merchant where
  fromTType' (Beam.MerchantT {..}) = do
    driverOfferBaseUrl' <- Kernel.Prelude.parseBaseUrl driverOfferBaseUrl
    gatewayUrl' <- Kernel.Prelude.parseBaseUrl gatewayUrl
    registryUrl' <- Kernel.Prelude.parseBaseUrl registryUrl
    pure $
      Just
        Domain.Types.Merchant.Merchant
          { aadhaarKeyExpiryTime = aadhaarKeyExpiryTime,
            aadhaarVerificationTryLimit = aadhaarVerificationTryLimit,
            arrivedPickupThreshold = maybe (Kernel.Types.Common.Distance 50 Kernel.Types.Common.Meter) (Kernel.Utils.Common.mkDistanceWithDefaultMeters distanceUnit arrivedPickupThresholdValue) arrivedPickupThreshold,
            bapId = bapId,
            bapUniqueKeyId = bapUniqueKeyId,
            cipherText = cipherText,
            country = country,
            createdAt = createdAt,
            defaultCity = city,
            defaultState = state,
            driverDistanceThresholdFromPickup = Kernel.Utils.Common.mkDistanceWithDefault distanceUnit driverDistanceThresholdFromPickupValue driverDistanceThresholdFromPickup,
            driverOfferApiKey = driverOfferApiKey,
            driverOfferBaseUrl = driverOfferBaseUrl',
            driverOfferMerchantId = driverOfferMerchantId,
            driverOnTheWayNotifyExpiry = fromMaybe 3600 driverOnTheWayNotifyExpiry,
            editPickupDistanceThreshold = Kernel.Utils.Common.mkDistanceWithDefault distanceUnit editPickupDistanceThresholdValue editPickupDistanceThreshold,
            fakeOtpEmails = fakeOtpEmails,
            fakeOtpMobileNumbers = fakeOtpMobileNumbers,
            fallbackShortId = Kernel.Types.Id.ShortId fallbackShortId,
            gatewayUrl = gatewayUrl',
            geoHashPrecisionValue = geoHashPrecisionValue,
            geofencingConfig = mkGeofencingConfig destinationRestriction originRestriction,
            id = Kernel.Types.Id.Id id,
            isAvoidToll = isAvoidToll,
            kaptureDisposition = kaptureDisposition,
            mediaFileSizeUpperLimit = mediaFileSizeUpperLimit,
            mediaFileUrlPattern = mediaFileUrlPattern,
            minimumDriverRatesCount = minimumDriverRatesCount,
            name = name,
            numOfAllowedEditPickupLocationAttemptsThreshold = numOfAllowedEditPickupLocationAttemptsThreshold,
            publicMediaFileUrlPattern = publicMediaFileUrlPattern,
            registryUrl = registryUrl',
            scheduleRideBufferTime = Kernel.Utils.Common.secondsToNominalDiffTime scheduleRideBufferTime,
            shortId = Kernel.Types.Id.ShortId shortId,
            signatureExpiry = signatureExpiry,
            signingPublicKey = signingPublicKey,
            subscriberId = Kernel.Types.Id.ShortId subscriberId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Merchant Domain.Types.Merchant.Merchant where
  toTType' (Domain.Types.Merchant.Merchant {..}) = do
    Beam.MerchantT
      { Beam.aadhaarKeyExpiryTime = aadhaarKeyExpiryTime,
        Beam.aadhaarVerificationTryLimit = aadhaarVerificationTryLimit,
        Beam.arrivedPickupThreshold = Just $ Kernel.Utils.Common.distanceToMeters arrivedPickupThreshold,
        Beam.arrivedPickupThresholdValue = Just $ Kernel.Utils.Common.distanceToHighPrecDistance (Just $ (.unit) editPickupDistanceThreshold) arrivedPickupThreshold,
        Beam.bapId = bapId,
        Beam.bapUniqueKeyId = bapUniqueKeyId,
        Beam.cipherText = cipherText,
        Beam.country = country,
        Beam.createdAt = createdAt,
        Beam.city = defaultCity,
        Beam.state = defaultState,
        Beam.driverDistanceThresholdFromPickup = Kernel.Utils.Common.distanceToHighPrecMeters driverDistanceThresholdFromPickup,
        Beam.driverDistanceThresholdFromPickupValue = Just $ Kernel.Utils.Common.distanceToHighPrecDistance (Just $ (.unit) editPickupDistanceThreshold) driverDistanceThresholdFromPickup,
        Beam.driverOfferApiKey = driverOfferApiKey,
        Beam.driverOfferBaseUrl = Kernel.Prelude.showBaseUrl driverOfferBaseUrl,
        Beam.driverOfferMerchantId = driverOfferMerchantId,
        Beam.driverOnTheWayNotifyExpiry = Just driverOnTheWayNotifyExpiry,
        Beam.distanceUnit = Just $ (.unit) editPickupDistanceThreshold,
        Beam.editPickupDistanceThreshold = Kernel.Utils.Common.distanceToHighPrecMeters editPickupDistanceThreshold,
        Beam.editPickupDistanceThresholdValue = Just $ Kernel.Utils.Common.distanceToHighPrecDistance (Just $ (.unit) editPickupDistanceThreshold) editPickupDistanceThreshold,
        Beam.fakeOtpEmails = fakeOtpEmails,
        Beam.fakeOtpMobileNumbers = fakeOtpMobileNumbers,
        Beam.fallbackShortId = Kernel.Types.Id.getShortId fallbackShortId,
        Beam.gatewayUrl = Kernel.Prelude.showBaseUrl gatewayUrl,
        Beam.geoHashPrecisionValue = geoHashPrecisionValue,
        Beam.destinationRestriction = (.destination) geofencingConfig,
        Beam.originRestriction = (.origin) geofencingConfig,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isAvoidToll = isAvoidToll,
        Beam.kaptureDisposition = kaptureDisposition,
        Beam.mediaFileSizeUpperLimit = mediaFileSizeUpperLimit,
        Beam.mediaFileUrlPattern = mediaFileUrlPattern,
        Beam.minimumDriverRatesCount = minimumDriverRatesCount,
        Beam.name = name,
        Beam.numOfAllowedEditPickupLocationAttemptsThreshold = numOfAllowedEditPickupLocationAttemptsThreshold,
        Beam.publicMediaFileUrlPattern = publicMediaFileUrlPattern,
        Beam.registryUrl = Kernel.Prelude.showBaseUrl registryUrl,
        Beam.scheduleRideBufferTime = Kernel.Utils.Common.nominalDiffTimeToSeconds scheduleRideBufferTime,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.signatureExpiry = signatureExpiry,
        Beam.signingPublicKey = signingPublicKey,
        Beam.subscriberId = Kernel.Types.Id.getShortId subscriberId,
        Beam.updatedAt = updatedAt
      }
