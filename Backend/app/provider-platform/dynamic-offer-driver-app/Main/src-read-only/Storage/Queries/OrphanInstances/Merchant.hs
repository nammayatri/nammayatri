{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Merchant where

import qualified Domain.Types
import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Geofencing
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Merchant as Beam

instance FromTType' Beam.Merchant Domain.Types.Merchant.Merchant where
  fromTType' (Beam.MerchantT {..}) = do
    registryUrl' <- Kernel.Prelude.parseBaseUrl registryUrl
    pure $
      Just
        Domain.Types.Merchant.Merchant
          { cipherText = cipherText,
            city = city,
            country = country,
            createdAt = createdAt,
            description = description,
            enabled = enabled,
            fleetOwnerEnabledCheck = fleetOwnerEnabledCheck,
            fromTime = fromTime,
            gatewayAndRegistryPriorityList = fromMaybe [Domain.Types.ONDC, Domain.Types.NY] gatewayAndRegistryPriorityList,
            geoHashPrecisionValue = geoHashPrecisionValue,
            geofencingConfig = Kernel.Types.Geofencing.GeofencingConfig originRestriction destinationRestriction,
            gstin = gstin,
            headCount = headCount,
            id = Kernel.Types.Id.Id id,
            info = info,
            internalApiKey = internalApiKey,
            mediaFileDocumentLinkExpires = Kernel.Prelude.fromMaybe 3600 mediaFileDocumentLinkExpires,
            minimumDriverRatesCount = minimumDriverRatesCount,
            mobileCountryCode = mobileCountryCode,
            mobileNumber = mobileNumber,
            name = name,
            onlinePayment = onlinePayment,
            overwriteAssociation = overwriteAssociation,
            registryUrl = registryUrl',
            shortId = Kernel.Types.Id.ShortId shortId,
            signatureExpiry = signatureExpiry,
            signingPrivateKey = signingPrivateKey,
            signingPublicKey = signingPublicKey,
            state = state,
            status = status,
            subscriberId = Kernel.Types.Id.ShortId subscriberId,
            toTime = toTime,
            uniqueKeyId = uniqueKeyId,
            updatedAt = updatedAt,
            verified = verified
          }

instance ToTType' Beam.Merchant Domain.Types.Merchant.Merchant where
  toTType' (Domain.Types.Merchant.Merchant {..}) = do
    Beam.MerchantT
      { Beam.cipherText = cipherText,
        Beam.city = city,
        Beam.country = country,
        Beam.createdAt = createdAt,
        Beam.description = description,
        Beam.enabled = enabled,
        Beam.fleetOwnerEnabledCheck = fleetOwnerEnabledCheck,
        Beam.fromTime = fromTime,
        Beam.gatewayAndRegistryPriorityList = Kernel.Prelude.Just gatewayAndRegistryPriorityList,
        Beam.geoHashPrecisionValue = geoHashPrecisionValue,
        Beam.destinationRestriction = Kernel.Types.Geofencing.destination geofencingConfig,
        Beam.originRestriction = Kernel.Types.Geofencing.origin geofencingConfig,
        Beam.gstin = gstin,
        Beam.headCount = headCount,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.info = info,
        Beam.internalApiKey = internalApiKey,
        Beam.mediaFileDocumentLinkExpires = Kernel.Prelude.Just mediaFileDocumentLinkExpires,
        Beam.minimumDriverRatesCount = minimumDriverRatesCount,
        Beam.mobileCountryCode = mobileCountryCode,
        Beam.mobileNumber = mobileNumber,
        Beam.name = name,
        Beam.onlinePayment = onlinePayment,
        Beam.overwriteAssociation = overwriteAssociation,
        Beam.registryUrl = Kernel.Prelude.showBaseUrl registryUrl,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.signatureExpiry = signatureExpiry,
        Beam.signingPrivateKey = signingPrivateKey,
        Beam.signingPublicKey = signingPublicKey,
        Beam.state = state,
        Beam.status = status,
        Beam.subscriberId = Kernel.Types.Id.getShortId subscriberId,
        Beam.toTime = toTime,
        Beam.uniqueKeyId = uniqueKeyId,
        Beam.updatedAt = updatedAt,
        Beam.verified = verified
      }
