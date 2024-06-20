{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Quote where

import qualified Data.Text
import qualified Domain.Types.Quote
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Storage.Beam.Quote as Beam
import Storage.Queries.Transformers.Quote
import qualified Storage.Queries.Transformers.Quote

instance FromTType' Beam.Quote Domain.Types.Quote.Quote where
  fromTType' (Beam.QuoteT {..}) = do
    providerUrl' <- Kernel.Prelude.parseBaseUrl providerUrl
    tripTerms' <- getTripTerms tripTermsId
    quoteDetails' <- Storage.Queries.Transformers.Quote.getQuoteDetails fareProductType distanceToNearestDriver rentalDetailsId driverOfferId specialZoneQuoteId distanceUnit distanceToNearestDriverValue
    merchantOperatingCityId' <- backfillMOCId merchantOperatingCityId merchantId
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    pure $
      Just
        Domain.Types.Quote.Quote
          { id = Kernel.Types.Id.Id id,
            requestId = Kernel.Types.Id.Id requestId,
            estimatedFare = Kernel.Types.Common.mkPrice currency estimatedFare,
            discount = Kernel.Types.Common.mkPrice currency <$> discount,
            estimatedTotalFare = Kernel.Types.Common.mkPrice currency estimatedTotalFare,
            estimatedPickupDuration = estimatedPickupDuration,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            providerId = providerId,
            providerUrl = providerUrl',
            itemId = itemId,
            tripTerms = tripTerms',
            quoteDetails = quoteDetails',
            merchantId = Kernel.Types.Id.Id merchantId,
            vehicleServiceTierType = vehicleVariant,
            vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
            vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
            isAirConditioned = isAirConditioned,
            serviceTierName = serviceTierName,
            serviceTierShortDesc = serviceTierShortDesc,
            merchantOperatingCityId = merchantOperatingCityId',
            specialLocationTag = specialLocationTag,
            specialLocationName = specialLocationName,
            clientBundleVersion = clientBundleVersion',
            clientSdkVersion = clientSdkVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion,
            backendConfigVersion = backendConfigVersion',
            backendAppVersion = backendAppVersion,
            isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
            isBlockedRoute = isBlockedRoute,
            tollChargesInfo = mkTollChargesInfo tollCharges tollNames currency,
            createdAt = createdAt,
            updatedAt = Kernel.Prelude.fromMaybe createdAt updatedAt,
            validTill = validTill
          }

instance ToTType' Beam.Quote Domain.Types.Quote.Quote where
  toTType' (Domain.Types.Quote.Quote {..}) = do
    Beam.QuoteT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.estimatedFare = (.amount) estimatedFare,
        Beam.currency = Just ((.currency) estimatedFare),
        Beam.discount = discount <&> (.amount),
        Beam.distanceToNearestDriver = Kernel.Types.Common.distanceToHighPrecMeters <$> Storage.Queries.Transformers.Quote.getDistanceToNearestDriver (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails),
        Beam.distanceToNearestDriverValue = Kernel.Types.Common.distanceToHighPrecDistance distanceUnit <$> Storage.Queries.Transformers.Quote.getDistanceToNearestDriver (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails),
        Beam.driverOfferId = Storage.Queries.Transformers.Quote.getDriverOfferId (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails),
        Beam.fareProductType = Storage.Queries.Transformers.Quote.getfareProduct (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails),
        Beam.rentalDetailsId = Storage.Queries.Transformers.Quote.getRentalDetailsId (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails),
        Beam.specialZoneQuoteId = Storage.Queries.Transformers.Quote.getSpecialZoneQuoteId (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails),
        Beam.estimatedTotalFare = (.amount) estimatedTotalFare,
        Beam.estimatedPickupDuration = estimatedPickupDuration,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.providerId = providerId,
        Beam.providerUrl = Kernel.Prelude.showBaseUrl providerUrl,
        Beam.itemId = itemId,
        Beam.tripTermsId = Kernel.Types.Id.getId <$> (tripTerms <&> (.id)),
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.vehicleVariant = vehicleServiceTierType,
        Beam.vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
        Beam.vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
        Beam.isAirConditioned = isAirConditioned,
        Beam.serviceTierName = serviceTierName,
        Beam.serviceTierShortDesc = serviceTierShortDesc,
        Beam.merchantOperatingCityId = Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.specialLocationTag = specialLocationTag,
        Beam.specialLocationName = specialLocationName,
        Beam.clientBundleVersion = fmap Kernel.Utils.Version.versionToText clientBundleVersion,
        Beam.clientSdkVersion = fmap Kernel.Utils.Version.versionToText clientSdkVersion,
        Beam.clientConfigVersion = fmap Kernel.Utils.Version.versionToText clientConfigVersion,
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.backendConfigVersion = fmap Kernel.Utils.Version.versionToText backendConfigVersion,
        Beam.backendAppVersion = backendAppVersion,
        Beam.isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
        Beam.isBlockedRoute = isBlockedRoute,
        Beam.tollCharges = tollChargesInfo <&> ((.amount) . (.tollCharges)),
        Beam.tollNames = tollChargesInfo <&> (.tollNames),
        Beam.createdAt = createdAt,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt,
        Beam.validTill = validTill
      }
