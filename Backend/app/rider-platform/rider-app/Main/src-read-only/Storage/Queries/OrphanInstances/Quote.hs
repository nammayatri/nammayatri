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
import qualified SharedLogic.Type
import qualified Storage.Beam.Quote as Beam
import qualified Storage.Queries.QuoteBreakup
import Storage.Queries.Transformers.Quote
import qualified Storage.Queries.Transformers.Quote

instance FromTType' Beam.Quote Domain.Types.Quote.Quote where
  fromTType' (Beam.QuoteT {..}) = do
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    merchantOperatingCityId' <- backfillMOCId merchantOperatingCityId merchantId
    providerUrl' <- Kernel.Prelude.parseBaseUrl providerUrl
    quoteBreakupList' <- Storage.Queries.QuoteBreakup.findAllByQuoteIdT id
    quoteDetails' <- Storage.Queries.Transformers.Quote.toQuoteDetails fareProductType tripCategory distanceToNearestDriver rentalDetailsId meterRideBppQuoteId staticBppQuoteId driverOfferId specialZoneQuoteId distanceUnit distanceToNearestDriverValue
    tripTerms' <- getTripTerms tripTermsId
    vehicleIconUrl' <- Kernel.Prelude.maybe (return Kernel.Prelude.Nothing) (Kernel.Prelude.fmap Kernel.Prelude.Just . parseBaseUrl) vehicleIconUrl
    pure $
      Just
        Domain.Types.Quote.Quote
          { backendAppVersion = backendAppVersion,
            backendConfigVersion = backendConfigVersion',
            billingCategory = Kernel.Prelude.fromMaybe SharedLogic.Type.PERSONAL billingCategory,
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion clientModelName clientManufacturer,
            clientSdkVersion = clientSdkVersion',
            createdAt = createdAt,
            discount = Kernel.Types.Common.mkPrice currency <$> discount,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            estimatedFare = Kernel.Types.Common.mkPrice currency estimatedFare,
            estimatedPickupDuration = estimatedPickupDuration,
            estimatedTotalFare = Kernel.Types.Common.mkPrice currency estimatedTotalFare,
            id = Kernel.Types.Id.Id id,
            isAirConditioned = isAirConditioned,
            isBlockedRoute = isBlockedRoute,
            isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
            isSafetyPlus = Kernel.Prelude.fromMaybe False isSafetyPlus,
            itemId = itemId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            providerId = providerId,
            providerUrl = providerUrl',
            quoteBreakupList = quoteBreakupList',
            quoteDetails = quoteDetails',
            requestId = Kernel.Types.Id.Id requestId,
            serviceTierName = serviceTierName,
            serviceTierShortDesc = serviceTierShortDesc,
            specialLocationName = specialLocationName,
            specialLocationTag = specialLocationTag,
            tollChargesInfo = mkTollChargesInfo tollCharges tollNames currency,
            tripCategory = tripCategory,
            tripTerms = tripTerms',
            updatedAt = Kernel.Prelude.fromMaybe createdAt updatedAt,
            validTill = validTill,
            vehicleIconUrl = vehicleIconUrl',
            vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
            vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
            vehicleServiceTierType = vehicleVariant
          }

instance ToTType' Beam.Quote Domain.Types.Quote.Quote where
  toTType' (Domain.Types.Quote.Quote {..}) = do
    Beam.QuoteT
      { Beam.backendAppVersion = backendAppVersion,
        Beam.backendConfigVersion = fmap Kernel.Utils.Version.versionToText backendConfigVersion,
        Beam.billingCategory = Kernel.Prelude.Just billingCategory,
        Beam.clientBundleVersion = fmap Kernel.Utils.Version.versionToText clientBundleVersion,
        Beam.clientConfigVersion = fmap Kernel.Utils.Version.versionToText clientConfigVersion,
        Beam.clientManufacturer = clientDevice >>= (.deviceManufacturer),
        Beam.clientModelName = clientDevice <&> (.deviceModel),
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.clientSdkVersion = fmap Kernel.Utils.Version.versionToText clientSdkVersion,
        Beam.createdAt = createdAt,
        Beam.currency = Just ((.currency) estimatedFare),
        Beam.discount = discount <&> (.amount),
        Beam.distanceToNearestDriver = Kernel.Types.Common.distanceToHighPrecMeters <$> Storage.Queries.Transformers.Quote.getDistanceToNearestDriver (Storage.Queries.Transformers.Quote.fromQuoteDetails quoteDetails),
        Beam.distanceToNearestDriverValue = Kernel.Types.Common.distanceToHighPrecDistance distanceUnit <$> Storage.Queries.Transformers.Quote.getDistanceToNearestDriver (Storage.Queries.Transformers.Quote.fromQuoteDetails quoteDetails),
        Beam.driverOfferId = Storage.Queries.Transformers.Quote.getDriverOfferId (Storage.Queries.Transformers.Quote.fromQuoteDetails quoteDetails),
        Beam.fareProductType = Storage.Queries.Transformers.Quote.getfareProduct (Storage.Queries.Transformers.Quote.fromQuoteDetails quoteDetails),
        Beam.meterRideBppQuoteId = Storage.Queries.Transformers.Quote.getMeterRideBppQuoteId (Storage.Queries.Transformers.Quote.fromQuoteDetails quoteDetails),
        Beam.rentalDetailsId = Storage.Queries.Transformers.Quote.getRentalDetailsId (Storage.Queries.Transformers.Quote.fromQuoteDetails quoteDetails),
        Beam.specialZoneQuoteId = Storage.Queries.Transformers.Quote.getSpecialZoneQuoteId (Storage.Queries.Transformers.Quote.fromQuoteDetails quoteDetails),
        Beam.staticBppQuoteId = Storage.Queries.Transformers.Quote.getStaticBppQuoteId (Storage.Queries.Transformers.Quote.fromQuoteDetails quoteDetails),
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.estimatedFare = (.amount) estimatedFare,
        Beam.estimatedPickupDuration = estimatedPickupDuration,
        Beam.estimatedTotalFare = (.amount) estimatedTotalFare,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isAirConditioned = isAirConditioned,
        Beam.isBlockedRoute = isBlockedRoute,
        Beam.isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
        Beam.isSafetyPlus = Kernel.Prelude.Just isSafetyPlus,
        Beam.itemId = itemId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.providerId = providerId,
        Beam.providerUrl = Kernel.Prelude.showBaseUrl providerUrl,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.serviceTierName = serviceTierName,
        Beam.serviceTierShortDesc = serviceTierShortDesc,
        Beam.specialLocationName = specialLocationName,
        Beam.specialLocationTag = specialLocationTag,
        Beam.tollCharges = tollChargesInfo <&> ((.amount) . (.tollCharges)),
        Beam.tollNames = tollChargesInfo <&> (.tollNames),
        Beam.tripCategory = tripCategory,
        Beam.tripTermsId = Kernel.Types.Id.getId <$> (tripTerms <&> (.id)),
        Beam.updatedAt = Kernel.Prelude.Just updatedAt,
        Beam.validTill = validTill,
        Beam.vehicleIconUrl = Kernel.Prelude.fmap showBaseUrl vehicleIconUrl,
        Beam.vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
        Beam.vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
        Beam.vehicleVariant = vehicleServiceTierType
      }
