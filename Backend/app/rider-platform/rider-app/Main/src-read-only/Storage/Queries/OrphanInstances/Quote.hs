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
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Storage.Beam.Quote as Beam
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
    quoteDetails' <- Storage.Queries.Transformers.Quote.getQuoteDetails fareProductType distanceToNearestDriver rentalDetailsId driverOfferId specialZoneQuoteId distanceUnit distanceToNearestDriverValue
    tripTerms' <- getTripTerms tripTermsId
    pure $
      Just
        Domain.Types.Quote.Quote
          { backendAppVersion = backendAppVersion,
            backendConfigVersion = backendConfigVersion',
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion,
            clientSdkVersion = clientSdkVersion',
            createdAt = createdAt,
            discount = Kernel.Types.Common.mkPrice currency <$> discount,
            estimatedFare = Kernel.Types.Common.mkPrice currency estimatedFare,
            estimatedTotalFare = Kernel.Types.Common.mkPrice currency estimatedTotalFare,
            id = Kernel.Types.Id.Id id,
            isBlockedRoute = isBlockedRoute,
            isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
            itemId = itemId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            providerId = providerId,
            providerUrl = providerUrl',
            quoteDetails = quoteDetails',
            requestId = Kernel.Types.Id.Id requestId,
            serviceTierName = serviceTierName,
            serviceTierShortDesc = serviceTierShortDesc,
            specialLocationTag = specialLocationTag,
            tollChargesInfo = mkTollChargesInfo tollCharges tollNames currency,
            tripTerms = tripTerms',
            updatedAt = Kernel.Prelude.fromMaybe createdAt updatedAt,
            validTill = validTill,
            vehicleServiceTierType = vehicleVariant
          }

instance ToTType' Beam.Quote Domain.Types.Quote.Quote where
  toTType' (Domain.Types.Quote.Quote {..}) = do
    Beam.QuoteT
      { Beam.backendAppVersion = backendAppVersion,
        Beam.backendConfigVersion = fmap Kernel.Utils.Version.versionToText backendConfigVersion,
        Beam.clientBundleVersion = fmap Kernel.Utils.Version.versionToText clientBundleVersion,
        Beam.clientConfigVersion = fmap Kernel.Utils.Version.versionToText clientConfigVersion,
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.clientSdkVersion = fmap Kernel.Utils.Version.versionToText clientSdkVersion,
        Beam.createdAt = createdAt,
        Beam.currency = Just ((.currency) estimatedFare),
        Beam.discount = discount <&> (.amount),
        Beam.distanceToNearestDriver = Kernel.Types.Common.distanceToHighPrecMeters <$> Storage.Queries.Transformers.Quote.getDistanceToNearestDriver (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails),
        Beam.distanceToNearestDriverValue = Kernel.Types.Common.distanceToHighPrecDistance (Storage.Queries.Transformers.Quote.getDistanceToNearestDriver (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails) <&> (.unit)) <$> Storage.Queries.Transformers.Quote.getDistanceToNearestDriver (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails),
        Beam.distanceUnit = Storage.Queries.Transformers.Quote.getDistanceToNearestDriver (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails) <&> (.unit),
        Beam.driverOfferId = Storage.Queries.Transformers.Quote.getDriverOfferId (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails),
        Beam.fareProductType = Storage.Queries.Transformers.Quote.getfareProduct (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails),
        Beam.rentalDetailsId = Storage.Queries.Transformers.Quote.getRentalDetailsId (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails),
        Beam.specialZoneQuoteId = Storage.Queries.Transformers.Quote.getSpecialZoneQuoteId (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails),
        Beam.estimatedFare = (.amount) estimatedFare,
        Beam.estimatedTotalFare = (.amount) estimatedTotalFare,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isBlockedRoute = isBlockedRoute,
        Beam.isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
        Beam.itemId = itemId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.providerId = providerId,
        Beam.providerUrl = Kernel.Prelude.showBaseUrl providerUrl,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.serviceTierName = serviceTierName,
        Beam.serviceTierShortDesc = serviceTierShortDesc,
        Beam.specialLocationTag = specialLocationTag,
        Beam.tollCharges = tollChargesInfo <&> ((.amount) . (.tollCharges)),
        Beam.tollNames = tollChargesInfo <&> (.tollNames),
        Beam.tripTermsId = Kernel.Types.Id.getId <$> (tripTerms <&> (.id)),
        Beam.updatedAt = Kernel.Prelude.Just updatedAt,
        Beam.validTill = validTill,
        Beam.vehicleVariant = vehicleServiceTierType
      }
