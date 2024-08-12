{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Quote (module Storage.Queries.Quote, module ReExport) where

import qualified Domain.Types.Quote
import qualified Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Sequelize as Se
import qualified Storage.Beam.Quote as Beam
import Storage.Queries.QuoteExtra as ReExport
import Storage.Queries.Transformers.Quote
import qualified Storage.Queries.Transformers.Quote

findAllBySRId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m [Domain.Types.Quote.Quote])
findAllBySRId requestId = do findAllWithKVAndConditionalDB [Se.Is Beam.requestId $ Se.Eq (Kernel.Types.Id.getId requestId)] Nothing

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Quote.Quote -> m (Maybe Domain.Types.Quote.Quote))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Quote.Quote -> m (Maybe Domain.Types.Quote.Quote))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Quote.Quote -> m ())
updateByPrimaryKey (Domain.Types.Quote.Quote {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.backendAppVersion backendAppVersion,
      Se.Set Beam.backendConfigVersion (fmap Kernel.Utils.Version.versionToText backendConfigVersion),
      Se.Set Beam.clientBundleVersion (fmap Kernel.Utils.Version.versionToText clientBundleVersion),
      Se.Set Beam.clientConfigVersion (fmap Kernel.Utils.Version.versionToText clientConfigVersion),
      Se.Set Beam.clientManufacturer (clientDevice >>= (.deviceManufacturer)),
      Se.Set Beam.clientModelName (clientDevice <&> (.deviceModel)),
      Se.Set Beam.clientOsType (clientDevice <&> (.deviceType)),
      Se.Set Beam.clientOsVersion (clientDevice <&> (.deviceVersion)),
      Se.Set Beam.clientSdkVersion (fmap Kernel.Utils.Version.versionToText clientSdkVersion),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.currency (Just ((.currency) estimatedFare)),
      Se.Set Beam.discount (discount <&> (.amount)),
      Se.Set Beam.distanceToNearestDriver (Kernel.Types.Common.distanceToHighPrecMeters <$> Storage.Queries.Transformers.Quote.getDistanceToNearestDriver (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails)),
      Se.Set Beam.distanceToNearestDriverValue (Kernel.Types.Common.distanceToHighPrecDistance distanceUnit <$> Storage.Queries.Transformers.Quote.getDistanceToNearestDriver (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails)),
      Se.Set Beam.driverOfferId (Storage.Queries.Transformers.Quote.getDriverOfferId (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails)),
      Se.Set Beam.fareProductType (Storage.Queries.Transformers.Quote.getfareProduct (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails)),
      Se.Set Beam.rentalDetailsId (Storage.Queries.Transformers.Quote.getRentalDetailsId (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails)),
      Se.Set Beam.specialZoneQuoteId (Storage.Queries.Transformers.Quote.getSpecialZoneQuoteId (Storage.Queries.Transformers.Quote.getQuoteDetails' quoteDetails)),
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.estimatedFare ((.amount) estimatedFare),
      Se.Set Beam.estimatedPickupDuration estimatedPickupDuration,
      Se.Set Beam.estimatedTotalFare ((.amount) estimatedTotalFare),
      Se.Set Beam.isAirConditioned isAirConditioned,
      Se.Set Beam.isBlockedRoute isBlockedRoute,
      Se.Set Beam.isCustomerPrefferedSearchRoute isCustomerPrefferedSearchRoute,
      Se.Set Beam.itemId itemId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Just $ Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.providerId providerId,
      Se.Set Beam.providerUrl (Kernel.Prelude.showBaseUrl providerUrl),
      Se.Set Beam.requestId (Kernel.Types.Id.getId requestId),
      Se.Set Beam.serviceTierName serviceTierName,
      Se.Set Beam.serviceTierShortDesc serviceTierShortDesc,
      Se.Set Beam.specialLocationName specialLocationName,
      Se.Set Beam.specialLocationTag specialLocationTag,
      Se.Set Beam.tollCharges (tollChargesInfo <&> ((.amount) . (.tollCharges))),
      Se.Set Beam.tollNames (tollChargesInfo <&> (.tollNames)),
      Se.Set Beam.tripTermsId (Kernel.Types.Id.getId <$> (tripTerms <&> (.id))),
      Se.Set Beam.updatedAt (Just _now),
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleModel vehicleModel,
      Se.Set Beam.vehicleServiceTierAirConditioned vehicleServiceTierAirConditioned,
      Se.Set Beam.vehicleServiceTierSeatingCapacity vehicleServiceTierSeatingCapacity,
      Se.Set Beam.vehicleVariant vehicleServiceTierType
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
