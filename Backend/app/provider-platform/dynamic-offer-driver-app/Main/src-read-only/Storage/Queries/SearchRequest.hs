{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SearchRequest (module Storage.Queries.SearchRequest, module ReExport) where

import qualified Domain.Types.Merchant
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.Trip
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Lib.Yudhishthira.Tools.Utils
import qualified Lib.Yudhishthira.Types
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequest as Beam
import Storage.Queries.SearchRequestExtra as ReExport

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m (Maybe Domain.Types.SearchRequest.SearchRequest))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByTransactionIdAndMerchantId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.SearchRequest.SearchRequest))
findByTransactionIdAndMerchantId transactionId providerId = do findOneWithKV [Se.And [Se.Is Beam.transactionId $ Se.Eq transactionId, Se.Is Beam.providerId $ Se.Eq (Kernel.Types.Id.getId providerId)]]

updateIsAdvancedBookingEnabled :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ())
updateIsAdvancedBookingEnabled isAdvanceBookingEnabled id = do updateWithKV [Se.Set Beam.isAdvanceBookingEnabled (Just isAdvanceBookingEnabled)] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateIsReallocationEnabled :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ())
updateIsReallocationEnabled isReallocationEnabled id = do updateWithKV [Se.Set Beam.isReallocationEnabled isReallocationEnabled] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePoolingConfigVersion :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ())
updatePoolingConfigVersion poolingConfigVersion id = do updateWithKV [Se.Set Beam.poolingConfigVersion poolingConfigVersion] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePoolingLogicVersion :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ())
updatePoolingLogicVersion poolingLogicVersion id = do updateWithKV [Se.Set Beam.poolingLogicVersion poolingLogicVersion] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateSearchTags :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe [Lib.Yudhishthira.Types.TagNameValue] -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ())
updateSearchTags searchTags id = do updateWithKV [Se.Set Beam.searchTags (Lib.Yudhishthira.Tools.Utils.tagsNameValueToTType searchTags)] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateTripCategory :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Domain.Types.Trip.TripCategory -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ())
updateTripCategory tripCategory id = do updateWithKV [Se.Set Beam.tripCategory tripCategory] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SearchRequest.SearchRequest -> m ())
updateByPrimaryKey (Domain.Types.SearchRequest.SearchRequest {..}) = do
  updateWithKV
    [ Se.Set Beam.area area,
      Se.Set Beam.autoAssignEnabled autoAssignEnabled,
      Se.Set Beam.bapCity bapCity,
      Se.Set Beam.bapCountry bapCountry,
      Se.Set Beam.bapId bapId,
      Se.Set Beam.bapUri (Kernel.Prelude.showBaseUrl bapUri),
      Se.Set Beam.cloudType cloudType,
      Se.Set Beam.configInExperimentVersions (Just $ toJSON configInExperimentVersions),
      Se.Set Beam.currency (Just currency),
      Se.Set Beam.customerCancellationDues customerCancellationDues,
      Se.Set Beam.customerLanguage customerLanguage,
      Se.Set Beam.customerNammaTags (Lib.Yudhishthira.Tools.Utils.tagsNameValueToTType customerNammaTags),
      Se.Set Beam.device device,
      Se.Set Beam.disabilityTag disabilityTag,
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.driverDefaultExtraFee (roundToIntegral <$> driverDefaultExtraFee),
      Se.Set Beam.driverDefaultExtraFeeAmount driverDefaultExtraFee,
      Se.Set Beam.driverIdForSearch (Kernel.Types.Id.getId <$> driverIdForSearch),
      Se.Set Beam.dynamicPricingLogicVersion dynamicPricingLogicVersion,
      Se.Set Beam.estimatedDistance estimatedDistance,
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.fromLocGeohash fromLocGeohash,
      Se.Set Beam.fromLocationId (Just $ Kernel.Types.Id.getId ((.id) fromLocation)),
      Se.Set Beam.hasStops hasStops,
      Se.Set Beam.isAdvanceBookingEnabled (Just isAdvanceBookingEnabled),
      Se.Set Beam.isBlockedRoute isBlockedRoute,
      Se.Set Beam.isCustomerPrefferedSearchRoute isCustomerPrefferedSearchRoute,
      Se.Set Beam.isDashboardRequest (Just isDashboardRequest),
      Se.Set Beam.isReallocationEnabled isReallocationEnabled,
      Se.Set Beam.isReserveRide isReserveRide,
      Se.Set Beam.isScheduled (Just isScheduled),
      Se.Set Beam.merchantOperatingCityId (Just $ Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.messageId messageId,
      Se.Set Beam.numberOfLuggages numberOfLuggages,
      Se.Set Beam.parcelQuantity parcelQuantity,
      Se.Set Beam.parcelType parcelType,
      Se.Set Beam.paymentMode paymentMode,
      Se.Set Beam.pickupGateId pickupGateId,
      Se.Set Beam.pickupZoneGateId pickupZoneGateId,
      Se.Set Beam.poolingConfigVersion poolingConfigVersion,
      Se.Set Beam.poolingLogicVersion poolingLogicVersion,
      Se.Set Beam.preferSafetyPlus (Kernel.Prelude.Just preferSafetyPlus),
      Se.Set Beam.providerId (Kernel.Types.Id.getId providerId),
      Se.Set Beam.returnTime returnTime,
      Se.Set Beam.riderId (Kernel.Types.Id.getId <$> riderId),
      Se.Set Beam.riderPreferredOption (Just riderPreferredOption),
      Se.Set Beam.roundTrip roundTrip,
      Se.Set Beam.searchTags (Lib.Yudhishthira.Tools.Utils.tagsNameValueToTType searchTags),
      Se.Set Beam.specialLocationName specialLocationName,
      Se.Set Beam.specialLocationTag specialLocationTag,
      Se.Set Beam.startTime (Just startTime),
      Se.Set Beam.toLocGeohash toLocGeohash,
      Se.Set Beam.toLocationId ((Kernel.Types.Id.getId . (.id)) <$> toLocation),
      Se.Set Beam.tollCharges tollCharges,
      Se.Set Beam.tollIds tollIds,
      Se.Set Beam.tollNames tollNames,
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.tripCategory tripCategory,
      Se.Set Beam.userBackendAppVersion userBackendAppVersion,
      Se.Set Beam.userBundleVersion (Kernel.Utils.Version.versionToText <$> userBundleVersion),
      Se.Set Beam.userManufacturer (userClientDevice >>= (.deviceManufacturer)),
      Se.Set Beam.userModelName (userClientDevice <&> (.deviceModel)),
      Se.Set Beam.userOsType (userClientDevice <&> (.deviceType)),
      Se.Set Beam.userOsVersion (userClientDevice <&> (.deviceVersion)),
      Se.Set Beam.userSdkVersion (Kernel.Utils.Version.versionToText <$> userSdkVersion),
      Se.Set Beam.validTill (Just validTill)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
