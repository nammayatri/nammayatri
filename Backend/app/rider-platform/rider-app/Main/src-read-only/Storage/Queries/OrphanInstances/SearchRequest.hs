{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SearchRequest where

import qualified Data.Text
import qualified Domain.Types.RefereeLink
import qualified Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Kernel.Utils.JSON
import qualified Kernel.Utils.Version
import qualified Storage.Beam.SearchRequest as Beam
import qualified Storage.Queries.Transformers.SearchRequest

instance FromTType' Beam.SearchRequest Domain.Types.SearchRequest.SearchRequest where
  fromTType' (Beam.SearchRequestT {..}) = do
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    fromLocation' <- Storage.Queries.Transformers.SearchRequest.getFromLocation id
    merchantOperatingCityId' <- Storage.Queries.Transformers.SearchRequest.backfillMOCId merchantId merchantOperatingCityId
    stops' <- Storage.Queries.Transformers.SearchRequest.getStops id hasStops
    toLocation' <- Storage.Queries.Transformers.SearchRequest.getToLocation id
    pure $
      Just
        Domain.Types.SearchRequest.SearchRequest
          { allJourneysLoaded = allJourneysLoaded,
            autoAssignEnabled = autoAssignEnabled,
            autoAssignEnabledV2 = autoAssignEnabledV2,
            availablePaymentMethods = Kernel.Types.Id.Id <$> availablePaymentMethods,
            backendAppVersion = backendAppVersion,
            backendConfigVersion = backendConfigVersion',
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion clientModelName clientManufacturer,
            clientId = Kernel.Types.Id.Id <$> clientId,
            clientReactNativeVersion = clientReactNativeVersion,
            clientSdkVersion = clientSdkVersion',
            configInExperimentVersions = fromMaybe [] (Kernel.Utils.JSON.valueToMaybe =<< configInExperimentVersions),
            createdAt = createdAt,
            customerExtraFee = Kernel.Utils.Common.mkPriceWithDefault customerExtraFeeAmount currency <$> customerExtraFee,
            destinationStopCode = destinationStopCode,
            device = device,
            disabilityTag = disabilityTag,
            distance = Kernel.Utils.Common.mkDistanceWithDefault distanceUnit distanceValue . Kernel.Types.Common.HighPrecMeters <$> distance,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverIdentifier = Domain.Types.RefereeLink.mkDriverIdentifier driverIdentifierType driverIdentifierValue,
            estimatedRideDuration = estimatedRideDuration,
            estimatedRideStaticDuration = estimatedRideStaticDuration,
            fromLocation = fromLocation',
            hasMultimodalSearch = hasMultimodalSearch,
            hasStops = hasStops,
            id = Kernel.Types.Id.Id id,
            initiatedBy = initiatedBy,
            isAdvanceBookingEnabled = isAdvanceBookingEnabled,
            isDashboardRequest = isDashboardRequest,
            isMeterRideSearch = isMeterRideSearch,
            journeyLegInfo = Storage.Queries.Transformers.SearchRequest.mkJourneyLegInfo agency convenienceCost isDeleted journeyId journeyLegOrder onSearchFailed pricingId skipBooking,
            language = language,
            maxDistance = Kernel.Utils.Common.mkDistanceWithDefault distanceUnit maxDistanceValue . Kernel.Types.Common.HighPrecMeters <$> maxDistance,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            originStopCode = originStopCode,
            placeNameSource = placeNameSource,
            recentLocationId = Kernel.Types.Id.Id <$> recentLocationId,
            returnTime = returnTime,
            riderId = Kernel.Types.Id.Id riderId,
            riderPreferredOption = fromMaybe Domain.Types.SearchRequest.OneWay riderPreferredOption,
            roundTrip = roundTrip,
            routeCode = routeCode,
            selectedPaymentMethodId = selectedPaymentMethodId,
            startTime = startTime,
            stops = stops',
            toLocation = toLocation',
            totalRidesCount = totalRidesCount,
            validTill = validTill,
            vehicleCategory = vehicleCategory
          }

instance ToTType' Beam.SearchRequest Domain.Types.SearchRequest.SearchRequest where
  toTType' (Domain.Types.SearchRequest.SearchRequest {..}) = do
    Beam.SearchRequestT
      { Beam.allJourneysLoaded = allJourneysLoaded,
        Beam.autoAssignEnabled = autoAssignEnabled,
        Beam.autoAssignEnabledV2 = autoAssignEnabledV2,
        Beam.availablePaymentMethods = Kernel.Types.Id.getId <$> availablePaymentMethods,
        Beam.backendAppVersion = backendAppVersion,
        Beam.backendConfigVersion = Kernel.Utils.Version.versionToText <$> backendConfigVersion,
        Beam.clientBundleVersion = Kernel.Utils.Version.versionToText <$> clientBundleVersion,
        Beam.clientConfigVersion = Kernel.Utils.Version.versionToText <$> clientConfigVersion,
        Beam.clientManufacturer = clientDevice >>= (.deviceManufacturer),
        Beam.clientModelName = clientDevice <&> (.deviceModel),
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.clientId = Kernel.Types.Id.getId <$> clientId,
        Beam.clientReactNativeVersion = clientReactNativeVersion,
        Beam.clientSdkVersion = Kernel.Utils.Version.versionToText <$> clientSdkVersion,
        Beam.configInExperimentVersions = Just $ toJSON configInExperimentVersions,
        Beam.createdAt = createdAt,
        Beam.currency = customerExtraFee <&> (.currency),
        Beam.customerExtraFee = customerExtraFee <&> (.amountInt),
        Beam.customerExtraFeeAmount = customerExtraFee <&> (.amount),
        Beam.destinationStopCode = destinationStopCode,
        Beam.device = device,
        Beam.disabilityTag = disabilityTag,
        Beam.distance = Kernel.Utils.Common.getHighPrecMeters . Kernel.Utils.Common.distanceToHighPrecMeters <$> distance,
        Beam.distanceValue = Kernel.Utils.Common.distanceToHighPrecDistance distanceUnit <$> distance,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverIdentifierType = driverIdentifier <&> (._type),
        Beam.driverIdentifierValue = driverIdentifier <&> (.value),
        Beam.estimatedRideDuration = estimatedRideDuration,
        Beam.estimatedRideStaticDuration = estimatedRideStaticDuration,
        Beam.fromLocationId = Just $ Kernel.Types.Id.getId ((.id) fromLocation),
        Beam.hasMultimodalSearch = hasMultimodalSearch,
        Beam.hasStops = hasStops,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.initiatedBy = initiatedBy,
        Beam.isAdvanceBookingEnabled = isAdvanceBookingEnabled,
        Beam.isDashboardRequest = isDashboardRequest,
        Beam.isMeterRideSearch = isMeterRideSearch,
        Beam.agency = journeyLegInfo >>= (.agency),
        Beam.convenienceCost = Kernel.Prelude.fmap (.convenienceCost) journeyLegInfo,
        Beam.isDeleted = journeyLegInfo >>= (.isDeleted),
        Beam.journeyId = Kernel.Prelude.fmap (.journeyId) journeyLegInfo,
        Beam.journeyLegOrder = Kernel.Prelude.fmap (.journeyLegOrder) journeyLegInfo,
        Beam.onSearchFailed = journeyLegInfo >>= (.onSearchFailed),
        Beam.pricingId = journeyLegInfo >>= (.pricingId),
        Beam.skipBooking = Kernel.Prelude.fmap (.skipBooking) journeyLegInfo,
        Beam.language = language,
        Beam.maxDistance = Kernel.Utils.Common.getHighPrecMeters . Kernel.Utils.Common.distanceToHighPrecMeters <$> maxDistance,
        Beam.maxDistanceValue = Kernel.Utils.Common.distanceToHighPrecDistance distanceUnit <$> distance,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.originStopCode = originStopCode,
        Beam.placeNameSource = placeNameSource,
        Beam.recentLocationId = Kernel.Types.Id.getId <$> recentLocationId,
        Beam.returnTime = returnTime,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.riderPreferredOption = Just riderPreferredOption,
        Beam.roundTrip = roundTrip,
        Beam.routeCode = routeCode,
        Beam.selectedPaymentMethodId = selectedPaymentMethodId,
        Beam.startTime = startTime,
        Beam.toLocationId = Kernel.Types.Id.getId <$> (toLocation <&> (.id)),
        Beam.totalRidesCount = totalRidesCount,
        Beam.validTill = validTill,
        Beam.vehicleCategory = vehicleCategory
      }
