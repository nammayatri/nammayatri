{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SearchRequest where

import qualified Data.Text
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
import qualified Kernel.Utils.Version
import qualified Storage.Beam.SearchRequest as Beam
import qualified Storage.Queries.Transformers.SearchRequest

instance FromTType' Beam.SearchRequest Domain.Types.SearchRequest.SearchRequest where
  fromTType' (Beam.SearchRequestT {..}) = do
    fromLocation' <- Storage.Queries.Transformers.SearchRequest.getFromLocation id
    toLocation' <- Storage.Queries.Transformers.SearchRequest.getToLocation id
    merchantOperatingCityId' <- Storage.Queries.Transformers.SearchRequest.backfillMOCId merchantId merchantOperatingCityId
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    pure $
      Just
        Domain.Types.SearchRequest.SearchRequest
          { id = Kernel.Types.Id.Id id,
            startTime = startTime,
            returnTime = returnTime,
            roundTrip = roundTrip,
            validTill = validTill,
            riderId = Kernel.Types.Id.Id riderId,
            clientId = Kernel.Types.Id.Id <$> clientId,
            fromLocation = fromLocation',
            toLocation = toLocation',
            distance = Kernel.Utils.Common.mkDistanceWithDefault distanceUnit distanceValue . Kernel.Types.Common.HighPrecMeters <$> distance,
            maxDistance = Kernel.Utils.Common.mkDistanceWithDefault distanceUnit maxDistanceValue . Kernel.Types.Common.HighPrecMeters <$> maxDistance,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            estimatedRideDuration = estimatedRideDuration,
            device = device,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            language = language,
            disabilityTag = disabilityTag,
            customerExtraFee = Kernel.Utils.Common.mkPriceWithDefault customerExtraFeeAmount currency <$> customerExtraFee,
            autoAssignEnabled = autoAssignEnabled,
            autoAssignEnabledV2 = autoAssignEnabledV2,
            availablePaymentMethods = Kernel.Types.Id.Id <$> availablePaymentMethods,
            selectedPaymentMethodId = Kernel.Types.Id.Id <$> selectedPaymentMethodId,
            riderPreferredOption = fromMaybe Domain.Types.SearchRequest.OneWay riderPreferredOption,
            createdAt = createdAt,
            clientBundleVersion = clientBundleVersion',
            clientSdkVersion = clientSdkVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion,
            backendConfigVersion = backendConfigVersion',
            backendAppVersion = backendAppVersion,
            isAdvanceBookingEnabled = isAdvanceBookingEnabled
          }

instance ToTType' Beam.SearchRequest Domain.Types.SearchRequest.SearchRequest where
  toTType' (Domain.Types.SearchRequest.SearchRequest {..}) = do
    Beam.SearchRequestT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.startTime = startTime,
        Beam.returnTime = returnTime,
        Beam.roundTrip = roundTrip,
        Beam.validTill = validTill,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.clientId = Kernel.Types.Id.getId <$> clientId,
        Beam.fromLocationId = Just $ Kernel.Types.Id.getId ((.id) fromLocation),
        Beam.toLocationId = Kernel.Types.Id.getId <$> (toLocation <&> (.id)),
        Beam.distance = Kernel.Utils.Common.getHighPrecMeters . Kernel.Utils.Common.distanceToHighPrecMeters <$> distance,
        Beam.distanceValue = Kernel.Utils.Common.distanceToHighPrecDistance distanceUnit <$> distance,
        Beam.maxDistance = Kernel.Utils.Common.getHighPrecMeters . Kernel.Utils.Common.distanceToHighPrecMeters <$> maxDistance,
        Beam.maxDistanceValue = Kernel.Utils.Common.distanceToHighPrecDistance distanceUnit <$> distance,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.estimatedRideDuration = estimatedRideDuration,
        Beam.device = device,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.language = language,
        Beam.disabilityTag = disabilityTag,
        Beam.currency = customerExtraFee <&> (.currency),
        Beam.customerExtraFee = customerExtraFee <&> (.amountInt),
        Beam.customerExtraFeeAmount = customerExtraFee <&> (.amount),
        Beam.autoAssignEnabled = autoAssignEnabled,
        Beam.autoAssignEnabledV2 = autoAssignEnabledV2,
        Beam.availablePaymentMethods = Kernel.Types.Id.getId <$> availablePaymentMethods,
        Beam.selectedPaymentMethodId = Kernel.Types.Id.getId <$> selectedPaymentMethodId,
        Beam.riderPreferredOption = Just riderPreferredOption,
        Beam.createdAt = createdAt,
        Beam.clientBundleVersion = Kernel.Utils.Version.versionToText <$> clientBundleVersion,
        Beam.clientSdkVersion = Kernel.Utils.Version.versionToText <$> clientSdkVersion,
        Beam.clientConfigVersion = Kernel.Utils.Version.versionToText <$> clientConfigVersion,
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.backendConfigVersion = Kernel.Utils.Version.versionToText <$> backendConfigVersion,
        Beam.backendAppVersion = backendAppVersion,
        Beam.isAdvanceBookingEnabled = isAdvanceBookingEnabled
      }
