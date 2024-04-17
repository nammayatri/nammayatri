{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SearchRequest where

import qualified Data.Text
import qualified Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
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
    toLocation' <- Storage.Queries.Transformers.SearchRequest.getToLocation id
    pure $
      Just
        Domain.Types.SearchRequest.SearchRequest
          { autoAssignEnabled = autoAssignEnabled,
            autoAssignEnabledV2 = autoAssignEnabledV2,
            availablePaymentMethods = Kernel.Types.Id.Id <$> availablePaymentMethods,
            backendAppVersion = backendAppVersion,
            backendConfigVersion = backendConfigVersion',
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion,
            clientId = Kernel.Types.Id.Id <$> clientId,
            clientSdkVersion = clientSdkVersion',
            createdAt = createdAt,
            customerExtraFee = Kernel.Utils.Common.mkPriceWithDefault customerExtraFeeAmount currency <$> customerExtraFee,
            device = device,
            disabilityTag = disabilityTag,
            distance = Kernel.Utils.Common.mkDistanceWithDefault distanceUnit distanceValue . Kernel.Types.Common.HighPrecMeters <$> distance,
            estimatedRideDuration = estimatedRideDuration,
            fromLocation = fromLocation',
            id = Kernel.Types.Id.Id id,
            language = language,
            maxDistance = Kernel.Utils.Common.mkDistanceWithDefault distanceUnit maxDistanceValue . Kernel.Types.Common.HighPrecMeters <$> maxDistance,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            riderId = Kernel.Types.Id.Id riderId,
            riderPreferredOption = fromMaybe Domain.Types.SearchRequest.OneWay riderPreferredOption,
            selectedPaymentMethodId = Kernel.Types.Id.Id <$> selectedPaymentMethodId,
            startTime = startTime,
            toLocation = toLocation',
            validTill = validTill
          }

instance ToTType' Beam.SearchRequest Domain.Types.SearchRequest.SearchRequest where
  toTType' (Domain.Types.SearchRequest.SearchRequest {..}) = do
    Beam.SearchRequestT
      { Beam.autoAssignEnabled = autoAssignEnabled,
        Beam.autoAssignEnabledV2 = autoAssignEnabledV2,
        Beam.availablePaymentMethods = Kernel.Types.Id.getId <$> availablePaymentMethods,
        Beam.backendAppVersion = backendAppVersion,
        Beam.backendConfigVersion = Kernel.Utils.Version.versionToText <$> backendConfigVersion,
        Beam.clientBundleVersion = Kernel.Utils.Version.versionToText <$> clientBundleVersion,
        Beam.clientConfigVersion = Kernel.Utils.Version.versionToText <$> clientConfigVersion,
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.clientId = Kernel.Types.Id.getId <$> clientId,
        Beam.clientSdkVersion = Kernel.Utils.Version.versionToText <$> clientSdkVersion,
        Beam.createdAt = createdAt,
        Beam.currency = customerExtraFee <&> (.currency),
        Beam.customerExtraFee = customerExtraFee <&> (.amountInt),
        Beam.customerExtraFeeAmount = customerExtraFee <&> (.amount),
        Beam.device = device,
        Beam.disabilityTag = disabilityTag,
        Beam.distance = Kernel.Utils.Common.getHighPrecMeters . Kernel.Utils.Common.distanceToHighPrecMeters <$> distance,
        Beam.distanceUnit = distance <&> (.unit),
        Beam.distanceValue = Kernel.Utils.Common.distanceToHighPrecDistance (distance <&> (.unit)) <$> distance,
        Beam.estimatedRideDuration = estimatedRideDuration,
        Beam.fromLocationId = Just $ Kernel.Types.Id.getId ((.id) fromLocation),
        Beam.id = Kernel.Types.Id.getId id,
        Beam.language = language,
        Beam.maxDistance = Kernel.Utils.Common.getHighPrecMeters . Kernel.Utils.Common.distanceToHighPrecMeters <$> maxDistance,
        Beam.maxDistanceValue = Kernel.Utils.Common.distanceToHighPrecDistance (distance <&> (.unit)) <$> distance,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.riderPreferredOption = Just riderPreferredOption,
        Beam.selectedPaymentMethodId = Kernel.Types.Id.getId <$> selectedPaymentMethodId,
        Beam.startTime = startTime,
        Beam.toLocationId = Kernel.Types.Id.getId <$> (toLocation <&> (.id)),
        Beam.validTill = validTill
      }
