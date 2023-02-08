module Fixtures.TransporterConfig (defaultTransporterConfig) where

import qualified Domain.Types.TransporterConfig as DTConf
import EulerHS.Prelude
import qualified Fixtures.BaseUrl as Fixtures
import qualified Fixtures.Person as Fixtures
import qualified Fixtures.Time as Fixtures
import qualified Kernel.External.FCM.Types as FCM (FCMConfig (..))
import Kernel.Types.Common (Meters (..))
import Kernel.Types.Time (Seconds (..))

defaultTransporterConfig :: DTConf.TransporterConfig
defaultTransporterConfig =
  DTConf.TransporterConfig
    { merchantId = Fixtures.defaultMerchantId,
      pickupLocThreshold = Just $ Meters 500,
      dropLocThreshold = Just $ Meters 500,
      rideTravelledDistThresholdWhenPickupOrDestIsDiff = Just $ Meters 700,
      rideTravelledDistThresholdWhenPickupAndDestIsSame = Just $ Meters 1200,
      rideTimeEstimatedThreshold = Just $ Seconds 900,
      waitingTimeEstimatedThreshold = Just $ Seconds 3,
      availabilityTimeWeightage = 70,
      acceptanceRatioWeightage = 40,
      cancellationRatioWeightage = -40,
      maxRadius = Meters 1500,
      minRadius = Meters 500,
      radiusStepSize = Meters 500,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime,
      fcmConfig = defaultFCMConfig
    }

defaultFCMConfig :: FCM.FCMConfig
defaultFCMConfig =
  FCM.FCMConfig
    { fcmUrl = Fixtures.defaultUrl,
      fcmServiceAccount = "",
      fcmTokenKeyPrefix = ""
    }
