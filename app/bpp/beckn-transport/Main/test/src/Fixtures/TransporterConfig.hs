module Fixtures.TransporterConfig (defaultTransporterConfig) where

import qualified Beckn.External.FCM.Types as FCM (FCMConfig (..))
import Beckn.Types.Common (Meters (..))
import Beckn.Types.Time (Seconds (..))
import qualified Domain.Types.TransporterConfig as DTConf
import EulerHS.Prelude
import qualified Fixtures.BaseUrl as Fixtures
import qualified Fixtures.Person as Fixtures
import qualified Fixtures.Time as Fixtures

defaultTransporterConfig :: DTConf.TransporterConfig
defaultTransporterConfig =
  DTConf.TransporterConfig
    { merchantId = Fixtures.defaultMerchantId,
      pickupLocThreshold = Just $ Meters 500,
      dropLocThreshold = Just $ Meters 500,
      rideTravelledDistanceThreshold = Just $ Meters 700,
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
