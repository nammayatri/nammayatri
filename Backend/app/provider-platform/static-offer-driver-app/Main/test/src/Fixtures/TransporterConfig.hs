 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
