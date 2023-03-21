{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Types.Merchant.TransporterConfig where

import Data.Time (UTCTime)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import EulerHS.Prelude hiding (id)
import Kernel.External.FCM.Types (FCMConfig)
import Kernel.Types.Common
import Kernel.Types.Id

data TransporterConfigD u = TransporterConfig
  { merchantId :: Id Merchant,
    pickupLocThreshold :: Maybe Meters,
    dropLocThreshold :: Maybe Meters,
    rideTravelledDistThresholdWhenPickupOrDestIsDiff :: Maybe Meters,
    rideTravelledDistThresholdWhenPickupAndDestIsSame :: Maybe Meters,
    rideTimeEstimatedThreshold :: Maybe Seconds,
    waitingTimeEstimatedThreshold :: Maybe Seconds,
    availabilityTimeWeightage :: Int,
    acceptanceRatioWeightage :: Int,
    cancellationRatioWeightage :: Int,
    maxRadius :: Meters,
    minRadius :: Meters,
    radiusStepSize :: Meters,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    fcmConfig :: FCMConfig
  }
  deriving (Generic, Show)

type TransporterConfig = TransporterConfigD 'Safe

instance FromJSON (TransporterConfigD 'Unsafe)

instance ToJSON (TransporterConfigD 'Unsafe)

data TransporterParameter

newtype ConfigKey = ConfigKey
  { getConfigKey :: Text
  }
  deriving stock (Generic)
  deriving newtype (Show, Read, FromJSON, ToJSON)
