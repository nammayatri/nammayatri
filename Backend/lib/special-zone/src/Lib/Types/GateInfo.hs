{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Types.GateInfo where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map
import Kernel.External.Maps (LatLong)
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Types.SpecialLocation

data GateInfoFull = GateInfoFull
  { id :: Id GateInfo,
    specialLocationId :: Id SpecialLocation,
    defaultDriverExtra :: Maybe Int,
    point :: LatLong,
    name :: Text,
    address :: Maybe Text,
    geoJson :: Maybe Text,
    canQueueUpOnGate :: Bool,
    gateType :: GateType,
    gateTags :: Maybe [Text],
    walkDescription :: Maybe Text,
    entryFeeAmount :: Maybe Double,
    minDriverThresholds :: Maybe (Map.Map Text Int),
    maxDriverThresholds :: Maybe (Map.Map Text Int),
    demandThresholds :: Maybe (Map.Map Text Int),
    defaultMinDriverThreshold :: Maybe Int,
    defaultMaxDriverThreshold :: Maybe Int,
    defaultDemandThreshold :: Maybe Int,
    notificationCooldownInSec :: Maybe Int,
    maxRideSkipsBeforeQueueRemoval :: Maybe Int,
    pickupZoneArrivalTimeoutInSec :: Maybe Int,
    pickupRequestResponseTimeoutInSec :: Maybe Int
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data GateType = Pickup | Drop | Parking
  deriving (Read, Show, Generic, Eq, FromJSON, ToJSON, ToSchema)

data GateInfo = GateInfo
  { id :: Id GateInfo,
    point :: LatLong,
    specialLocationId :: Id SpecialLocation,
    defaultDriverExtra :: Maybe Int,
    name :: Text,
    address :: Maybe Text,
    geom :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    canQueueUpOnGate :: Bool,
    gateType :: GateType,
    merchantId :: Maybe (Id Merchant),
    merchantOperatingCityId :: Maybe (Id MerchantOperatingCity),
    gateTags :: Maybe [Text],
    walkDescription :: Maybe Text,
    entryFeeAmount :: Maybe Double,
    -- Per vehicle variant thresholds (stored as JSON in DB).
    -- Key: vehicle service tier text (e.g. "AUTO_RICKSHAW", "SEDAN").
    -- Value lookup falls back to defaultXxx field below when key is missing.
    minDriverThresholds :: Maybe (Map.Map Text Int),
    maxDriverThresholds :: Maybe (Map.Map Text Int),
    demandThresholds :: Maybe (Map.Map Text Int),
    defaultMinDriverThreshold :: Maybe Int,
    defaultMaxDriverThreshold :: Maybe Int,
    defaultDemandThreshold :: Maybe Int,
    notificationCooldownInSec :: Maybe Int,
    maxRideSkipsBeforeQueueRemoval :: Maybe Int,
    pickupZoneArrivalTimeoutInSec :: Maybe Int,
    pickupRequestResponseTimeoutInSec :: Maybe Int
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

-- | Lookup helpers: return the per-variant value if present, else the gate's default.
minDriverThresholdFor :: GateInfo -> Text -> Maybe Int
minDriverThresholdFor gate variant =
  (Map.lookup variant =<< gate.minDriverThresholds) <|> gate.defaultMinDriverThreshold

maxDriverThresholdFor :: GateInfo -> Text -> Maybe Int
maxDriverThresholdFor gate variant =
  (Map.lookup variant =<< gate.maxDriverThresholds) <|> gate.defaultMaxDriverThreshold

demandThresholdFor :: GateInfo -> Text -> Maybe Int
demandThresholdFor gate variant =
  (Map.lookup variant =<< gate.demandThresholds) <|> gate.defaultDemandThreshold
