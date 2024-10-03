{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module RemoteConfig.Types (
    module RemoteConfig.Types
  , module Reexport
  ) where

import Prelude
import Data.Maybe(Maybe(..))
import Common.Types.App(ReelButtonConfig(..), ReelItem(..), ReelVideoThresholdConfig(..)) as Reexport
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Foreign.Generic (class Decode, class Encode)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultEnumDecode, defaultEnumEncode)

type RCSubscription = {
    max_dues_limit :: Number,
    low_dues_warning_limit :: Number,
    high_due_warning_limit :: Number
}

type HVConfigs = {
  selfie_flow_id :: String,
  pan_flow_id :: String,
  aadhaar_flow_id :: String
}

data CancellationRateEntity = HOW_TO_REDUCE | WHAT_HAPPENS

derive instance genericCancellationRateEntity :: Generic CancellationRateEntity _
instance decodeCancellationRateEntity :: Decode CancellationRateEntity where decode = defaultEnumDecode
instance encodeCancellationRateEntity :: Encode CancellationRateEntity where encode = defaultEnumEncode
instance eqCancellationRateEntity :: Eq CancellationRateEntity where eq = genericEq

type CancellationRateConfig = {
  title :: String,
  description :: String,
  entity_type :: CancellationRateEntity
}

type CancellationThresholdConfig  = {
  warning1 :: Int,
  warning2 :: Int
}

type ReferralPopUpDelays = {
  refer_now :: Int,
  add_upi :: Int, 
  verify_upi :: Int
}

type MetroCoinsEvent = {
  coins :: Int,
  minDistance :: Int
}

type EnableOtpRideConfig = {
    enableOtpRide :: Boolean
}

type EnableScheduledRides = {
  enableScheduledRides :: Boolean
}