{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Merchant.MerchantMessage where

import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import Kernel.Types.Id
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data MessageKey
  = INVITE_TO_UNEXISTENT_EMERGENCY_NUMBER
  | SET_AS_RIDE_EMERGENCY_NUMBER
  | SET_AS_DEFAULT_EMERGENCY_NUMBER
  | SEND_OTP
  | SEND_BOOKING_OTP
  | SEND_SOS_ALERT
  | MARK_RIDE_AS_SAFE
  deriving (Generic, Show, Read, FromJSON, ToJSON, Eq, Ord)

$(mkBeamInstancesForEnum ''MessageKey)

data MerchantMessageD (s :: UsageSafety) = MerchantMessage
  { merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    messageKey :: MessageKey,
    message :: Text,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic)

type MerchantMessage = MerchantMessageD 'Safe

instance FromJSON (MerchantMessageD 'Unsafe)

instance ToJSON (MerchantMessageD 'Unsafe)
