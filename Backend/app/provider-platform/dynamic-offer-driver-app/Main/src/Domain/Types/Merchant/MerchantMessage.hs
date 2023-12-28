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

import Data.Default.Class
import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Types.Id
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data MessageKey
  = SEND_OTP
  | WELCOME_TO_PLATFORM
  | ALTERNATE_NUMBER_OTP
  | ONBOARD_SUPPORT_SMS_TEMPLATE
  | END_RIDE_MESSAGE
  | ONBOARDING_YATRI_MESSAGE
  | BOOKING_MESSAGE
  | CASH_COLLECTED_MESSAGE
  | SEND_PAYMENT_LINK
  | WHATSAPP_CLEAR_DUES_CALL_MISSED_MESSAGE
  | WHATSAPP_CLEAR_DUES_MESSAGE
  | WHATSAPP_CLEAR_DUES_MESSAGE_TO_BLOCKED_DRIVERS
  | WHATSAPP_SETUP_AUTOPAY_MESSAGE
  | WHATSAPP_SWITCH_PLAN_MESSAGE
  | WHATSAPP_HOW_IT_WORKS_MESSAGE
  | SMS_CLEAR_DUES_CALL_MISSED_MESSAGE
  | SMS_CLEAR_DUES_MESSAGE
  | SMS_CLEAR_DUES_MESSAGE_TO_BLOCKED_DRIVERS
  | SMS_SETUP_AUTOPAY_MESSAGE
  | SMS_SWITCH_PLAN_MESSAGE
  | SMS_HOW_IT_WORKS_MESSAGE
  deriving (Generic, Show, Read, FromJSON, ToJSON, Eq, Ord, ToSchema)

$(mkBeamInstancesForEnum ''MessageKey)

data MerchantMessageD (s :: UsageSafety) = MerchantMessage
  { merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    messageKey :: MessageKey,
    message :: Text,
    templateId :: Text,
    jsonData :: MerchantMessageDefaultDataJSON,
    containsUrlButton :: Bool,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)

type MerchantMessage = MerchantMessageD 'Safe

instance FromJSON (MerchantMessageD 'Unsafe)

instance ToJSON (MerchantMessageD 'Unsafe)

data MerchantMessageDefaultDataJSON = MerchantMessageDefaultDataJSON
  { var1 :: Maybe Text,
    var2 :: Maybe Text,
    var3 :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

instance Default MerchantMessageDefaultDataJSON where
  def =
    MerchantMessageDefaultDataJSON
      { var1 = Nothing,
        var2 = Nothing,
        var3 = Nothing
      }
