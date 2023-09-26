{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.Merchant.PushNotification where

import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Merchant (Merchant)
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data PushNotificationKey
  = PAYMENT_FAILED_AUTOPAY
  | PAYMENT_FAILED_MANUAL
  | PAYMENT_PENDING
  | PLAN_SWITCH
  deriving (Generic, Show, Read, FromJSON, ToJSON, Eq, Ord)

$(mkBeamInstancesForEnum ''PushNotificationKey)

data NotificationSubType
  = LOW_ACCOUNT_BALANCE
  | PAYMENT_FAILED_LOW_ACCOUNT_BALANCE
  | SWITCH_PLAN
  | PAYMENT_PENDING_AUTOPAY_SET
  | PAYMENT_PENDING_AUTOPAY_NOT_SET
  | AUTOPAY_PAYMENT_FAILED
  | MANUAL_PAYMENT_FAILED
  deriving (Generic, Show, Read, FromJSON, ToJSON, Eq, Ord)

$(mkBeamInstancesForEnum ''NotificationSubType)

data PushNotificationD (s :: UsageSafety) = PushNotification
  { merchantId :: Id Merchant,
    pushNotificationKey :: PushNotificationKey,
    language :: Language,
    udf1 :: Maybe Text,
    notificationSubType :: NotificationSubType,
    icon :: Maybe Text,
    title :: Text,
    body :: Text
  }
  deriving (Generic, Show)

type PushNotification = PushNotificationD 'Safe

instance FromJSON (PushNotificationD 'Unsafe)

instance ToJSON (PushNotificationD 'Unsafe)
