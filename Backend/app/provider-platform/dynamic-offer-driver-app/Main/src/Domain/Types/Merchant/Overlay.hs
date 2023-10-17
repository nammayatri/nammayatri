{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.Merchant.Overlay where

import Data.Aeson
import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Merchant (Merchant)
import Domain.Types.Plan (PaymentMode (..))
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id

data OverlayCondition
  = PaymentOverdueGreaterThan Int
  | PaymentOverdueBetween Int Int
  | FreeTrialDaysLeft Int
  | InvoiceGenerated PaymentMode
  | InactiveAutopay
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data OverlayD (s :: UsageSafety) = Overlay
  { id :: Id Overlay,
    merchantId :: Id Merchant,
    overlayKey :: Text,
    language :: Language,
    udf1 :: Maybe Text,
    title :: Maybe Text,
    description :: Maybe Text,
    imageUrl :: Maybe Text,
    okButtonText :: Maybe Text,
    cancelButtonText :: Maybe Text,
    actions :: [Text],
    link :: Maybe Text,
    method :: Maybe Text,
    reqBody :: Value,
    endPoint :: Maybe Text,
    delay :: Maybe Int,
    contactSupportNumber :: Maybe Text,
    toastMessage :: Maybe Text,
    secondaryActions :: Maybe [Text],
    socialMediaLinks :: Maybe [FCM.FCMMediaLink]
  }
  deriving (Generic, Show)

type Overlay = OverlayD 'Safe

instance FromJSON (OverlayD 'Unsafe)

instance ToJSON (OverlayD 'Unsafe)
