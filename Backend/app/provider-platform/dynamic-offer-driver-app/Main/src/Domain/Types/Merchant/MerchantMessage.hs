{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.Merchant.MerchantMessage where

import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Merchant (Merchant)
import Kernel.Prelude
import Kernel.Types.Common (fromFieldEnum)
import Kernel.Types.Id

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
  deriving (Generic, Show, Read, FromJSON, ToJSON, Eq, Ord)

instance FromField MessageKey where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be MessageKey where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be MessageKey

instance FromBackendRow Postgres MessageKey

instance IsString MessageKey where
  fromString = show

data MerchantMessageD (s :: UsageSafety) = MerchantMessage
  { merchantId :: Id Merchant,
    messageKey :: MessageKey,
    message :: Text,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)

type MerchantMessage = MerchantMessageD 'Safe

instance FromJSON (MerchantMessageD 'Unsafe)

instance ToJSON (MerchantMessageD 'Unsafe)
