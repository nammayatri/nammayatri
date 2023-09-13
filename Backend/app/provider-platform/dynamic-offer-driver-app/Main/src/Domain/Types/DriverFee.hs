{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.DriverFee where

import Data.Aeson
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person (Driver)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, Money)
import Kernel.Types.Id
import Servant.API
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data DriverFee = DriverFee
  { id :: Id DriverFee,
    merchantId :: Id Merchant,
    driverId :: Id Driver,
    govtCharges :: Money,
    platformFee :: PlatformFee,
    numRides :: Int,
    payBy :: UTCTime,
    totalEarnings :: Money,
    startTime :: UTCTime,
    endTime :: UTCTime,
    status :: DriverFeeStatus,
    collectedBy :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    feeType :: FeeType,
    offerId :: Maybe Text,
    planOfferTitle :: Maybe Text,
    billNumber :: Maybe Int,
    autopayPaymentStage :: Maybe AutopayPaymentStage,
    stageUpdatedAt :: Maybe UTCTime,
    feeWithoutDiscount :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, Eq)

data PlatformFee = PlatformFee
  { fee :: HighPrecMoney,
    cgst :: HighPrecMoney,
    sgst :: HighPrecMoney
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

data DriverFeeStatus = ONGOING | PAYMENT_PENDING | PAYMENT_OVERDUE | CLEARED | EXEMPTED | COLLECTED_CASH | INACTIVE deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

data FeeType = MANDATE_REGISTRATION | RECURRING_INVOICE | RECURRING_EXECUTION_INVOICE deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

data AutopayPaymentStage = NOTIFICATION_SCHEDULED | NOTIFICATION_ATTEMPTING | EXECUTION_SCHEDULED | EXECUTION_ATTEMPTING | EXECUTION_SUCCESS | EXECUTION_FAILED deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

paymentProcessingLockKey :: Text -> Text
paymentProcessingLockKey driverId = "Payment:Processing:DriverId" <> driverId

mandateProcessingLockKey :: Text -> Text
mandateProcessingLockKey mandateId = "Mandate:Processing:MandateId" <> mandateId

billNumberGenerationLockKey :: Text -> Text
billNumberGenerationLockKey merchantId = "DriverFee:BillNumber:Processing:MerchantId" <> merchantId --- make lock on merchant Id

instance FromHttpApiData DriverFeeStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = BF.first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData DriverFeeStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

instance FromHttpApiData FeeType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = BF.first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData FeeType where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

$(mkBeamInstancesForEnum ''DriverFeeStatus)

$(mkBeamInstancesForEnum ''FeeType)

$(mkBeamInstancesForEnum ''AutopayPaymentStage)
