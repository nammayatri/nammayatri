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

module Domain.Types.DriverFee where

import Data.Aeson
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person (Driver)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, Money)
import Kernel.Types.Id
import Kernel.Utils.TH (mkHttpInstancesForEnum)
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
    collectedAt :: Maybe UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    feeType :: FeeType,
    offerId :: Maybe Text,
    planOfferTitle :: Maybe Text,
    billNumber :: Maybe Int,
    autopayPaymentStage :: Maybe AutopayPaymentStage,
    schedulerTryCount :: Int,
    stageUpdatedAt :: Maybe UTCTime,
    amountPaidByCoin :: Maybe HighPrecMoney,
    feeWithoutDiscount :: Maybe HighPrecMoney,
    overlaySent :: Bool
  }
  deriving (Generic, Show, Eq)

data PlatformFee = PlatformFee
  { fee :: HighPrecMoney,
    cgst :: HighPrecMoney,
    sgst :: HighPrecMoney
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

data DriverFeeStatus = ONGOING | PAYMENT_PENDING | PAYMENT_OVERDUE | CLEARED | EXEMPTED | COLLECTED_CASH | INACTIVE | CLEARED_BY_YATRI_COINS deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

data FeeType = MANDATE_REGISTRATION | RECURRING_INVOICE | RECURRING_EXECUTION_INVOICE deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

data AutopayPaymentStage = NOTIFICATION_SCHEDULED | NOTIFICATION_ATTEMPTING | EXECUTION_SCHEDULED | EXECUTION_ATTEMPTING | EXECUTION_SUCCESS | EXECUTION_FAILED deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

paymentProcessingLockKey :: Text -> Text
paymentProcessingLockKey driverId = "Payment:Processing:DriverId" <> driverId

mandateProcessingLockKey :: Text -> Text
mandateProcessingLockKey driverId = "Mandate:Processing:DriverId" <> driverId

billNumberGenerationLockKey :: Text -> Text
billNumberGenerationLockKey merchantId = "DriverFee:BillNumber:Processing:MerchantId" <> merchantId --- make lock on merchant Id

$(mkBeamInstancesForEnum ''DriverFeeStatus)

$(mkBeamInstancesForEnum ''FeeType)

$(mkBeamInstancesForEnum ''AutopayPaymentStage)

$(mkHttpInstancesForEnum ''DriverFeeStatus)

$(mkHttpInstancesForEnum ''FeeType)
