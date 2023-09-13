{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Payment.Domain.Types.PaymentTransaction where

import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Payment.Domain.Types.Common
import Lib.Payment.Domain.Types.PaymentOrder as DOrder

data PaymentTransaction = PaymentTransaction
  { id :: Id PaymentTransaction,
    txnUUID :: Maybe Text,
    paymentMethodType :: Maybe Text,
    paymentMethod :: Maybe Text,
    respMessage :: Maybe Text,
    respCode :: Maybe Text,
    gatewayReferenceId :: Maybe Text,
    orderId :: Id PaymentOrder,
    merchantId :: Id Merchant,
    amount :: HighPrecMoney,
    currency :: Payment.Currency,
    dateCreated :: Maybe UTCTime,
    statusId :: Int,
    status :: Payment.TransactionStatus,
    juspayResponse :: Maybe Text, -- webhook resp dump
    mandateStatus :: Maybe Payment.MandateStatus,
    mandateStartDate :: Maybe UTCTime,
    mandateEndDate :: Maybe UTCTime,
    mandateId :: Maybe Text,
    bankErrorMessage :: Maybe Text,
    bankErrorCode :: Maybe Text,
    mandateFrequency :: Maybe Payment.MandateFrequency,
    mandateMaxAmount :: Maybe HighPrecMoney,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)
