{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module TransactionLogs.ONDC.PPFLog where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)

-- | PPF Network Observability Log - pushed to ONDC at each payment status transition
-- These logs enable ONDC to verify settlement independently without controlling it.
data PPFNetworkObservabilityLog = PPFNetworkObservabilityLog
  { -- | BECKN network order ID
    networkOrderId :: Text,
    -- | BECKN transaction ID
    transactionId :: Text,
    -- | PA/PG payment transaction ID
    paymentTransactionId :: Maybe Text,
    -- | PA/PG payment reference
    paymentReference :: Maybe Text,
    -- | Total order value
    orderAmount :: HighPrecMoney,
    -- | Amount actually paid by customer
    paymentAmount :: HighPrecMoney,
    -- | Amount due to seller/BPP
    sellerShare :: HighPrecMoney,
    -- | BAP commission (buyer finder fee)
    buyerAppCommission :: HighPrecMoney,
    -- | Logistics fee (if applicable)
    logisticsFee :: Maybe HighPrecMoney,
    -- | ONDC network fee
    networkFee :: Maybe HighPrecMoney,
    -- | GST component
    gstAmount :: Maybe HighPrecMoney,
    -- | Order status
    orderStatus :: Text,
    -- | PPF payment status (INITIATED/COLLECTED/HELD/RELEASED/SETTLED/REFUNDED/FAILED)
    paymentStatus :: Text,
    -- | Settlement status (PENDING/IN_PROGRESS/SETTLED/FAILED)
    settlementStatus :: Text,
    -- | Domain (MOBILITY/FRFS)
    domain :: Text,
    -- | Collector subscriber ID (BAP)
    collectorSubscriberId :: Text,
    -- | Receiver subscriber ID (BPP)
    receiverSubscriberId :: Text,
    -- | Withholding amount (if applicable)
    withholdingAmount :: Maybe HighPrecMoney,
    -- | Settlement reference number
    settlementRefNo :: Maybe Text,
    -- | Currency
    currency :: Currency,
    -- | Timestamp when order was created
    createdTimestamp :: UTCTime,
    -- | Timestamp when order was fulfilled
    fulfilledTimestamp :: Maybe UTCTime,
    -- | Timestamp when settlement was completed
    settledTimestamp :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON PPFNetworkObservabilityLog where
  parseJSON = genericParseJSON ppfLogOptions

instance ToJSON PPFNetworkObservabilityLog where
  toJSON = genericToJSON ppfLogOptions

ppfLogOptions :: Options
ppfLogOptions =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = camelTo2 '_'
    }
