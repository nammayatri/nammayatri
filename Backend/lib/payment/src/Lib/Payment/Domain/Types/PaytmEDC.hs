{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Payment.Domain.Types.PaytmEDC where

import Data.Aeson
import Kernel.Prelude

-- | Paytm EDC Configuration
data PaytmEDCConfig = PaytmEDCConfig
  { paytmMid :: Text,        -- Merchant ID from Paytm
    channelId :: Text,       -- Channel ID from Paytm
    merchantKey :: Text,     -- Secret key for checksum (should be encrypted in production)
    baseUrl :: Text,         -- API endpoint
    callbackUrl :: Text      -- Webhook callback URL
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- | Request Head
data PaytmEDCRequestHead = PaytmEDCRequestHead
  { requestTimeStamp :: Text,  -- time is in year month day then hh ss krke "yyyy-MM-dd HH:mm:ss"
    channelId :: Text,
    checksum :: Text,
    version :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- | Merchant Extended Info for controlling EDC behavior
data MerchantExtendedInfo = MerchantExtendedInfo
  { autoAccept :: Maybe Text,     -- auto accept will be good.
    paymentMode :: Maybe Text     -- idhar all rakhte hai.
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- | Request Body
data PaytmEDCRequestBody = PaytmEDCRequestBody
  { paytmMid :: Text,
    paytmTid :: Maybe Text,               -- isko kaise map krna hai vo dekhte hai.
    transactionDateTime :: Text,          -- "yyyy-MM-dd HH:mm:ss"
    merchantTransactionId :: Text,
    merchantReferenceNo :: Maybe Text,
    transactionAmount :: Text,      -- ye paise me hai to 100 se multiply.
    merchantExtendedInfo :: Maybe MerchantExtendedInfo,
    callbackUrl :: Maybe Text  -- ye baar aur confirm krlete hai.
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- Sale Request
data PaytmEDCSaleRequest = PaytmEDCSaleRequest
  { head :: PaytmEDCRequestHead,
    body :: PaytmEDCRequestBody
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- Response Result Info
data PaytmEDCResultInfo = PaytmEDCResultInfo
  { resultStatus :: Text,     -- "A" (Accepted), "F" (Failed), "P" (Pending)
    resultCode :: Text,
    resultMsg :: Text,
    resultCodeId :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- | Response Head
data PaytmEDCResponseHead = PaytmEDCResponseHead
  { responseTimeStamp :: Maybe Text,
    channelId :: Maybe Text,
    version :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

--  Response Body
data PaytmEDCResponseBody = PaytmEDCResponseBody
  { merchantTransactionId :: Maybe Text,
    paytmMid :: Maybe Text,
    paytmTid :: Maybe Text,
    resultInfo :: PaytmEDCResultInfo
  }
  deriving (Generic, Show, ToJSON, FromJSON)

--  Sale Response
data PaytmEDCSaleResponse = PaytmEDCSaleResponse
  { head :: PaytmEDCResponseHead,
    body :: PaytmEDCResponseBody
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- response bhi lagbhag same hi hai.
type PaytmEDCWebhookPayload = PaytmEDCSaleResponse

-- | Paytm EDC Transaction Status
data PaytmEDCStatus
  = EDC_ACCEPTED    -- "A"
  | EDC_FAILED      -- "F"
  | EDC_PENDING     -- "P"
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | Parse Paytm status code to our status
parsePaytmStatus :: Text -> PaytmEDCStatus
parsePaytmStatus "A" = EDC_ACCEPTED
parsePaytmStatus "F" = EDC_FAILED
parsePaytmStatus _   = EDC_PENDING

-- | Webhook acknowledgment response
data PaytmEDCAckResponse = PaytmEDCAckResponse
  { status :: Text  -- "SUCCESS" or "FAILED"
  }
  deriving (Generic, Show, ToJSON, FromJSON)
