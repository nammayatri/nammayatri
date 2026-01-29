{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Payment.Domain.PaytmEDC
  ( createEDCSaleRequest,
    buildChecksum,
    verifyChecksum,
    paytmEDCWebhookHandler,
    initiateEDCPayment,
  )
where

import qualified Crypto.Hash.SHA256 as SHA256
import Crypto.MAC.HMAC (hmac)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Payment.Domain.Types.PaytmEDC

-- | HMAC-SHA256 with 64-byte block size
hmacSHA256 :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmacSHA256 = hmac SHA256.hash 64

-- Checksum = Base64( HMAC-SHA256( merchantKey, JSON(body) ) )
buildChecksum :: Text -> PaytmEDCRequestBody -> Text
buildChecksum merchantKey body =
  let bodyJson = LBS.toStrict $ A.encode body
      keyBytes = encodeUtf8 merchantKey
      hmacResult = hmacSHA256 keyBytes bodyJson
      checksum = Base64.encode hmacResult
   in decodeUtf8 checksum

-- | Verify checksum from Paytm webhook
verifyChecksum :: Text -> PaytmEDCResponseBody -> Text -> Bool
verifyChecksum merchantKey body receivedChecksum =
  let bodyJson = LBS.toStrict $ A.encode body
      keyBytes = encodeUtf8 merchantKey
      hmacResult = hmacSHA256 keyBytes bodyJson
      expectedChecksum = decodeUtf8 $ Base64.encode hmacResult
   in expectedChecksum == receivedChecksum

-- | Format timestamp for Paytm API (yyyy-MM-dd HH:mm:ss)
formatPaytmTimestamp :: UTCTime -> Text
formatPaytmTimestamp = pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

-- | Build the full sale request with checksum
createEDCSaleRequest ::
  PaytmEDCConfig ->
  Text ->             -- merchantTransactionId
  Text ->             -- terminalId (TID)
  Int ->              -- amount in paise
  UTCTime ->          -- current time
  PaytmEDCSaleRequest
createEDCSaleRequest config merchantTxnId terminalId amountInPaise now =
  let timestamp = formatPaytmTimestamp now

      -- as we need body for checksum so build this first.
      body = PaytmEDCRequestBody
        { paytmMid = config.paytmMid,
          paytmTid = Just terminalId,
          transactionDateTime = timestamp,
          merchantTransactionId = merchantTxnId,
          merchantReferenceNo = Nothing,
          transactionAmount = show amountInPaise,
          merchantExtendedInfo = Just MerchantExtendedInfo
            { autoAccept = Just "True",   -- Auto-accept payment
              paymentMode = Just "QR"     -- Show QR code on EDC
            },
          callbackUrl = Just config.callbackUrl
        }

      -- Calculate checksum from body
      checksum = buildChecksum config.merchantKey body

      -- Build head with checksum
      headPart = PaytmEDCRequestHead
        { requestTimeStamp = timestamp,
          channelId = config.channelId,
          checksum = checksum,
          version = Just "1.0"
        }
   in PaytmEDCSaleRequest
        { head = headPart,
          body = body
        }

-- | High-level function to initiate EDC payment
initiateEDCPayment ::
  ( MonadFlow m,
    Log m,
    MonadTime m
  ) =>
  PaytmEDCConfig ->
  Text ->             -- bookingId or orderId
  Text ->             -- terminalId (TID)
  HighPrecMoney ->    -- amount
  m PaytmEDCSaleRequest
initiateEDCPayment config orderId terminalId amount = do
  now <- getCurrentTime
  txnId <- generateGUID

  -- Convert amount to paise (multiply by 100)
  let amountInPaise = round (amount * 100) :: Int

  logInfo $ "Initiating EDC payment for orderId: " <> orderId
    <> ", terminalId: " <> terminalId
    <> ", amount: " <> show amountInPaise <> " paise"

  let request = createEDCSaleRequest config txnId terminalId amountInPaise now

-- ye transaction we have to store in db to uska bhi flow dekhna padega.

  pure request

-- Handle webhook callback from Paytm EDC
paytmEDCWebhookHandler ::
  ( MonadFlow m,
    Log m
  ) =>
  PaytmEDCConfig ->
  PaytmEDCWebhookPayload ->
  Text ->  -- checksum from header
  m PaytmEDCAckResponse
paytmEDCWebhookHandler config payload receivedChecksum = do
  let isValid = verifyChecksum config.merchantKey payload.body receivedChecksum

  unless isValid $ do
    logError "Invalid checksum in Paytm EDC webhook"
    throwError $ InvalidRequest "Invalid checksum"

  -- Step 2: Extract transaction details
  let txnId = payload.body.merchantTransactionId
      status = parsePaytmStatus payload.body.resultInfo.resultStatus
      resultCode = payload.body.resultInfo.resultCode
      resultMsg = payload.body.resultInfo.resultMsg

  logInfo $ "Received EDC webhook for txnId: " <> fromMaybe "unknown" txnId
    <> ", status: " <> show status
    <> ", resultCode: " <> resultCode
    <> ", resultMsg: " <> resultMsg

  -- Step 3: Update transaction status in DB
  case status of
    EDC_ACCEPTED -> do
      logInfo "Payment accepted, updating order status to CHARGED"
      -- TODO: QOrder.updateStatus txnId Payment.CHARGED
    EDC_FAILED -> do
      logError $ "Payment failed: " <> resultMsg
      -- TODO: QOrder.updateStatus txnId Payment.FAILED
    EDC_PENDING -> do
      logInfo "Payment pending"
      -- TODO: Handle pending status

  -- Step 4: Return acknowledgment
  pure $ PaytmEDCAckResponse { status = "SUCCESS" }
