{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Recon
  ( DReconReq (..),
    DReconOrderEntry (..),
    buildReconDomainReq,
  )
where

import qualified BecknV2.OnDemand.Types.Recon as ReconSpec
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import Tools.Error

-- | Domain-level recon request parsed from BECKN recon message.
-- Used on the BPP (Receiver) side.
data DReconReq = DReconReq
  { collectorSubscriberId :: Text,
    domain :: Text,
    orderEntries :: [DReconOrderEntry]
  }
  deriving (Show, Generic)

-- | Individual order entry from a recon request
data DReconOrderEntry = DReconOrderEntry
  { orderId :: Text,
    transactionId :: Text,
    paymentTransactionId :: Maybe Text,
    paymentReference :: Maybe Text,
    orderAmount :: HighPrecMoney,
    paymentAmount :: HighPrecMoney,
    sellerShare :: HighPrecMoney,
    buyerAppCommission :: HighPrecMoney,
    orderStatus :: Text,
    paymentStatus :: Text,
    settlementStatus :: Text,
    currency :: Text,
    createdAt :: Maybe Text,
    fulfilledAt :: Maybe Text
  }
  deriving (Show, Generic)

-- | Parse BECKN recon request into domain types.
-- Called by the BPP when receiving recon from BAP.
buildReconDomainReq ::
  (MonadFlow m) =>
  ReconSpec.ReconReq ->
  m DReconReq
buildReconDomainReq reconReq = do
  let ctx = reconReq.reconReqContext
  collectorSubId <- ctx.contextBapId & fromMaybeM (InvalidRequest "Missing BAP subscriber ID in recon context")
  domainText <- ctx.contextDomain & fromMaybeM (InvalidRequest "Missing domain in recon context")
  let orderRecons = reconReq.reconReqMessage.reconMessageOrderRecons
  entries <- mapM parseOrderEntry orderRecons
  pure
    DReconReq
      { collectorSubscriberId = collectorSubId,
        domain = domainText,
        orderEntries = entries
      }

-- | Parse an individual order entry from a BECKN recon message.
-- Logs warnings for invalid amounts instead of silently defaulting to 0.
parseOrderEntry :: (MonadFlow m) => ReconSpec.OrderRecon -> m DReconOrderEntry
parseOrderEntry entry = do
  let orderId = entry.orderReconOrderId
  orderAmt <- parseAmount orderId "orderAmount" entry.orderReconOrderAmount.reconAmountValue
  paymentAmt <- parseAmount orderId "paymentAmount" entry.orderReconPaymentAmount.reconAmountValue
  sellerAmt <- parseAmount orderId "sellerShare" entry.orderReconSellerShare.reconAmountValue
  commissionAmt <- parseAmount orderId "buyerAppCommission" entry.orderReconBuyerAppCommission.reconAmountValue
  pure
    DReconOrderEntry
      { orderId = orderId,
        transactionId = entry.orderReconTransactionId,
        paymentTransactionId = entry.orderReconPaymentTransactionId,
        paymentReference = entry.orderReconPaymentReference,
        orderAmount = orderAmt,
        paymentAmount = paymentAmt,
        sellerShare = sellerAmt,
        buyerAppCommission = commissionAmt,
        orderStatus = entry.orderReconOrderStatus,
        paymentStatus = entry.orderReconPaymentStatus,
        settlementStatus = entry.orderReconSettlementStatus,
        currency = entry.orderReconOrderAmount.reconAmountCurrency,
        createdAt = entry.orderReconTimestamps.reconTimestampsCreatedAt,
        fulfilledAt = entry.orderReconTimestamps.reconTimestampsFulfilledAt
      }

-- | Parse an amount from text with warning on failure.
-- Defaults to 0 with a warning log to not silently mask data issues.
parseAmount :: (MonadFlow m) => Text -> Text -> Text -> m HighPrecMoney
parseAmount orderId fieldName amtText =
  case readMaybe (T.unpack amtText) of
    Just amt -> pure amt
    Nothing -> do
      logWarning $ "Invalid " <> fieldName <> " amount '" <> amtText <> "' for orderId: " <> orderId <> ", defaulting to 0"
      pure 0
