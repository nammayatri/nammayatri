{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Recon
  ( buildReconReq,
    settlementStatusToWire,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Types.Recon as ReconSpec
import qualified Data.UUID.V4 as UUIDv4
import qualified Domain.Types.BecknConfig as DBC
import Domain.Types.PPFRecon
import Kernel.Prelude
import qualified Kernel.Types.TimeRFC339 as TimeRFC339
import Kernel.Utils.Common

-- | Build a BECKN recon request from domain PPFRecon entries.
-- Called by the BAP (Collector) to send recon to BPP (Receiver).
buildReconReq ::
  (MonadFlow m) =>
  DBC.BecknConfig ->
  [PPFRecon] ->
  m ReconSpec.ReconReq
buildReconReq bapConfig reconEntries = do
  now <- getCurrentTime
  messageId <- liftIO UUIDv4.nextRandom
  orderRecons <- mapM buildOrderRecon reconEntries
  let context =
        Spec.Context
          { contextAction = Just "recon",
            contextBapId = Just bapConfig.subscriberId,
            contextBapUri = Just $ showBaseUrl bapConfig.subscriberUrl,
            contextBppId = Nothing, -- set per-BPP when sending
            contextBppUri = Nothing,
            contextDomain = Just bapConfig.domain,
            contextKey = Nothing,
            contextLocation = Nothing,
            contextMessageId = Just messageId,
            contextTimestamp = Just $ TimeRFC339.UTCTimeRFC3339 now,
            contextTransactionId = Nothing,
            contextTtl = Nothing,
            contextVersion = Just "2.0.0"
          }
  pure
    ReconSpec.ReconReq
      { reconReqContext = context,
        reconReqMessage =
          ReconSpec.ReconMessage
            { reconMessageOrderRecons = orderRecons
            }
      }

-- | Build an OrderRecon from a PPFRecon domain type
buildOrderRecon :: (MonadFlow m) => PPFRecon -> m ReconSpec.OrderRecon
buildOrderRecon recon = do
  let currencyText = show recon.orderAmount.currency
      mkAmount amt = ReconSpec.ReconAmount currencyText (show amt)
      mkMaybeAmount mPrice = (\p -> ReconSpec.ReconAmount currencyText (show p.amount)) <$> mPrice
  pure
    ReconSpec.OrderRecon
      { orderReconOrderId = recon.networkOrderId,
        orderReconTransactionId = recon.transactionId,
        orderReconPaymentTransactionId = recon.paymentTransactionId,
        orderReconPaymentReference = recon.paymentReference,
        orderReconOrderAmount = mkAmount recon.orderAmount.amount,
        orderReconPaymentAmount = mkAmount recon.paymentAmount.amount,
        orderReconSellerShare = mkAmount recon.sellerShare.amount,
        orderReconBuyerAppCommission = mkAmount recon.buyerAppCommission.amount,
        orderReconSettlement = buildSettlement recon currencyText,
        orderReconOrderStatus = recon.orderStatus,
        orderReconPaymentStatus = show recon.paymentStatus,
        orderReconSettlementStatus = settlementStatusToWire recon.settlementStatus,
        orderReconTimestamps =
          ReconSpec.ReconTimestamps
            { reconTimestampsCreatedAt = Just $ show (TimeRFC339.UTCTimeRFC3339 recon.createdAt),
              reconTimestampsFulfilledAt = show . TimeRFC339.UTCTimeRFC3339 <$> recon.fulfilledTimestamp,
              reconTimestampsSettledAt = show . TimeRFC339.UTCTimeRFC3339 <$> recon.settledTimestamp
            },
        orderReconWithholdingAmount = mkMaybeAmount recon.withholdingAmount,
        orderReconTds = mkMaybeAmount recon.tds,
        orderReconTcs = mkMaybeAmount recon.tcs,
        orderReconNetworkFee = mkMaybeAmount recon.networkFee,
        orderReconGstAmount = mkMaybeAmount recon.gstAmount
      }

-- | Build settlement details if available
buildSettlement :: PPFRecon -> Text -> Maybe ReconSpec.ReconSettlement
buildSettlement recon currencyText =
  case recon.settlementAmount of
    Just settleAmt ->
      Just
        ReconSpec.ReconSettlement
          { reconSettlementId = Nothing,
            reconSettlementAmount = ReconSpec.ReconAmount currencyText (show settleAmt.amount),
            reconSettlementRefNo = recon.settlementRefNo,
            reconSettlementStatus = settlementStatusToWire recon.settlementStatus,
            reconSettlementCommission = Just $ ReconSpec.ReconAmount currencyText (show recon.buyerAppCommission.amount),
            reconSettlementWithholdingAmount = (\p -> ReconSpec.ReconAmount currencyText (show p.amount)) <$> recon.withholdingAmount,
            reconSettlementTds = (\p -> ReconSpec.ReconAmount currencyText (show p.amount)) <$> recon.tds,
            reconSettlementTcs = (\p -> ReconSpec.ReconAmount currencyText (show p.amount)) <$> recon.tcs
          }
    Nothing -> Nothing

-- | Map internal settlement status enum to ONDC wire format.
-- ONDC expects "SETTLED"/"FAILED" not "SETTLEMENT_SETTLED"/"SETTLEMENT_FAILED".
settlementStatusToWire :: PPFSettlementStatus -> Text
settlementStatusToWire SETTLEMENT_SETTLED = "SETTLED"
settlementStatusToWire SETTLEMENT_FAILED = "FAILED"
settlementStatusToWire s = show s
