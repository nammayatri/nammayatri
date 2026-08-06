module Beckn.ACL.OnReceiverRecon
  ( ReconReport (..),
    buildOnReceiverReconReq,
  )
where

import qualified BecknV2.RSF.Types as Spec
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339 (..))
import Kernel.Utils.Common
import qualified SharedLogic.RSFLedger as RSFLedger

-- One entry of the response. orderId = Nothing makes it a settlement-level (UTR) entry.
-- echo is the BAP_CLAIM leg the echo fields (invoice_no, settlement_id, ...) are read back from.
data ReconReport = ReconReport
  { orderId :: Maybe Text,
    utr :: Text,
    verdict :: RSFLedger.WireVerdict,
    echo :: Maybe RSFLedger.Entry
  }

-- Phase 4 (step 11): the inbound context echoed back with the new action and a fresh timestamp.
-- Every order answers with order_recon_status 02 (Finale); diff amount and message only when not 01.
buildOnReceiverReconReq :: Spec.RSFContext -> UTCTime -> Text -> [ReconReport] -> Spec.OnReceiverReconReq
buildOnReceiverReconReq inboundCtx now receiverAppId reports =
  Spec.OnReceiverReconReq
    { onReceiverReconReqContext =
        inboundCtx
          { Spec.rsfContextAction = Just "on_receiver_recon",
            Spec.rsfContextTimestamp = Just (UTCTimeRFC3339 now)
          },
      onReceiverReconReqMessage =
        Spec.RSFOnReceiverReconMessage
          { rsfOnReceiverReconMessageOrderbook =
              Spec.RSFOnReceiverReconOrderbook
                { rsfOnReceiverReconOrderbookOrders = map buildWireOrder reports
                }
          }
    }
  where
    buildWireOrder report =
      Spec.RSFOnReceiverReconOrder
        { rsfOnOrderId = report.orderId,
          rsfOnOrderInvoiceNo = report.orderId >> report.echo >>= (.invoiceNo),
          rsfOnOrderCollectorAppId = report.echo >>= (.collectorAppId),
          rsfOnOrderReceiverAppId = Just receiverAppId,
          rsfOnOrderOrderReconStatus = Just "02",
          rsfOnOrderTransactionId = report.echo >>= (.orderTransactionId),
          rsfOnOrderSettlementId = report.echo >>= (.settlementId),
          rsfOnOrderCounterpartyReconStatus = Just report.verdict.status,
          rsfOnOrderCounterpartyDiffAmount =
            if report.verdict.diff == 0
              then Nothing
              else
                Just
                  Spec.RSFCounterpartyDiffAmount
                    { rsfDiffAmountCurrency = Just "INR",
                      rsfDiffAmountValue = Just (showAmount report.verdict.diff)
                    },
          rsfOnOrderMessage = report.verdict.code $> Spec.RSFDiffMessage report.verdict.name report.verdict.code,
          rsfOnOrderSettlementReferenceNo = Just report.utr
        }

-- "100" rather than "100.0" for whole amounts, as the collector sends them.
showAmount :: HighPrecMoney -> Text
showAmount amount = let t = T.pack (show amount) in fromMaybe t (T.stripSuffix ".0" t)
