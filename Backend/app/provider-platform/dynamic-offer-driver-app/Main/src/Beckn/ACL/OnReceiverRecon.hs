module Beckn.ACL.OnReceiverRecon (buildOnReceiverReconReq) where

import qualified BecknV2.RSF.Types as Spec
import qualified BecknV2.RSF.Utils as RSFUtils
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339 (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder as RSO
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement as RUS
import SharedLogic.RSFOrderStatus (computeOrderStatus)

buildOnReceiverReconReq ::
  (MonadFlow m) =>
  RUS.ReconUtrSettlement ->
  [RSO.ReconSettlementOrder] ->
  Text ->
  Text ->
  m Spec.OnReceiverReconReq
buildOnReceiverReconReq utr orders bppId bppUri = do
  now <- getCurrentTime
  messageId <- generateGUID
  let sendableOrders = filter (\rso -> rso.ourReconStatus /= RSO.PENDING) orders
      grouped = Map.fromListWith (<>) [(r.orderId, [r]) | r <- sendableOrders]
      wireOrders = map (uncurry buildWireOrder) (Map.toList grouped)
      ctx =
        RSFUtils.buildRSFContext
          "ONDC:NTS10"
          "on_receiver_recon"
          utr.bapId
          utr.bapUri
          bppId
          bppUri
          messageId
          (UTCTimeRFC3339 now)
          (Just "P2D")
  pure
    Spec.OnReceiverReconReq
      { onReceiverReconReqContext = ctx,
        onReceiverReconReqMessage =
          Spec.RSFOnReceiverReconMessage
            { rsfOnReceiverReconMessageOrderbook =
                Spec.RSFOnReceiverReconOrderbook
                  { rsfOnReceiverReconOrderbookOrders = wireOrders
                  }
            }
      }

buildWireOrder :: Text -> [RSO.ReconSettlementOrder] -> Spec.RSFOnReceiverReconOrder
buildWireOrder orderId rsoRows =
  let firstRow = head rsoRows
      fare = fromMaybe 0 firstRow.platformGrossFare
      (orderVerdict, orderDiff) = computeOrderStatus fare rsoRows
      (cpStatus, diffAmt, diffMsg) = verdictToWire orderVerdict (Just orderDiff)
      settlementDetails = map buildSettlementDetail rsoRows
   in Spec.RSFOnReceiverReconOrder
        { rsfOnOrderId = Just orderId,
          rsfOnOrderInvoiceNo = firstRow.invoiceNo,
          rsfOnOrderCollectorAppId = Nothing,
          rsfOnOrderReceiverAppId = Nothing,
          rsfOnOrderOrderReconStatus = Just firstRow.wireOrderReconStatus,
          rsfOnOrderTransactionId = Just firstRow.orderTransactionId,
          rsfOnOrderSettlementId = Just firstRow.settlementId,
          rsfOnOrderCounterpartyReconStatus = Just cpStatus,
          rsfOnOrderCounterpartyDiffAmount = diffAmt,
          rsfOnOrderMessage = diffMsg,
          rsfOnOrderSettlementDetails = Just settlementDetails
        }

buildSettlementDetail :: RSO.ReconSettlementOrder -> Spec.RSFOnReconSettlementDetail
buildSettlementDetail rso =
  Spec.RSFOnReconSettlementDetail
    { rsfOnReconSdSettlementId = Just rso.settlementId,
      rsfOnReconSdSettlementReferenceNo = Just rso.settlementReferenceNo
    }

verdictToWire ::
  RSO.OrderReconVerdict ->
  Maybe HighPrecMoney ->
  (Text, Maybe Spec.RSFCounterpartyDiffAmount, Maybe Spec.RSFDiffMessage)
verdictToWire verdict mbDiff = case verdict of
  RSO.PAID -> ("01", Nothing, Nothing)
  RSO.UNDERPAID ->
    ( "03",
      mkDiffAmount mbDiff,
      Just $ Spec.RSFDiffMessage (Just "Settlement amount less than expected") (Just "less")
    )
  RSO.OVERPAID ->
    ( "02",
      mkDiffAmount mbDiff,
      Just $ Spec.RSFDiffMessage (Just "Settlement amount more than expected") (Just "more")
    )
  RSO.NOT_PAID -> ("04", Nothing, Nothing)
  RSO.UNMATCHED ->
    ( "03",
      mkDiffAmount mbDiff,
      Just $ Spec.RSFDiffMessage (Just "Order not found in platform records") (Just "less")
    )
  RSO.PENDING -> ("01", Nothing, Nothing)

mkDiffAmount :: Maybe HighPrecMoney -> Maybe Spec.RSFCounterpartyDiffAmount
mkDiffAmount = fmap $ \amt ->
  Spec.RSFCounterpartyDiffAmount
    { rsfDiffAmountCurrency = Just "INR",
      rsfDiffAmountValue = Just (T.pack $ show (abs amt))
    }
