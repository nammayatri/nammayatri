module Beckn.ACL.FRFSSeller.OnCancel
  ( CancelOutcome (..),
    buildOnCancelReq,
    buildOnCancelErrorReq,
  )
where

import qualified Beckn.ACL.FRFS.Utils as ACLUtils
import qualified Beckn.ACL.FRFSSeller.OnConfirm as OnConfirm
import qualified Beckn.ACL.FRFSSeller.OnInit as OnInit
import qualified Beckn.ACL.FRFSSeller.OnSearch as OnSearch
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import Kernel.Prelude
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339 (..))

data CancelOutcome = CancelOutcome
  { orderStatus :: Spec.OrderStatus,
    dropTicketTags :: Bool,
    refundAmount :: Text,
    refundPerTicket :: Text,
    cancellationCharges :: Text,
    settlementAmount :: Text
  }
  deriving (Show, Eq)

buildOnCancelReq :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> CancelOutcome -> OnConfirm.ConfirmedOrder -> Spec.OnCancelReq
buildOnCancelReq self now ctx outcome order =
  Spec.OnCancelReq
    { onCancelReqContext = mkCallbackContext self now ctx,
      onCancelReqError = Nothing,
      onCancelReqMessage = Just (Spec.ConfirmReqMessage {confirmReqMessageOrder = mkOrder now outcome order})
    }

buildOnCancelErrorReq :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> Spec.Error -> Spec.OnCancelReq
buildOnCancelErrorReq self now ctx err =
  Spec.OnCancelReq
    { onCancelReqContext = mkCallbackContext self now ctx,
      onCancelReqError = Just err,
      onCancelReqMessage = Nothing
    }

mkCallbackContext :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> Spec.Context
mkCallbackContext self now ctx =
  ctx{Spec.contextAction = ACLUtils.encodeToText' Spec.ON_CANCEL,
      Spec.contextBppId = Just self.subscriberId,
      Spec.contextBppUri = Just self.subscriberUrl,
      Spec.contextTimestamp = Just (UTCTimeRFC3339 now),
      Spec.contextTtl = Just self.callbackTtl,
      Spec.contextVersion = Just self.contextVersion
     }

mkOrder :: UTCTime -> CancelOutcome -> OnConfirm.ConfirmedOrder -> Spec.Order
mkOrder now outcome order =
  built
    { Spec.orderStatus = ACLUtils.encodeToText' outcome.orderStatus,
      Spec.orderUpdatedAt = Just now,
      Spec.orderCancellation =
        Just
          Spec.Cancellation
            { -- Always CONSUMER, including technical cancellations. Not an oversight: the live
              -- Go service hardcodes it the same way (@metro_transformer.go:1033@ and @:1565@,
              -- via @integrationConstant.CancelledByUser@), and this has to match on the wire.
              cancellationCancelledBy = Just "CONSUMER",
              cancellationTime = Just now
            },
      Spec.orderFulfillments = map stripAuthorization <$> built.orderFulfillments,
      Spec.orderQuote = withRefundBreakup <$> built.orderQuote
    }
  where
    built =
      OnConfirm.mkOrder
        order{OnConfirm.account = order.account{OnInit.settlementAmount = outcome.settlementAmount}}

    stripAuthorization fulfillment =
      fulfillment
        { Spec.fulfillmentStops = map dropStartAuth <$> fulfillment.fulfillmentStops,
          Spec.fulfillmentTags = if outcome.dropTicketTags then Nothing else fulfillment.fulfillmentTags
        }
      where
        dropStartAuth stop
          | stop.stopType == ACLUtils.encodeToText' Spec.START = stop{Spec.stopAuthorization = Nothing}
          | otherwise = stop

    withRefundBreakup quote =
      quote
        { Spec.quotationPrice = Just (OnInit.mkPrice order.currency outcome.cancellationCharges),
          Spec.quotationBreakup = (<> [refundLine, chargesLine]) <$> quote.quotationBreakup
        }

    refundLine =
      Spec.QuotationBreakupInner
        { quotationBreakupInnerTitle = Just (show Spec.REFUND),
          quotationBreakupInnerPrice = Just (OnInit.mkPrice order.currency (negated outcome.refundAmount)),
          quotationBreakupInnerItem = perTicketRefundItem
        }

    chargesLine =
      Spec.QuotationBreakupInner
        { quotationBreakupInnerTitle = Just (show Spec.CANCELLATION_CHARGES),
          quotationBreakupInnerPrice = Just (OnInit.mkPrice order.currency outcome.cancellationCharges),
          quotationBreakupInnerItem = Nothing
        }

    perTicketRefundItem = do
      breakup <- built.orderQuote >>= (.quotationBreakup)
      baseFare <- find (\line -> line.quotationBreakupInnerTitle == Just (show Spec.BASE_FARE)) breakup
      item <- baseFare.quotationBreakupInnerItem
      pure item{Spec.itemPrice = Just (OnInit.mkPrice order.currency (negated outcome.refundPerTicket))}

negated :: Text -> Text
negated amount
  | amount `elem` ["0", "0.0", "0.00"] = amount
  | otherwise = "-" <> amount
