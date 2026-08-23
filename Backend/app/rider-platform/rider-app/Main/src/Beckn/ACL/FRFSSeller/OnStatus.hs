module Beckn.ACL.FRFSSeller.OnStatus
  ( buildOnStatusReq,
    buildOnStatusErrorReq,
  )
where

import qualified Beckn.ACL.FRFS.Utils as ACLUtils
import qualified Beckn.ACL.FRFSSeller.OnConfirm as OnConfirm
import qualified Beckn.ACL.FRFSSeller.OnSearch as OnSearch
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import Kernel.Prelude
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339 (..))

buildOnStatusReq :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> Spec.OrderStatus -> OnConfirm.ConfirmedOrder -> Spec.OnStatusReq
buildOnStatusReq self now ctx orderStatus order =
  Spec.OnStatusReq
    { onStatusReqContext = mkCallbackContext self now ctx,
      onStatusReqError = Nothing,
      onStatusReqMessage = Just (Spec.ConfirmReqMessage {confirmReqMessageOrder = mkOrder now orderStatus order})
    }

buildOnStatusErrorReq :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> Spec.Error -> Spec.OnStatusReq
buildOnStatusErrorReq self now ctx err =
  Spec.OnStatusReq
    { onStatusReqContext = mkCallbackContext self now ctx,
      onStatusReqError = Just err,
      onStatusReqMessage = Nothing
    }

mkCallbackContext :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> Spec.Context
mkCallbackContext self now ctx =
  ctx{Spec.contextAction = ACLUtils.encodeToText' Spec.ON_STATUS,
      Spec.contextBppId = Just self.subscriberId,
      Spec.contextBppUri = Just self.subscriberUrl,
      Spec.contextTimestamp = Just (UTCTimeRFC3339 now),
      Spec.contextTtl = Just OnSearch.sellerCallbackTtl,
      Spec.contextVersion = Just OnSearch.sellerContextVersion
     }

mkOrder :: UTCTime -> Spec.OrderStatus -> OnConfirm.ConfirmedOrder -> Spec.Order
mkOrder now orderStatus order =
  (OnConfirm.mkOrder order{OnConfirm.tickets = map blankCancelledToken order.tickets})
    { Spec.orderStatus = ACLUtils.encodeToText' orderStatus,
      Spec.orderUpdatedAt = Just now
    }

blankCancelledToken :: OnConfirm.IssuedTicket -> OnConfirm.IssuedTicket
blankCancelledToken ticket
  | ticket.qrStatus == "CANCELLED" = ticket{OnConfirm.qrToken = ""}
  | otherwise = ticket
