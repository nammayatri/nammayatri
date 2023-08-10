module Screens.RideHistoryScreen.Transformer where

import Prelude

import Components.PaymentHistoryListItem.Controller as PaymentHistoryListItem
import Services.API (PaymentDetailsEntity(..), PaymentBreakUp(..), TxnInfo(..))
import Common.Types.App (PaymentStatus(..), APIPaymentStatus(..))
import Data.Array as DA
import Data.Maybe (Maybe(..))

getPaymentHistoryItemList :: Array PaymentDetailsEntity -> Array PaymentHistoryListItem.Config
getPaymentHistoryItemList arr = map (\item -> getPaymentHistoryItem item) arr

getPaymentHistoryItem :: PaymentDetailsEntity -> PaymentHistoryListItem.Config
getPaymentHistoryItem (PaymentDetailsEntity item) =
  let firstTxnInfo = DA.last item.txnInfo
  in  {
    isSelected : false
    , charges : item.charges
    , totalEarning : item.totalEarnings
    , totalRides : item.totalRides
    , date : item.date
    , status : if DA.length item.txnInfo /= 0 then(case firstTxnInfo of
                Just (TxnInfo item) -> case item.status of
                                        NEW -> Pending
                                        PENDING_VBV -> Pending
                                        CHARGED -> Success
                                        AUTHENTICATION_FAILED -> Failed
                                        AUTHORIZATION_FAILED -> Failed
                                        JUSPAY_DECLINED -> Failed
                                        AUTHORIZING -> Pending
                                        COD_INITIATED -> Pending
                                        STARTED -> Pending
                                        AUTO_REFUNDED -> Pending
                Nothing -> Pending)
                else if item.status == "COLLECTED_CASH" then Success
                else Pending
    , id : case firstTxnInfo of
                Just (TxnInfo item) -> item.id
                Nothing -> ""
    , paymentBreakUp : (\(PaymentBreakUp charge) -> {
        description: charge.component
        , amount : charge.amount
    }) <$> item.chargesBreakup
}