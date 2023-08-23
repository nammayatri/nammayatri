module Screens.RideHistoryScreen.Transformer where

import Prelude

import Components.PaymentHistoryListItem.Controller as PaymentHistoryListItem
import Services.API (PaymentDetailsEntity(..), PaymentBreakUp(..), TxnInfo(..))
import Common.Types.App (PaymentStatus(..), APIPaymentStatus(..))
import Data.Array as DA
import Data.Maybe (Maybe(..))

getPaymentHistoryItemList :: Array PaymentDetailsEntity -> Array PaymentHistoryListItem.Config
getPaymentHistoryItemList arr = appendArray $ map (\item -> let (PaymentDetailsEntity x) = item
    in (map (\(TxnInfo txnInfo) -> getPaymentHistoryItemm item txnInfo.status ) x.txnInfo)
    ) arr

appendArray :: Array (Array PaymentHistoryListItem.Config) -> Array PaymentHistoryListItem.Config
appendArray item = case (DA.head item) of
  Nothing -> []
  Just a -> a <> (appendArray (case (DA.tail item) of
    Nothing -> []
    Just b -> b))

getPaymentHistoryItemm :: PaymentDetailsEntity -> APIPaymentStatus -> PaymentHistoryListItem.Config
getPaymentHistoryItemm (PaymentDetailsEntity item) payStatus =
  let firstTxnInfo = DA.last item.txnInfo
  in  {
    isSelected : false
    , charges : item.charges
    , totalEarning : item.totalEarnings
    , totalRides : item.totalRides
    , date : item.date
    , status :  case payStatus of
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
    , id : case firstTxnInfo of
                Just (TxnInfo txn) -> txn.id
                Nothing -> item.driverFeeId
    , paymentBreakUp : (\(PaymentBreakUp charge) -> {
        description: charge.component
        , amount : charge.amount
    }) <$> item.chargesBreakup
}