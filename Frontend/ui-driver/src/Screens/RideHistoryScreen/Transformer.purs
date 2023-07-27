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
    , status : case firstTxnInfo of
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
                Nothing -> Pending
    , id : case firstTxnInfo of
                Just (TxnInfo item) -> item.id
                Nothing -> ""
    , paymentBreakUp : (\(PaymentBreakUp charge) -> {
        description: charge.component
        , amount : charge.amount
    }) <$> item.chargesBreakup
}

dummy :: Array PaymentHistoryListItem.Config
dummy = [
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : Pending
    , id : "1"
    , paymentBreakUp : (\(PaymentBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : Pending
    , id : "2"
    , paymentBreakUp : (\(PaymentBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : Pending
    , id : "3"
    , paymentBreakUp : (\(PaymentBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : Pending
    , id : "4"
    , paymentBreakUp : (\(PaymentBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : Pending
    , id : "5"
    , paymentBreakUp : (\(PaymentBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : Success
    , id : "6"
    , paymentBreakUp : (\(PaymentBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : Pending
    , id : "7"
    , paymentBreakUp : (\(PaymentBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : Failed
    , id : "8"
    , paymentBreakUp : (\(PaymentBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : Success
    , id : "9"
    , paymentBreakUp : (\(PaymentBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray}

]
dummyArray :: Array PaymentBreakUp
dummyArray = [
       PaymentBreakUp {
            "component": "GST",
            "amount": 10.0
        },
       PaymentBreakUp {
            "component": "PlatformFee",
            "amount": 130.0
        }
    ]