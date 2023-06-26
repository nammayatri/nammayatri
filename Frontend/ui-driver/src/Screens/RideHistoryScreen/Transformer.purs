module Screens.RideHistoryScreen.Transformer where

import Prelude

import Components.PaymentHistoryListItem.Controller as PaymentHistoryListItem
import Services.APITypes (PaymentDetailsEntity(..), PaymentStatus(..), PaymnetBreakUp(..), TxnInfo(..))
import Data.Array as DA
import Data.Maybe (Maybe(..))

getPaymentHistoryItemList :: Array PaymentDetailsEntity -> Array PaymentHistoryListItem.Config
getPaymentHistoryItemList _ = dummy --map (\item -> getPaymentHistoryItem item)

getPaymentHistoryItem :: PaymentDetailsEntity -> PaymentHistoryListItem.Config
getPaymentHistoryItem (PaymentDetailsEntity item) =
  let firstTxnInfo = (item.txnInfo) DA.!! 0
  in  {
    isSelected : false
    , charges : item.charges
    , totalEarning : item.totalEarnings
    , totalRides : item.totalRides
    , date : item.date
    , status : case firstTxnInfo of
                Just (TxnInfo item) -> item.status
                Nothing -> PENDING
    , id : case firstTxnInfo of
                Just (TxnInfo item) -> item.id
                Nothing -> ""
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
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
    , status : PENDING
    , id : "1"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : PENDING
    , id : "2"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : PENDING
    , id : "3"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : PENDING
    , id : "4"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : PENDING
    , id : "5"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : SUCCESS
    , id : "6"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : PENDING
    , id : "7"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : FAILED
    , id : "8"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140
    , totalEarning : 2500
    , totalRides : 13
    , date : "2023-04-12"
    , status : SUCCESS
    , id : "9"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray}

]
dummyArray :: Array PaymnetBreakUp
dummyArray = [
       PaymnetBreakUp {
            "component": "GST",
            "amount": 10.0
        },
       PaymnetBreakUp {
            "component": "PlatformFee",
            "amount": 130.0
        }
    ]