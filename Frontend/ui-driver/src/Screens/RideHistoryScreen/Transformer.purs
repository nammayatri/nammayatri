module Screens.RideHistoryScreen.Transformer where

import Prelude

import Components.PaymentHistoryListItem.Controller as PaymentHistoryListItem
import Data.Lens ((^.))
import Services.APITypes (PaymentDetailsEntity(..), PaymentStatus(..), PaymnetBreakUp(..))
import Services.Accessor (_id, _status)

getPaymentHistoryItemList :: Array PaymentDetailsEntity -> Array PaymentHistoryListItem.Config
getPaymentHistoryItemList _ = dummy --map (\item -> getPaymentHistoryItem item)

getPaymentHistoryItem :: PaymentDetailsEntity -> PaymentHistoryListItem.Config
getPaymentHistoryItem (PaymentDetailsEntity item) = {
    isSelected : false
    , charges : item.charges
    , totalEarning : item.totalEarning
    , totalRides : item.totalRides
    , date : item.date
    , status : (item.txnInfo) ^. _status
    , id : (item.txnInfo) ^. _id
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description: charge.component
        , amount : charge.amount
    }) <$> item.chargesBreakup
}

dummy :: Array PaymentHistoryListItem.Config
dummy = [
  {isSelected : false
    , charges : 140.0
    , totalEarning : 2500.0
    , totalRides : 13
    , date : "2023-04-12"
    , status : PENDING
    , id : "1"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140.0
    , totalEarning : 2500.0
    , totalRides : 13
    , date : "2023-04-12"
    , status : PENDING
    , id : "2"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140.0
    , totalEarning : 2500.0
    , totalRides : 13
    , date : "2023-04-12"
    , status : PENDING
    , id : "3"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140.0
    , totalEarning : 2500.0
    , totalRides : 13
    , date : "2023-04-12"
    , status : PENDING
    , id : "4"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140.0
    , totalEarning : 2500.0
    , totalRides : 13
    , date : "2023-04-12"
    , status : PENDING
    , id : "5"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140.0
    , totalEarning : 2500.0
    , totalRides : 13
    , date : "2023-04-12"
    , status : SUCCESS
    , id : "6"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140.0
    , totalEarning : 2500.0
    , totalRides : 13
    , date : "2023-04-12"
    , status : PENDING
    , id : "7"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140.0
    , totalEarning : 2500.0
    , totalRides : 13
    , date : "2023-04-12"
    , status : FAILED
    , id : "8"
    , paymentBreakUp : (\(PaymnetBreakUp charge) -> {
        description : charge.component
        , amount : charge.amount
    }) <$> dummyArray},
  {isSelected : false
    , charges : 140.0
    , totalEarning : 2500.0
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