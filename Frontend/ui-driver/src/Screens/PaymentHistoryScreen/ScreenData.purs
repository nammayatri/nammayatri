module Screens.PaymentHistoryScreen.ScreenData where

import Common.Types.App (PaymentStatus(..))
import Screens.Types (PaymentHistoryScreenState, PaymentHistorySubview(..), PaymentListItem, TransactionListItem)

initData :: PaymentHistoryScreenState
initData = {
    data: {
        paymentListItem : dummyPaymentListItem,
        transactionListItem : dummyTransactionListItem,
        manualPaymentRidesListItem : dManualPaymentRidesListItem
    },

    props: {
        subView : PaymentHistory,
        autoPayHistory : true
    }
}


dummyPaymentListItem :: Array PaymentListItem
dummyPaymentListItem = [
    {paidDate : "String",
    rideTakenDate : "String",
    amount : "String",
    paymentMethod : "String",
    paymentStatus : Success},
    {paidDate : "String",
    rideTakenDate : "String",
    amount : "String",
    paymentMethod : "String",
    paymentStatus : Success},
    {paidDate : "String",
    rideTakenDate : "String",
    amount : "String",
    paymentMethod : "String",
    paymentStatus : Success}
]

dummyTransactionListItem :: Array TransactionListItem
dummyTransactionListItem = [
    {key : "String",
    title : "String",
    val : "String"},
    {key : "String",
    title : "String",
    val : "String"},
    {key : "OFFER",
    title : "String",
    val : "String"},
    {key : "PAYMENT_METHOD",
    title : "String",
    val : "String"},
    {key : "TXN_ID",
    title : "String",
    val : "String"}
]

dManualPaymentRidesListItem :: Array TransactionListItem
dManualPaymentRidesListItem = [
    {key : "String",
    title : "String",
    val : "String"},
    {key : "String",
    title : "String",
    val : "String"},
    {key : "OFFER",
    title : "String",
    val : "String"},
    {key : "String",
    title : "String",
    val : "String"},
    {key : "TXN_ID",
    title : "String",
    val : "String"}
]