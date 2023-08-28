{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PaymentHistoryScreen.ScreenData where

import Common.Types.App (PaymentStatus(..))
import Screens.Types (PaymentHistoryScreenState, PaymentHistorySubview(..), PaymentListItem, TransactionListItem)

initData :: PaymentHistoryScreenState
initData = {
    data: {
        paymentListItem : [],
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
    paymentStatus : Success},
    {paidDate : "String",
    rideTakenDate : "String",
    amount : "String",
    paymentStatus : Success},
    {paidDate : "String",
    rideTakenDate : "String",
    amount : "String",
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