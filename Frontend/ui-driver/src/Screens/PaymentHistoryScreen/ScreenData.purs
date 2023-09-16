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
import Data.Maybe as Mb
import Screens.Types (PaymentHistoryScreenState, PaymentHistorySubview(..), PaymentListItem, TransactionListItem, PlanCardConfig)
import Services.API (FeeType(..))

initData :: PaymentHistoryScreenState
initData = {
    data: {
        transactions : [],
        transactionDetails : {
            notificationStatus : Pending,
            statusTime : "",
            details : [],
            manualSpecificDetails : []
        },
        planData : dummyPlanConfig
    },

    props: {
        subView : PaymentHistory,
        autoPayHistory : true,
        autoPaySetup : false
    }
}


dummyPaymentListItem :: Array PaymentListItem
dummyPaymentListItem = [
    {
        invoiceId:"f434e4c7-943e-44f1-8aee-e91572f4f7b3",
        ridesTakenDate : "2023-09-07",
        totalRides: 1,
        transactionDate : "2023-09-11",
        driverFeeId: "f434e4c7-943e-44f1-8aee-e91572f4f7b3",
        paymentStatus: Success,
        totalEarnings: 80,
        chargesBreakup: [
            {
                amount: 4.0,
                component: "Government Charges"
            },
            {
                amount: 0.0,
                component: "Platform Fee"
            },
            {
                amount: 0.0,
                component: "CGST"
            },
            {
                amount: 0.0,
                component: "SGST"
            }
        ],
        totalCharges: 4,
        feeType : AUTOPAY_PAYMENT
    },
    {
        invoiceId: "33ba9a1f-db60-41fd-ad6c-38f2f3532140",
        totalRides: 2,
        ridesTakenDate : "2023-09-08",
        transactionDate : "2023-09-08",
        driverFeeId: "33ba9a1f-db60-41fd-ad6c-38f2f3532140",
        paymentStatus: Pending,
        totalEarnings: 340,
        chargesBreakup: [
            {
                amount: 14.0,
                component: "Government Charges"
            },
            {
                amount: 10.0,
                component: "Platform Fee"
            },
            {
                amount: 0.9,
                component: "CGST"
            },
            {
                amount: 0.9,
                component: "SGST"
            }
        ],
        totalCharges: 26,
        feeType : AUTOPAY_PAYMENT
    },
    {
        invoiceId: "f0ad4af0-37b9-4278-ba2e-38e710900e7f",
        totalRides: 2,
        ridesTakenDate : "2023-09-07",
        transactionDate : "2023-09-07",
        driverFeeId: "f0ad4af0-37b9-4278-ba2e-38e710900e7f",
        paymentStatus: Pending,
        totalEarnings: 184,
        chargesBreakup: [
            {
                amount: 8.0,
                component: "Government Charges"
            },
            {
                amount: 20.0,
                component: "Platform Fee"
            },
            {
                amount: 1.8,
                component: "CGST"
            },
            {
                amount: 1.8,
                component: "SGST"
            }
        ],
        totalCharges: 32,
        feeType : AUTOPAY_PAYMENT
    },
    {
        invoiceId: "367b6de7-8682-4e3e-889a-83ab9ad2022e",
        totalRides: 1,
        ridesTakenDate : "2023-09-05",
        transactionDate : "2023-09-05",
        driverFeeId: "367b6de7-8682-4e3e-889a-83ab9ad2022e",
        paymentStatus: Failed,
        totalEarnings: 146,
        chargesBreakup: [
            {
                amount: 7.0,
                component: "Government Charges"
            },
            {
                amount: 0.0,
                component: "Platform Fee"
            },
            {
                amount: 0.0,
                component: "CGST"
            },
            {
                amount: 0.0,
                component: "SGST"
            }
        ],
        totalCharges: 7,
        feeType : MANUAL_PAYMENT
    }
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
    title : "DAILY UNLIMITED",
    val : "String"},
    {key : "TXN_ID",
    title : "String",
    val : "String"}
]

dummyDetails :: PaymentListItem
dummyDetails = {
        invoiceId:"f434e4c7-943e-44f1-8aee-e91572f4f7b3",
        ridesTakenDate : "2023-09-07",
        totalRides: 1,
        transactionDate : "2023-09-11",
        driverFeeId: "f434e4c7-943e-44f1-8aee-e91572f4f7b3",
        paymentStatus: Success,
        totalEarnings: 80,
        chargesBreakup: [
            {
                amount: 4.0,
                component: "Government Charges"
            },
            {
                amount: 0.0,
                component: "Platform Fee"
            },
            {
                amount: 0.0,
                component: "CGST"
            },
            {
                amount: 0.0,
                component: "SGST"
            }
        ],
        totalCharges: 4,
        feeType : MANUAL_PAYMENT
    }

dummyPlanConfig :: PlanCardConfig
dummyPlanConfig = 
    { id : ""
    , title : ""
    , description : ""
    , isSelected : false
    , offers : [
                {
                title : Mb.Nothing,
                isGradient : true,
                gradient : [],
                hasImage : true,
                imageURL : "",
                offerDescription : Mb.Nothing,
                addedFromUI : false
                }
                ]
    , priceBreakup : []
    , frequency : ""
    , freeRideCount : 1
    , showOffer : true
}