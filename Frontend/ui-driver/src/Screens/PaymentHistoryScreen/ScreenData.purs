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
import Screens.Types (PaymentHistoryScreenState, PaymentHistorySubview(..), PaymentListItem, PlanCardConfig, TransactionListItem, PromoConfig, InvoiceListItem)
import Services.API (AutopayPaymentStage(..), FeeType(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array((!!))
import Prelude (show, ($))
import Data.String (Pattern(..), split)

initData :: PaymentHistoryScreenState
initData = {
    data: {
        autoPayList : [],
        manualPayList : [],
        transactionDetails : {
            notificationStatus : Mb.Just NOTIFICATION_SCHEDULED,
            paymentStatus : Pending,
            statusTime : "",
            details : [],
            manualSpecificDetails : [],
            isSplit : false,
            isAutoPayFailed : false,
            feeType : AUTOPAY_PAYMENT,
            numOfDriverFee : 0
        },
        planData : dummyPlanConfig
    },

    props: {
        subView : PaymentHistory,
        autoPayHistory : true,
        autoPaySetup : false,
        selectedDue : "",
        offset : 0,
        enableLoadMore : true,
        weeks : [],
        invoicePopupVisible : false,
        startDate : Nothing,
        endDate : Nothing,
        selectedTimeSpan : dummyDateItem,
        showError : false,
        errorMessage : "",
        downloadInvoice : false,
        invoiceData : dummyInvoiceData
    }
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

dummyPromoConfig :: PromoConfig
dummyPromoConfig = {
                        title : Mb.Nothing,
                        offerDescription : Mb.Nothing,
                        isGradient : false,
                        gradient : [],
                        hasImage : false,
                        imageURL : "",
                        addedFromUI : false
                    }

dummyInvoiceData :: Array InvoiceListItem
dummyInvoiceData = [
  { createdAt : "2019-05-26T19:50:34.4400000Z"
  , totalRides : 23
  , cgst : 2.25
  , sgst : 2.24
  , platformFee : 20.50
  , totalFee : 25.00
  , driverFeeId : "adfj-3458-akdf-859n"
  , status : show Pending
  , planTitle : ""
  , offerTitle : ""
  , debitedOn : "2020-05-26T19:50:34.4400000Z"
  , billNumber : "NY/8948933"
  },
  { createdAt : "2019-05-26T19:50:34.4400000Z"
  , totalRides : 23
  , cgst : 2.25
  , sgst : 2.24
  , platformFee : 20.50
  , totalFee : 25.00
  , driverFeeId : "adfk-adms-kadk-akdf"
  , status : show Success
  , planTitle : ""
  , offerTitle : ""
  , debitedOn : "2020-05-26T19:50:34.4400000Z"
  , billNumber : "NY/1438918"
  }
]

dummyDateItem = {date : 0, isInRange : false, isStart : false, isEnd : false, utcDate : "", shortMonth : "", year : 0, intMonth : 0}