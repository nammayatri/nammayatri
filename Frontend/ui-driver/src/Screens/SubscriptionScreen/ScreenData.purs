{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.SubscriptionScreen.ScreenData where

import Common.Types.App (PaymentStatus(..))
import Data.Maybe as Mb
import Screens.Types (AutoPayStatus(..), KeyValType, OptionsMenuState(..), PlanCardConfig, PromoConfig, SubscribePopupType(..), SubscriptionScreenState, SubscriptionSubview(..), DueItem)
import Services.API (FeeType(..), PaymentBreakUp(..))

initData :: SubscriptionScreenState
initData = {
    data: {
        driverId : "",
        paymentMode : "",
        planId : "",
        orderId : Mb.Nothing,
        errorMessage : "",
        joinPlanData : {
            allPlans : [],
            subscriptionStartDate : ""
        },
        myPlanData : {
            dueItems : dummyDues,
            planEntity : dummyPlanConfig,
            autoPayStatus : NO_AUTOPAY,
            lowAccountBalance : Mb.Just 5.0,
            paymentMethodWarning : false,
            switchAndSave : false,
            maxDueAmount : 0.0,
            currentDueAmount : 0.0,
            mandateStatus : ""
        },
        managePlanData : {
            currentPlan : dummyPlanConfig,
            alternatePlans : []
        },
        autoPayDetails : {
            isActive : false,
            detailsList : [],
            payerUpiId : Mb.Nothing,
            pspLogo : ""
        }
    },
    props : {
        isSelectedLangTamil : false,
        subView : NoSubView,
        popUpState : Mb.Nothing,
        paymentStatus : Mb.Nothing,
        resumeBtnVisibility : false,
        showError : false,
        showShimmer : true ,
        refreshPaymentStatus : false,
        confirmCancel : false,
        joinPlanProps : {
            paymentMode : "",
            selectedPlanItem : Mb.Nothing
        },
        myPlanProps : {
            isDuesExpanded : false,
            isDueViewExpanded : false,
            overDue : false,
            multiTypeDues : true,
            dueType : MANUAL_PAYMENT
        },
        managePlanProps : {
            selectedPlanItem : dummyPlanConfig
        },
        currentLat : 0.0,
        currentLon : 0.0,
        destLat : 0.0,
        destLon : 0.0,
        kioskLocation : [],
        prevSubView : NoSubView,
        noKioskLocation : false,
        optionsMenuState : ALL_COLLAPSED,
        redirectToNav : ""
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

dummyDues :: Array DueItem
dummyDues = [
  {
    tripDate: "2023-09-13T12:34:56Z",
    amount: 25.0,
    earnings: 200.0,
    noOfRides: 5,
    scheduledAt: "2023-09-11T12:34:56Z",
    paymentStatus: "Paid",
    feeBreakup: "Breakup",
    plan: "Plan",
    mode: MANUAL_PAYMENT
  },
  {
    tripDate: "2023-09-12T12:34:57Z",
    amount: 20.0,
    earnings: 300.0,
    noOfRides: 16,
    scheduledAt: "2023-09-11T12:34:57Z",
    paymentStatus: "Paid",
    feeBreakup: "Breakup",
    plan: "Plan",
    mode: MANUAL_PAYMENT
  },
  {
    tripDate: "2023-09-11T12:34:58Z",
    amount: 35.0,
    earnings: 500.0,
    noOfRides: 9,
    scheduledAt: "2023-09-11T12:34:58Z",
    paymentStatus: "Paid",
    feeBreakup: "Breakup",
    plan: "Plan",
    mode: AUTOPAY_PAYMENT
  }]