module Screens.SubscriptionScreen.ScreenData where

import Common.Types.App (PaymentStatus(..))
import Data.Maybe as Mb
import Screens.Types (AutoPayStatus(..), KeyValType, PaymentMethod(..), PlanCardConfig, SubscribePopupType(..), SubscriptionScreenState, SubscriptionSubview(..), PromoConfig)
import Services.API (PaymentBreakUp(..))

initData :: SubscriptionScreenState
initData = {
    data: {
        driverId : "",
        paymentMode : "",
        planId : "",
        orderId : "",
        errorMessage : "",
        joinPlanData : {
            allPlans : [],--dummyPlanConfigArray,
            subscriptionStartDate : ""
        },
        myPlanData : {
            dueItems : [],--[{tripDate : "6 Sept, 2023", amount : "₹25"}, {tripDate : "5 Sept, 2023", amount : "₹3.5"}, {tripDate : "4 Sept, 2023", amount : "₹3.5"}, {tripDate : "3 Sept, 2023", amount : "₹3.5"}, {tripDate : "2 Sept, 2023", amount : "₹3.5"}],
            planEntity : dummyPlanConfig,
            paymentMethod : UPI_AUTOPAY,
            autoPayStatus : ACTIVE_AUTOPAY,
            lowAccountBalance : false,
            paymentMethodWarning : false,
            switchAndSave : false,
            maxDueAmount : 100.0,
            currentDueAmount : 0.0,
            mandateStatus : ""
        },
        managePlanData : {
            currentPlan : dummyPlanConfig,
            alternatePlans : []-- dummyPlanConfigArray
        },
        autoPayDetails : {
            registeredPG : "",
            isActive : false,
            detailsList : [],
            payerUpiId : "",
            pspLogo : ""
        }
    },
    props : {
        subView : NoSubView,
        popUpState : Mb.Nothing,
        paymentStatus : Mb.Nothing, -- Mb.Just Pending,
        resumeBtnVisibility : false,
        showError : false,
        showShimmer : true ,
        refreshPaymentStatus : false,
        confirmCancel : false,
        joinPlanProps : {
            selectedPlan : Mb.Nothing,
            paymentMode : ""
        },
        myPlanProps : {
            isDuesExpanded : false
        },
        managePlanProps : {
            selectedPlan : "1"
        }
    }
}




dummyPlanConfig :: PlanCardConfig
dummyPlanConfig = 
    {
        id : "-1"
    , title : "DAILY UNLIMITED"
    , description : "Enjoy UNLIMITED trips, every day!"
    , isSelected : false
    , offers : [
                {
                title : Mb.Just "Freedom Offer: 76% off APPLIED",
                isGradient : true,
                gradient : ["#FFE7C2", "#FFFFFF", "#DDFFEB"],
                hasImage : true,
                imageURL : "ny_ic_discount,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_discount.png",
                offerDescription : Mb.Just "Freedom offer : <b> ₹6/Day from Sep 1-30 </b> <br>Valid only if you join by <b> Aug 16. </b> <br> <b> No charges till Aug 31 </b>"
                }
                ]
    , priceBreakup : dummyPriceBreakup
    , frequency : "DAILY"
    , freeRideCount : 1
}

dummyDetailsList :: Array KeyValType
dummyDetailsList = [
    { key : "Amount",
      val : "35"
    },
    { key : "Amount",
      val : "35"
    },
    { key : "Amount",
      val : "35"
    },
    { key : "Amount",
      val : "35"
    },
    { key : "Amount",
      val : "35"
    },
    { key : "Amount",
      val : "35"
    }
]

dummyPlanConfigArray :: Array PlanCardConfig
dummyPlanConfigArray = [{
    id : "1",
    title : "DAILY UNLIMITED",
    description : "Enjoy UNLIMITED trips, every day!",
    isSelected : true,
    offers : [
                {
                title : Mb.Just "Freedom Offer: 96% off APPLIED",
                isGradient : true,
                gradient : ["#FFE7C2", "#FFFFFF", "#DDFFEB"],
                hasImage : true,
                imageURL : "ny_ic_discount,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_discount.png",
                offerDescription : Mb.Just "Freedom offer : <b> ₹6/Day from Sep 1-30 </b> <br>Valid only if you join by <b> Aug 16. </b> <br> <b> No charges till Aug 31 </b>"
                }
            ],
    priceBreakup : dummyPriceBreakup,
    frequency : "DAILY",
    freeRideCount : 1
    },
    { id : "0"
    , title : "DAILY PER TRIP"
    , description : "Up to a maximum of ₹35 per day"
    , isSelected : false
    , offers : []
    , priceBreakup : dummyPriceBreakup2
    , frequency : "PER_RIDE"
    , freeRideCount : 0
    }
]
dummyPriceBreakup :: Array PaymentBreakUp
dummyPriceBreakup = 
    [
        PaymentBreakUp {component : "INITIAL_BASE_FEE", amount : 25.0},
        PaymentBreakUp {component : "REGISTRATION_FEE", amount : 0.0},
        PaymentBreakUp {component : "MAX_FEE_LIMIT", amount : 0.0},
        PaymentBreakUp {component : "DISCOUNTED_FEE", amount : 24.0},
        PaymentBreakUp {component : "FINAL_FEE", amount : 1.0}
    ]

dummyPriceBreakup2 :: Array PaymentBreakUp
dummyPriceBreakup2 = 
    [
        PaymentBreakUp {component : "INITIAL_BASE_FEE", amount : 3.5},
        PaymentBreakUp {component : "REGISTRATION_FEE", amount : 0.0},
        PaymentBreakUp {component : "MAX_FEE_LIMIT", amount : 0.0},
        PaymentBreakUp {component : "DISCOUNTED_FEE", amount : 0.0},
        PaymentBreakUp {component : "FINAL_FEE", amount : 3.5}
    ]