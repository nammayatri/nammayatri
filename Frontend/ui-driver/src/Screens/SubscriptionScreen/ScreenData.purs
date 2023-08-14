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
            allPlans : [],
            subscriptionStartDate : ""
        },
        myPlanData : {
            dueItems : [],
            planEntity : dummyPlanConfig,
            paymentMethod : UPI_AUTOPAY,
            autoPayStatus : ACTIVE_AUTOPAY,
            lowAccountBalance : false,
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
        paymentStatus : Mb.Nothing,
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
            selectedPlan : ""
        }
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
                offerDescription : Mb.Nothing
                }
                ]
    , priceBreakup : []
    , frequency : ""
    , freeRideCount : 1
}