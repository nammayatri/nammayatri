module Screens.SubscriptionScreen.ScreenData where

import Common.Types.App (PaymentStatus(..))
import Data.Maybe as Mb
import Screens.Types (AutoPayStatus(..), KeyValType, PaymentMethod(..), PlanCardConfig, SubscribePopupType(..), SubscriptionScreenState, SubscriptionSubview(..))

initData :: SubscriptionScreenState
initData = {
    data: {
        driverId : "",
        paymentMode : "",
        planId : "",
        joinPlanData : {
            allPlans : [
                {
                id : "0",
                title : "DAILY PER TRIP",
                description : "Up to a maximum of ₹35 per day",
                isSelected : false,
                offers : [],
                offerDescription : "",
                planPrice : 3.5
                },
                {
                id : "1",
                title : "DAILY UNLIMITED",
                description : "Enjoy UNLIMITED trips, every day!",
                isSelected : true,
                offers : [
                            {
                            title : "First Ride FREE",
                            isGradient : false,
                            gradient : [],
                            hasImage : false,
                            imageURL : ""
                            },
                            {
                            title : "Freedom Offer: 76% off APPLIED",
                            isGradient : true,
                            gradient : ["#FFE7C2", "#FFFFFF", "#DDFFEB"],
                            hasImage : true,
                            imageURL : "ny_ic_discount,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_discount.png"
                            }
                         ],
                offerDescription : "Freedom offer : <b> ₹6/Day from Sep 1-30 </b> <br>Valid only if you join by <b> Aug 16. </b> <br> <b> No charges till Aug 31 </b>",
                planPrice : 25.0
                }
            ]
        },
        myPlanData : {
            dueItems : [{tripDate : "6 Sept, 2023", amount : "₹25"}, {tripDate : "5 Sept, 2023", amount : "₹3.5"}, {tripDate : "4 Sept, 2023", amount : "₹3.5"}, {tripDate : "3 Sept, 2023", amount : "₹3.5"}, {tripDate : "2 Sept, 2023", amount : "₹3.5"}],
            offers : [
                {
                title : "First Ride FREE",
                isGradient : false,
                gradient : [],
                hasImage : false,
                imageURL : ""
                },
                {
                title : "Freedom Offer: 76% off APPLIED",
                isGradient : true,
                gradient : ["#FFE7C2", "#FFFFFF", "#DDFFEB"],
                hasImage : true,
                imageURL : "ny_ic_discount,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_discount.png"
                }
            ],
            paymentMethod : UPI_AUTOPAY,
            autoPayStatus : AUTOPAY_ACTIVE
        },
        managePlanData : {
            currentPlan : {
                id : "0",
                title : "DAILY PER TRIP",
                description : "Up to a maximum of ₹35 per day",
                isSelected : false,
                offers : [],
                offerDescription : "",
                planPrice : 3.5
                },
            alternatePlans : [
                {
                id : "1",
                title : "DAILY UNLIMITED",
                description : "Enjoy UNLIMITED trips, every day!",
                isSelected : true,
                offers : [
                            {
                            title : "First Ride FREE",
                            isGradient : false,
                            gradient : [],
                            hasImage : false,
                            imageURL : ""
                            },
                            {
                            title : "Freedom Offer: 76% off APPLIED",
                            isGradient : true,
                            gradient : ["#FFE7C2", "#FFFFFF", "#DDFFEB"],
                            hasImage : true,
                            imageURL : "ny_ic_discount,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_discount.png"
                            }
                         ],
                offerDescription : "Freedom offer : <b> ₹6/Day from Sep 1-30 </b> <br>Valid only if you join by <b> Aug 16. </b> <br> <b> No charges till Aug 31 </b>",
                planPrice : 25.0
                }
            ]
        },
        autoPayDetails : {
            registeredPG : "",
            isActive : false,
            detailsList : dummyDetailsList
        }
    },
    props : {
        subView : MyPlan,
        popUpState : Mb.Nothing, --Mb.Just SuccessPopup
        paymentStatus : Mb.Just Success,-- Mb.Nothing,
        resumeBtnVisibility : false,
        joinPlanProps : {
            selectedPlan : "1"
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
dummyPlanConfig = {
    id : "1"
  , title : "title"
  , description : "description"
  , isSelected : false
  , offers : [{
            title : "offerTitle"
        , isGradient : false
        , gradient : [""]
        , hasImage : true
        , imageURL : ""
        }
  ]
  , offerDescription : "offerDescription"
  , planPrice : 3.5
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