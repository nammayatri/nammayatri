module Screens.SubscriptionScreen.ScreenData where

import Screens.Types (SubscriptionScreenState, SubscriptionSubview(..))

initData :: SubscriptionScreenState
initData = {
    data : {
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
            ]
        },
        managePlanData : {
            allPlans : [
                {
                title : "DAILY PER TRIP",
                description : "Up to a maximum of ₹35 per day",
                isSelected : false,
                offers : [],
                offerDescription : "",
                planPrice : 3.5
                },
                {
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
        }
    },
    props : {
        subView : ManagePlan,
        myPlanProps : {
            isDuesExpanded : false
        },
        managePlanProps : {}
    }
}
