module Screens.SubscriptionScreen.Transformer where

import Prelude

import Data.Array as DA
import Data.List.Lazy (Pattern)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, toLower)
import Engineering.Helpers.Commons (convertUTCtoISC)
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.Types (PlanCardConfig, PromoConfig, SubscriptionScreenState, KeyValType)
import Services.API (GetCurrentPlanResp(..), MandateData(..), OfferEntity(..), PlanEntity(..), UiPlansResp(..))

planListTransformer :: UiPlansResp -> Array PlanCardConfig
planListTransformer (UiPlansResp planResp) = do
    let planEntityArray = planResp.list
    map (\ (PlanEntity planEntity) -> {
    id : planEntity.id ,
    title : planEntity.name ,
    description : planEntity.description ,
    isSelected : false ,
    offers : (if planEntity.freeRideCount > 0 then [freeRideOfferConfig] else []) <> getPromoConfig planEntity.offers ,
    priceBreakup : planEntity.planFareBreakup,
    frequency : planEntity.frequency,
    freeRideCount : planEntity.freeRideCount
    }) planEntityArray

getPromoConfig :: Array OfferEntity -> Array PromoConfig
getPromoConfig offerEntityArr = (map (\ (OfferEntity item) ->  {     
    title : item.title ,
    isGradient : true ,
    gradient : ["#FFE7C2", "#FFFFFF", "#DDFFEB"],
    hasImage : true ,
    imageURL : "ny_ic_discount,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_discount.png" ,
    offerDescription : item.description
    }) offerEntityArr)

myPlanListTransformer :: GetCurrentPlanResp -> PlanCardConfig
myPlanListTransformer (GetCurrentPlanResp getCurrentPlanResp) = do 
    let (PlanEntity planEntity) = getCurrentPlanResp.currentPlanDetails
    {   id : planEntity.id ,
        title : planEntity.name ,
        description : planEntity.description ,
        isSelected : false ,
        offers : (if planEntity.freeRideCount > 0 then [freeRideOfferConfig] else []) <> getPromoConfig planEntity.offers ,
        priceBreakup : planEntity.planFareBreakup,
        frequency : planEntity.frequency,
        freeRideCount : planEntity.freeRideCount
    }

freeRideOfferConfig :: PromoConfig
freeRideOfferConfig = 
    {  
    title : Just "First Ride FREE",
    isGradient : false,
    gradient : [],
    hasImage : false,
    imageURL : "",
    offerDescription : Nothing
    }

alternatePlansTransformer :: UiPlansResp -> SubscriptionScreenState -> Array PlanCardConfig
alternatePlansTransformer (UiPlansResp planResp) state = do
    let planEntityArray = planResp.list
    let alternatePlansArray = (DA.filter(\(PlanEntity item) -> item.id /= state.data.myPlanData.planEntity.id) planEntityArray)
    map (\ (PlanEntity planEntity) -> {
    id : planEntity.id ,
    title : planEntity.name ,
    description : planEntity.description ,
    isSelected : false ,
    offers : (if planEntity.freeRideCount > 0 then [freeRideOfferConfig] else []) <> getPromoConfig planEntity.offers ,
    priceBreakup : planEntity.planFareBreakup,
    frequency : planEntity.frequency,
    freeRideCount : planEntity.freeRideCount
    }) alternatePlansArray

getAutoPayDetailsList :: MandateData -> Array KeyValType
getAutoPayDetailsList (MandateData mandateData) = 
    [   {key : getString MAX_AMOUNT, val : show mandateData.maxAmount},
        {key : getString FREQUENCY, val : toLower mandateData.frequency},
        {key : getString STATRED_ON, val : convertUTCtoISC  mandateData.startDate "Do MMM YYYY"},
        {key : getString EXPIRES_ON, val : convertUTCtoISC  mandateData.endDate "Do MMM YYYY"}
    ]

getPspIcon :: String -> String 
getPspIcon vpa = do
    let handleName = ((split (Pattern "@") (vpa)) DA.!! 1)
    case handleName of 
        Nothing -> "ny_ic_defaultpg,"
        Just handle -> case handle of
            "ybl" -> "ny_ic_phonepe,"
            "ibl" -> "ny_ic_phonepe,"
            "axl" -> "ny_ic_phonepe,"
            "okhdfcbank" -> "ny_ic_gpay,"
            "okicici" -> "ny_ic_gpay,"
            "oksbi" -> "ny_ic_gpay,"
            "okaxis" -> "ny_ic_gpay,"
            "paytm" -> "ny_ic_paytm,"
            "apl" -> "ny_ic_amazonpay,"
            "yapl" -> "ny_ic_amazonpay,"
            "indus" -> "ny_ic_induspay,"
            "upi" -> "ny_ic_bhim,"
            _ -> "ny_ic_defaultpg,"