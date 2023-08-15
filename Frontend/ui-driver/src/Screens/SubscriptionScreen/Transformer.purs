module Screens.SubscriptionScreen.Transformer where

import Prelude

import Data.Array (cons, (!!), length)
import Data.Array as DA
import Data.List.Lazy (Pattern)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, toLower)
import Engineering.Helpers.Commons (convertUTCtoISC)
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.Types (PlanCardConfig, PromoConfig, SubscriptionScreenState, KeyValType)
import Services.API (GetCurrentPlanResp(..), MandateData(..), OfferEntity(..), PlanEntity(..), UiPlansResp(..))
import Storage (getValueToLocalStore, KeyStore(..))


getPromoConfig :: Array OfferEntity -> Array PromoConfig
getPromoConfig offerEntityArr = (map (\ (OfferEntity item) ->  {     
    title : Just $ decodeOfferDescription (fromMaybe "" item.title) ,
    isGradient : true ,
    gradient : ["#FFE7C2", "#FFFFFF", "#DDFFEB"],
    hasImage : true ,
    imageURL : "ny_ic_discount,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_discount.png" ,
    offerDescription : Just $ decodeOfferDescription (fromMaybe "" item.description)
    }) offerEntityArr)

myPlanListTransformer :: GetCurrentPlanResp -> PlanCardConfig
myPlanListTransformer (GetCurrentPlanResp getCurrentPlanResp) = do 
    let (PlanEntity planEntity) = getCurrentPlanResp.currentPlanDetails
    {   id : planEntity.id ,
        title : planEntity.name ,
        description : planEntity.description,
        isSelected : false ,
        offers : (if planEntity.freeRideCount > 0 then [freeRideOfferConfig] else if (planEntity.name == "DAILY PER RIDE") then [noChargesOfferConfig] else []) <> getPromoConfig planEntity.offers ,
        priceBreakup : planEntity.planFareBreakup,
        frequency : planEntity.frequency,
        freeRideCount : planEntity.freeRideCount,
        showOffer : planEntity.name /= "DAILY PER RIDE"
    }



planListTransformer :: UiPlansResp -> Array PlanCardConfig
planListTransformer (UiPlansResp planResp) =
    let planEntityArray = planResp.list
        plansplit = DA.partition (\(PlanEntity item) -> item.name == "DAILY UNLIMITED") planEntityArray
        sortedPlanEntityList = (plansplit.yes) <> (plansplit.no)
    in map (\ (PlanEntity planEntity) -> 
        {
            id : planEntity.id ,
            title : planEntity.name ,
            description : planEntity.description ,
            isSelected : false ,
            offers : (if planEntity.freeRideCount > 0 then [freeRideOfferConfig] else if (planEntity.name == "DAILY PER RIDE") then [noChargesOfferConfig] else []) <> getPromoConfig planEntity.offers ,
            priceBreakup : planEntity.planFareBreakup,
            frequency : planEntity.frequency,
            freeRideCount : planEntity.freeRideCount,
            showOffer : planEntity.name /= "DAILY PER RIDE"
        }
    ) sortedPlanEntityList

decodeOfferDescription :: String -> String
decodeOfferDescription str = do
    let strArray = split (Pattern "-*$*-") str
    fromMaybe "" (strArray !! (getLanguage (length strArray)))
    where 
        getLanguage len = do
            case getValueToLocalStore LANGUAGE_KEY of
                "KN_IN" | len > 1 -> 1
                "HI_IN" | len > 2 -> 2
                "BN_IN" | len > 3 -> 3
                "ML_IN" | len > 4 -> 4
                _ -> 0
    

freeRideOfferConfig :: PromoConfig
freeRideOfferConfig = 
    {  
    title : Just $ getString FIRST_FREE_RIDE,
    isGradient : false,
    gradient : [],
    hasImage : false,
    imageURL : "",
    offerDescription : Nothing
    }

noChargesOfferConfig :: PromoConfig
noChargesOfferConfig = 
    {  
    title : Nothing,
    isGradient : false,
    gradient : [],
    hasImage : false,
    imageURL : "",
    offerDescription : Just $ "<b>" <> getString DAILY_PER_RIDE_DESC <> "</b>"
    }

alternatePlansTransformer :: UiPlansResp -> SubscriptionScreenState -> Array PlanCardConfig
alternatePlansTransformer (UiPlansResp planResp) state =
    let planEntityArray = planResp.list
        alternatePlansArray = (DA.filter(\(PlanEntity item) -> item.id /= state.data.myPlanData.planEntity.id) planEntityArray)
    in map (\ (PlanEntity planEntity) -> 
        {   id : planEntity.id ,
            title : planEntity.name ,
            description : planEntity.description ,
            isSelected : false ,
            offers : (if planEntity.freeRideCount > 0 then [freeRideOfferConfig] else if (planEntity.name == "DAILY PER RIDE") then [noChargesOfferConfig] else []) <> getPromoConfig planEntity.offers ,
            priceBreakup : planEntity.planFareBreakup,
            frequency : planEntity.frequency,
            freeRideCount : planEntity.freeRideCount,
            showOffer : planEntity.name /= "DAILY PER RIDE"
        }
    ) alternatePlansArray


getAutoPayDetailsList :: MandateData -> Array KeyValType
getAutoPayDetailsList (MandateData mandateDetails) = 
    [   {key : getString MAX_AMOUNT, val : "₹" <> show mandateDetails.maxAmount},
        {key : getString FREQUENCY, val : getFrequencyText mandateDetails.frequency},
        {key : getString STATRED_ON, val : convertUTCtoISC  mandateDetails.startDate "Do MMM YYYY"},
        {key : getString EXPIRES_ON, val : convertUTCtoISC  mandateDetails.endDate "Do MMM YYYY"}
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

getFrequencyText :: String -> String
getFrequencyText constructor = 
    case constructor of 
        "ONETIME"  -> getString ONETIME
        "DAILY" -> getString DAILY
        "WEEKLY" -> getString WEEKLY
        "FORTNIGHTLY" -> getString FORTNIGHTLY
        "MONTHLY" -> getString MONTHLY
        "BIMONTHLY"  -> getString BIMONTHLY
        "QUARTERLY" -> getString QUARTERLY
        "HALFYEARLY" -> getString HALFYEARLY
        "YEARLY" -> getString YEARLY
        "ASPRESENTED" -> getString ASPRESENTED
        _ -> toLower constructor

getSelectedId :: UiPlansResp -> Maybe String
getSelectedId (UiPlansResp planResp) = do
    let planEntityArray = planResp.list
    let dailyPerRidePlan = (DA.find(\(PlanEntity item) -> item.name == "DAILY UNLIMITED") planEntityArray)
    case dailyPerRidePlan of 
        Just (PlanEntity plan) -> Just plan.id
        Nothing -> Nothing