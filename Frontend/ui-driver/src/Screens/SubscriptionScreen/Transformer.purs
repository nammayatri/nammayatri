{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}



module Screens.SubscriptionScreen.Transformer 
  where

import Prelude

import Common.Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Components.PaymentHistoryListItem (PaymentBreakUp)
import Data.Array (cons, length, mapWithIndex, (!!))
import Data.Array as DA
import Data.Int (floor, fromNumber, toNumber)
import Data.List.Lazy (Pattern)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number.Format (fixed, toStringWith)
import Data.String (Pattern(..), split, toLower)
import Engineering.Helpers.Commons (convertUTCtoISC, getCurrentUTC)
import Helpers.Utils (fetchImage, FetchImageFrom(..), getFixedTwoDecimals)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Types (SubscriptionConfig, GradientConfig)
import Screens.Types (KeyValType, PlanCardConfig, PromoConfig, SubscriptionScreenState, DueItem)
import Services.API (DriverDuesEntity(..), FeeType(..), GetCurrentPlanResp(..), MandateData(..), OfferEntity(..), PaymentBreakUp(..), PlanEntity(..), UiPlansResp(..))
import Storage (getValueToLocalStore, KeyStore(..))

type PlanData = {
    title :: String,
    description :: String
}

getMultiLanguagePlanData :: Boolean -> PlanData -> PlanData
getMultiLanguagePlanData isLocalized planData = do
    let dailyUnlimited = getString DAILY_UNLIMITED
    if isLocalized then planData
    else do
        if planData.title ==  dailyUnlimited 
            then {title : getString DAILY_UNLIMITED, description : getString DAILY_UNLIMITED_PLAN_DESC}
            else {title : getString DAILY_PER_RIDE, description : getString DAILY_PER_RIDE_PLAN_DESC} 

getPromoConfig :: Array OfferEntity -> Array GradientConfig -> Array PromoConfig
getPromoConfig offerEntityArr gradientConfig =
    (map (\ (OfferEntity item) ->  do
        let gradient' = DA.find (\( item') -> item'.id == item.offerId) gradientConfig
        {     
            title : Just $ decodeOfferDescription (fromMaybe "" item.title) ,
            isGradient : isJust gradient' || item.title == Just "Freedom Offer: 96% off APPLIED-*$*-ಫ್ರೀಡಮ್ ಆಫರ್: 96% ರಷ್ಟು ರಿಯಾಯಿತಿ",
            gradient : if item.title == Just "Freedom Offer: 96% off APPLIED-*$*-ಫ್ರೀಡಮ್ ಆಫರ್: 96% ರಷ್ಟು ರಿಯಾಯಿತಿ" then [Color.orange200, Color.white900, Color.green300] else (fromMaybe {id : "", colors : []} gradient').colors,
            hasImage : true ,
            imageURL : fetchImage FF_ASSET "ny_ic_discount" ,
            offerDescription : Just $ decodeOfferDescription (fromMaybe "" item.description),
            addedFromUI : false,
            isPaidByYatriCoins : false
        }
    ) offerEntityArr)

myPlanListTransformer :: PlanEntity -> Maybe Boolean -> Array GradientConfig -> PlanCardConfig
myPlanListTransformer planEntity isLocalized' config = do 
    let isLocalized = fromMaybe false isLocalized'
    getPlanCardConfig planEntity isLocalized false config

planListTransformer :: UiPlansResp -> Boolean -> Array GradientConfig -> Array PlanCardConfig
planListTransformer (UiPlansResp planResp) isIntroductory gradientConfig =
    let planEntityArray = planResp.list
        plansplit = DA.partition (\(PlanEntity item) -> item.name == getString DAILY_UNLIMITED) planEntityArray
        sortedPlanEntityList = (plansplit.yes) <> (plansplit.no)
        isLocalized = fromMaybe false planResp.isLocalized 
    in 
    if isIntroductory 
        then [introductoryPlanConfig Language]
    else map (\ planEntity -> getPlanCardConfig planEntity isLocalized isIntroductory gradientConfig) sortedPlanEntityList 

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
                "TA_IN" | len > 5 -> 5
                _ -> 0
    
decodeOfferPlan :: String -> {plan :: String, offer :: String}
decodeOfferPlan str = do
    let strArray = split (Pattern "-*@*-") str
    {plan : (decodeOfferDescription $ fromMaybe "" (strArray !! 0)), offer : (decodeOfferDescription $ fromMaybe "" (strArray !! 1))}

freeRideOfferConfig :: LazyCheck -> PromoConfig
freeRideOfferConfig lazy = 
    {  
    title : Just $ getString FIRST_FREE_RIDE,
    isGradient : false,
    gradient : [],
    hasImage : false,
    imageURL : "",
    offerDescription : Nothing,
    addedFromUI : false,
    isPaidByYatriCoins : false
    }

introductoryOfferConfig :: LazyCheck -> PromoConfig
introductoryOfferConfig lazy = 
    {  
    title : Just $ getString INTRODUCTORY_OFFER_TO_BE_ANNOUNCED_SOON,
    isGradient : true,
    gradient : [Color.blue600, Color.blue600],
    hasImage : true,
    imageURL : fetchImage FF_ASSET "ny_ic_lock",
    offerDescription : Just $ getString NO_CHARGES_TILL,
    addedFromUI : false,
    isPaidByYatriCoins : false
    }

noChargesOfferConfig :: LazyCheck -> PromoConfig
noChargesOfferConfig lazy= 
    {  
    title : Nothing,
    isGradient : false,
    gradient : [],
    hasImage : false,
    imageURL : "",
    offerDescription : Just $ "<b>" <> getString DAILY_PER_RIDE_DESC <> "</b>",
    addedFromUI : true,
    isPaidByYatriCoins : false
    }

alternatePlansTransformer :: UiPlansResp -> SubscriptionScreenState -> Array PlanCardConfig
alternatePlansTransformer (UiPlansResp planResp) state =
    let planEntityArray = planResp.list
        alternatePlansArray = (DA.filter(\(PlanEntity item) -> item.id /= state.data.myPlanData.planEntity.id) planEntityArray)
        isLocalized = fromMaybe false planResp.isLocalized
    in map (\ planEntity -> getPlanCardConfig planEntity isLocalized false state.data.config.subscriptionConfig.gradientConfig) alternatePlansArray


getAutoPayDetailsList :: MandateData -> Array KeyValType
getAutoPayDetailsList (MandateData mandateDetails) = 
    [   {key : getString MAX_AMOUNT, val : "₹" <> getFixedTwoDecimals mandateDetails.maxAmount},
        {key : getString FREQUENCY, val : getFrequencyText mandateDetails.frequency},
        {key : getString STATRED_ON, val : convertUTCtoISC  mandateDetails.startDate "Do MMM YYYY"},
        {key : getString EXPIRES_ON, val : convertUTCtoISC  mandateDetails.endDate "Do MMM YYYY"}
    ]


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
    let dailyPerRidePlan = (DA.find(\(PlanEntity item) -> item.name == getString DAILY_UNLIMITED) planEntityArray)
    case dailyPerRidePlan of 
        Just (PlanEntity plan) -> Just plan.id
        Nothing -> Nothing

getSelectedPlan :: UiPlansResp -> Array GradientConfig -> Maybe PlanCardConfig
getSelectedPlan (UiPlansResp planResp) gradientConfig = do
    let planEntityArray = planResp.list
        planEntity' = planEntityArray DA.!! 0
    case planEntity' of 
        Just entity  -> let (PlanEntity planEntity) = entity
                            isLocalized = fromMaybe false planResp.isLocalized
                        in Just $ getPlanCardConfig entity isLocalized false gradientConfig
        Nothing -> Nothing


getPlanCardConfig :: PlanEntity -> Boolean -> Boolean -> Array GradientConfig -> PlanCardConfig
getPlanCardConfig (PlanEntity planEntity) isLocalized isIntroductory gradientConfig = 
    let planData = getMultiLanguagePlanData isLocalized {title : planEntity.name, description : planEntity.description}
    in  {
            id : planEntity.id ,
            title : planData.title ,
            description : planData.description ,
            isSelected : false ,
            offers : (if planEntity.freeRideCount > 0 
                        then [freeRideOfferConfig Language] 
                        else if isIntroductory then [introductoryOfferConfig Language] else []) <> getPromoConfig planEntity.offers gradientConfig,
            priceBreakup : planEntity.planFareBreakup,
            frequency : planEntity.frequency,
            freeRideCount : planEntity.freeRideCount,
            showOffer : planEntity.name /= getString DAILY_PER_RIDE
        }

constructDues :: Array DriverDuesEntity -> Boolean -> Array DueItem
constructDues duesArr showFeeBreakup = (mapWithIndex (\ ind (DriverDuesEntity item) ->  
  let offerAndPlanDetails = fromMaybe "" item.offerAndPlanDetails
  in
  {    
    tripDate: item.rideTakenOn,
    amount: item.driverFeeAmount,
    earnings: item.totalEarnings,
    noOfRides: item.totalRides,
    scheduledAt: convertUTCtoISC (fromMaybe "" item.executionAt) "Do MMM YYYY, h:mm A",
    paymentStatus: "",
    feeBreakup: if showFeeBreakup then getFeeBreakup offerAndPlanDetails item.totalRides else "",
    plan: offerAndPlanDetails,
    mode: item.feeType,
    autoPayStage : item.autoPayStage,
    randomId : (getCurrentUTC "") <> show ind,
    isSplit : item.isSplit,
    specialZoneRideCount : item.specialZoneRideCount,
    specialZoneAmount : item.specialZoneAmount,
    amountPaidByYatriCoins : case item.isCoinCleared of
                                true -> Just item.driverFeeAmount
                                false -> item.coinDiscountAmount
  }) duesArr)

getFeeBreakup :: String -> Int -> String
getFeeBreakup plan rides = 
    let planWithTranslations = fromMaybe "" ((split (Pattern "-*@*-") plan) !! 0)
        planInEng = fromMaybe "" ((split (Pattern "-*$*-") planWithTranslations) !! 0)
        planConfig = getPlanAmountConfig planInEng
    in
    case planConfig.isFixed of
        true -> "₹" <> getFixedTwoDecimals planConfig.value
        false -> if planConfig.value <= (toNumber rides) * planConfig.perRide 
                    then "₹" <> getFixedTwoDecimals planConfig.value
                 else show rides <> " " <> getString RIDES <> " X " <>  "₹" <> getFixedTwoDecimals planConfig.perRide
    

getPlanAmountConfig :: String -> {value :: Number, isFixed :: Boolean, perRide :: Number}
getPlanAmountConfig plan = case plan of
                            "DAILY UNLIMITED" -> {value : 25.0, isFixed : true, perRide : 0.0}
                            "DAILY PER RIDE" -> {value : 35.0, isFixed : false, perRide : 3.5}
                            _ ->  {value : 25.0, isFixed : true, perRide : 0.0}

introductoryPlanConfig :: LazyCheck -> PlanCardConfig
introductoryPlanConfig lazy =  {
    id : "dummy",
    title : getString DAILY_UNLIMITED,
    description : "",
    isSelected : true,
    frequency : "PER_DAY",
    freeRideCount : 0,
    offers : [introductoryOfferConfig Language],
    priceBreakup : [PaymentBreakUp{amount: 25.0, component: "FINAL_FEE"}],
    showOffer : true
} 

