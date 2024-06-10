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
import Common.Types.App (LazyCheck(..), ReelModal(..))
import Common.RemoteConfig.Utils as RemoteConfig
import ConfigProvider
import Components.PaymentHistoryListItem (PaymentBreakUp)
import Data.Array (cons, length, mapWithIndex, (!!))
import Data.Array as DA
import Data.Int (floor, fromNumber, toNumber)
import Data.List.Lazy (Pattern)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number.Format (fixed, toStringWith)
import Data.String (Pattern(..), split, toLower, null)
import Engineering.Helpers.Commons (convertUTCtoISC, getCurrentUTC)
import Helpers.Utils (fetchImage, FetchImageFrom(..), splitBasedOnLanguage)
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import MerchantConfig.Types (CityConfig, SubscriptionConfig, GradientConfig, AppConfig, StaticViewPlans)
import Screens.Types (KeyValType, PlanCardConfig, PromoConfig, SubscriptionScreenState, DueItem)
import Services.API (ReelVideoThresholdConfig, ReelsResp, DriverDuesEntity(..), FeeType(..), GetCurrentPlanResp(..), MandateData(..), OfferEntity(..), PaymentBreakUp(..), PlanEntity(..), UiPlansResp(..))
import Storage (getValueToLocalStore, KeyStore(..))
import Locale.Utils
import RemoteConfig as RC
import Services.API as API
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Foldable (foldl)
import Engineering.Helpers.Utils (getFixedTwoDecimals)

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
            else {title : getString DAILY_PER_RIDE, description : getString $ DAILY_PER_RIDE_PLAN_DESC (getCurrency appConfig <> "35")} 

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

myPlanListTransformer :: PlanEntity -> Maybe Boolean -> SubscriptionConfig -> PlanCardConfig
myPlanListTransformer planEntity isLocalized' subsConfig = do 
    let isLocalized = fromMaybe false isLocalized'
    getPlanCardConfig planEntity isLocalized false subsConfig

planListTransformer :: UiPlansResp -> Boolean -> SubscriptionConfig -> CityConfig -> Array PlanCardConfig
planListTransformer (UiPlansResp planResp) isIntroductory config cityConfig =
    let planEntityArray = planResp.list
        plansplit = DA.partition (\(PlanEntity item) -> item.name == getString DAILY_UNLIMITED) planEntityArray
        sortedPlanEntityList = (plansplit.yes) <> (plansplit.no)
        isLocalized = fromMaybe false planResp.isLocalized 
    in 
    if isIntroductory 
        then fetchIntroductoryPlans config cityConfig
    else map (\ planEntity -> getPlanCardConfig planEntity isLocalized isIntroductory config) sortedPlanEntityList 

decodeOfferDescription :: String -> String
decodeOfferDescription str = do
    let strArray = split (Pattern "-*$*-") str
    fromMaybe "" (strArray !! (getLanguage (length strArray)))
    where 
        getLanguage len = do
            case getLanguageLocale languageKey of
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

freeRideOfferConfig :: Int -> PromoConfig
freeRideOfferConfig count = 
    {  
    title : Just $ getString $ (if count > 1 then FIRST_RIDES_FREE else FIRST_RIDE_FREE) $ show count,
    isGradient : false,
    gradient : [],
    hasImage : false,
    imageURL : "",
    offerDescription : Nothing,
    addedFromUI : false,
    isPaidByYatriCoins : false
    }

introductoryOfferConfig :: SubscriptionConfig -> String -> PromoConfig
introductoryOfferConfig config offerName =
    let vehicleVariant = getValueToLocalStore VEHICLE_VARIANT
        city = getValueToLocalStore DRIVER_LOCATION
        configRemote = RemoteConfig.subscriptionsConfigVariantLevel city vehicleVariant
        offer = case offerName of 
                    "FREE_RIDE_OFFER" -> getString FIRST_FREE_RIDE
                    "NO_CHARGES_TILL" -> getVarString NO_CHARGES_TILL [splitBasedOnLanguage configRemote.noChargesTillDate]
                    _ -> splitBasedOnLanguage offerName 
    in
    {  
    title : Just offer,
    isGradient : true,
    gradient : [config.benefitsBgColor, config.benefitsBgColor],
    hasImage : true,
    imageURL : fetchImage FF_ASSET "ny_ic_benefits_filled_blue",
    offerDescription : Nothing,
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
    in map (\ planEntity -> getPlanCardConfig planEntity isLocalized false state.data.config.subscriptionConfig) alternatePlansArray


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

getSelectedPlan :: UiPlansResp -> SubscriptionConfig -> Maybe PlanCardConfig
getSelectedPlan (UiPlansResp planResp) config = do
    let planEntityArray = planResp.list
        planEntity' = planEntityArray DA.!! 0
    case planEntity' of 
        Just entity  -> let (PlanEntity planEntity) = entity
                            isLocalized = fromMaybe false planResp.isLocalized
                        in Just $ getPlanCardConfig entity isLocalized false config
        Nothing -> Nothing


getPlanCardConfig :: PlanEntity -> Boolean -> Boolean -> SubscriptionConfig -> PlanCardConfig
getPlanCardConfig (PlanEntity planEntity) isLocalized isIntroductory  config = 
    let planData = getMultiLanguagePlanData isLocalized {title : planEntity.name, description : planEntity.description}
    in  {
            id : planEntity.id ,
            title : planData.title ,
            description : planData.description ,
            isSelected : false ,
            offers : (if planEntity.freeRideCount > 0 
                        then [freeRideOfferConfig planEntity.freeRideCount] 
                        else if isIntroductory then [introductoryOfferConfig config ""] else []) <> getPromoConfig planEntity.offers config.gradientConfig,
            priceBreakup : planEntity.planFareBreakup,
            frequency : planEntity.frequency,
            freeRideCount : planEntity.freeRideCount,
            showOffer : planEntity.name /= getString DAILY_PER_RIDE
        }

constructDues :: Array DriverDuesEntity -> Boolean -> Array DueItem
constructDues duesArr showFeeBreakup = (mapWithIndex (\ ind (DriverDuesEntity item) ->  
  let offerAndPlanDetails = fromMaybe "" item.offerAndPlanDetails
      feeBreakup = if showFeeBreakup then getFeeBreakup item.maxRidesEligibleForCharge item.coinDiscountAmount (max 0.0 (item.driverFeeAmount - (fromMaybe 0.0 item.totalSpecialZoneCharges))) item.totalRides else ""
      noOfRides = item.totalRides + fromMaybe 0 item.specialZoneRideCount
  in
  {    
    tripDate: item.rideTakenOn,
    amount: item.driverFeeAmount,
    earnings: item.totalEarnings,
    noOfRides: noOfRides,
    scheduledAt: convertUTCtoISC (fromMaybe "" item.executionAt) "Do MMM YYYY, h:mm A",
    paymentStatus: "",
    feeBreakup: feeBreakup,
    plan: offerAndPlanDetails,
    mode: item.feeType,
    autoPayStage : item.autoPayStage,
    randomId : (getCurrentUTC "") <> show ind,
    isSplit : item.isSplit,
    specialZoneRideCount : item.specialZoneRideCount,
    totalSpecialZoneCharges : item.totalSpecialZoneCharges,
    amountPaidByYatriCoins : case item.isCoinCleared of
                                true -> Just item.driverFeeAmount
                                false -> item.coinDiscountAmount
  }) duesArr)

getFeeBreakup :: Maybe Int -> Maybe Number -> Number -> Int -> String
getFeeBreakup maxRidesEligibleForCharge pointsApplied driverFeeAmount totalRides = do
    case maxRidesEligibleForCharge of
        Nothing ->  getStringFeeBreakup
        Just maxRides -> do
            let ridesToConsider = min totalRides maxRides
            if ridesToConsider /= 0 then getStringFeeBreakup
            else getString $ NO_OPEN_MARKET_RIDES "NO_OPEN_MARKET_RIDES"
   where 
    getStringFeeBreakup = (if driverFeeAmount > 0.0 then "₹" <> getFixedTwoDecimals driverFeeAmount else "") <> maybe "" ( \x ->if x == 0.0 then "" else  " + " <> getFixedTwoDecimals x ) pointsApplied

getPlanAmountConfig :: String -> {value :: Number, isFixed :: Boolean, perRide :: Number}
getPlanAmountConfig plan = case plan of
                            "DAILY UNLIMITED" -> {value : 25.0, isFixed : true, perRide : 0.0}
                            "DAILY PER RIDE" -> {value : 35.0, isFixed : false, perRide : 3.5}
                            _ ->  {value : 25.0, isFixed : true, perRide : 0.0}

introductoryPlanConfig :: SubscriptionConfig -> StaticViewPlans ->  PlanCardConfig
introductoryPlanConfig config planConfig =  {
    id : "dummy",
    title : getTranslatedString planConfig.name,
    description : getTranslatedString planConfig.planDesc,
    isSelected : false,
    frequency : planConfig.frequency,
    freeRideCount : 0,
    offers : if null planConfig.introductoryOffer
                then []
                else [introductoryOfferConfig config planConfig.introductoryOffer],
    priceBreakup : [PaymentBreakUp{amount: planConfig.price, component: "FINAL_FEE"}],
    showOffer : not $ null planConfig.introductoryOffer
} 

transformReelsPurescriptDataToNativeData :: Array RC.ReelItem -> ReelModal
transformReelsPurescriptDataToNativeData reelsData = do
  let transformedData = map (\ eachItem ->
    { id : eachItem.id
    , thumbnailImageUrl : eachItem.thumbnailImageUrl
    , videoUrl : eachItem.videoUrl
    , title : eachItem.title
    , description : eachItem.description
    , shareLink : eachItem.shareLink
    , carouselBigImageUrl : eachItem.carouselBigImageUrl
    , carouselSmallImageUrl : eachItem.carouselSmallImageUrl
    , carouselTextString : eachItem.carouselTextString
    , carouselTextColor : eachItem.carouselTextColor
    , bottomButtonConfig : eachItem.bottomButtonConfig
    , sideButtonConfig : eachItem.sideButtonConfig
    , thresholdConfig : Nothing
    } ) reelsData

  { reelData : transformedData
  , titleConfig : {
      size : 18,
      color : Color.white900,
      maxLines : 2
    }
  , descriptionConfig : {
      size : 14,
      color : Color.white900,
      maxLines : 2
    }
  , reelExtraConfig : Just {
      bounceAnimationEnabled : Just true,
      bounceAnimationCount : Just 2,
      bounceAnimationDuration : Just 400,
      progressBarColor : Just Color.white40Alpha,
      progressBarVisible : Just true,
      autoSwipeToNext : Just false,
      seekEnabled : Just true
    }
  }

fetchIntroductoryPlans :: SubscriptionConfig -> CityConfig -> Array PlanCardConfig
fetchIntroductoryPlans subsConfig cityConfig = do
    let vehicleCategory = getValueToLocalStore VEHICLE_CATEGORY
        finalVehicleCategory = if not cityConfig.variantSubscriptionConfig.enableCabsSubscriptionView 
                                then "AutoCategory"
                                else
                                    case vehicleCategory of
                                        _ | vehicleCategory == "CarCategory" || vehicleCategory == "AutoCategory" || vehicleCategory == "BikeCategory" -> vehicleCategory
                                        _ -> "AutoCategory"
        plans = cityConfig.variantSubscriptionConfig.staticViewPlans
    map (\planConfig -> introductoryPlanConfig subsConfig planConfig) $ DA.filter (\planConfig -> planConfig.variantCategory == finalVehicleCategory) plans

getTranslatedString :: String -> String
getTranslatedString str = case str of
                    "DAILY_UNLIMITED" -> getString DAILY_UNLIMITED
                    "DAILY_PER_RIDE" -> getString DAILY_PER_RIDE
                    "CAB_DAILY_UNLIMITED_OFFER" -> getString DAILY_UNLIMITED_PLAN_DESC
                    "CAB_DAILY_PER_RIDE_OFFER" -> getString $ DAILY_PER_RIDE_PLAN_DESC (getCurrency appConfig <> "9")
                    _ -> splitBasedOnLanguage str