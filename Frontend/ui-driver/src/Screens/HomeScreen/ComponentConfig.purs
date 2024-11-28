{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.ComponentConfig where

import Language.Strings
import Common.Types.Config
import Common.Types.App (LazyCheck(..), PolylineAnimationConfig)
import Common.Types.App as CommonTypes
import Components.Banner as Banner
import Components.BannerCarousel as BannerCarousel
import Components.ChatView as ChatView
import Data.Function.Uncurried (runFn2)
import Components.ErrorModal (primaryButtonConfig)
import Components.GoToLocationModal as GoToLocationModal
import Components.InAppKeyboardModal as InAppKeyboardModal
import Components.MakePaymentModal as MakePaymentModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.RateCard as RateCard
import Components.RatingCard as RatingCard
import Components.RequestInfoCard as RequestInfoCard
import Components.RideActionModal as RideActionModal
import Components.RideCompletedCard as RideCompletedCard
import Components.RideCompletedCard.Controller (Theme(..))
import Components.SelectListModal as SelectListModal
import Data.Array as DA
import Data.Int (fromString)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Maybe (Maybe(..), fromMaybe, isJust,maybe)
import Data.String as DS
import Data.Tuple 
import Data.Array (elem)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Suggestions (getSuggestionsfromKey, chatSuggestion)
import Font.Size as FontSize
import Font.Style (Style(..))
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getMerchantVehicleSize, onBoardingSubscriptionScreenCheck, getCityConfig, getPurpleRideConfigForVehicle)
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit,div,mod,unit, ($), (-), (/), (<), (<=), (<>), (==), (>=), (||), (>), (/=), show, map, (&&), not, bottom, (<>), (*), negate, otherwise, (+),(<$>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), Accessiblity(..), cornerRadius, padding, gravity)
import PrestoDOM.Types.DomAttributes as PTD
import Resource.Constants as Const
import Screens.Types (AutoPayStatus(..), SubscriptionBannerType(..), SubscriptionPopupType(..), GoToPopUpType(..))
import Screens.Types as ST
import Services.API as API
import Services.API (PaymentBreakUp(..), PromotionPopupConfig(..), Status(..), BookingTypes(..))
import Storage (KeyStore(..), getValueToLocalNativeStore, getValueToLocalStore)
import Mobility.Prelude (boolToVisibility)
import Styles.Colors as Color
import Components.ErrorModal as ErrorModal
import MerchantConfig.Utils as MU
import MerchantConfig.Types
import PrestoDOM.Types.DomAttributes (Corners(..))
import ConfigProvider as CP
import Locale.Utils
import RemoteConfig (RCCarousel(..))
import Mobility.Prelude (boolToVisibility)
import Constants 
import LocalStorage.Cache (getValueFromCache)
import Engineering.Helpers.Utils (getFixedTwoDecimals, isAmbulance)
import Common.Resources.Constants
import Data.Function.Uncurried (runFn3)
import DecodeUtil (getAnyFromWindow)
import Resource.Constants as RC
import ConfigProvider 
import Data.Int 
import Styles.Types (Color(..), FontStyle(..))
import RemoteConfig as RemoteConfig
import Components.SelectPlansModal.Controller as SelectPlansModal
import RemoteConfig.Utils
import Debug
import Components.PopUpModal.Controller as PopUpModal
import Control.Apply as CA
import Resource.Localizable.TypesV2 as LT2
import Resource.Localizable.StringsV2 as StringsV2

--------------------------------- rideActionModalConfig -------------------------------------
rideActionModalConfig :: ST.HomeScreenState -> RideActionModal.Config
rideActionModalConfig state = 
  let
  config = RideActionModal.config
  Tuple stage rideData = case state.props.bookingStage, state.data.advancedRideData of
                              ADVANCED, Just advRideInfo -> Tuple state.props.advancedRideStage advRideInfo
                              _, _  -> Tuple state.props.currentStage state.data.activeRide
  isDelivery = state.data.activeRide.tripType == ST.Delivery
  sourceAddressTitleText = fromMaybe (fromMaybe "" ((DS.split (DS.Pattern ",") (rideData.source)) DA.!! 0)) rideData.sourceArea
  destinationAddressTitleText = (\destination -> fromMaybe (fromMaybe "" ((DS.split (DS.Pattern ",") destination) DA.!! 0)) rideData.destinationArea)
  rideActionModalConfig' = config {
    startRideActive = (state.props.currentStage == ST.RideAccepted || (state.props.currentStage == ST.ChatWithCustomer && (Const.getHomeStageFromString $ getValueToLocalStore PREVIOUS_LOCAL_STAGE) /= ST.RideStarted ) ),
    arrivedStopActive = state.props.arrivedAtStop,
    totalDistance = if state.data.activeRide.distance <= 0.0 then "0.0" else if(state.data.activeRide.distance < 1000.0) then HU.parseFloat (state.data.activeRide.distance) 2 <> " m" else HU.parseFloat((state.data.activeRide.distance / 1000.0)) 2 <> " km",
    customerName = if DS.length (fromMaybe "" ((DS.split (DS.Pattern " ") (state.data.activeRide.riderName)) DA.!! 0)) < 4
                      then (fromMaybe "" ((DS.split (DS.Pattern " ") (state.data.activeRide.riderName)) DA.!! 0)) <> " " <> (fromMaybe "" ((DS.split (DS.Pattern " ") (state.data.activeRide.riderName)) DA.!! 1))
                      else
                        (fromMaybe "" ((DS.split (DS.Pattern " ") (rideData.riderName)) DA.!! 0)),
    sourceAddress  {
      titleText = sourceAddressTitleText,
      detailText = (maybe "" (\addr -> addr <> ", ") rideData.extraFromLocationInfo) <> rideData.source
    },
    destinationAddress = (\destination -> {
      titleText : destinationAddressTitleText destination,
      detailText : (maybe "" (\addr -> addr <> ", " ) rideData.extraToLocationInfo) <> destination
    }) <$> state.data.activeRide.destination,
    stopAddress = (\stop -> {
      titleText : fromMaybe "" ((DS.split (DS.Pattern ",") stop) DA.!! 0),
      detailText : stop
    }) <$> state.data.activeRide.nextStopAddress,
    lastStopAddress = (\stop -> {
      titleText : fromMaybe "" ((DS.split (DS.Pattern ",") stop) DA.!! 0),
      detailText : stop
    }) <$> state.data.activeRide.lastStopAddress,
    estimatedRideFare = state.data.activeRide.estimatedFare,
    notifiedCustomer = state.data.activeRide.notifiedCustomer,
    currentStage = state.props.currentStage,
    unReadMessages = state.props.unReadMessages,
    vehicleType = state.data.vehicleType,
    vehicleServiceTier = state.data.vehicleType,
    specialLocationTag = rideData.specialLocationTag,
    requestedVehicleVariant = rideData.requestedVehicleVariant,
    accessibilityTag = rideData.disabilityTag,
    appConfig = state.data.config,
    waitTimeStatus = state.props.waitTimeStatus,
    waitTimeSeconds = state.data.activeRide.waitTimeSeconds,
    rideScheduledTime = state.data.activeRide.tripScheduledAt,
    rideType = state.data.activeRide.tripType,
    rideStartRemainingTime = state.props.rideStartRemainingTime,
    tripDuration = (\tripDuration -> (if (tripDuration / 3600) < 10 then "0" else "") <> (show ( tripDuration / 3600) <> ":") <> (if (tripDuration `mod` 3600) / 60 < 10 then "0" else "") <> show ( (tripDuration `mod` 3600) / 60)  <> " Hr") <$> state.data.activeRide.tripDuration,
    rideStartTime = state.data.activeRide.tripStartTime,
    startODOReading = fromMaybe "--" $ show <$> state.data.activeRide.startOdometerReading,
    estimatedTollCharges = state.data.activeRide.estimatedTollCharges,
    driverVehicle = state.data.activeRide.driverVehicle,
    cityConfig = state.data.cityConfig,
    isOdometerReadingsRequired = state.props.isOdometerReadingsRequired,
    serviceTierAndAC = state.data.activeRide.serviceTier,
    capacity = state.data.activeRide.capacity,
    acRide = isAcRide state,
    isAdvanced = (state.props.bookingStage == ADVANCED),
    bookingFromOtherPlatform = state.data.activeRide.bookingFromOtherPlatform,
    bapName = state.data.activeRide.bapName,
    distance = case state.data.route DA.!! 0 of
                  Just (API.Route obj) ->  obj.distance
                  Nothing -> 0
  , parkingCharge = state.data.activeRide.parkingCharge
  , isDelivery = isDelivery
  , delivery = if isDelivery then getDeliveryDetails state else Nothing
  , isSourceDetailsExpanded = state.props.isSourceDetailsExpanded
  , isDestinationDetailsExpanded = if state.props.currentStage == ST.RideStarted then true else not state.props.isSourceDetailsExpanded
  , stops = rideData.stops
  }
  in rideActionModalConfig'
  where 
    isAcRide :: ST.HomeScreenState -> Maybe Boolean
    isAcRide state = if (RC.getCategoryFromVariant state.data.vehicleType) == Just ST.AutoCategory then Nothing else state.data.activeRide.acRide

getDeliveryDetails :: ST.HomeScreenState -> Maybe RideActionModal.DeliveryDetails
getDeliveryDetails state = 
  Just $ {
    sender : ({
      name : maybe "" (\(API.PersonDetails pd) -> pd.name) state.data.activeRide.senderPersonDetails,
      phoneNumber : maybe "" (\(API.PersonDetails pd) -> pd.primaryExophone) state.data.activeRide.senderPersonDetails,
      premises : state.data.activeRide.extraFromLocationInfo,
      exophoneNumber : Nothing,
      instructions : state.data.activeRide.senderInstructions
    }:: RideActionModal.PersonAndDeliveryInfo),
    receiver : ( {
      name : maybe "" (\(API.PersonDetails pd) -> pd.name) state.data.activeRide.receiverPersonDetails,
      phoneNumber : maybe "" (\(API.PersonDetails pd) -> pd.primaryExophone) state.data.activeRide.receiverPersonDetails,
      premises : state.data.activeRide.extraToLocationInfo,
      exophoneNumber : Nothing,
      instructions : state.data.activeRide.receiverInstructions
    } :: RideActionModal.PersonAndDeliveryInfo)
  }

---------------------------------------- endRidePopUp -----------------------------------------
endRidePopUp :: ST.HomeScreenState -> PopUpModal.Config
endRidePopUp state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    primaryText {text = (getString END_RIDE)},
    secondaryText {text = (getString ARE_YOU_SURE_YOU_WANT_TO_END_THE_RIDE)},
    option1 {text = (getString GO_BACK), enableRipple = true},
    option2 {text = (getString END_RIDE), enableRipple = true}
  }
in popUpConfig'

------------------------------------------ cancelRideModalConfig ---------------------------------
cancelRideModalConfig :: ST.HomeScreenState -> SelectListModal.Config
cancelRideModalConfig state = let
  config = SelectListModal.config
  lastIndex = ((DA.length state.data.cancelRideModal.selectionOptions) -1)
  cancelRideModalConfig' = config {
    activeIndex = state.data.cancelRideModal.activeIndex,
    hint = (getString HELP_US_WITH_YOUR_REASON),
    strings {
      mandatory = (getString MANDATORY),
      limitReached = ((getString MAX_CHAR_LIMIT_REACHED) <> " 100 " <> (getString OF) <> " 100")
    },
    headingTextConfig{
      text = ((getString CANCEL_RIDE) <> "?")
    },
    subHeadingTextConfig{
      text = (getString PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL)
    },
    showAllOptionsText = (getString SHOW_ALL_OPTIONS),
    selectionOptions = state.data.cancelRideModal.selectionOptions,
    isLimitExceeded = ((DS.length (state.data.cancelRideModal.selectedReasonDescription)) >= 100),
    activeReasonCode = Just state.data.cancelRideModal.selectedReasonCode,
    primaryButtonTextConfig {
      firstText = (getString GO_BACK)
    , secondText = (getString CANCEL_RIDE)
    },
    isSelectButtonActive = case state.data.cancelRideModal.activeIndex of
                              Just index -> true
                              Nothing    -> false
  }
  in cancelRideModalConfig'

-------------------------------------genderBannerConfig------------------------------------
genderBannerConfig :: forall a. ST.HomeScreenState -> (BannerCarousel.Action -> a) -> BannerCarousel.Config  (BannerCarousel.Action -> a)
genderBannerConfig _ action =
  let
    config = BannerCarousel.config action
    config' = config
      {
        backgroundColor = Color.green600,
        title = (getString COMPLETE_YOUR_PROFILE_AND_FIND_MORE_RIDES),
        titleColor = Color.white900,
        actionText = (getString UPDATE_NOW),
        actionTextColor = Color.white900,
        imageUrl = fetchImage FF_ASSET "ny_ic_driver_gender_banner",
        "type" = BannerCarousel.Gender
      }
  in config'

autpPayBannerCarousel :: forall a. ST.HomeScreenState -> (BannerCarousel.Action -> a) -> BannerCarousel.Config (BannerCarousel.Action -> a)
autpPayBannerCarousel state action =
  let config = BannerCarousel.config action
      bannerConfig = autopayBannerConfig state false
  in config {
     backgroundColor = bannerConfig.backgroundColor,
        title = bannerConfig.title,
        titleColor = bannerConfig.titleColor,
        actionText = bannerConfig.actionText.text,
        actionTextColor = actionTextColor_,
        actionTextBackgroundColour = actionTextBackgroundColour_,
        actionIconUrl = actionIconUrl_,
        actionIconVisibility = true,
        imageUrl = bannerConfig.imageUrl,
        isBanner = bannerConfig.isBanner,
        imageHeight = bannerConfig.imageHeight,
        imageWidth = bannerConfig.imageWidth,
        actionTextStyle = bannerConfig.actionText.style,
        actionArrowIconVisibility = false,
        titleStyle = bannerConfig.titleStyle,
        "type" = BannerCarousel.AutoPay,
        imagePadding = bannerConfig.imagePadding
  }
  where
    actionIconUrl_ = (HU.getAssetLink FunctionCall) <> case state.props.autoPayBanner of
                            DUE_LIMIT_WARNING_BANNER -> "ny_ic_money_filled_red.png"
                            CLEAR_DUES_BANNER -> "ny_ic_money_filled.png"
                            LOW_DUES_BANNER -> "ny_ic_money_filled_black.png"
                            _ -> "ny_ic_arrow_right_green.png"

    actionTextBackgroundColour_ = case state.props.autoPayBanner of
          DUE_LIMIT_WARNING_BANNER -> Color.red
          CLEAR_DUES_BANNER -> Color.black900
          LOW_DUES_BANNER -> Color.yellow900
          _ -> Color.white900

    actionTextColor_ = case state.props.autoPayBanner of
          DUE_LIMIT_WARNING_BANNER -> Color.white900
          CLEAR_DUES_BANNER -> Color.white900
          LOW_DUES_BANNER -> Color.black900
          _ -> Color.green600

advancedRideBannerCarousel :: forall a. ST.HomeScreenState -> (BannerCarousel.Action -> a) -> BannerCarousel.Config (BannerCarousel.Action -> a)
advancedRideBannerCarousel state action =
  let config = BannerCarousel.config action
      imageHeight = if configureImage then (V 75) else (V 105)
      configureImage = false
      imageWidth = if configureImage then (V 98) else (V 118)
      actionTextStyle = if configureImage then FontStyle.Body3 else FontStyle.ParagraphText
      titleStyle = if configureImage then FontStyle.Body4 else FontStyle.Body7
  in config {
        backgroundColor = Color.blue600,
        title = getString GET_ADVANCED_RIDE,
        titleColor = Color.blue900,
        actionText = getString LEARN_MORE,
        actionTextColor = Color.white900,
        actionTextBackgroundColour = Color.blue900,
        actionIconUrl = (HU.getAssetLink FunctionCall) <> "ny_ic_info_white_filled.png",
        actionIconVisibility = true,
        imageUrl = (HU.getAssetLink FunctionCall) <> "ny_ic_advanced_booking.png",
        isBanner = true,
        imageHeight = imageHeight,
        imageWidth = imageWidth,
        actionTextStyle = actionTextStyle,
        actionArrowIconVisibility = false,
        titleStyle = titleStyle,
        "type" = BannerCarousel.AdvancedRide,
        imagePadding = PaddingVertical 5 5
  }

accessbilityBannerConfig :: forall a. ST.HomeScreenState -> (BannerCarousel.Action -> a) -> BannerCarousel.Config  (BannerCarousel.Action -> a)
accessbilityBannerConfig _ action = 
  let 
    config = BannerCarousel.config action
    config' = config  
      {
        backgroundColor = Color.lightPurple,
        title = getString LEARN_HOW_YOU_CAN_HELP_CUSTOMERS_REQUIRING_SPECIAL_ASSISTANCE,
        titleColor = Color.purple,
        actionText = getString LEARN_MORE,
        actionTextColor = Color.purple,
        imageUrl = fetchImage FF_ASSET "ny_ic_purple_badge",
        isBanner = true,
        margin = Margin 0 0 0 0,
        "type" = BannerCarousel.Disability
      }
  in config'

------------------------------------ linkAadhaarPopupConfig -----------------------------
linkAadhaarPopupConfig :: ST.HomeScreenState -> PopUpModal.Config
linkAadhaarPopupConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    gravity = CENTER,
    margin = MarginHorizontal 24 24 ,
    buttonLayoutMargin = Margin 16 0 16 20 ,
    primaryText {
      text = (getString AADHAAR_LINKING_REQUIRED)
    , margin = Margin 16 24 16 4 },
    secondaryText {
      text = (getString $ AADHAAR_LINKING_REQUIRED_DESCRIPTION "AADHAAR_LINKING_REQUIRED_DESCRIPTION")
    , margin = MarginBottom 24},
    option1 {
      text = (getString LINK_AADHAAR_ID)
    , background = Color.black900
    , color = Color.yellow900
    },
    option2 {
      visibility = false
    },
    backgroundClickable = true,
    dismissPopup = true,
    cornerRadius = (PTD.Corners 15.0 true true true true),
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET "ny_ic_aadhaar_logo"
    , visibility = VISIBLE
    , height = V 178
    , width = V 204
    }
  }
  in popUpConfig'

newStopPopupConfig :: ST.HomeScreenState -> PopUpModal.Config
newStopPopupConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    optionButtonOrientation = "VERTICAL",
    gravity = CENTER,
    margin = Margin 16 20 16 20,
    buttonLayoutMargin = Margin 16 0 16 20 ,
    primaryText {
      text = (getString CUSTOMER_ADDED_A_STOP)
    , margin = Margin 16 24 16 24
    , textStyle = Heading2
    , color = Color.black
    },
    secondaryText {
      text = ""
    , margin = MarginBottom 24
    , visibility = GONE
    },
    option1 {
      text = (getString NAVIGATE_TO_LOCATION)
    , margin = Margin 16 0 16 8
    , background = Color.black900
    , color = Color.yellow900
    ,gravity = CENTER
      , width = MATCH_PARENT
    },
    option2 {
      text = (getString CLOSE)
    , margin = Margin 16 0 16 8
    , background = Color.white900
    , color = Color.black900
    , strokeColor = Color.white900
    , gravity = CENTER
    , width = MATCH_PARENT
    },
    cornerRadius = (PTD.Corners 15.0 true true true true),
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET "customer_added_a_stop"
    , visibility = VISIBLE
    , height = V 189
    , width = V 193
    , margin = MarginTop 40
    }
    ,backgroundClickable = false
    ,dismissPopup = false
  }
  in popUpConfig'

offerPopupConfig :: Boolean -> PromotionPopupConfig -> PopUpModal.Config
offerPopupConfig isImageUrl  (PromotionPopupConfig ob) = 
  PopUpModal.config {
    gravity = CENTER,
    margin = MarginHorizontal 24 24 ,
    buttonLayoutMargin = Margin 16 0 16 5 ,
    topTitle {
      text = ob.heading
    , visibility = VISIBLE
    },
    primaryText {
      text = ob.title
    , margin = Margin 16 24 16 4 },
    secondaryText {
      text = ob.description
    , margin = MarginBottom 24},
    option1 {
      text = getString JOIN_NOW
    , background = Color.black900
    , color = Color.yellow900
    },
    option2 {
      visibility = false
    },
    backgroundClickable = false,
    cornerRadius = (PTD.Corners 15.0 true true true true),
    coverImageConfig {
      imageUrl = if isImageUrl then "," <> ob.imageUrl else ob.imageUrl
    , visibility = VISIBLE
    , height = V 178
    , width = V 204
    }
  , optionWithHtml  {
    textOpt1 {
      text = getString MAYBE_LATER
    , visibility = VISIBLE
    , textStyle = FontStyle.SubHeading2
    , color = Color.black650
    } 
    , height = V 24
    , margin = MarginBottom 20
    , visibility = true
    , background = Color.white900
    , strokeColor = Color.white900
  }
}

freeTrialRidesEndingPopupConfig :: ST.HomeScreenState -> PopUpModal.Config
freeTrialRidesEndingPopupConfig state = 
  let autoPayStatus = state.data.paymentState.autoPayStatus
      freeTrialRidesLeft = fromMaybe 0 $ CA.lift2 (-) state.data.plansState.freeTrialRides state.data.plansState.totalRidesTaken
      currentFreeRidesTaken = fromMaybe 0 $ state.data.plansState.totalRidesTaken
  in
  PopUpModal.config {
    gravity = CENTER,
    margin = MarginHorizontal 24 24 ,
    buttonLayoutMargin = Margin 16 0 16 5 ,
    primaryText {
      visibility = GONE
    , margin = Margin 16 16 16 4 },
    secondaryText {
      text = if autoPayStatus == NO_AUTOPAY then getString JOIN_A_PLAN_TO_CONTINUE_TAKING_RIDES else getString SETUP_AUTOPAY_FOR_EASY_PAYMENTS
    , margin = MarginVertical 10 24
    },
    option1 {
      text = if autoPayStatus == NO_AUTOPAY then getString JOIN_NOW else getString SETUP_AUTOPAY
    , background = Color.black900
    , color = Color.yellow900
    , enableRipple = true
    },
    option2 {
      visibility = false
    },
    backgroundClickable = true,
    cornerRadius = (PTD.Corners 15.0 true true true true),
    coverImageConfig {
      imageUrl = fetchImage COMMON_ASSET $ case freeTrialRidesLeft of
        5 -> "ny_5_rides_left"
        2 -> "ny_2_rides_left"
        _ -> "ny_"<> show  freeTrialRidesLeft <>"_rides_left"
    , visibility = VISIBLE
    , height = V 220
    , width = V 280
    , margin = MarginTop 20
    }
  , optionWithHtml  {
    textOpt1 {
      text = getString NOT_NOW
    , visibility = VISIBLE
    , textStyle = FontStyle.SubHeading2
    , color = Color.black650
    } 
    , height = V 24
    , margin = MarginVertical 0 20
    , visibility = true
    , background = Color.white900
    , strokeColor = Color.white900
    }
  , popUpHeaderConfig {
    gravity = CENTER
    , visibility = VISIBLE
    , headingText {
      text = getString $ N_MORE_FREE_RIDES $ show freeTrialRidesLeft
      , visibility = VISIBLE
      , margin = Margin 0 12 0 0
    }
    , subHeadingText {
      text = getString $ N_FREE_RIDES_COMPLETED $ show currentFreeRidesTaken
      , visibility = VISIBLE
      , textStyle = SubHeading1
      , margin = Margin 0 12 0 0
    }
  }
}

freeTrialEndingPopupConfig :: ST.HomeScreenState -> PopUpModal.Config
freeTrialEndingPopupConfig state = 
  let autoPayStatus = state.data.paymentState.autoPayStatus
      noOfDaysLeft = fromMaybe 0 (fromString (getValueToLocalNativeStore FREE_TRIAL_DAYS)) 
  in
  PopUpModal.config {
    gravity = CENTER,
    margin = MarginHorizontal 24 24 ,
    buttonLayoutMargin = Margin 16 0 16 5 ,
    primaryText {
      text = case noOfDaysLeft of
        2 -> getString FREE_TRIAL_ENDING_TOMORROW
        1 -> getString FREE_TRIAL_ENDS_TONIGHT
        _ -> getString $ FREE_TRIAL_ENDING_IN_N_DAYS $ show noOfDaysLeft
    , margin = Margin 16 16 16 4 },
    secondaryText {
      text = if autoPayStatus == NO_AUTOPAY then getString JOIN_A_PLAN_TO_CONTINUE_TAKING_RIDES else getString SETUP_AUTOPAY_FOR_EASY_PAYMENTS
    , margin = MarginBottom 24
    },
    option1 {
      text = if autoPayStatus == NO_AUTOPAY then getString JOIN_NOW else getString SETUP_AUTOPAY
    , background = Color.black900
    , color = Color.yellow900
    , enableRipple = true
    },
    option2 {
      visibility = false
    },
    backgroundClickable = true,
    cornerRadius = (PTD.Corners 15.0 true true true true),
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET $ case noOfDaysLeft of
        3 -> "ny_ic_2_days_left"
        2 -> "ny_ic_1_days_left"
        1 -> "ny_ic_offer_ends_tonight"
        _ -> "ny_"<> show noOfDaysLeft <>"_days_left"
    , visibility = VISIBLE
    , height = V 220
    , width = V 280
    , margin = MarginTop 20
    }
  , optionWithHtml  {
    textOpt1 {
      text = getString NOT_NOW
    , visibility = VISIBLE
    , textStyle = FontStyle.SubHeading2
    , color = Color.black650
    } 
    , height = V 24
    , margin = MarginVertical 0 20
    , visibility = true
    , background = Color.white900
    , strokeColor = Color.white900
    }
  }

paymentPendingPopupConfig :: ST.HomeScreenState -> PopUpModal.Config
paymentPendingPopupConfig state =
  let popupType = state.props.subscriptionPopupType
      dues = if state.data.paymentState.totalPendingManualDues /= 0.0 then "( ₹" <> getFixedTwoDecimals state.data.paymentState.totalPendingManualDues <> ") " else ""
      isHighDues = state.data.paymentState.totalPendingManualDues >= state.data.subsRemoteConfig.high_due_warning_limit
  in
  PopUpModal.config {
    gravity = CENTER,
    margin = MarginHorizontal 24 24 ,
    buttonLayoutMargin = Margin 16 0 16 if popupType == LOW_DUES_CLEAR_POPUP then 20 else 5 ,
    dismissPopup = true,
    topTitle {
      text = case popupType of
                GO_ONLINE_BLOCKER -> getString PAYMENT_PENDING_ALERT
                _  -> getString $ if isHighDues then DUES_PENDING else CLEAR_YOUR_DUES_EARLY
    , visibility = VISIBLE
    , gravity = CENTER
    },
    primaryText {
      text = getString case popupType of 
                         LOW_DUES_CLEAR_POPUP -> LOW_DUES_CLEAR_POPUP_DESC 
                         SOFT_NUDGE_POPUP     -> PAYMENT_PENDING_SOFT_NUDGE
                         GO_ONLINE_BLOCKER    -> PAYMENT_PENDING_ALERT_DESC "PAYMENT_PENDING_ALERT_DESC"
                         _                    -> LOW_DUES_CLEAR_POPUP_DESC
    , margin = Margin 16 16 16 4
    , textStyle = SubHeading2
    , color = Color.black700
    , visibility = VISIBLE },
    secondaryText {
      text = "<span style='color:#2194FF'><u>"<> getString WATCH_VIDEO_FOR_HELP <>"</u></span>"
    , textStyle = SubHeading2
    , margin = MarginBottom 24
    , isClickable = true
    , suffixImage = {
        visibility : VISIBLE
        , imageUrl : fetchImage FF_ASSET "ny_ic_youtube"
        , height : (V 24)
        , width : (V 24)
        , margin : MarginLeft 4 
        , padding : Padding 0 0 0 0
      }
    },
    option1 {
      text = getString CLEAR_DUES <> dues
    , background = Color.black900
    , color = Color.yellow900
    , showShimmer = state.data.paymentState.showShimmer
    , enableRipple = true
    },
    option2 {
      visibility = false
    },
    backgroundClickable = true,
    cornerRadius = (PTD.Corners 15.0 true true true true),
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET $ case popupType of
                          GO_ONLINE_BLOCKER  -> "ny_ic_dues_pending"
                          _ ->  if isHighDues 
                                then "ny_ic_dues_pending" 
                                else "ny_ic_clear_dues_early"
    , visibility = VISIBLE
    , height = V 220
    , width = V 280
    }
  , optionWithHtml  {
    textOpt1 {
      text = getString case popupType of
                          SOFT_NUDGE_POPUP  -> GO_ONLINE_POPUP
                          GO_ONLINE_BLOCKER -> VIEW_DUE_DETAILS
                          _ -> VIEW_DUE_DETAILS
    , visibility =  VISIBLE
    , textStyle = FontStyle.SubHeading2
    , color = Color.black650
    } 
    , height = V 24
    , margin = MarginBottom 20
    , visibility = popupType == SOFT_NUDGE_POPUP || popupType == GO_ONLINE_BLOCKER
    , background = Color.white900
    , strokeColor = Color.white900
    }
  }

offerConfigParams :: ST.HomeScreenState -> PromotionPopupConfig
offerConfigParams state = PromotionPopupConfig $ {
  title : getString LIMITED_TIME_OFFER,
  description : getString JOIN_THE_UNLIMITED_PLAN,
  imageUrl : fetchImage FF_ASSET "ny_ic_limited_time_offer",
  buttonText : getString JOIN_NOW,
  heading : getString MY_PLAN_TITLE
}

------------------------------------ cancelConfirmationConfig -----------------------------
cancelConfirmationConfig :: ST.HomeScreenState -> PopUpModal.Config
cancelConfirmationConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    gravity = CENTER,
    margin = MarginHorizontal 24 24 ,
    buttonLayoutMargin = Margin 16 24 16 20 ,
    primaryText {
      text = 
        let
          isAmbulance = RC.getCategoryFromVariant state.data.vehicleType == Just ST.AmbulanceCategory
          ambulanceText =  StringsV2.getStringV2 LT2.canceling_this_booking_may_affect_the_emergency_medical
          nonAmbulanceText = case state.data.activeRide.specialLocationTag of
            Nothing -> getString FREQUENT_CANCELLATIONS_WILL_LEAD_TO_BLOCKING
            Just specialLocationTag -> getString $ getCancelAlertText $ (HU.getRideLabelData (Just specialLocationTag)).cancelText
        in
          if isAmbulance then ambulanceText else nonAmbulanceText
    , margin = Margin 16 24 16 0 },
    secondaryText {
      visibility = if state.data.activeRide.specialLocationTag == (Just "GOTO") || RC.getCategoryFromVariant state.data.vehicleType == Just ST.AmbulanceCategory then VISIBLE else GONE,
      text = if state.data.activeRide.specialLocationTag == (Just "GOTO") then getString GO_TO_CANCELLATION_DESC else  StringsV2.getStringV2 LT2.drivers_are_permitted_to_cancel_ambulance_bookings,
      margin = MarginTop 6
      },
    option1 {
      text = (getString CONTINUE)
    , width = V $ (((EHC.screenWidth unit)-92)/2) 
    , isClickable = state.data.cancelRideConfirmationPopUp.continueEnabled
    , timerValue = state.data.cancelRideConfirmationPopUp.delayInSeconds
    , enableTimer = true
    , background = Color.white900
    , strokeColor = Color.black500
    , color = Color.black700
    , enableRipple = state.data.cancelRideConfirmationPopUp.continueEnabled
    },
    option2 {
      text = (getString GO_BACK)
    , margin = MarginLeft 12
    , width = V $ (((EHC.screenWidth unit)-92)/2)
    , color = Color.yellow900
    , strokeColor = Color.black900
    , background = Color.black900
    , enableRipple = true
    },
    backgroundClickable = false,
    cornerRadius = (PTD.Corners 15.0 true true true true),
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET  if (state.data.activeRide.specialLocationTag == Nothing || (HU.getRequiredTag state.data.activeRide.specialLocationTag) == Nothing) 
                    then if (RC.decodeVehicleType $ getValueToLocalStore VEHICLE_CATEGORY) == Just ST.AmbulanceCategory then "ny_ic_cancel_prevention_ambulance" else "ny_ic_frequent_cancellation_blocking"
                  else (HU.getRideLabelData state.data.activeRide.specialLocationTag).cancelConfirmImage
    , visibility = VISIBLE
    , margin = Margin 16 10 16 0
    , height = V 250
    , width = MATCH_PARENT
    },
    timerId = "PopUp"
  }
  in popUpConfig'

driverRCPopUpConfig :: ST.HomeScreenState -> PopUpModal.Config 
driverRCPopUpConfig state = let 
  config' = PopUpModal.config 
  popUpConfig' = config'{
    backgroundClickable = false,
    gravity = CENTER,
    buttonLayoutMargin =(Margin 0 0 0 0),
    cornerRadius = (PTD.Corners 16.0 true true true true), 
    padding = Padding 16 24 16 16,
    optionButtonOrientation = "VERTICAL",
    margin = MarginHorizontal 24 24, 
    primaryText {
      text =  getString RC_DEACTIVATED, 
      margin = MarginTop 16 
    }, 
    secondaryText {
      text = getString RC_DEACTIVATED_DETAILS, 
      color = Color.black700,
      margin = (Margin 0 0 0 0)
    } ,
    option1 {
      background = Color.black900,
      text = getString GO_TO_VEHICLE_DETAILS,
      color = Color.yellow900, 
      margin = MarginTop 24,
      width = MATCH_PARENT, 
      padding = Padding 16 6 16 6,
      height = WRAP_CONTENT 
    } , 
    option2 {
      background = Color.white900, 
      text = getString CLOSE,
      width = MATCH_PARENT, 
      height = WRAP_CONTENT ,
      color =  Color.black650,
      strokeColor = Color.white900, 
      padding = Padding 16 6 16 6, 
      margin = Margin 0 8 0 0
    }, 
    coverImageConfig {
      visibility = VISIBLE,
      imageUrl = fetchImage FF_ASSET "ny_rc_deactivated",
      height = V 182,
      width = V 280
    }
  }
  in popUpConfig' 

------------------------------------ chatViewConfig -----------------------------
chatViewConfig :: ST.HomeScreenState -> ChatView.Config
chatViewConfig state = let
  suggestionList = 
    if showSuggestions state 
      then  if ( state.data.activeRide.tripType == ST.Rental && state.props.currentStage == ST.ChatWithCustomer) 
              then getSuggestionsfromKey chatSuggestion "dsRentalInitial" 
            else if (state.data.activeRide.notifiedCustomer) 
              then getSuggestionsfromKey chatSuggestion "driverInitialAP" 
            else getSuggestionsfromKey chatSuggestion "driverInitialBP" 
    else state.data.chatSuggestionsList
  config = ChatView.config
  chatViewConfig' = config {
    userConfig {
      userName = state.data.activeRide.riderName,
      appType = "Driver"
    }
    , messages = state.data.messages
    , messagesSize = state.data.messagesSize
    , sendMessageActive = state.props.sendMessageActive
    , vehicleNo = ""
    , chatSuggestionsList = suggestionList
    , hint = (getString MESSAGE)
    , suggestionHeader = (getString START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS)
    , emptyChatHeader = (getString START_YOUR_CHAT_WITH_THE_DRIVER)
    , mapsText = (getString MAPS)
    , languageKey = (getLanguageLocale languageKey)
    , transparentGrey = Color.transparentGrey
    , canSendSuggestion = state.props.canSendSuggestion
    , enableCall = (not (state.data.activeRide.disabilityTag == Just ST.HEAR_IMPAIRMENT))
    , enableSuggestions = state.data.config.feature.enableSuggestions
    , showNavigate = if state.data.activeRide.tripType == ST.Rental then isJust state.data.activeRide.nextStopLat && isJust state.data.activeRide.nextStopLon else true
    , useSuggestionsView = true
  }
  in chatViewConfig'

showSuggestions :: ST.HomeScreenState -> Boolean
showSuggestions state = do
  let canShowSuggestions = case (DA.last state.data.messages) of 
                            Just value -> not $ value.sentBy == "Driver"
                            Nothing -> true
  DA.null state.data.chatSuggestionsList && canShowSuggestions && ((show $ DA.length $ JB.getChatMessages FunctionCall) == state.data.messagesSize || state.data.messagesSize == "-1")

silentModeConfig :: ST.HomeScreenState -> PopUpModal.Config
silentModeConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    gravity = CENTER
  , cornerRadius = (PTD.Corners 15.0 true true true true)
  , backgroundClickable = false
  , margin = (Margin 16 0 16 0)
  , primaryText {
      text = getString TRY_SILENT_MODE
    }
  , secondaryText {
      text =  getString SILENT_MODE_PROMPT
    }
    , option1 {
      text =   getString GO_OFFLINE
      , width = (V 140)
      , enableRipple = true
    }
  , option2 {
      width = (V 170)
      , text =  getString GO_SILENT
      , enableRipple = true
    }
  , popUpHeaderConfig = config'.popUpHeaderConfig
  }
  in popUpConfig'


enterOtpStateConfig :: ST.HomeScreenState -> InAppKeyboardModal.InAppKeyboardModalState
enterOtpStateConfig state = 
  let appConfig = CP.getAppConfig CP.appConfig
      config' = InAppKeyboardModal.config
      inAppModalConfig' = config'{
      otpIncorrect = if (state.props.otpAttemptsExceeded) then false else (state.props.otpIncorrect),
      otpAttemptsExceeded = (state.props.otpAttemptsExceeded),
      inputTextConfig {
        text = state.props.rideOtp,
        focusIndex = state.props.enterOtpFocusIndex
        , textStyle = FontStyle.Heading1
      },
      headingConfig {
        text = if state.props.endRideOtpModal then (getString ENTER_END_RIDE_OTP) else getString (ENTER_OTP)
      },
      errorConfig {
        text = if (state.props.otpIncorrect && state.props.wrongVehicleVariant) then (getString OTP_INVALID_FOR_THIS_VEHICLE_VARIANT) else if state.props.otpIncorrect then (getString ENTERED_WRONG_OTP)  else (getString OTP_LIMIT_EXCEEDED),
        visibility = if (state.props.otpIncorrect || state.props.otpAttemptsExceeded) then VISIBLE else GONE
      },
      subHeadingConfig {
        text = getString (PLEASE_ASK_THE_CUSTOMER_FOR_THE_OTP),
        visibility = if (state.props.otpAttemptsExceeded) then GONE else VISIBLE
      , textStyle = FontStyle.Body1
      },
      imageConfig {
        alpha = if(DS.length state.props.rideOtp < 4) then 0.3 else 1.0
      },
      modalType = ST.OTP,
      showRetakeParcelImage = state.data.activeRide.tripType == ST.Delivery && state.props.currentStage == ST.RideAccepted,
      enableDeviceKeyboard = appConfig.inAppKeyboardModalConfig.enableDeviceKeyboard,
      confirmBtnColor = if state.props.endRideOtpModal then Color.red else Color.darkMint
      }
      in inAppModalConfig'


enterOdometerReadingConfig :: ST.HomeScreenState -> InAppKeyboardModal.InAppKeyboardModalState
enterOdometerReadingConfig state = let
  config' = InAppKeyboardModal.config
  appConfig = CP.getAppConfig CP.appConfig
  inAppModalConfig' = config'{
      otpIncorrect = if (state.props.otpAttemptsExceeded) then false else (state.props.otpIncorrect),
      otpAttemptsExceeded = (state.props.otpAttemptsExceeded),
      inputTextConfig {
        text = state.props.odometerValue,
        focusIndex = state.props.enterOtpFocusIndex,
        textStyle = FontStyle.Heading1
      },
      headingConfig {
        text = if state.props.endRideOdometerReadingModal then getString ENTER_FINAL_ODO_READING else getString (ENTER_CURRENT_ODOMETER_READING),
        margin = (MarginLeft 0)
      },
      errorConfig {
        text = getString (ODOMETER_READING_VALIDATION_FAILED),
        visibility = boolToVisibility $ state.props.isInvalidOdometer
      },
      textBoxConfig {
        textBoxesArray = [0,1,2,3,4],
        width = V 42,
        height = V 46,
        margin = (Margin 6 0 6 0)
      },
      subHeadingConfig {
        text =  getString (ENTER_THE_LAST_4_DIGITS_OF_ODOMETER),
        visibility = boolToVisibility $ not state.props.otpAttemptsExceeded,
        textStyle = FontStyle.Body1,
        gravity = CENTER
      },
      imageConfig {
        alpha = if DS.length state.props.odometerValue < 4 then 0.5 else 1.0
      },
      modalType = ST.ODOMETER,
      confirmBtnColor = if state.props.endRideOdometerReadingModal then Color.red else Color.darkMint,
      isDismissable = true,
      enableDeviceKeyboard = false
      }
      in inAppModalConfig'

driverStatusIndicators :: Array ST.PillButtonState
driverStatusIndicators = [
    {
      status : ST.Offline,
      background : Color.red,
      imageUrl : fetchImage FF_ASSET "ic_driver_status_offline",
      textColor : Color.white900
    },
    {
        status : ST.Silent,
        background : Color.blue800,
        imageUrl : fetchImage FF_ASSET "ic_driver_status_silent",
        textColor : Color.white900
    },
    {
      status : ST.Online,
        background : Color.darkMint,
        imageUrl : fetchImage FF_ASSET "ic_driver_status_online",
        textColor : Color.white900
    }
]
getCancelAlertText :: String -> STR
getCancelAlertText key = case key of
  "ZONE_CANCEL_TEXT_PICKUP" -> ZONE_CANCEL_TEXT_PICKUP
  "ZONE_CANCEL_TEXT_DROP" -> ZONE_CANCEL_TEXT_DROP
  "GO_TO_CANCELLATION_TITLE" -> GO_TO_CANCELLATION_TITLE
  _ -> FREQUENT_CANCELLATIONS_WILL_LEAD_TO_BLOCKING

mapRouteConfig :: String -> String -> Boolean -> PolylineAnimationConfig -> JB.MapRouteConfig
mapRouteConfig srcIcon destIcon isAnim animConfig  = JB.mapRouteConfig {
    sourceSpecialTagIcon = srcIcon
  , destSpecialTagIcon = destIcon
  , vehicleSizeTagIcon = getMerchantVehicleSize unit
  , isAnimation = isAnim 
  , autoZoom = true
  , polylineAnimationConfig = animConfig
}


specialZonePopupConfig :: ST.HomeScreenState -> RequestInfoCard.Config
specialZonePopupConfig state = let
  config = RequestInfoCard.config
  specialZonePopupConf' = config{
    title {
      text = getString SPECIAL_PICKUP_ZONE 
    }
  , primaryText {
      text = getString SPECIAL_PICKUP_ZONE_POPUP_INFO
    }
  , secondaryText {
      visibility = GONE
    }
  , imageConfig {
      imageUrl = fetchImage COMMON_ASSET "ny_ic_sp_pickup_zone_map",
      height = V 122,
      width = V 116,
      visibility = VISIBLE
    }
  , buttonConfig {
      text = getString GOT_IT
    }
  }
  in specialZonePopupConf'

requestInfoCardConfig :: LazyCheck -> RequestInfoCard.Config
requestInfoCardConfig _ = let
  config = RequestInfoCard.config
  requestInfoCardConfig' = config{
    bulletPoints = [getString EARNINGS_PER_KM_DESC_1 , getString EARNINGS_PER_KM_DESC_2],
    title {
      text = getString EARNINGS_P_KM
    }
  , primaryText {
      text = getString $ BONUS_PRIMARY_TEXT "BONUS_PRIMARY_TEXT",
      visibility = GONE
    }
  , secondaryText {
      text = getString $ BONUS_SECONDARY_TEXT "BONUS_SECONDARY_TEXT",
      visibility = GONE
    }
  , imageConfig {
      imageUrl = fetchImage FF_ASSET "ny_ic_rupee_per_km",
      height = V 122,
      width = V 116
    }
  , buttonConfig {
      text = getString GOT_IT
    }
  }
  in requestInfoCardConfig'

waitTimeInfoCardConfig :: ST.HomeScreenState -> RequestInfoCard.Config
waitTimeInfoCardConfig state = 
  let cityConfig = state.data.cityConfig
      tripScheduledAt = fromMaybe "" state.data.activeRide.tripScheduledAt
      currentTime = EHC.getCurrentUTC ""
      time_diff = runFn2 JB.differenceBetweenTwoUTC currentTime tripScheduledAt

  in
    RequestInfoCard.config {
      title {
        text = getString WAIT_TIMER
      }
    , primaryText {
        text = if rideInProgress then 
                getVarString THIS_EXTRA_AMOUNT_THE_CUSTOMER_WILL_PAY [show maxWaitTimeInMinutes] else getString HOW_LONG_WAITED_FOR_PICKUP,
        padding = Padding 16 16 0 0
      }
    , secondaryText {
        text = getString $ CUSTOMER_WILL_PAY_FOR_EVERY_MINUTE ("₹" <> (show chargesOb.perMinCharges)) (show maxWaitTimeInMinutes),
        visibility = if rideInProgress then GONE else VISIBLE,
        padding = PaddingLeft 16
      }
    , imageConfig {
        imageUrl = fetchImage FF_ASSET "ny_ic_waiting_info",
        height = V 130,
        width = V 130,
        visibility = VISIBLE,
        padding = Padding 0 4 1 0
      }
    , buttonConfig {
        text = getString GOT_IT,
        padding = PaddingVertical 16 20
      }
    }
    where rideInProgress = state.data.activeRide.status == API.INPROGRESS
          chargesOb = HU.getChargesOb state.data.activeRide.tripType state.data.cityConfig state.data.activeRide.driverVehicle
          maxWaitTimeInMinutes = Int.floor $ Int.toNumber chargesOb.freeSeconds / 60.0

makePaymentState :: ST.HomeScreenState -> MakePaymentModal.MakePaymentModalState
makePaymentState state = 
  let payableAndGST = EHC.formatCurrencyWithCommas (show state.data.paymentState.payableAndGST)
      useTimeRange = (state.data.config.subscriptionConfig.showLaterButtonforTimeRange && not JB.withinTimeRange "14:00:00" "10:00:00" (EHC.convertUTCtoISC(EHC.getCurrentUTC "") "HH:mm:ss"))
  in 
  {
    title : getString GREAT_JOB,
    description : getDescription state,
    description2 : getString $ COMPLETE_PAYMENT_TO_CONTINUE "COMPLETE_PAYMENT_TO_CONTINUE",
    okButtontext : ( case getLanguageLocale languageKey of
                          "EN_US" -> "Pay ₹" <> payableAndGST <> " now"
                          "HI_IN" -> "अभी ₹" <> payableAndGST <>" का भुगतान करें"
                          "KN_IN" -> "ಈಗ ₹"<> payableAndGST <>" ಪಾವತಿಸಿ"
                          "TA_IN" -> "இப்போது ₹" <> payableAndGST <> " செலுத்துங்கள்"
                          "BN_IN" -> "এখন " <> payableAndGST <> " পে করুন"
                          "TE_IN" -> "చెల్లించండి ₹" <> payableAndGST <> " ఇప్పుడు"
                          _       -> "Pay ₹" <> payableAndGST <> " now"
                      ),
    cancelButtonText : if useTimeRange || state.data.paymentState.laterButtonVisibility then Just $ getString LATER else Nothing,
    ridesCount : state.data.paymentState.rideCount,
    feeItem : [
      { feeType : MakePaymentModal.TOTAL_COLLECTED,
        title : getString TOTAL_MONEY_COLLECTED,
        val : state.data.paymentState.totalMoneyCollected},
      { feeType : MakePaymentModal.EARNED_OF_THE_DAY,
        title : getString FARE_EARNED_OF_THE_DAY,
        val : (state.data.paymentState.totalMoneyCollected - state.data.paymentState.payableAndGST)},
      { feeType : MakePaymentModal.GST_PAYABLE,
        title : getString GST_PLUS_PAYABLE,
        val : state.data.paymentState.payableAndGST}
    ]
  }

getDescription :: ST.HomeScreenState -> String
getDescription state =  case getLanguageLocale languageKey of
                        "EN_US" -> (("You have completed <b>"<> (show state.data.paymentState.rideCount)) <> (if state.data.paymentState.rideCount == 1 then " Ride</b>" else " Rides</b>"))
                        "HI_IN" -> "आपने " <>  show state.data.paymentState.rideCount <> "सवारी पूरी कर ली हैं"
                        "BN_IN" -> "আপনি" <> show state.data.paymentState.rideCount <> "টি রাইড সম্পূর্ণ করেছেন"
                        "TA_IN" -> "நீங்கள்  "<> (show state.data.paymentState.rideCount) <>" சவாரிகளை முடித்துவிட்டீர்கள்!"
                        "KN_IN" -> "ನೀವು ನಿನ್ನೆ "<> (show state.data.paymentState.rideCount) <>" ರೈಡ್‌ಗಳನ್ನು ಪೂರ್ಣಗೊಳಿಸಿದ್ದೀರಿ!"
                        -- "TE_IN" -> ""
                        _       -> (("You have completed <b>"<> (show state.data.paymentState.rideCount)) <> (if state.data.paymentState.rideCount == 1 then " Ride</b>" else " Rides"))

rateCardState :: ST.HomeScreenState -> RateCard.Config
rateCardState state =
  let
    config' = RateCard.config
    rateCardConfig' =
      config'
        { title = getString FEE_BREAKUP
        , description = getString (YATRI_SATHI_FEE_PAYABLE_FOR_DATE "YATRI_SATHI_FEE_PAYABLE_FOR_DATE") <> " " <> state.data.paymentState.date
        , buttonText = Nothing
        , currentRateCardType = CommonTypes.PaymentFareBreakup
        , primaryButtonConfig {
            text = getString GOT_IT
          , visibility = VISIBLE
          }
        , additionalStrings = [
          {key : "FEE_CORRESPONDING_TO_DISTANCE", val : getString FEE_CORRESPONDING_TO_THE_DISTANCE},
          {key : "GOT_IT", val : getString GOT_IT},
          {key : "TOTAL_PAYABLE", val : getString TOTAL_PAYABLE},
          {key : "TOTAL_PAYABLE_VAL", val : "₹" <> (show state.data.paymentState.payableAndGST)}]
          
        , fareList = getChargesBreakup state.data.paymentState.chargesBreakup

        }
  in
    rateCardConfig'


getChargesBreakup :: Array PaymentBreakUp -> Array CommonTypes.FareList
getChargesBreakup paymentBreakUpArr = map (\(PaymentBreakUp item) -> {val : "₹" <>  (show item.amount),
  key : case item.component of
        "Government Charges" -> getString GOVERMENT_CHARGES
        "Platform Fee" -> getString PLATFORM_FEE
        _ -> item.component
    } ) paymentBreakUpArr

autopayBannerConfig :: ST.HomeScreenState -> Boolean -> Banner.Config -- Make sure to update the mapping in carousel config also.
autopayBannerConfig state configureImage =
  let
    config = Banner.config
    bannerType = state.props.autoPayBanner
    dues = getFixedTwoDecimals state.data.paymentState.totalPendingManualDues
    isVehicleAuto = (RC.getCategoryFromVariant state.data.vehicleType) == Just ST.AutoCategory
    config' = config
      {
        backgroundColor = case bannerType of
                        CLEAR_DUES_BANNER -> Color.yellow900
                        DUE_LIMIT_WARNING_BANNER -> Color.pearl
                        LOW_DUES_BANNER -> Color.yellow800
                        _ -> Color.green600,
        title = case bannerType of
                  FREE_TRIAL_BANNER -> getString SETUP_AUTOPAY_BEFORE_THE_TRAIL_PERIOD_EXPIRES 
                  SETUP_AUTOPAY_BANNER -> getString SETUP_AUTOPAY_FOR_EASY_PAYMENTS
                  _ | bannerType == CLEAR_DUES_BANNER || bannerType == LOW_DUES_BANNER -> getVarString CLEAR_DUES_BANNER_TITLE [dues]
                  DUE_LIMIT_WARNING_BANNER -> getVarString DUE_LIMIT_WARNING_BANNER_TITLE [getFixedTwoDecimals state.data.subsRemoteConfig.max_dues_limit]
                  _ -> "",
        titleColor = case bannerType of
                        DUE_LIMIT_WARNING_BANNER -> Color.red
                        _ | bannerType == CLEAR_DUES_BANNER || bannerType == LOW_DUES_BANNER -> Color.black900
                        _ -> Color.white900,
        actionText {
          text = case bannerType of
                        _ | bannerType == DUE_LIMIT_WARNING_BANNER || bannerType == CLEAR_DUES_BANNER || bannerType == LOW_DUES_BANNER -> getString PAY_NOW
                        _ -> (getString SETUP_NOW),
          textColor = case bannerType of
                            _ | bannerType == CLEAR_DUES_BANNER || bannerType == LOW_DUES_BANNER -> Color.black900
                            DUE_LIMIT_WARNING_BANNER -> Color.red
                            _ -> Color.white900,
          style = if configureImage then FontStyle.Body3 else FontStyle.ParagraphText
        },
        imageUrl = fetchImage COMMON_ASSET $ case bannerType of
                      FREE_TRIAL_BANNER -> "ic_free_trial_period" 
                      SETUP_AUTOPAY_BANNER -> "ny_ic_setup_autopay"
                      _ | bannerType == CLEAR_DUES_BANNER || bannerType == LOW_DUES_BANNER -> "ny_ic_clear_dues_v2"
                      DUE_LIMIT_WARNING_BANNER -> "ny_ic_due_limit_warning"
                      _ -> "",
        imageHeight = if configureImage then (V 75) else (V 105),
        imageWidth = if configureImage then (V 98) else (V 118),
        titleStyle = if configureImage then FontStyle.Body4 else FontStyle.Body7,
        imagePadding = case bannerType of
                            _ | bannerType == CLEAR_DUES_BANNER || bannerType == LOW_DUES_BANNER -> PaddingTop 0
                            _ -> PaddingVertical 5 5
      }
  in config'
  
accessibilityPopUpConfig :: ST.HomeScreenState -> PopUpModal.Config
accessibilityPopUpConfig state = 
  let 
    config = PopUpModal.config
    popupData = getAccessibilityPopupData state state.data.activeRide.disabilityTag state.data.activeRide.notifiedCustomer
    config' = config
      {
        gravity = CENTER,
        margin = MarginHorizontal 24 24 ,
        buttonLayoutMargin = Margin 16 0 16 20 ,
        onlyTopTitle = GONE,
        topTitle {
          text = popupData.title
        , gravity = CENTER
        , visibility = boolToVisibility $ popupData.title /= ""
        },
        primaryText {
          text = popupData.primaryText
        , visibility = boolToVisibility $ popupData.primaryText /= ""
        , margin = Margin 16 24 16 4 },
        secondaryText {
          text = popupData.secondaryText
        , visibility = boolToVisibility $ popupData.secondaryText /= ""
        , textStyle = SubHeading2
        , margin = if popupData.primaryText == "" then (MarginVertical 20 24) else (MarginBottom 24)},
        option1 {
          text = getString GOT_IT
        , background = Color.black900
        , color = Color.yellow900
        },
        option2 {
          visibility = false
        },
        backgroundClickable = false,
        cornerRadius = (PTD.Corners 15.0 true true true true),
        coverImageConfig {
          imageUrl = popupData.imageUrl
        , visibility = if popupData.videoUrl /= "" && (HU.shouldShowPurpleVideos state || (popupData.mediaType /= "Audio")) then GONE else VISIBLE
        , height = V 160
        , width = MATCH_PARENT
        , margin = Margin 16 16 16 0
        },
        coverMediaConfig {
          visibility = boolToVisibility $ state.data.activeRide.disabilityTag == Just ST.SAFETY || (popupData.videoUrl /= "" && (HU.shouldShowPurpleVideos state || (popupData.mediaType == "Audio")))
        , width = WRAP_CONTENT
        , padding = if popupData.mediaType == "Audio" then Padding 6 6 6 6 else Padding 16 16 16 0
        , mediaType = popupData.mediaType
        , mediaUrl = popupData.videoUrl
        , id = popupData.videoId
        , background = if popupData.mediaType == "Audio" then Color.blue600 else Color.white900
        , stroke = if popupData.mediaType == "Audio" then "1," <> Color.grey900 else Color.white900
        , margin = if popupData.mediaType == "Audio" then Margin 16 0 16 12 else Margin 0 0 0 0
        , height =  if popupData.mediaType == "Audio" then V 60 else WRAP_CONTENT
        , audioAutoPlay = state.props.safetyAudioAutoPlay
        , cornerRadius = 4.0
        , coverMediaText {
            visibility = boolToVisibility $ popupData.coverMediaText /= ""
          , text = popupData.coverMediaText
          }
        }
      }
  in config'

rentalInfoPopUpConfig :: ST.HomeScreenState -> PopUpModal.Config
rentalInfoPopUpConfig state = 
  let 
    config = PopUpModal.config
    tripDuration = (fromMaybe 0 state.data.activeRide.tripDuration) / 3600
    tripDistance = fromMaybe 0 $ Int.fromNumber $ state.data.activeRide.distance / 1000.0
    rideInfo = (show tripDuration) <> "h / " <> (show tripDistance) <> "km"
    tripType = state.data.activeRide.tripType
    destinationCity = fromMaybe "" $state.data.activeRide.destinationCity
    text  = if tripType == ST.Rental then ((getString $ THERE_MIGHT_BE_MULTIPLE_STOPS_IN_THIS_RENTAL_RIDE rideInfo) <>"<br></br><span style='color:#2194FF'><u>"<> getString WATCH_VIDEO_FOR_HELP <>"</u></span>") else (getString PLEASE_ENSURE_THAT_YOUR_VEHICLE_IS_READY_FOR_INTERCITY_TRIP <> destinationCity)
    config' = config
      {
        gravity = CENTER,
        margin = MarginHorizontal 24 24 ,
        buttonLayoutMargin = Margin 16 0 16 20 ,
        onlyTopTitle = GONE,
        topTitle {
          text = ""
        , gravity = CENTER
        , visibility = VISIBLE
        },
        primaryText {
          text = if tripType == ST.Rental then (getString RENTAL_RIDE_ACCEPTED)  else (getString INTERCITY_RIDE_ACCEPTED)<> "!"
        , visibility = VISIBLE
        , margin = Margin 16 24 16 4 },
        secondaryText {
          text = text
        , visibility = VISIBLE
        , textStyle = SubHeading2
        , margin = MarginBottom 24
        , isClickable  = tripType ==ST.Rental

        },
        option1 {
          text = getString GOT_IT
        , background = Color.black900
        , color = Color.yellow900
        },
        option2 {
          visibility = false
        },
        backgroundClickable = false,
        cornerRadius = (PTD.Corners 15.0 true true true true),
        coverImageConfig {
          imageUrl =  if tripType == ST.Rental then fetchImage FF_ASSET "ny_ic_rental_info" else fetchImage FF_ASSET "ny_ic_intercity_info"
        , visibility = VISIBLE
        , height = V 160
        , width = MATCH_PARENT
        , margin = Margin 0 16 0 0
        }
      }
  in config'



advancedRidePopUpConfig :: ST.HomeScreenState -> PopUpModal.Config
advancedRidePopUpConfig state = 
  let 
    config = PopUpModal.config
    config' = config
      {
        gravity = CENTER,
        margin = MarginHorizontal 24 24 ,
        buttonLayoutMargin = Margin 16 0 16 20 ,
        onlyTopTitle = VISIBLE,
        dismissPopup = true,
        optionButtonOrientation = "VERTICAL",
        topTitle {
          text = getString ADVANCED_RIDE_POPUP_TITLE 
        , gravity = LEFT
        , visibility = VISIBLE
        , width = WRAP_CONTENT
        , margin = Margin 12 0 12 0
        , textStyle = SubHeading2
        },
        primaryText {
          visibility = GONE
        },
        secondaryText {
        visibility = GONE},
        option1 {
          text = getString WATCH_VIDEO
        , background = Color.black900
        , width = MATCH_PARENT
        , gravity = CENTER
        , color = Color.yellow900
        },
        option2 {
          text = getString GOT_IT
          , width = MATCH_PARENT
          , gravity = CENTER
          , background = Color.white900
          , color = Color.black900
          , strokeColor = Color.white900
          , margin = Margin 0 0 0 0
        },
        backgroundClickable = true,
        cornerRadius = (PTD.Corners 15.0 true true true true),
        coverImageConfig {
          imageUrl = fetchImage FF_ASSET "ny_ic_pop_advanced_ride_feat"
        , visibility = VISIBLE
        , height = V 160
        , width = MATCH_PARENT
        , margin = Margin 12 12 12 12
        },
        popUpHeaderConfig{
           visibility = VISIBLE
          , width = MATCH_PARENT
          , height = WRAP_CONTENT
          , margin = Margin 0 0 0 0
          , gravity = CENTER
          , padding = Padding 12 12 12 12
          , orientation = "HORIZONTAL"
          , backgroundColor = Color.blue600
          , headingText {
              margin =  Margin 0 0 0 0
            , color = Color.black900
            , gravity = LEFT
            , text = getString ADVANCE_BOOKING
            , visibility = VISIBLE
          }
          , subHeadingText {
              margin = Margin 0 0 0 0
            , color = Color.black700
            , gravity = LEFT
            , text = getString FEATURE_UPDATE
            , visibility = VISIBLE
          }
          , imageConfig {
              imageUrl = fetchImage FF_ASSET "ny_ic_pop_day_illustration"
            , height = V 60
            , width = V 60
            , margin = Margin 0 0 0 0
            , visibility = VISIBLE
          }
          
        }
      }
  in config'

type ContentConfig = 
  { title :: String,
    coverMediaText :: String,
    primaryText :: String,
    secondaryText :: String,
    imageUrl :: String,
    videoUrl :: String, 
    mediaType :: String,
    videoId :: String,
    background :: String,
    textColor :: String,
    clickable :: Boolean
  }


genericAccessibilityPopUpConfig :: ST.HomeScreenState -> PopUpModal.Config
genericAccessibilityPopUpConfig state = let 
  config' = PopUpModal.config
    {
      gravity = CENTER,
        margin = MarginHorizontal 24 24 ,
        buttonLayoutMargin = Margin 16 0 16 20 ,
        primaryText {
          text = getString WHAT_ARE_PURPLE_RIDES
        , margin = Margin 16 24 16 4 },
        secondaryText {
          text = getString LEARN_HOW_YOU_CAN_HELP_CUSTOMERS_REQUIRING_SPECIAL_ASSISTANCE
        , textStyle = SubHeading2
        , margin = MarginBottom 24},
        option1 {
          text = getString GOT_IT
        , background = Color.black900
        , color = Color.yellow900
        },
        option2 {
          visibility = false
        },
        backgroundClickable = false,
        cornerRadius = (PTD.Corners 15.0 true true true true),
        coverImageConfig {
          visibility = GONE
        },
        coverMediaConfig {
          visibility = VISIBLE
        , height = V 202
        , width = MATCH_PARENT
        , padding = Padding 16 16 16 0
        , mediaType = "PortraitVideoLink"
        , mediaUrl = HU.getGenericAccessibilityVideo state
        , id = "GenericAccessibilityCoverVideo"
        }
    }
  in config'

chatBlockerPopUpConfig :: ST.HomeScreenState -> PopUpModal.Config
chatBlockerPopUpConfig state = let
  config' = PopUpModal.config 
  popUpConfig' = config'{
    gravity = CENTER,
    cornerRadius = (PTD.Corners 24.0 true true true true),
    margin = (MarginHorizontal 16 16),
    primaryText {text = (getString CUSTOMER_HAS_LOW_VISION)},
    secondaryText {text = (getString PLEASE_CONSIDER_CALLING_THEM )},
    option1{ text = (getString GOT_IT),
      width = MATCH_PARENT,
      background = Color.black900,
      strokeColor = Color.black900,
      color = Color.yellow900,
      margin = MarginHorizontal 16 16
      },
    option2{text = (getString PROCEED_TO_CHAT),
      strokeColor = Color.white900,
      color = Color.black650,
      background = Color.white900,
      margin = MarginHorizontal 16 16 ,
      width = MATCH_PARENT},
    optionButtonOrientation = "VERTICAL",
    dismissPopup = true
  }
  in popUpConfig'

accessibilityConfig :: LazyCheck -> ContentConfig
accessibilityConfig dummy = { title : "", coverMediaText : "", primaryText : getString CUSTOMER_MAY_NEED_ASSISTANCE, secondaryText : getString CUSTOMER_HAS_DISABILITY_PLEASE_ASSIST_THEM, videoUrl : "", imageUrl : fetchImage FF_ASSET "ny_ic_disability_illustration", mediaType : "", videoId : "", background : Color.lightPurple, textColor : Color.purple, clickable : false}

getDisabilityTypeVideo :: ST.HomeScreenState -> String -> String
getDisabilityTypeVideo state pwdtype =
  let cityConfig = HU.getCityConfig state.data.config.cityConfig (getValueToLocalNativeStore DRIVER_LOCATION) 
      purpleRideConfigForVehicle = getPurpleRideConfigForVehicle state.data.linkedVehicleVariant cityConfig.purpleRideConfig
      disabilityToVideo = DA.find (\item -> item.disabilityType == pwdtype) purpleRideConfigForVehicle.disabilityToVideo
  in maybe "" (\obj -> obj.videoUrl) disabilityToVideo

getAccessibilityPopupData :: ST.HomeScreenState -> Maybe ST.DisabilityType -> Boolean -> ContentConfig
getAccessibilityPopupData state pwdtype isDriverArrived = 
  let accessibilityConfig' = accessibilityConfig Config
      city = state.data.cityConfig
      driverLocation = DS.toLower $ getValueToLocalStore DRIVER_LOCATION
      language = getLanguage $ getLanguageLocale languageKey
      videoUrl' = "https://assets.moving.tech/beckn/audios/ny_audio_safety" <> language <> ".mp3"
  in case pwdtype, isDriverArrived of 
      Just ST.BLIND_AND_LOW_VISION, true ->  accessibilityConfig' 
                                              { secondaryText = getString CUSTOMER_HAS_POOR_VISION_SOUND_HORN_AT_PICKUP ,
                                                imageUrl = fetchImage FF_ASSET (getUpdatedAssets city),
                                                videoUrl = "",
                                                mediaType = "",
                                                videoId = ""
                                              }   
      Just ST.BLIND_AND_LOW_VISION, false -> accessibilityConfig'
                                              { secondaryText = getString CUSTOMER_HAS_LOW_VISION_CALL_THEM_INSTEAD_OF_CHATTING,
                                                imageUrl = fetchImage FF_ASSET "ny_ic_blind_pickup",
                                                videoUrl = getDisabilityTypeVideo state (show ST.BLIND_AND_LOW_VISION),
                                                mediaType = "VideoLink",
                                                videoId = "BlindOrLowVisionCoverVideo"
                                              }
      Just ST.HEAR_IMPAIRMENT, true ->      accessibilityConfig'
                                              { secondaryText = getString CUSTOMER_HAS_POOR_HEARING_MESSAGE_THEM_AT_PICKUP,
                                                imageUrl = fetchImage FF_ASSET (getUpdatedAssets city),
                                                videoUrl = "",
                                                mediaType = "",
                                                videoId = ""
                                              }   
      Just ST.HEAR_IMPAIRMENT, false ->     accessibilityConfig'
                                              { secondaryText= getString CUSTOMER_HAS_POOR_HEARING_CHAT_WITH_THEM_INSTEAD_OF_CALLING ,
                                                imageUrl = fetchImage FF_ASSET "ny_ic_deaf_pickup",
                                                videoUrl = getDisabilityTypeVideo state (show ST.HEAR_IMPAIRMENT),
                                                mediaType = "VideoLink",
                                                videoId = "HearingImpairmentCoverVideo"
                                              }
      Just ST.LOCOMOTOR_DISABILITY, true -> accessibilityConfig'
                                              { secondaryText = getString CUSTOMER_HAS_LOW_MOBILITY_STORE_THEIR_SUPPORT_AT_PICKUP ,
                                                imageUrl = fetchImage FF_ASSET (getUpdatedAssets city),
                                                videoUrl = "",
                                                mediaType = "",
                                                videoId = ""
                                              }    
      Just ST.LOCOMOTOR_DISABILITY, false -> accessibilityConfig' 
                                              { secondaryText = getString CUSTOMER_HAS_LOW_MOBILITY_GO_TO_EXACT_LOC, 
                                                imageUrl = fetchImage FF_ASSET (getUpdatedAssets city),
                                                videoUrl = getDisabilityTypeVideo state (show ST.LOCOMOTOR_DISABILITY),
                                                mediaType = "VideoLink",
                                                videoId = "LocomotorDisabilityCoverVideo"
                                              }
      Just ST.SAFETY, _ ->        accessibilityConfig' 
                                  { title = if driverLocation == "bangalore" then getString OUR_SAFETY_PARTNER else "",
                                    primaryText = "",
                                    secondaryText = "",
                                    coverMediaText = getString CUSTOMER_SAFETY_OUR_RESP_HAPPY_RIDE, 
                                    imageUrl = if  driverLocation == "bangalore" then fetchImage FF_COMMON_ASSET "ny_ic_bangalure_city_police" else fetchImage FF_COMMON_ASSET "ny_ic_general_safety" ,
                                    videoUrl = "",
                                    mediaType = "",
                                    videoId = ""
                                  }
      Just ST.SPECIAL_ZONE_PICKUP, _ -> accessibilityConfig' 
                                      { title = getString SPECIAL_PICKUP_ZONE,
                                        primaryText = "",
                                        secondaryText = getString SPECIAL_PICKUP_ZONE_POPUP_INFO,
                                        imageUrl = fetchImage COMMON_ASSET "ny_ic_location_unserviceable_green",
                                        videoUrl = "",
                                        mediaType = "",
                                        videoId = ""
                                      }
      Just ST.OTHER_DISABILITY, _ ->     accessibilityConfig' 
      _ , _-> accessibilityConfig' 

  where 
    getUpdatedAssets :: CityConfig -> String 
    getUpdatedAssets cityConfig = 
      if cityConfig.cityCode == "std:040" then 
        case pwdtype, isDriverArrived of 
          Just ST.BLIND_AND_LOW_VISION, true ->  "ny_ic_blind_arrival_generic" 
          Just ST.HEAR_IMPAIRMENT, true -> "ny_ic_deaf_arrival_generic"
          Just ST.LOCOMOTOR_DISABILITY, true -> "ny_ic_locomotor_arrival_black_yellow"
          Just ST.LOCOMOTOR_DISABILITY, false -> "ny_ic_locomotor_pickup_generic"
          _ , _-> "ny_ic_disability_purple"
        else 
          case pwdtype , isDriverArrived of 
            Just ST.BLIND_AND_LOW_VISION, true ->  "ny_ic_blind_arrival"
            Just ST.HEAR_IMPAIRMENT, true -> "ny_ic_deaf_arrival"
            Just ST.LOCOMOTOR_DISABILITY, true -> "ny_ic_locomotor_arrival"
            Just ST.LOCOMOTOR_DISABILITY, false -> "ny_ic_locomotor_pickup"
            _ , _-> "ny_ic_disability_purple"
    getLanguage :: String -> String
    getLanguage lang = 
      let language = DS.toLower $ DS.take 2 lang
      in if not (DS.null language) then "_" <> language else "_en"

getAccessibilityHeaderText :: ST.HomeScreenState -> ContentConfig
getAccessibilityHeaderText state = 
  let config = accessibilityConfig Config
    in  if state.data.activeRide.bookingFromOtherPlatform then
          config {primaryText = (getString $ SOME_FEATURE_ARE_NOT_AVAILABLE_WITH_THIS_PROVIDER (getString $ MERCHANT_NAME "")) <> ".&nbsp;<span style=\"color:#2194FF;\">" <> getString WHY <> "</span>&nbsp", secondaryText = "", imageUrl = fetchImage FF_ASSET "ny_ic_alert_hexagon", background = Color.grey900, textColor = Color.black700, clickable = true}
        else
          if state.data.activeRide.status == NEW then 
            case state.data.activeRide.disabilityTag, state.data.activeRide.notifiedCustomer of   
              Just ST.HEAR_IMPAIRMENT, false -> config {primaryText = getString CUSTOMER_HAS_HEARING_IMPAIRMENT, secondaryText = getString PLEASE_CHAT_AND_AVOID_CALLS, imageUrl = fetchImage FF_ASSET "ny_ic_poor_hearing"}
              Just ST.BLIND_AND_LOW_VISION, false -> config {primaryText = getString CUSTOMER_HAS_LOW_VISION, secondaryText = getString PLEASE_CALL_AND_AVOID_CHATS, imageUrl = fetchImage FF_ASSET "ic_accessibility_vision"}
              Just ST.LOCOMOTOR_DISABILITY, false -> config {primaryText = getString CUSTOMER_HAS_LOW_MOBILITY, secondaryText = getString PLEASE_GO_TO_EXACT_PICKUP,  imageUrl = fetchImage FF_ASSET "ny_ic_disability_purple"}
              Just ST.OTHER_DISABILITY, false -> config {primaryText = getString CUSTOMER_HAS_DISABILITY, secondaryText = getString PLEASE_ASSIST_THEM_IF_NEEDED, imageUrl = fetchImage FF_ASSET "ny_ic_disability_purple"}
              Nothing, false -> config {primaryText = getString CUSTOMER_HAS_DISABILITY, secondaryText = getString PLEASE_ASSIST_THEM_IF_NEEDED, imageUrl = fetchImage FF_ASSET "ny_ic_disability_purple"}
          -- else if state.data.activeRide.isDriverArrived then
              -- case state.data.activeRide.disabilityTag of   
              Just ST.HEAR_IMPAIRMENT, true -> config {primaryText = getString CUSTOMER_HAS_HEARING_IMPAIRMENT, secondaryText = getString MESSAGE_THEM_AT_PICKUP, imageUrl = fetchImage FF_ASSET "ny_ic_poor_hearing"}
              Just ST.BLIND_AND_LOW_VISION, true -> config {primaryText = getString CUSTOMER_HAS_LOW_VISION, secondaryText = getString SOUND_HORN_ONCE_AT_PICKUP, imageUrl = fetchImage FF_ASSET "ic_accessibility_vision"}
              Just ST.LOCOMOTOR_DISABILITY, true -> config {primaryText = getString CUSTOMER_HAS_LOW_MOBILITY, secondaryText = getString HELP_WITH_THEIR_MOBILITY_AID, imageUrl = fetchImage FF_ASSET "ny_ic_disability_purple"}
              Just ST.OTHER_DISABILITY, true -> config {primaryText = getString CUSTOMER_HAS_DISABILITY, secondaryText = getString PLEASE_ASSIST_THEM_IF_NEEDED, imageUrl = fetchImage FF_ASSET "ny_ic_disability_purple"}
              Just ST.SAFETY, _ -> config {primaryText = getString CUSTOMER_SAFETY_FIRST, secondaryText = getString LETS_ENSURE_SAFE_RIDE, imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_green_sheild", background = Color.green100, textColor = Color.green900}
              Just ST.SPECIAL_ZONE_PICKUP, _ -> config {primaryText = getString SPECIAL_PICKUP_ZONE, secondaryText = getString PRIORITY_RIDE_EXPIERENCE, imageUrl = fetchImage COMMON_ASSET "ny_ic_sp_zone_green",background = Color.green100, textColor = Color.green900}
              Nothing, true -> config {primaryText = getString CUSTOMER_HAS_DISABILITY, secondaryText = getString PLEASE_ASSIST_THEM_IF_NEEDED, imageUrl = fetchImage FF_ASSET "ny_ic_disability_purple"}
          else 
            case state.data.activeRide.disabilityTag of   
              Just ST.HEAR_IMPAIRMENT -> config {primaryText = getString CUSTOMER_HAS_HEARING_IMPAIRMENT, secondaryText = getString PLEASE_HELP_THEM_AS_YOU_CAN, imageUrl = fetchImage FF_ASSET "ny_ic_poor_hearing"}
              Just ST.BLIND_AND_LOW_VISION -> config {primaryText = getString CUSTOMER_HAS_LOW_VISION, secondaryText = getString PLEASE_HELP_THEM_AS_YOU_CAN, imageUrl = fetchImage FF_ASSET "ic_accessibility_vision"}
              Just ST.LOCOMOTOR_DISABILITY -> config {primaryText = getString CUSTOMER_HAS_LOW_MOBILITY, secondaryText = getString PLEASE_HELP_THEM_AS_YOU_CAN, imageUrl = fetchImage FF_ASSET "ny_ic_disability_purple"}
              Just ST.OTHER_DISABILITY -> config {primaryText = getString CUSTOMER_HAS_DISABILITY, secondaryText = getString PLEASE_HELP_THEM_AS_YOU_CAN, imageUrl = fetchImage FF_ASSET "ny_ic_disability_purple"}
              Just ST.SAFETY -> config {primaryText =  getString CUSTOMER_SAFETY_FIRST, secondaryText = getString LETS_ENSURE_SAFE_RIDE, imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_green_sheild", background = Color.green100, textColor = Color.green900}
              Just ST.SPECIAL_ZONE_PICKUP -> config {primaryText = getString SPECIAL_PICKUP_ZONE, secondaryText = getString PRIORITY_RIDE_EXPIERENCE, imageUrl = fetchImage COMMON_ASSET "ny_ic_sp_zone_green", background = Color.green100, textColor = Color.green900}
              Nothing -> config {primaryText = getString CUSTOMER_HAS_DISABILITY, secondaryText = getString PLEASE_HELP_THEM_AS_YOU_CAN, imageUrl = fetchImage FF_ASSET "ny_ic_disability_purple"}

getRideCompletedConfig :: ST.HomeScreenState -> RideCompletedCard.Config 
getRideCompletedConfig state = let
  isRentalRide = state.data.activeRide.tripType == ST.Rental
  config = RideCompletedCard.config
  autoPayBanner = state.props.autoPayBanner
  autoPayStatus = state.data.paymentState.autoPayStatus
  payerVpa = state.data.endRideData.payerVpa
  bannerConfig = autopayBannerConfig state false
  disability = state.data.endRideData.disability /= Nothing
  specialZonePickup = isJust $ state.data.endRideData.specialZonePickup
  topPillConfig = constructTopPillConfig disability specialZonePickup
  metroRideCoinData = state.data.endRideData.metroRideCoinData
  showDriverBottomCard = state.data.config.rideCompletedCardConfig.showSavedCommission || isJust state.data.endRideData.tip
  viewOrderConfig = [ {condition : isJust metroRideCoinData, elementView :  RideCompletedCard.COINS_EARNED_VIEW },
                      {condition : (not isRentalRide) && (autoPayBanner == DUE_LIMIT_WARNING_BANNER), elementView :  RideCompletedCard.BANNER },
                      {condition : (not isRentalRide) && (autoPayStatus == ACTIVE_AUTOPAY && payerVpa /= ""), elementView :  RideCompletedCard.QR_VIEW },
                      {condition : (not isRentalRide) && not (autoPayStatus == ACTIVE_AUTOPAY) && state.data.config.subscriptionConfig.enableSubscriptionPopups && (getValueToLocalNativeStore SHOW_SUBSCRIPTIONS == "true"), elementView :  RideCompletedCard.NO_VPA_VIEW },
                      {condition : (not isRentalRide) &&  (autoPayBanner /= DUE_LIMIT_WARNING_BANNER), elementView :  RideCompletedCard.BANNER },
                      {condition : disability, elementView :  RideCompletedCard.BADGE_CARD },
                      {condition : showDriverBottomCard, elementView :  RideCompletedCard.DRIVER_BOTTOM_VIEW},
                      {condition : isRentalRide, elementView : RideCompletedCard.RENTAL_RIDE_VIEW}
                    ]
  pspIcon = (Const.getPspIcon payerVpa)
  isVehicleAuto = (RC.getCategoryFromVariant state.data.vehicleType) == Just ST.AutoCategory
  endrideQrAnim = if isVehicleAuto then "lottie/end_ride_qr_anim.json" else "lottie/end_ride_qr_anim_cab.json"
  config' = config{
    isFreeRide = state.props.isFreeRide,
    serviceTierAndAC = state.data.endRideData.serviceTier,
    capacity = state.data.endRideData.capacity,
    primaryButtonConfig {
      width = MATCH_PARENT,
      margin = MarginTop 0,
      textConfig {
        text = if isJust state.data.advancedRideData then getString GO_TO_ADVANCED_RIDE else getString FARE_COLLECTED
      },
      enableRipple = true,
      rippleColor = Color.rippleShade
    },
    topCard {
      title = getString COLLECT_VIA_UPI_QR_OR_CASH,
      finalAmount = state.data.endRideData.finalAmount,
      initialAmount = state.data.endRideData.finalAmount,
      fareUpdatedVisiblity = state.props.isFreeRide,
      gradient =  ["#F5F8FF","#E2EAFF"],
      infoPill {
        text = getString COLLECT_VIA_CASE_UPI,
        color = Color.white900,
        cornerRadius = 100.0,
        padding = Padding 16 16 16 16,
        margin = MarginTop 16,
        background = Color.peacoat,
        stroke = "1," <> Color.peacoat,
        alpha = 0.8,
        fontStyle = Body1,
        visible = if state.props.isFreeRide then VISIBLE else GONE
      },
      topPill = topPillConfig,
      bottomText = if state.data.activeRide.tripType == ST.Delivery then getString DELIVERY_DETAILS else getString RIDE_DETAILS
    },
    driverBottomCard {
      visible = showDriverBottomCard,
      savedMoney = (if state.data.config.rideCompletedCardConfig.showSavedCommission then [{amount  : (state.data.endRideData.finalAmount * 18) / 100 , reason : getString SAVED_DUE_TO_ZERO_COMMISSION}] else []) <> (case state.data.endRideData.tip of 
                            Just val -> [{amount : val, reason : getString TIP_EARNED_FROM_CUSTOMER}]
                            Nothing -> [])
    },
    contactSupportPopUpConfig{
      gravity = CENTER,
      cornerRadius = PTD.Corners 15.0 true true true true,
      margin = MarginHorizontal 16 16,
      padding = PaddingTop 24,
      buttonLayoutMargin = Margin 0 0 0 0,
      primaryText {
        text = getString CONTACT_SUPPORT <> "?",
        margin = MarginBottom 12
      },
      secondaryText {
        text = getString $ YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT",
        margin = MarginBottom 16
      },
      option1 {
        text = getString CANCEL
      },
      option2 {
        text = getString CALL_SUPPORT
      }
    },
    badgeCard{
      visible = disability,
      image = fetchImage FF_ASSET "ny_ic_disability_confetti_badge",
      imageWidth = V 152, 
      imageHeight = V 106,
      text1 = getString BADGE_EARNED,
      text2 = getString PURPLE_RIDE_CHAMPION,
      background = Color.mangolia
    },
    rentalRideConfig {
    showRideOdometerReading = state.props.isOdometerReadingsRequired,
    rideStartODOReading = (fromMaybe "--" $ show <$> state.data.activeRide.startOdometerReading) <> " Km",
    rideEndODOReading = (fromMaybe "--" $ show <$> state.data.activeRide.endOdometerReading) <> " Km",
    baseRideDuration =  HU.formatSecIntoHourMins (fromMaybe 0 state.data.activeRide.tripDuration),
    baseRideDistance = if state.data.activeRide.distance <= 0.0 then "0.0" else if(state.data.activeRide.distance < 1000.0) then HU.parseFloat (state.data.activeRide.distance) 2 <> " m" else HU.parseFloat((state.data.activeRide.distance / 1000.0)) 2 <> " km",
    actualRideDuration = HU.formatSecIntoHourMins $ fromMaybe (fromMaybe 0 $ Int.fromNumber state.data.activeRide.distance) state.data.endRideData.actualRideDuration,
    actualRideDistance = show ((fromMaybe 0 state.data.endRideData.actualRideDistance) / 1000) <> " km",
    startRideOdometerImage = getValueToLocalStore RIDE_START_ODOMETER,
    endRideOdometerImage = getValueToLocalStore RIDE_END_ODOMETER,
    rideStartedAt = fromMaybe "" state.data.endRideData.tripStartTime,
    rideEndedAt = fromMaybe "" state.data.endRideData.tripEndTime
  },  
    showContactSupportPopUp = state.props.showContactSupportPopUp,
    driverUpiQrCard {
      text = getString GET_DIRECTLY_TO_YOUR_BANK_ACCOUNT,
      id = "renderQRViewOnRideComplete",
      vpa = payerVpa,
      vpaIcon = fetchImage FF_ASSET pspIcon,
      collectCashText = getString OR_COLLECT_CASH_DIRECTLY,
      paymentVpa = "upi://pay?pa=" <> payerVpa <> "&am=" <> (show $ state.data.endRideData.finalAmount)
    },
    noVpaCard {
      title = getString SETUP_AUTOPAY_TO_ACCEPT_PAYMENT,
      collectCashText = getString COLLECT_CASH_DIRECTLY
    },
    accessibility = DISABLE,
    theme = LIGHT,
    isPrimaryButtonSticky = true,
    bannerConfig = bannerConfig{isBanner = autoPayBanner /= NO_SUBSCRIPTION_BANNER},
    viewsByOrder = map (_.elementView) (DA.filter (_.condition) viewOrderConfig),
    lottieQRAnim {
      visible = state.data.config.rideCompletedCardConfig.lottieQRAnim,
      url = (HU.getAssetsBaseUrl FunctionCall) <> endrideQrAnim
    }
  , additionalCharges = additionalCharges
  , coinsEarned  {
      title = maybe "" (\coinData -> getString $ POINTS_EARNED_ $ show coinData.coinsEarned) metroRideCoinData
    , subTitle = maybe "" (\coinData -> if coinData.metroRideType == API.FromMetro then getString FOR_METRO_PICKUP_RIDE else getString FOR_METRO_DROP_RIDE) metroRideCoinData
    }
  , showIntercityDetails  = state.data.activeRide.tripType == ST.Intercity
  , parkingCharges {
    parkingChargesTitle = getString PLEASE_COLLECT_PARKING_CHARGES,
    parkingChargesDescription = getString INCURRED_DURING_TRIP
  }
  , variant = getValueToLocalStore VEHICLE_VARIANT
  , driverCity = getValueToLocalStore DRIVER_LOCATION
  , driverInvoiceText = StringsV2.getStringV2 LT2.invoice_generated_from_driver_to_rider
  }
  in config'

  where
    additionalCharges = [
      {
        text : if state.data.toll.tollAmbigous then getString TOLL_ROAD_CHANGED else if state.data.toll.finalCharge > 0.0  then getString $ RIDE_TOLL_FARE_INCLUDES $  (getCurrency appConfig) <> (show $ round $ state.data.toll.finalCharge) else getString TOLL_ROAD_CHANGED
      , visibility : boolToVisibility $ state.data.toll.estimatedCharge >0.0 ||  state.data.toll.finalCharge > 0.0
      , image : fetchImage FF_COMMON_ASSET "ny_ic_blue_toll"
      , textColor : Color.blue800
      },
      {
        text : maybe "" (\estimatedCharge -> getString $ PARKING_CHARGES_INCLUDED $ (getCurrency appConfig) <> (show $ round $ estimatedCharge)) state.data.parking.estimatedCharge
      , visibility : boolToVisibility $ maybe false (\parkingCharge -> parkingCharge >= 0.0) state.data.parking.estimatedCharge 
      , image : fetchImage FF_COMMON_ASSET "ny_ic_parking_logo_blue"
      , textColor : Color.blue800
      }
    ]

type TopPillConfig = {
  visible :: Boolean,
  text :: String,
  textColor :: String,
  background :: String,
  icon :: Maybe String
}

completeYourProfileConfig :: ST.HomeScreenState -> PopUpModal.Config
completeYourProfileConfig state = PopUpModal.config 
  { 
    cornerRadius = (Corners 24.0 true true false false),
    dismissPopup = true,
    margin = Margin 0 0 0 0,
    padding = Padding 16 20 16 0,
    primaryText = PopUpModal.config.primaryText{
      text = getString COMPLETE_PROFILE_MSG,
      color = Color.black900,
      gravity = CENTER,
      isClickable = false,
      padding = (Padding 0 0 0 0),
      margin = (Margin 5 0 5 24),
      visibility = VISIBLE,
      textStyle = Heading3,
      accessibilityHint = ""
    },
    secondaryText = PopUpModal.config.secondaryText {
      visibility = GONE
    },
    option2 = PopUpModal.config.option2 {
      text = getString COMPLETE_PROFILE,
      margin = Margin 0 0 0 0
    },
    option1 = PopUpModal.config.option2 {
      visibility = false
    }
  }

favPopUpConfig :: ST.HomeScreenState -> PopUpModal.Config 
favPopUpConfig state = 
  PopUpModal.config 
  { cornerRadius = (Corners 15.0 true true true true),
    margin = Margin 16 0 16 0,
    padding = Padding 16 20 16 0,
    gravity = CENTER,
    dismissPopup = true,
    topTitle = PopUpModal.config.topTitle {
      text = state.data.favPopUp.title
    , visibility = VISIBLE
    , width = MATCH_PARENT
    , height = WRAP_CONTENT
    , margin = Margin 10 0 10 0
    , color = Color.black800
    , gravity = CENTER
    , textStyle = Heading3
    },
    coverImageConfig = PopUpModal.config.coverImageConfig{
      visibility = VISIBLE
    , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_person_with_heart"
    , height = V 138
    , width = V 142
    , margin = MarginTop 20
    },
    primaryText = PopUpModal.config.primaryText{
      text = state.data.favPopUp.message,
      color = Color.black800,
      gravity = CENTER,
      isClickable = false,
      padding = (Padding 0 0 0 0),
      margin = (MarginBottom 20),
      visibility = VISIBLE,
      textStyle = Body1,
      accessibilityHint = ""
    },
    secondaryText = PopUpModal.config.secondaryText{
      visibility = GONE
    },
    option2 = PopUpModal.config.option2 {
      text = getString GOT_IT,
      margin = MarginLeft 12 
    },
    option1 = PopUpModal.config.option2 {
      visibility = false
    }
}

constructTopPillConfig :: Boolean -> Boolean -> TopPillConfig
constructTopPillConfig disability specialZonePickup
  | disability = {
      visible: true,
      text: getString PURPLE_RIDE,
      textColor: Color.white900,
      background: Color.blueMagenta,
      icon : Nothing
    }
  | specialZonePickup = {
      visible: true,
      text: "Zone pickup",
      textColor: Color.white900,
      background: Color.green900,
      icon : Just "ny_ic_location_pin_white"
    }
  | otherwise = {
      visible: false,
      text: "",
      textColor: Color.white900,
      background: Color.blueMagenta,
      icon : Nothing
    }

getRatingCardConfig :: ST.HomeScreenState -> RatingCard.RatingCardConfig
getRatingCardConfig state = RatingCard.ratingCardConfig {
    data {
      rating = state.data.endRideData.rating
    },
    primaryButtonConfig {
      textConfig {
        text = getString SUBMIT_FEEDBACK
      },
      isClickable = if state.data.endRideData.rating > 0 then true else false,
      alpha = if state.data.endRideData.rating > 0 then 1.0 else 0.4,
      id = "RatingCardPrimayButton"
    },
    title = getString ( if state.data.activeRide.tripType == ST.Delivery then RATE_YOUR_DELIVERY_WITH else RATE_YOUR_RIDE_WITH1 )<> " " <> state.data.endRideData.riderName <> " " <>  getString RATE_YOUR_RIDE_WITH2,
    feedbackPlaceHolder = getString HELP_US_WITH_YOUR_FEEDBACK,
    closeImgVisible = VISIBLE
  }

subsBlockerPopUpConfig :: ST.HomeScreenState -> PopUpModal.Config
subsBlockerPopUpConfig state = PopUpModal.config {
    cornerRadius = PTD.Corners 15.0 true true true true
    , buttonLayoutMargin = MarginTop 0
    , margin = MarginHorizontal 16 16
    , padding = Padding 16 16 16 16
    , gravity = CENTER
    , backgroundColor =  Color.black9000
    , backgroundClickable = false
    , optionButtonOrientation = "HORIZONTAL"
  ,primaryText {
      text = getString JOIN_A_PLAN_TO_START_EARNING
    , margin = Margin 16 16 16 0
    , color = Color.black800
    , textStyle = Heading2
    },
    option1 {
      text = getString JOIN_NOW
    , color = Color.yellow900
    , background = Color.black900
    , visibility = true
    , margin = MarginTop 16
    , width = MATCH_PARENT

    },
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET "ny_ic_sub_save_more"
    , visibility = VISIBLE
    , width = V 280
    , height = V 250
    },
  secondaryText {visibility = GONE},
  option2 { 
    visibility = false
  },
  optionWithHtml {
    textOpt1 {
      color = Color.black650
      , text = getString NEED_HELP
      , textStyle = SubHeading2
      , visibility = VISIBLE
    }
    , textOpt2 {
      color = Color.blue800
      , textStyle = SubHeading2
      , text = getString CALL_SUPPORT
      , visibility = VISIBLE
    } 
    , image {
        imageUrl = fetchImage FF_ASSET "ny_ic_phone_filled_blue"
        , height = V 16
        , width = V 16
        , visibility = VISIBLE
        , margin = Margin 3 1 3 0
      }
    , strokeColor = Color.white900
    , margin = MarginHorizontal 16 16
    , background = Color.white900
    , visibility = true
    , isClickable = true
    },
  dismissPopup = false
      }

gotoKnowMoreConfig :: ST.HomeScreenState-> PopUpModal.Config
gotoKnowMoreConfig state = PopUpModal.config {
    optionButtonOrientation = "HORIZONTAL",
    buttonLayoutMargin = Margin 16 0 16 20,
    gravity = CENTER,
    margin = MarginHorizontal 20 20,
    cornerRadius = PTD.Corners 15.0 true true true true,
    primaryText{ text = getString KNOW_MORE},
    secondaryText{text = getString THIS_FEATURE_WILL_BE_APPLICABLE,
    margin = MarginVertical 16 20,
    color = Color.black600},
    option1 {
      text = getString GO_BACK,
      margin = MarginHorizontal 16 16,
      color = "#339DFF",
      background = Color.white900,
      strokeColor = Color.white900,
      width = MATCH_PARENT
    },
    option2 {
      visibility = false
    }
  }
-------------------------------------------------DriverRequestPopuop------------------------------------------

gotoRequestPopupConfig :: ST.HomeScreenState -> PopUpModal.Config
gotoRequestPopupConfig state = PopUpModal.config {
    gravity = CENTER,
    optionButtonOrientation = "HORIZONTAL",
    buttonLayoutMargin = Margin 16 0 16 20,
    margin = MarginHorizontal 20 20, 
    primaryText {
      text = strings.primaryText
    , textStyle = Heading2
    , margin = Margin 16 0 16 10},
    secondaryText{
      text = strings.secondaryText
    , textStyle = Body5
    , margin = MarginBottom 20 },
    option1 {
      text = strings.buttonText
    , color = Color.yellow900
    , background = Color.black900
    , strokeColor = Color.transparent
    , textStyle = FontStyle.SubHeading1
    , width = MATCH_PARENT
    },
    option2 { visibility = false
    },
    cornerRadius = PTD.Corners 15.0 true true true true,
    coverImageConfig {
      imageUrl = strings.imageURL
    , visibility = VISIBLE
    , margin = Margin 16 20 16 24
    , width = MATCH_PARENT
    , height = V 270
    }
  }
  where (PopupReturn strings) = gotoCounterStrings state.data.driverGotoState.goToPopUpType

newtype PopupReturn = PopupReturn {
  primaryText :: String,
  secondaryText :: String,
  imageURL :: String,
  buttonText :: String
} 

gotoCounterStrings :: GoToPopUpType -> PopupReturn
gotoCounterStrings popupType = 
  let vehicleVarient  = (getValueFromCache (show VEHICLE_VARIANT) JB.getKeyInSharedPrefKeys)
  in 
  case popupType of 
    MORE_GOTO_RIDES -> PopupReturn { primaryText : getString MORE_GOTO_RIDE_COMING
                              , secondaryText : getString MORE_GOTO_RIDE_COMING_DESC
                              , imageURL : if vehicleVarient == "BIKE" then fetchImage FF_ASSET "ny_ic_goto_bike" else getImage "ny_ic_goto_more_rides" ",https://assets.moving.tech/beckn/jatrisaathi/driver/images/ny_ic_goto_more_rides.png"
                              , buttonText : getString OKAY
                              }
    REDUCED 0 -> PopupReturn { primaryText : getString GOTO_REDUCED_TO_ZERO
                              , secondaryText : getString DUE_TO_MULTIPLE_CANCELLATIONS <> " 0."
                              , imageURL : getImage "ny_ic_gotodriver_zero" "ny_ic_gotodriver_zero,https://assets.moving.tech/beckn/jatrisaathi/driver/images/ny_ic_gotodriver_zero.png"
                              , buttonText : getString OK_GOT_IT
                              }
    REDUCED n -> PopupReturn { primaryText : getString GOTO_REDUCED_TO <> " " <> show n
                              , secondaryText : getString DUE_TO_MULTIPLE_CANCELLATIONS <> " " <> show  n <> "."
                              , imageURL : getImage "ny_ic_gotodriver_one" "ny_ic_gotodriver_one,https://assets.moving.tech/beckn/jatrisaathi/driver/images/ny_ic_gotodriver_one.png"
                              , buttonText : getString OK_GOT_IT
                              }
    VALIDITY_EXPIRED -> PopupReturn { primaryText : getString VALIDITY_EXPIRED_STR
                              , secondaryText : getString VALIDITY_EXPIRED_DESC
                              , imageURL : fetchImage FF_ASSET "ny_ic_validity_expired"
                              , buttonText : getString OK_GOT_IT
                              }
    REACHED_HOME -> PopupReturn { primaryText : getString GOTO_LOC_REACHED
                              , secondaryText : getString YOU_ARE_ALMOST_AT_LOCATION
                              , imageURL : if vehicleVarient == "BIKE" then fetchImage FF_ASSET "ny_ic_goto_bike_arrived" else getImage "ny_ic_goto_arrived" ",https://assets.moving.tech/beckn/jatrisaathi/driver/images/ny_ic_goto_arrived.png"
                              , buttonText : getString OK_GOT_IT
                              }
    NO_POPUP_VIEW -> PopupReturn { primaryText : "" , secondaryText : "" , imageURL : "" , buttonText : "" }
  where 
    getImage current new = 
      let isVehicleRickshaw = (getValueFromCache (show VEHICLE_VARIANT) JB.getKeyInSharedPrefKeys) == "AUTO_RICKSHAW"
      in if isVehicleRickshaw then fetchImage FF_ASSET current else new

------------------------------------------------------------------------------gotoLocInRange------------------------------------------------------------------------------------
gotoLocInRangeConfig :: ST.HomeScreenState-> PopUpModal.Config
gotoLocInRangeConfig _ = PopUpModal.config {
    optionButtonOrientation = "HORIZONTAL",
    buttonLayoutMargin = Margin 16 0 16 20,
    gravity = CENTER,
    margin = MarginHorizontal 20 20,
    cornerRadius = PTD.Corners 15.0 true true true true,
    primaryText{ text = getString YOU_ARE_VERY_CLOSE},
    secondaryText{
      text = getString GOTO_IS_APPLICABLE_FOR,
      margin = MarginVertical 16 20,
      color = Color.black600},
    option1 {
      text = getString GOT_IT,
      margin = MarginHorizontal 16 16,
      color = Color.yellow900,
      background = Color.black900,
      strokeColor = Color.white900,
      width = MATCH_PARENT
    },
    option2 {
      visibility = false
    }
  }

disableGotoConfig :: ST.HomeScreenState-> PopUpModal.Config
disableGotoConfig _ = PopUpModal.config {
  optionButtonOrientation = "VERTICAL",
  buttonLayoutMargin = Margin 16 0 16 20,
  gravity = CENTER,
  backgroundClickable = false,
  margin = MarginHorizontal 20 20,
  cornerRadius = PTD.Corners 15.0 true true true true,
  primaryText{ text = getString DISABLE_GOTO_STR},
  secondaryText{text = getString YOU_STILL_HAVE_TIME_LEFT,
  margin = Margin 0 16 0 20,
  color = Color.black600},
  option1 {
    text = getString YES_DISABLE,
    margin = MarginHorizontal 16 16,
    color = Color.yellow900,
    background = Color.black900,
    strokeColor = Color.white900,
    width = MATCH_PARENT
  },
  option2 {
    text = getString CANCEL,
    margin = MarginHorizontal 16 16,
    color = Color.black650,
    background = Color.white900,
    strokeColor = Color.white900,
    width = MATCH_PARENT
  }
}


locationListItemConfig :: ST.GoToLocation -> ST.HomeScreenState -> GoToLocationModal.GoToModalConfig
locationListItemConfig state homeScreenState = GoToLocationModal.config 
  { id = state.id,
    lat = state.lat,
    lon = state.lon,
    address = state.address,
    tag = state.tag,
    isSelectable = true,
    isEditEnabled = false,
    isSelected = homeScreenState.data.driverGotoState.selectedGoTo == state.id,
    removeAcText = Nothing,
    editAcText = Nothing,
    disabled = state.disabled
  }

primaryButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
primaryButtonConfig _ = PrimaryButton.config
  { textConfig
      { text = getString ADD_LOCATION}
    , margin = (Margin 16 15 16 24)
    , height = V 52
  }


enableButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
enableButtonConfig state = PrimaryButton.config
  { textConfig 
    { text = getString YES_ENABLE
    , textStyle = SubHeading1
    , weight = Just 1.0
    }
  , height = WRAP_CONTENT
  , gravity = CENTER
  , cornerRadius = 8.0
  , padding = Padding 10 14 10 15
  , margin = MarginLeft 0
  , id = "EnableGoto"
  , alpha = if state.data.driverGotoState.selectedGoTo /= "" then 1.0 else 0.5
  , isClickable = state.data.driverGotoState.selectedGoTo /= ""
  , enableLoader = JB.getBtnLoader "EnableGoto"
  , lottieConfig 
    { lottieURL = (HU.getAssetsBaseUrl FunctionCall) <> "lottie/primary_button_loader.json"
    , width = V 90
    , height = V 50
    }
  }

cancelButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
cancelButtonConfig _ = PrimaryButton.config
  { textConfig
    { text = getString CANCEL
    , gravity = LEFT
    , height = WRAP_CONTENT
    , textStyle = SubHeading1
    , weight = Just 1.0
    , color = Color.black900
    }
  , height = WRAP_CONTENT
  , gravity = CENTER
  , cornerRadius = 8.0
  , padding = Padding 10 14 10 15
  , margin = MarginLeft 0
  , stroke = "1," <> Color.grey800
  , background = Color.white900
  }

gotoButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
gotoButtonConfig state = PrimaryButton.config
  { textConfig 
    { text = if (state.data.driverGotoState.isGotoEnabled) then state.data.driverGotoState.timerInMinutes else getString GO_TO
    , textStyle = Tags
    , weight = Just 1.0
    , gravity = CENTER
    , color = gotoTimer.textColor
    }
  , height = WRAP_CONTENT
  , gravity = CENTER
  , cornerRadius = 22.0
  , width = WRAP_CONTENT
  , padding = if (state.data.driverGotoState.isGotoEnabled) then Padding 16 11 16 11 else Padding 24 11 24 11
  , margin = MarginLeft 0
  , isPrefixImage = true
  , stroke = "0," <> Color.black900
  , background = gotoTimer.bgColor
  , prefixImageConfig
    { imageUrl = gotoTimer.imageString
    , height = V 15
    , width = V 15
    , margin = MarginRight 5
    }
  , id = "GotoClick"
  , alpha = if state.data.driverGotoState.gotoCount == 0 then 0.3 else 1.0
  , enableLoader = JB.getBtnLoader "GotoClick"
  , enableRipple = true
  , rippleColor = Color.rippleShade
  , lottieConfig 
    { lottieURL = (HU.getAssetsBaseUrl FunctionCall) <> "lottie/primary_button_loader.json"
    , width = V 100
    , height = V 35
    , autoDisableLoader = false
    }
  }
  where gotoTimer = gotoTimerConfig state.data.driverGotoState.isGotoEnabled

gotoTimerConfig :: Boolean -> {bgColor :: String , imageString :: String, textColor :: String }
gotoTimerConfig enabled 
  | enabled = {bgColor : Color.green900, imageString : fetchImage FF_ASSET "ny_pin_check_white", textColor : Color.white900}
  | otherwise = {bgColor : Color.white900, imageString : fetchImage FF_ASSET "ny_ic_goto_icon_map_pin_check", textColor : Color.black800}

sourceUnserviceableConfig :: ST.HomeScreenState -> ErrorModal.Config
sourceUnserviceableConfig state =
  let
    config = ErrorModal.config
    errorModalConfig' =
      config
        { height = if state.data.config.enableMockLocation && state.props.isMockLocation then MATCH_PARENT else WRAP_CONTENT
        , background = Color.white900
        , stroke = ("1," <> Color.borderGreyColor)
        , imageConfig
          { imageUrl = fetchImage FF_ASSET "ny_ic_location_unserviceable"
          , height = V 99
          , width = V 133
          , margin = (Margin 0 50 0 20)
          }
        , errorConfig
          { text = getString UNABLE_TO_GET_YOUR_LOCATION
          , color = Color.black800
          , margin = (MarginBottom 5)
          }
        , errorDescriptionConfig
          { text = getString TURN_OFF_ANY_MOCK_LOCATION_APP_AND_RESTART
          , color = Color.black700
          , margin = (Margin 20 0 20 (40 + EHC.safeMarginBottom))
          }
        , buttonConfig
          { text = (getString CHANGE_LOCATION)
          , margin = (Margin 16 0 16 (20 + EHC.safeMarginBottom))
          , background = state.data.config.primaryBackground
          , color = state.data.config.primaryTextColor
          , visibility = GONE
          }
        }
  in
    errorModalConfig'

accountBlockedPopup :: ST.HomeScreenState -> PopUpModal.Config
accountBlockedPopup state = PopUpModal.config {
    gravity = CENTER,
    backgroundClickable = false,
    optionButtonOrientation = "VERTICAL",
    buttonLayoutMargin = Margin 16 0 16 20,
    margin = MarginHorizontal 25 25, 
    primaryText {
      text = getString ACCOUNT_BLOCKED
    , textStyle = Heading2
    , margin = Margin 16 0 16 10},
    secondaryText{
      text = getString YOU_HAVE_BEEN_BLOCKED_FROM_TAKING_RIDES
    , textStyle = Body5
    , margin = Margin 16 0 16 15 },
    option1 {
      text = getString CALL_SUPPORT
    , color = Color.yellow900
    , background = Color.black900
    , strokeColor = Color.transparent
    , textStyle = FontStyle.SubHeading1
    , width = MATCH_PARENT
    },
    option2 {
    text = getString DISMISS,
    margin = MarginHorizontal 16 16,
    color = Color.black650,
    background = Color.white900,
    strokeColor = Color.white900,
    width = MATCH_PARENT
  },
    cornerRadius = PTD.Corners 15.0 true true true true,
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET "ny_failed"
    , visibility = VISIBLE
    , margin = Margin 16 20 16 24
    , width = V 160
    , height = V 118
    }
  }

accountBlockedDueToCancellationsPopup :: ST.HomeScreenState -> PopUpModal.Config
accountBlockedDueToCancellationsPopup state = 
  let blockedExpiryTime = EHC.convertUTCtoISC state.data.blockExpiryTime "hh:mm A"
      blockedExpiryDate = EHC.convertUTCtoISC state.data.blockExpiryTime "DD-MM-YYYY"
  in
  PopUpModal.config {
    gravity = CENTER,
    backgroundClickable = false,
    optionButtonOrientation = "VERTICAL",
    buttonLayoutMargin = Margin 16 0 16 20,
    margin = MarginHorizontal 25 25, 
    primaryText {
      text = getString $ BLOCKED_TILL blockedExpiryTime blockedExpiryDate
    , textStyle = Heading2
    , margin = Margin 16 0 16 10},
    secondaryText{
      text = getString DUE_TO_HIGHER_CANCELLATION_RATE_YOU_ARE_BLOCKED
    , textStyle = Body5
    , margin = Margin 16 0 16 15 },
    option1 {
      text = getString CALL_SUPPORT
    , color = Color.yellow900
    , background = Color.black900
    , strokeColor = Color.transparent
    , textStyle = FontStyle.SubHeading1
    , width = MATCH_PARENT
    , image {
        imageUrl = fetchImage FF_ASSET "ny_ic_phone_filled_yellow"
        , height = V 16
        , width = V 16
        , visibility = VISIBLE
        , margin = MarginRight 8
      }
    },
    option2 {
    text = getString CLOSE,
    margin = MarginHorizontal 16 16,
    color = Color.black650,
    background = Color.white900,
    strokeColor = Color.white900,
    width = MATCH_PARENT
  },
    cornerRadius = PTD.Corners 15.0 true true true true,
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET "ny_ic_account_blocked"
    , visibility = VISIBLE
    , margin = Margin 16 16 16 16
    , width = MATCH_PARENT
    , height = V 250
    }
  }

vehicleNotSupportedPopup :: ST.HomeScreenState -> PopUpModal.Config
vehicleNotSupportedPopup state = 
  let cityConfig = state.data.cityConfig
      vehicletype = case state.data.vehicleType of
        _ | elem state.data.vehicleType ["AMBULANCE_TAXI", "AMBULANCE_TAXI_OXY", "AMBULANCE_AC", "AMBULANCE_AC_OXY", "AMBULANCE_VENTILATOR"] -> getString AMBULANCE
        _ -> HU.getVehicleType state.data.vehicleType
  in PopUpModal.config {
      gravity = CENTER,
      backgroundClickable = false,
      optionButtonOrientation = "HORIZONTAL",
      buttonLayoutMargin = Margin 16 0 16 20,
      margin = MarginHorizontal 25 25, 
      primaryText {
        text = vehicletype <> getString IS_NOT_SUPPORTED_YET
      , textStyle = Heading2
      , margin = Margin 16 0 16 10},
      secondaryText{
        text = getString $ WE_WILL_NOFITY_YOU_WHEN_IT_IS_AVAILABLE vehicletype
      , textStyle = Body5
      , margin = Margin 16 0 16 15 },
      option1 {
        text = getString OKAY
      , width = MATCH_PARENT
      },
      option2 {
      visibility = false
      },
      cornerRadius = PTD.Corners 15.0 true true true true,
      coverImageConfig {
        imageUrl = case state.data.linkedVehicleCategory of 
                   "BIKE" -> fetchImage FF_ASSET "ny_ic_bike_not_supported"
                   "AUTO_RICKSHAW" -> fetchImage FF_ASSET "ny_ic_auto_not_supported"
                   "DELIVERY_LIGHT_GOODS_VEHICLE" -> fetchImage FF_ASSET "ny_ic_truck_not_supported"
                   _ | elem state.data.linkedVehicleCategory ["AMBULANCE_TAXI", "AMBULANCE_TAXI_OXY", "AMBULANCE_AC", "AMBULANCE_AC_OXY", "AMBULANCE_VENTILATOR"] -> fetchImage FF_ASSET "ny_ic_ambulance_not_supported"
                   _ -> fetchImage FF_ASSET cityConfig.vehicleNSImg
      , visibility = VISIBLE
      , margin = MarginHorizontal 16 16
      , width = V 300
      , height = V 315
      }
        }

bgLocPopup :: ST.HomeScreenState -> PopUpModal.Config
bgLocPopup state = 
  PopUpModal.config {
      gravity = CENTER,
      backgroundClickable = false,
      optionButtonOrientation = "HORIZONTAL",
      buttonLayoutMargin = Margin 16 0 16 20,
      margin = MarginHorizontal 25 25, 
      primaryText {
        text = getString ENABLE_LOC_PERMISSION_TO_GET_RIDES
      , textStyle = Heading2
      , margin = Margin 16 0 16 10},
      secondaryText{
        text = getString ENABLE_LOC_PER_FROM_SETTINGS
      , textStyle = Body5
      , margin = Margin 16 0 16 15 },
      option1 {
        text = getString ENABLE_PERMISSION_STR
      , color = Color.yellow900
      , background = Color.black900
      , strokeColor = Color.transparent
      , textStyle = FontStyle.SubHeading1
      , width = MATCH_PARENT
      , enableRipple = true
      },
      option2 {
      visibility = false
    },
      cornerRadius = PTD.Corners 15.0 true true true true,
      coverImageConfig {
        imageUrl = fetchImage FF_ASSET "ny_ic_bgloc"
      , visibility = GONE
      , width = V 300
      , height = V 315
      }
    , coverLottie{
        visibility = VISIBLE
      , id = EHC.getNewIDWithTag "bgLocLottie"
      , height =V 300
      , width = V 300
      , config{
          rawJson = (HU.getAssetsBaseUrl FunctionCall) <> "lottie/" <>  (if state.data.config.appData.name =="Mana Yatri" then "enable_locatio_permission_lottie_manayatri" else "enable_locatio_permission_lottie") <> ".json"
        , lottieId =  EHC.getNewIDWithTag "bgLocLottie"
        }
      }
    }
  
interOperableInfoPopup :: ST.HomeScreenState -> PopUpModal.Config
interOperableInfoPopup state = PopUpModal.config {
      gravity = CENTER
    , backgroundClickable = false
    , optionButtonOrientation = "VERTICAL"
    , buttonLayoutMargin = Margin 16 0 16 20
    , margin = MarginHorizontal 25 25
    , topTitle {
        text = getString THIRD_PARTY_RIDES
        , visibility = VISIBLE
        , width = MATCH_PARENT
        , height = WRAP_CONTENT 
        , margin = Margin 16 20 16 0
        , color = Color.black800
        , gravity = CENTER
        }
    , primaryText {
        text = getString THIRD_PARTY_RIDES_ARE_REQUESTED_WITH_BY_A_USERS_FROM_ANOTHER_APP
      , color = Color.black700
      , gravity = CENTER
      , margin = Margin 16 8 16 0
      , textStyle = Heading3
      }
    , secondaryText{
        text = getString SOME_FEATURES_MAY_NOT_BE_AVAILABLE
      , textStyle = Body5
      , margin = Margin 16 8 16 15 
      , color = Color.black700
      }
    , option1 {
        text = getString CALL_SUPPORT
      , width = V 1
      , height = V 1      
      }
    , option2 {
        text = getString GOT_IT
      , margin = MarginHorizontal 0 0
      , color = Color.black650
      , background = Color.white900
      , strokeColor = Color.black500
      , width = MATCH_PARENT
      }
  , cornerRadius = PTD.Corners 15.0 true true true true
  , coverMediaConfig {
        visibility = VISIBLE
      , height = WRAP_CONTENT
    }
  }  
    
introducingCoinsPopup :: ST.HomeScreenState -> PopUpModal.Config
introducingCoinsPopup state = PopUpModal.config {
    cornerRadius = PTD.Corners 15.0 true true true true
    , buttonLayoutMargin = MarginTop 0
    , margin = MarginHorizontal 16 16
    , padding = Padding 16 16 16 16
    , gravity = CENTER
    , backgroundColor =  Color.black9000
    , backgroundClickable = false
    , optionButtonOrientation = "HORIZONTAL"
  ,primaryText {
      text = getString INTRODUCING_YATRI_POINTS <> " 🎉"
    , margin = MarginHorizontal 16 16
    , color = Color.black800
    , textStyle = Heading2
    },
    option1 {
      text = getString CHECK_NOW
    , color = Color.yellow900
    , background = Color.black900
    , visibility = true
    , margin = MarginTop 16
    , width = MATCH_PARENT

    },
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET "ny_ic_coin_balance"
    , visibility = VISIBLE
    , width = V 280
    , height = V 250
    },
  secondaryText {
    text = getString $ NOW_EARN_POINTS_FOR_EVERY_RIDE_AND_REFERRAL_AND_USE_THEM_TO_GET_REWARDS "NOW_EARN_POINTS_FOR_EVERY_RIDE_AND_REFERRAL_AND_USE_THEM_TO_GET_REWARDS"
    , margin = Margin 16 4 16 0
    , color = Color.black700
    , textStyle = SubHeading2
  },
  option2 { 
    visibility = false
  },
  optionWithHtml {
    textOpt1 {
      color = Color.black650
      , text = getString MAYBE_LATER
      , textStyle = SubHeading2
      , visibility = VISIBLE
    }
    , strokeColor = Color.white900
    , margin = MarginHorizontal 16 16
    , background = Color.white900
    , visibility = true
    , isClickable = true
    },
  dismissPopup = false
    }

coinEarnedPopup :: ST.HomeScreenState -> PopUpModal.Config
coinEarnedPopup state =
  let
    popupConfig = getCoinEarnedPopupConfig state
  in
    PopUpModal.config
      { cornerRadius = Corners 15.0 true true true true
      , margin = MarginHorizontal 16 16
      , padding = Padding 16 16 16 16
      , gravity = CENTER
      , backgroundColor = Color.black9000
      , backgroundClickable = true
      , buttonLayoutMargin = MarginBottom 0
      , primaryText
        { text = popupConfig.primaryText
        , margin = Margin 16 16 16 0
        , visibility = VISIBLE
        , color = popupConfig.primaryTextColor
        , textStyle = Heading2
        }
      , option1
        { text = popupConfig.option1
        , color = Color.yellow900
        , background = Color.black900
        , visibility = true
        , margin = MarginTop 16
        , width = MATCH_PARENT
        }
      , coverImageConfig
        { imageUrl = popupConfig.coverImageConfig
        , visibility = boolToVisibility (popupConfig.coverImageConfig /= "")
        , width = MATCH_PARENT
        , height = V 210
        }
      , coverLottieConfig
        { lottieUrl = popupConfig.coverLottieConfig
        , width = MATCH_PARENT
        , height = V 210
        , visibility = boolToVisibility (popupConfig.coverLottieConfig /= "")
        , id = (EHC.getNewIDWithTag "PopupLottieView")
        }
      , option2 { visibility = false }
      , secondaryText
        { text = popupConfig.secondaryText
        , color = Color.black700
        , margin = Margin 16 4 16 0
        , textStyle = SubHeading2
        , visibility = boolToVisibility popupConfig.secondaryTextVisibility 
        },
      optionWithHtml {
        textOpt1 {
          color = Color.black650
          , text = popupConfig.optionWithHtml
          , textStyle = SubHeading2
          , visibility = VISIBLE
        }
        , strokeColor = Color.white900
        , margin = MarginHorizontal 16 16
        , background = Color.white900
        , visibility = popupConfig.optionWithHtmlVisibility
        , isClickable = true
      }
      , dismissPopup = true
      , popUpHeaderConfig {
        gravity = CENTER
        , visibility = boolToVisibility $ popupConfig.headerTextVisibility || popupConfig.subHeadingTextVisibility
        , headingText {
          text = popupConfig.headerText
          , visibility = boolToVisibility popupConfig.headerTextVisibility
        }
        , subHeadingText {
          text = popupConfig.subHeadingText
          , visibility = boolToVisibility popupConfig.subHeadingTextVisibility
          , textStyle = SubHeading1
        }
      }
    }

type CoinEarnedPopupConfig = 
  { primaryText :: String
  , secondaryText :: String
  , secondaryTextVisibility :: Boolean
  , option1 :: String
  , optionWithHtml :: String
  , optionWithHtmlVisibility :: Boolean
  , coverImageConfig :: String
  , coverLottieConfig :: String
  , headerText :: String
  , subHeadingText :: String
  , headerTextVisibility :: Boolean
  , subHeadingTextVisibility :: Boolean
  , primaryTextColor :: Color
  }

defaultCoinPopupConfig :: CoinEarnedPopupConfig
defaultCoinPopupConfig = 
  { primaryText: ""
  , secondaryText: ""
  , secondaryTextVisibility: false
  , option1: ""
  , optionWithHtml: ""
  , optionWithHtmlVisibility: false
  , coverImageConfig: ""
  , coverLottieConfig: ""
  , headerText: ""
  , subHeadingText: ""
  , headerTextVisibility: false
  , subHeadingTextVisibility: false
  , primaryTextColor: Color.black800
  }

createCoinPopupConfig :: String -> String -> Boolean -> String -> String -> Boolean -> String -> String -> String -> String -> Boolean -> Boolean -> Maybe Color -> CoinEarnedPopupConfig
createCoinPopupConfig primary secondary secondaryVis opt1 optHtml optHtmlVis coverImg coverLottie header subHead headerVis subHeadVis primaryTextColor =
  { primaryText: primary
  , secondaryText: secondary
  , secondaryTextVisibility: secondaryVis
  , option1: opt1
  , optionWithHtml: optHtml
  , optionWithHtmlVisibility: optHtmlVis
  , coverImageConfig: coverImg
  , coverLottieConfig: coverLottie
  , headerText: header
  , subHeadingText: subHead
  , headerTextVisibility: headerVis
  , subHeadingTextVisibility: subHeadVis
  , primaryTextColor: fromMaybe Color.black800 primaryTextColor
  }

getCoinEarnedPopupConfig :: ST.HomeScreenState -> CoinEarnedPopupConfig
getCoinEarnedPopupConfig state = do 
  let coinsConfig = getCoinsConfigData $ DS.toLower $ getValueToLocalStore DRIVER_LOCATION
  case state.props.coinPopupType of
    ST.RIDE_MORE_EARN_COIN -> createCoinPopupConfig (getString RIDE_MORE_AND_EARN_POINTS) (getString TAKE_MORE_RIDES_TO_EARN_MORE_POINTS_AND_CONVERT_IT_TO_SUBSCRIPTION_DISCOUNTS) true (getString OKAY) (getString CHECK_YATRI_POINTS) true (HU.fetchImage HU.FF_ASSET "ny_ic_ride_more_earn_more") "" "" "" false false Nothing
    ST.TWO_MORE_RIDES -> createCoinPopupConfig (getString TWO_MORE_RIDES_TO_GO) (getString $ TAKE_TWO_MORE_RIDES_TO_EARN_POINTS $ coinsConfig.eightPlusRidesCoins) true (getString OKAY) "" false (HU.fetchImage HU.FF_ASSET "ny_ic_two_more_rides") "" "" "" false false Nothing
    ST.ONE_MORE_RIDE -> createCoinPopupConfig (getString ONE_MORE_RIDE_TO_GO) (getString $ TAKE_ONE_MORE_RIDE_TO_EARN_POINTS $ coinsConfig.eightPlusRidesCoins) true (getString OKAY) "" false (HU.fetchImage HU.FF_ASSET "ny_ic_one_more_ride") "" "" "" false false Nothing
    ST.EIGHT_RIDE_COMPLETED -> createCoinPopupConfig (getString RIDE_MORE_EARN_MORE) (getString $ LIMITED_TIME_OFFER_UNTIL ((EHC.convertUTCtoISC (runFn2 EHC.getDateMinusNDays coinsConfig.monsoonOfferDate 1) "Do MMM"))) false (getString CHECK_YATRI_POINTS) (getString CHECK_YATRI_POINTS) false (HU.fetchImage HU.COMMON_ASSET "ny_ic_eight_rides_completed") "" (getString CONGRATULATIONS <> "🎉") "" true false Nothing
    ST.FIVE_RIDE_COMPLETED -> createCoinPopupConfig (getString $ ONLY_5_MORE_RIDES_FOR_N_POINTS "50") (getString $ LIMITED_TIME_OFFER_UNTIL ((EHC.convertUTCtoISC (runFn2 EHC.getDateMinusNDays coinsConfig.monsoonOfferDate 1) "Do MMM"))) false (getString CHECK_YATRI_POINTS) (getString CHECK_YATRI_POINTS) false (HU.fetchImage HU.COMMON_ASSET "ny_ic_five_rides_completed_v4") "" (getString CONGRATULATIONS <> "🎉") (getString $ YOU_GOT_N_POINTS "20") true true (Just Color.blue800)
    ST.SIX_RIDE_COMPLETED -> createCoinPopupConfig (getString $ ONLY_4_MORE_RIDES_FOR_N_POINTS "45") (getString $ LIMITED_TIME_OFFER_UNTIL ((EHC.convertUTCtoISC (runFn2 EHC.getDateMinusNDays coinsConfig.monsoonOfferDate 1) "Do MMM"))) true (getString CHECK_YATRI_POINTS) (getString CHECK_YATRI_POINTS) false (HU.fetchImage HU.COMMON_ASSET "ny_ic_six_rides_completed_v2") "" (getString CONGRATULATIONS <> "🎉") (getString $ YOU_GOT_N_POINTS "15") true true (Just Color.blue800)
    ST.TEN_RIDE_COMPLETED -> createCoinPopupConfig (getString RIDE_MORE_EARN_MORE) (getString $ LIMITED_TIME_OFFER_UNTIL ((EHC.convertUTCtoISC (runFn2 EHC.getDateMinusNDays coinsConfig.monsoonOfferDate 1) "Do MMM"))) true (getString CHECK_YATRI_POINTS) (getString CHECK_YATRI_POINTS) false (HU.fetchImage HU.COMMON_ASSET "ny_ic_ten_rides_completed_v5") "" (getString CONGRATULATIONS <> "🎉") "" true false (Just Color.blue800)
    ST.TWO_RIDE_COMPLETED -> createCoinPopupConfig (getString $ ONLY_4_MORE_RIDES_FOR_N_POINTS "20") (getString $ LIMITED_TIME_OFFER_UNTIL ((EHC.convertUTCtoISC (runFn2 EHC.getDateMinusNDays coinsConfig.monsoonOfferDate 1) "Do MMM"))) false (getString CHECK_YATRI_POINTS) (getString CHECK_YATRI_POINTS) false (HU.fetchImage HU.COMMON_ASSET "ny_ic_two_rides_completed_v5") "" (getString CONGRATULATIONS <> "🎉") (getString $ YOU_GOT_N_POINTS "5") true true (Just Color.blue800)
    ST.REFER_AND_EARN_COIN -> createCoinPopupConfig (getString $ REFER_NAMMA_YATRI_APP_TO_CUSTOMERS_AND_EARN_POINTS "REFER_NAMMA_YATRI_APP_TO_CUSTOMERS_AND_EARN_POINTS") "" false (getString REFER_NOW) (getString LATER) true (HU.fetchImage HU.FF_ASSET "ny_ic_refer_and_earn_coin") "" "" "" false false Nothing
    ST.CONVERT_COINS_TO_CASH -> createCoinPopupConfig (getString CONVERT_YOUR_POINTS_TO_DISCOUNT) (getString CONVERT_YOUR_POINTS_TO_GET_DISCOUNT_ON_YOUR_SUBSCRIPTION) true (getString CONVERT_NOW) (getString LATER) true "" (coinsConfig.coinConversionPopupLottie) "" "" false false Nothing
    _ -> defaultCoinPopupConfig

isAcWorkingPopupConfig :: ST.HomeScreenState -> PopUpModal.Config
isAcWorkingPopupConfig state = PopUpModal.config {
    gravity = CENTER,
    backgroundClickable = true,
    optionButtonOrientation = "HORIZONTAL",
    buttonLayoutMargin = MarginBottom 10,
    dismissPopup = true,
    isVisible = not (isAmbulance state.data.linkedVehicleCategory),
    margin = MarginHorizontal 25 25, 
    primaryText {
      text = getString IS_YOUR_CAR_AC_TURNED_ON_AND_WORKING,
      textStyle = Heading2,
      margin = Margin 16 0 16 10},
    secondaryText{
      text = getString YOU_CAN_ALWAYS_CHANGE_THIS_FROM_PROFILE,
      textStyle = ParagraphText,
      margin = Margin 16 0 16 15 },
    option1 {
      text = getString YES,
      color = Color.black700,
      background = Color.blue600,
      textStyle = FontStyle.SubHeading1,
      strokeColor = Color.blue600
    },
    option2 {
      text = getString NO,
      color = Color.black700,
      background = Color.blue600,
      strokeColor = Color.blue600
    },
    cornerRadius = PTD.Corners 15.0 true true true true,
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET "ny_ic_ac_working_popup"
    , visibility = VISIBLE
    , margin = MarginVertical 20 24
    , width = MATCH_PARENT
    , height = V 190
    }
  }

topAcDriverPopUpConfig :: ST.HomeScreenState -> PopUpModal.Config
topAcDriverPopUpConfig state = let 
  appName = fromMaybe state.data.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
  config' = PopUpModal.config
    { gravity = CENTER,
      margin = MarginHorizontal 24 24 ,
      buttonLayoutMargin = Margin 16 0 16 20 ,
      optionButtonOrientation = "VERTICAL",
      primaryText {
        text = getString $ TOP_AC_DRIVER appName,
        margin = Margin 18 24 18 24
    },
      secondaryText { visibility = GONE },
      option1 {
        text = getString WATCH_VIDEO,
        margin = MarginHorizontal 16 16,
        color = Color.yellow900,
        background = Color.black900,
        strokeColor = Color.white900,
        width = MATCH_PARENT
      },
        option2 {
        text = getString MAYBE_LATER,
        margin = Margin 16 7 16 0,
        color = Color.black650,
        background = Color.white900,
        strokeColor = Color.white900,
        width = MATCH_PARENT
      },
      backgroundClickable = true,
      dismissPopup = true,
      cornerRadius = Corners 15.0 true true true true,
      coverImageConfig {
        visibility = VISIBLE,
        height = V 215,
        width = V 320,
        margin = Margin 17 20 17 0,
        imageUrl = HU.fetchImage HU.FF_ASSET "ny_ac_explanation"
      }
    }
  in config'

referralEarnedConfig :: ST.HomeScreenState -> PopUpModal.Config
referralEarnedConfig state =
  let
    config = PopUpModal.config
    requestInfoCardConfig' =
      config
        { primaryText 
          { visibility = GONE
          }
        , secondaryText 
          { text = "Referred customer completed their 1st ride. Refer more to earn more"
          , margin = MarginTop 8
          , textStyle = SubHeading2
          }
        , topTitle 
          { text = "₹100 referral bonus earned!"
          , textStyle = Heading2
          , margin = MarginBottom 8
          , visibility = VISIBLE
          }
        , coverImageConfig
          { imageUrl = fetchImage COMMON_ASSET "ny_ic_referral_earned"
          , height = V $ JB.getHeightFromPercent 30
          , width = V $ (EHC.screenWidth unit) - 64
          , visibility = VISIBLE
          , margin = MarginLeft 0
          }
        , option1 
          { text = getString CHECK_NOW
          , width = MATCH_PARENT
          , margin = MarginTop 24
          , background = Color.black900
          , color = Color.yellow900
          }
        , option2
          { text = getString OKAY
          , width = MATCH_PARENT
          , margin = MarginTop 8
          , background = Color.transparent
          , color = Color.black650
          , strokeColor = Color.transparent
          }
        , backgroundColor = Color.black9000
        , gravity = CENTER
        , padding = Padding 16 20 16 16
        , cornerRadius = Corners 16.0 true true true true
        , margin = MarginHorizontal 16 16
        , buttonLayoutMargin = MarginLeft 0
        , optionButtonOrientation = "VERTICAL"
        , dismissPopup = true
        }
  in
    requestInfoCardConfig'

referNowConfig :: ST.HomeScreenState -> PopUpModal.Config
referNowConfig state =
  let
    config = PopUpModal.config
    requestInfoCardConfig' =
      config
        { primaryText 
          { visibility = GONE
          }
        , secondaryText 
          { text = getString $ EARN_FOR_EACH_REFERRAL $ state.data.config.currency <> show (fromMaybe 0 state.data.payoutRewardAmount)
          , margin = MarginTop 24
          , textStyle = Heading2
          }
        , coverImageConfig
          { imageUrl = fetchImage COMMON_ASSET "ny_ic_refer_now"
          , height = V $ JB.getHeightFromPercent 31
          , width = V $ (EHC.screenWidth unit) - 64
          , visibility = VISIBLE
          , margin = MarginLeft 0
          }
        , option1 
          { text = getString REFER_NOW
          , width = MATCH_PARENT
          , margin = MarginTop 24
          , background = Color.black900
          , color = Color.yellow900
          }
        , option2
          { text = getString CLOSE
          , width = MATCH_PARENT
          , height = V 36
          , margin = MarginTop 8
          , background = Color.transparent
          , color = Color.black650
          , strokeColor = Color.transparent
          }
        , backgroundColor = Color.black9000
        , gravity = CENTER
        , padding = Padding 16 20 16 16
        , cornerRadius = Corners 16.0 true true true true
        , margin = MarginHorizontal 16 16
        , buttonLayoutMargin = MarginLeft 0
        , optionButtonOrientation = "VERTICAL"
        , dismissPopup = true
        }
  in
    requestInfoCardConfig'

addUPIConfig :: ST.HomeScreenState -> PopUpModal.Config
addUPIConfig state =
  let
    config = PopUpModal.config
    requestInfoCardConfig' =
      config
        { primaryText 
          { visibility = GONE
          }
        , secondaryText 
          { text = getString ADD_UPI_TO_RECEIVE_REFERRAL_REWARD
          , margin = MarginTop 24
          , textStyle = Heading2
          }
        , coverImageConfig
          { imageUrl = fetchImage COMMON_ASSET "ny_ic_add_upi_large"
          , height = V $ JB.getHeightFromPercent 31
          , width = V $ (EHC.screenWidth unit) - 64
          , visibility = VISIBLE
          , margin = MarginLeft 0
          }
        , option1 
          { text = getString ADD_NOW
          , width = MATCH_PARENT
          , margin = MarginTop 24
          , background = Color.black900
          , color = Color.yellow900
          }
        , option2
          { text = getString LATER
          , width = MATCH_PARENT
          , margin = MarginTop 8
          , height = V 36
          , background = Color.transparent
          , color = Color.black650
          , strokeColor = Color.transparent
          }
        , backgroundColor = Color.black9000
        , gravity = CENTER
        , padding = Padding 16 20 16 16
        , cornerRadius = Corners 16.0 true true true true
        , margin = MarginHorizontal 16 16
        , buttonLayoutMargin = MarginLeft 0
        , optionButtonOrientation = "VERTICAL"
        , dismissPopup = true
        }
  in
    requestInfoCardConfig'

verifyUPI :: ST.HomeScreenState -> PopUpModal.Config
verifyUPI state =
  let
    config = PopUpModal.config
    requestInfoCardConfig' =
      config
        { primaryText 
          { visibility = GONE
          }
        , secondaryText 
          { text = getString DO_YOU_WANT_TO_RECEIVE_AMOUNT_HERE
          , margin = MarginTop 24
          , textStyle = Heading2
          }
        , upiDetailConfig {
            visibility = VISIBLE,
            upiID = fromMaybe "" state.data.payoutVpa,
            accountName = fromMaybe "" state.data.payoutVpaBankAccount,
            imageConfig {
              visibility = VISIBLE
              , imageUrl = fetchImage COMMON_ASSET "ny_ic_add_upi_cirlce"
              , height = V 36
              , width = V 36
              , margin = MarginRight 16
            }
          }
        , coverImageConfig
          { imageUrl = fetchImage COMMON_ASSET "ny_ic_confirm_upi"
          , height = V $ JB.getHeightFromPercent 31
          , width = V $ (EHC.screenWidth unit) - 64
          , visibility = VISIBLE
          , margin = MarginLeft 0
          }
        , option1 
          { text = getString YES_PAY_TO_THIS_ACCOUNT
          , width = MATCH_PARENT
          , margin = MarginTop 24
          , background = Color.black900
          , color = Color.yellow900
          }
        , option2
          { text = getString I_WILL_ADD_DIFFERENT_ACCOUNT
          , width = MATCH_PARENT
          , margin = MarginTop 8
          , height = V 36
          , background = Color.transparent
          , color = Color.black650
          , strokeColor = Color.transparent
          }
        , backgroundColor = Color.black9000
        , gravity = CENTER
        , padding = Padding 16 20 16 16
        , cornerRadius = Corners 16.0 true true true true
        , margin = MarginHorizontal 16 16
        , buttonLayoutMargin = MarginLeft 0
        , optionButtonOrientation = "VERTICAL"
        , dismissPopup = true
        }
  in
    requestInfoCardConfig'

selectPlansModalState :: ST.HomeScreenState -> SelectPlansModal.SelectPlansState
selectPlansModalState state = SelectPlansModal.config
  {
    selectedPlan = state.data.plansState.selectedPlan,
    plansList = case state.data.plansState.selectedPlan of
                  Just justPlan -> map (\plan -> plan {isSelected = plan.id == justPlan.id }) state.data.plansState.plansList
                  Nothing -> state.data.plansState.plansList
  }


customerDeliveryCallPopUpData :: ST.HomeScreenState -> Array { text :: String, imageWithFallback :: String, type :: ST.DeliverCallType, data :: String }
customerDeliveryCallPopUpData state = 
   [
    { text: (getString CALL_RECEIVER)
    , imageWithFallback: HU.fetchImage HU.COMMON_ASSET "ic_phone_filled_red"
    , type: ST.RECEIVER
    , data: (maybe "" (\(API.PersonDetails det) -> det.name) state.data.activeRide.receiverPersonDetails)
    },
    { text: (getString CALL_SENDER)
    , imageWithFallback: HU.fetchImage HU.COMMON_ASSET "ic_phone_filled_green"
    , type: ST.SENDER
    , data: (maybe "" (\(API.PersonDetails det) -> det.name) state.data.activeRide.senderPersonDetails)
    }
    ]
  
parcelIntroductionPopup :: ST.HomeScreenState -> PopUpModal.Config
parcelIntroductionPopup state = PopUpModal.config {
    gravity = CENTER,
    backgroundClickable = true,
    dismissPopup = true,
    isVisible = true,
    margin = MarginHorizontal 24 24,
    padding = Padding 16 24 16 0,
    optionButtonOrientation = "HORIZONTAL",
    cornerRadius = PTD.Corners 15.0 true true true true,
    primaryText {
      text = StringsV2.getStringV2 LT2.a_new_way_to_earn_parcel,
      textStyle = Heading2,
      color = Color.black800,
      margin = Margin 0 16 0 4
      },
    secondaryText{
      text = StringsV2.getStringV2 LT2.seamless_earning_experience_click_below,
      textStyle = SubHeading2,
      color = Color.black700,
      margin = MarginHorizontal 0 0
      },
    option1 {
      text = StringsV2.getStringV2 LT2.watch_now,
      color = Color.yellow900,
      background = Color.black900,
      textStyle = FontStyle.SubHeading3,
      strokeColor = Color.black900,
      margin = MarginTop 24
    },
    option2 { visibility = false },
    coverImageConfig {
      imageUrl = fetchImage COMMON_ASSET "ny_ic_earn_through_parcel"
    , visibility = VISIBLE
    , width = V (EHC.screenWidth unit - 80)
    , height = V (EHC.screenWidth unit - 180)
    }
  }
  
disableMetroWarriorWarningPopup :: ST.HomeScreenState-> PopUpModal.Config
disableMetroWarriorWarningPopup _ = PopUpModal.config {
    buttonLayoutMargin = Margin 16 0 16 20 ,
    optionButtonOrientation = "VERTICAL",
    gravity = CENTER,
    margin = MarginHorizontal 20 20,
    cornerRadius = PTD.Corners 15.0 true true true true,
    backgroundClickable = true,
    dismissPopup = true,
    primaryText{ 
        text = getString DISABLE_METRO_WARRIORS_INFO,
        margin = Margin 16 16 16 16
    },
    secondaryText{
      text = getString GOTO_IS_APPLICABLE_FOR,
      margin = MarginVertical 16 20,
      color = Color.black600,
      visibility = GONE},
    option1 {
      text = getString ENABLE_GOTO,
      margin = MarginHorizontal 16 16,
      color = Color.yellow900,
      background = Color.black900,
      strokeColor = Color.white900,
      width = MATCH_PARENT
    },
    option2 {
      text = getString CANCEL,
      margin = Margin 16 7 16 0,
      color = Color.black650,
      background = Color.white900,
      strokeColor = Color.white900,
      width = MATCH_PARENT
    }
  }
  
rideEndStopsWarningPopup :: ST.HomeScreenState -> PopUpModal.Config
rideEndStopsWarningPopup state = PopUpModal.config {
    gravity = CENTER,
    backgroundClickable = true,
    optionButtonOrientation = "HORIZONTAL",
    buttonLayoutMargin = MarginBottom 10,
    dismissPopup = true,
    margin = MarginHorizontal 25 25, 
    primaryText {
      text = getString END_RIDE_WITH_STOPS,
      textStyle = Heading2,
      margin = Margin 16 0 16 10},
    secondaryText { visibility = GONE },
    option1 {
      text = getString END_RIDE,
      color = Color.black700,
      background = Color.white900,
      textStyle = FontStyle.SubHeading1,
      strokeColor = Color.black500
    },
    option2 {
      text = getString CANCEL,
      color = Color.yellow900,
      background = Color.black900,
      strokeColor = Color.black900
    },
    cornerRadius = PTD.Corners 15.0 true true true true,
    coverImageConfig {
      imageUrl = fetchImage COMMON_ASSET "ny_ic_more_stops_left"
    , visibility = VISIBLE
    , margin = MarginVertical 20 24
    , width = MATCH_PARENT
    , height = V 190
    }
  }
