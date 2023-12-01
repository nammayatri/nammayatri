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

import Common.Types.App (LazyCheck(..), PolylineAnimationConfig)
import Common.Types.App as CommonTypes
import Components.Banner as Banner
import Components.ChatView as ChatView
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
import Components.StatsModel as StatsModel
import Data.Array as DA
import Data.Int (fromString)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Suggestions (getSuggestionsfromKey)
import Font.Size as FontSize
import Font.Style (Style(..))
import Font.Style (Style(..))
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), isYesterday, getMerchantVehicleSize, onBoardingSubscriptionScreenCheck)
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (unit, ($), (-), (/), (<), (<=), (<>), (==), (>=), (||), (>), (/=), show, map, (&&), not, bottom, (<>), (*), negate, otherwise, (+))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), Accessiblity(..), cornerRadius, padding, gravity)
import PrestoDOM.Types.DomAttributes as PTD
import Resource.Constants as Const
import Screens.Types (AutoPayStatus(..), SubscriptionBannerType(..), SubscriptionPopupType(..), GoToPopUpType(..))
import Screens.Types as ST
import Services.API as SA
import Services.API (PaymentBreakUp(..), PromotionPopupConfig(..), Status(..))
import Storage (KeyStore(..), getValueToLocalNativeStore, getValueToLocalStore)
import Styles.Colors as Color
import Components.ErrorModal as ErrorModal
import MerchantConfig.Utils as MU
import PrestoDOM.Types.DomAttributes (Corners(..))

--------------------------------- rideActionModalConfig -------------------------------------
rideActionModalConfig :: ST.HomeScreenState -> RideActionModal.Config
rideActionModalConfig state = 
  let
  config = RideActionModal.config
  rideActionModalConfig' = config {
    startRideActive = (state.props.currentStage == ST.RideAccepted || state.props.currentStage == ST.ChatWithCustomer),
    totalDistance = if state.data.activeRide.distance <= 0.0 then "0.0" else if(state.data.activeRide.distance < 1000.0) then HU.parseFloat (state.data.activeRide.distance) 2 <> " m" else HU.parseFloat((state.data.activeRide.distance / 1000.0)) 2 <> " km",
    customerName = if DS.length (fromMaybe "" ((DS.split (DS.Pattern " ") (state.data.activeRide.riderName)) DA.!! 0)) < 4
                      then (fromMaybe "" ((DS.split (DS.Pattern " ") (state.data.activeRide.riderName)) DA.!! 0)) <> " " <> (fromMaybe "" ((DS.split (DS.Pattern " ") (state.data.activeRide.riderName)) DA.!! 1))
                      else
                        (fromMaybe "" ((DS.split (DS.Pattern " ") (state.data.activeRide.riderName)) DA.!! 0)),
    sourceAddress  {
      titleText = fromMaybe "" ((DS.split (DS.Pattern ",") (state.data.activeRide.source)) DA.!! 0),
      detailText = state.data.activeRide.source
    },
    destinationAddress {
      titleText = fromMaybe "" ((DS.split (DS.Pattern ",") (state.data.activeRide.destination)) DA.!! 0),
      detailText = state.data.activeRide.destination
    },
    estimatedRideFare = state.data.activeRide.estimatedFare,
    notifiedCustomer = state.data.activeRide.notifiedCustomer,
    currentStage = state.props.currentStage,
    unReadMessages = state.props.unReadMessages,
    specialLocationTag = state.data.activeRide.specialLocationTag,
    requestedVehicleVariant = state.data.activeRide.requestedVehicleVariant,
    accessibilityTag = state.data.activeRide.disabilityTag,
    appConfig = state.data.config,
    waitTimeStatus = state.props.waitTimeStatus,
    waitTimeSeconds = state.data.activeRide.waitTimeSeconds,
    thresholdTime = state.data.config.waitTimeConfig.thresholdTime
    }
    in rideActionModalConfig'

---------------------------------------- endRidePopUp -----------------------------------------
endRidePopUp :: ST.HomeScreenState -> PopUpModal.Config
endRidePopUp state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    primaryText {text = (getString END_RIDE)},
    secondaryText {text = (getString ARE_YOU_SURE_YOU_WANT_TO_END_THE_RIDE)},
    option1 {text =(getString GO_BACK)},
    option2 {text = (getString END_RIDE)}
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

---------------------------------- statsModelConfig --------------------------------
statsModelConfig :: ST.HomeScreenState -> StatsModel.Config
statsModelConfig state =
  let
    config = StatsModel.config
    config' = config
      { countTextConfig { text = getString TRIP_COUNT }
      , earningsTextConfig { text = getString TODAYS_EARNINGS }
      , bonusTextConfig { text = getString BONUS_EARNED }
      , textConfig { text = "" }
      , totalRidesOfDay = state.data.totalRidesOfDay
      , totalEarningsOfDay = state.data.totalEarningsOfDay
      , bonusEarned = state.data.bonusEarned
      }
  in config'

-------------------------------------genderBannerConfig------------------------------------
genderBannerConfig :: ST.HomeScreenState -> Banner.Config
genderBannerConfig state =
  let
    config = Banner.config
    config' = config
      {
        backgroundColor = Color.green600,
        title = (getString COMPLETE_YOUR_PROFILE_AND_FIND_MORE_RIDES),
        titleColor = Color.white900,
        actionText = (getString UPDATE_NOW),
        actionTextColor = Color.white900,
        imageUrl = fetchImage FF_ASSET "ny_ic_driver_gender_banner",
        isBanner = true
      }
  in config'


accessbilityBannerConfig :: ST.HomeScreenState -> Banner.Config
accessbilityBannerConfig state = 
  let 
    config = Banner.config
    config' = config  
      {
        backgroundColor = Color.lightPurple,
        title = getString LEARN_HOW_YOU_CAN_HELP_CUSTOMERS_REQUIRING_SPECIAL_ASSISTANCE,
        titleColor = Color.purple,
        actionText = getString LEARN_MORE,
        actionTextColor = Color.purple,
        imageUrl = fetchImage FF_ASSET "ny_ic_purple_badge",
        isBanner = true,
        stroke = "1,"<>Color.fadedPurple
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
      text = (getString AADHAAR_LINKING_REQUIRED_DESCRIPTION)
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
        3 -> getString FREE_TRIAL_ENDING_IN_2_DAYS
        2 -> getString FREE_TRIAL_ENDING_TOMORROW
        1 -> getString FREE_TRIAL_ENDS_TONIGHT
        _ -> ""
    , margin = Margin 16 16 16 4 },
    secondaryText {
      text = if autoPayStatus == NO_AUTOPAY then getString JOIN_A_PLAN_TO_CONTINUE_TAKING_RIDES else getString SETUP_AUTOPAY_FOR_EASY_PAYMENTS
    , margin = MarginBottom 24
    },
    option1 {
      text = if autoPayStatus == NO_AUTOPAY then getString JOIN_NOW else getString SETUP_AUTOPAY
    , background = Color.black900
    , color = Color.yellow900
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
        _ -> ""
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
      dues = if state.data.paymentState.totalPendingManualDues /= 0.0 then "( ₹" <> HU.getFixedTwoDecimals state.data.paymentState.totalPendingManualDues <> ") " else ""
      isHighDues = state.data.paymentState.totalPendingManualDues >= state.data.config.subscriptionConfig.highDueWarningLimit
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
                         GO_ONLINE_BLOCKER    -> PAYMENT_PENDING_ALERT_DESC
                         _                    -> LOW_DUES_CLEAR_POPUP_DESC
    , margin = Margin 16 16 16 4
    , textStyle = SubHeading2
    , color = Color.black700
    , visibility = VISIBLE },
    secondaryText {
      text = "<span style='color:#2194FF'><u>"<> getString WATCH_VIDEO_FOR_HELP <>"</u></span>"
    , textStyle = SubHeading2
    , margin = MarginBottom 24
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
    },
    option2 {
      visibility = false
    },
    backgroundClickable = true,
    cornerRadius = (PTD.Corners 15.0 true true true true),
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET $ case popupType of
                          GO_ONLINE_BLOCKER  -> "ny_ic_payment_pending"
                          _ ->  if isHighDues 
                                then "ny_ic_payment_pending" 
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
      text = case state.data.activeRide.specialLocationTag of
              Nothing -> getString FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES
              Just specialLocationTag -> getString $ getCancelAlertText $ (HU.getRideLabelData (Just specialLocationTag)).cancelText
    , margin = Margin 16 24 16 0 },
    secondaryText {
      visibility = if state.data.activeRide.specialLocationTag == (Just "GOTO") then VISIBLE else GONE,
      text = getString GO_TO_CANCELLATION_DESC,
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
    },
    option2 {
      text = (getString GO_BACK)
    , margin = MarginLeft 12
    , width = V $ (((EHC.screenWidth unit)-92)/2)
    , color = Color.yellow900
    , strokeColor = Color.black900
    , background = Color.black900
    },
    backgroundClickable = false,
    cornerRadius = (PTD.Corners 15.0 true true true true),
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET  if (state.data.activeRide.specialLocationTag == Nothing || (HU.getRequiredTag state.data.activeRide.specialLocationTag) == Nothing) 
                    then "ic_cancel_prevention"
                  else (HU.getRideLabelData state.data.activeRide.specialLocationTag).cancelConfirmImage
    , visibility = VISIBLE
    , margin = Margin 16 20 16 0
    , height = V 178
    }
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
    , suggestionsList = if showSuggestions state then if (state.data.activeRide.notifiedCustomer) then getSuggestionsfromKey "driverInitialAP" else getSuggestionsfromKey "driverInitialBP" else state.data.suggestionsList
    , hint = (getString MESSAGE)
    , suggestionHeader = (getString START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS)
    , emptyChatHeader = (getString START_YOUR_CHAT_WITH_THE_DRIVER)
    , mapsText = (getString MAPS)
    , languageKey = (getValueToLocalStore LANGUAGE_KEY)
    , grey700 = Color.grey700
    , blue600 = Color.blue600
    , blue900 = Color.blue900
    , transparentGrey = Color.transparentGrey
    , green200 = Color.green200
    , grey900 = Color.grey900
    , grey800 = Color.grey800
    , blue800 = Color.blue800
    , white900 = Color.white900
    , black800 = Color.black800
    , black700 = Color.black700
    , canSendSuggestion = state.props.canSendSuggestion
    , enableCall = (not (state.data.activeRide.disabilityTag == Just ST.HEAR_IMPAIRMENT))
  }
  in chatViewConfig'

showSuggestions :: ST.HomeScreenState -> Boolean
showSuggestions state = do
  let canShowSuggestions = case (DA.last state.data.messages) of 
                            Just value -> not $ value.sentBy == "Driver"
                            Nothing -> true
  DA.null state.data.suggestionsList && canShowSuggestions && ((show $ DA.length $ JB.getChatMessages FunctionCall) == state.data.messagesSize || state.data.messagesSize == "-1")

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
    }
  , option2 {
      width = (V 170)
      , text =  getString GO_SILENT
    }
  }
  in popUpConfig'


enterOtpStateConfig :: ST.HomeScreenState -> InAppKeyboardModal.InAppKeyboardModalState
enterOtpStateConfig state = let
      config' = InAppKeyboardModal.config
      inAppModalConfig' = config'{
      otpIncorrect = if (state.props.otpAttemptsExceeded) then false else (state.props.otpIncorrect),
      otpAttemptsExceeded = (state.props.otpAttemptsExceeded),
      inputTextConfig {
        text = state.props.rideOtp,
        -- pattern = "[0-9]*,4",
        focusIndex = state.props.enterOtpFocusIndex
        , textStyle = FontStyle.Heading1
      },
      headingConfig {
        text = getString (ENTER_OTP)
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
      modalType = ST.OTP
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
  _ -> FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES

mapRouteConfig :: String -> String -> Boolean -> PolylineAnimationConfig -> JB.MapRouteConfig
mapRouteConfig srcIcon destIcon isAnim animConfig= {
    sourceSpecialTagIcon : srcIcon
  , destSpecialTagIcon : destIcon
  , vehicleSizeTagIcon : (getMerchantVehicleSize unit)
  , isAnimation : isAnim 
  , polylineAnimationConfig : animConfig
}

requestInfoCardConfig :: LazyCheck -> RequestInfoCard.Config
requestInfoCardConfig _ = let
  config = RequestInfoCard.config
  requestInfoCardConfig' = config{
    title {
      text = getString WHAT_IS_NAMMA_YATRI_BONUS
    }
  , primaryText {
      text = getString BONUS_PRIMARY_TEXT
    }
  , secondaryText {
      text = getString BONUS_SECONDARY_TEXT,
      visibility = VISIBLE
    }
  , imageConfig {
      imageUrl = fetchImage FF_ASSET "ny_ic_bonus",
      height = V 122,
      width = V 116
    }
  , buttonConfig {
      text = getString GOT_IT
    }
  }
  in requestInfoCardConfig'

waitTimeInfoCardConfig :: ST.HomeScreenState -> RequestInfoCard.Config
waitTimeInfoCardConfig state = RequestInfoCard.config {
    title {
      text = getString WAIT_TIMER
    }
  , primaryText {
      text = if rideInProgress then 
              getVarString THIS_EXTRA_AMOUNT_THE_CUSTOMER_WILL_PAY [show maxWaitTimeInMinutes] else getString HOW_LONG_WAITED_FOR_PICKUP,
      padding = Padding 16 16 0 0
    }
  , secondaryText {
      text = getVarString CUSTOMER_WILL_PAY_FOR_EVERY_MINUTE [show maxWaitTimeInMinutes],
      visibility = if rideInProgress then GONE else VISIBLE,
      padding = PaddingLeft 16
    }
  , imageConfig {
      imageUrl = fetchImage FF_ASSET "ny_ic_waiting_auto",
      height = V 130,
      width = V 130,
      padding = Padding 0 4 1 0
    }
  , buttonConfig {
      text = getString GOT_IT,
      padding = PaddingVertical 16 20
    }
  }
  where rideInProgress = state.data.activeRide.status == SA.INPROGRESS
        maxWaitTimeInMinutes = Int.floor $ Int.toNumber state.data.config.waitTimeConfig.thresholdTime / 60.0


makePaymentState :: ST.HomeScreenState -> MakePaymentModal.MakePaymentModalState
makePaymentState state = 
  let payableAndGST = EHC.formatCurrencyWithCommas (show state.data.paymentState.payableAndGST)
      useTimeRange = (state.data.config.subscriptionConfig.showLaterButtonforTimeRange && not JB.withinTimeRange "14:00:00" "10:00:00" (EHC.convertUTCtoISC(EHC.getCurrentUTC "") "HH:mm:ss"))
  in 
  {
    title : getString GREAT_JOB,
    description : getDescription state,
    description2 : getString COMPLETE_PAYMENT_TO_CONTINUE,
    okButtontext : ( case getValueToLocalStore LANGUAGE_KEY of
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
getDescription state =  case getValueToLocalStore LANGUAGE_KEY of
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
        , description = getString YATRI_SATHI_FEE_PAYABLE_FOR_DATE <> " " <> state.data.paymentState.date
        , buttonText = Nothing
        , currentRateCardType = CommonTypes.PaymentFareBreakup
        , primaryButtonText = getString GOT_IT
        , additionalStrings = [
          {key : "FEE_CORRESPONDING_TO_DISTANCE", val : getString FEE_CORRESPONDING_TO_THE_DISTANCE},
          {key : "GOT_IT", val : getString GOT_IT},
          {key : "TOTAL_PAYABLE", val : getString TOTAL_PAYABLE},
          {key : "TOTAL_PAYABLE_VAL", val : "₹" <> (show state.data.paymentState.payableAndGST)}]
          
        , fareList = getChargesBreakup state.data.paymentState.chargesBreakup

        }
  in
    rateCardConfig'

paymentStatusConfig :: ST.HomeScreenState -> Banner.Config
paymentStatusConfig state = 
  let 
    config = Banner.config
    config' = config
      { 
        backgroundColor = state.data.paymentState.bannerBG,
        title = state.data.paymentState.bannerTitle,
        titleColor = state.data.paymentState.bannerTitleColor,
        actionText = state.data.paymentState.banneActionText,
        actionTextColor = state.data.paymentState.actionTextColor,
        imageUrl = state.data.paymentState.bannerImage,
        isBanner = true
      }
  in config'


getChargesBreakup :: Array PaymentBreakUp -> Array CommonTypes.FareList
getChargesBreakup paymentBreakUpArr = map (\(PaymentBreakUp item) -> {val : "₹" <>  (show item.amount),
  key : case item.component of
        "Government Charges" -> getString GOVERMENT_CHARGES
        "Platform Fee" -> getString PLATFORM_FEE
        _ -> item.component
    } ) paymentBreakUpArr

autopayBannerConfig :: ST.HomeScreenState -> Boolean -> Banner.Config
autopayBannerConfig state configureImage =
  let
    config = Banner.config
    bannerType = state.props.autoPayBanner
    dues = HU.getFixedTwoDecimals state.data.paymentState.totalPendingManualDues
    config' = config
      {
        backgroundColor = case bannerType of
                        CLEAR_DUES_BANNER -> Color.yellow900
                        DUE_LIMIT_WARNING_BANNER -> Color.pearl
                        LOW_DUES_BANNER -> Color.yellow800
                        _ -> Color.green600,
        title = case bannerType of
                  FREE_TRIAL_BANNER -> getString SETUP_AUTOPAY_BEFORE_THE_TRAIL_PERIOD_EXPIRES 
                  SETUP_AUTOPAY_BANNER -> getString SETUP_AUTOPAY_NOW_TO_GET_SPECIAL_DISCOUNTS
                  _ | bannerType == CLEAR_DUES_BANNER || bannerType == LOW_DUES_BANNER -> getVarString CLEAR_DUES_BANNER_TITLE [dues]
                  DUE_LIMIT_WARNING_BANNER -> getVarString DUE_LIMIT_WARNING_BANNER_TITLE [HU.getFixedTwoDecimals state.data.config.subscriptionConfig.maxDuesLimit]
                  _ -> "",
        titleColor = case bannerType of
                        DUE_LIMIT_WARNING_BANNER -> Color.red
                        _ | bannerType == CLEAR_DUES_BANNER || bannerType == LOW_DUES_BANNER -> Color.black900
                        _ -> Color.white900,
        actionText = case bannerType of
                        _ | bannerType == DUE_LIMIT_WARNING_BANNER || bannerType == CLEAR_DUES_BANNER || bannerType == LOW_DUES_BANNER -> getString PAY_NOW
                        _ -> (getString SETUP_NOW),
        actionTextColor = case bannerType of
                            _ | bannerType == CLEAR_DUES_BANNER || bannerType == LOW_DUES_BANNER -> Color.black900
                            DUE_LIMIT_WARNING_BANNER -> Color.red
                            _ -> Color.white900,
        imageUrl = fetchImage FF_ASSET $ case bannerType of
                      FREE_TRIAL_BANNER -> "ic_free_trial_period" 
                      SETUP_AUTOPAY_BANNER -> "ny_ic_autopay_setup_banner"
                      _ | bannerType == CLEAR_DUES_BANNER || bannerType == LOW_DUES_BANNER -> "ny_ic_clear_dues_banner"
                      DUE_LIMIT_WARNING_BANNER -> "ny_ic_due_limit_warning"
                      _ -> "",
        isBanner = bannerType /= NO_SUBSCRIPTION_BANNER && not state.props.rideActionModal,
        imageHeight = if configureImage then (V 75) else (V 105),
        imageWidth = if configureImage then (V 98) else (V 118),
        actionTextStyle = if configureImage then FontStyle.Body3 else FontStyle.ParagraphText,
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
        primaryText {
          text = popupData.primaryText
        , margin = Margin 16 24 16 4 },
        secondaryText {
          text = popupData.secondaryText
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
          imageUrl = popupData.imageUrl
        , visibility = if popupData.videoUrl /= "" && state.data.config.purpleRideConfig.showPurpleVideos then GONE else VISIBLE
        , height = V 160
        , width = MATCH_PARENT
        , margin = Margin 16 16 16 0
        },
        coverVideoConfig {
          visibility = if popupData.videoUrl /= "" && state.data.config.purpleRideConfig.showPurpleVideos then VISIBLE else GONE 
        , height = V 175
        , width = MATCH_PARENT
        , padding = Padding 16 16 16 0
        , mediaType = popupData.mediaType
        , mediaUrl = popupData.videoUrl
        , id = popupData.videoId
        }
      }
  in config'

type ContentConfig = 
  { primaryText :: String,
    secondaryText :: String,
    imageUrl :: String,
    videoUrl :: String, 
    mediaType :: String,
    videoId :: String
    
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
        coverVideoConfig {
          visibility = VISIBLE
        , height = V 202
        , width = MATCH_PARENT
        , padding = Padding 16 16 16 0
        , mediaType = "PortraitVideoLink"
        , mediaUrl = state.data.config.purpleRideConfig.genericAccessibilityVideo
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
accessibilityConfig dummy = { primaryText : getString CUSTOMER_MAY_NEED_ASSISTANCE, secondaryText : getString CUSTOMER_HAS_DISABILITY_PLEASE_ASSIST_THEM, videoUrl : "", imageUrl : fetchImage FF_ASSET "ny_ic_disability_illustration", mediaType : "", videoId : ""}

getAccessibilityPopupData :: ST.HomeScreenState -> Maybe ST.DisabilityType -> Boolean -> ContentConfig
getAccessibilityPopupData state pwdtype isDriverArrived = 
  let accessibilityConfig' = accessibilityConfig Config
      city = (HU.getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION))
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
                                                videoUrl = state.data.config.purpleRideConfig.visualImpairmentVideo,
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
                                                videoUrl = state.data.config.purpleRideConfig.hearingImpairmentVideo,
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
                                                videoUrl = state.data.config.purpleRideConfig.physicalImpairmentVideo, 
                                                mediaType = "VideoLink",
                                                videoId = "LocomotorDisabilityCoverVideo"
                                              }
      Just ST.OTHER_DISABILITY, true ->      accessibilityConfig'   
      Just ST.OTHER_DISABILITY, false ->     accessibilityConfig' 
      _ , _-> accessibilityConfig' 

  where 
    getUpdatedAssets :: CommonTypes.CityConfig -> String 
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

getAccessibilityHeaderText :: ST.HomeScreenState -> ContentConfig
getAccessibilityHeaderText state = if state.data.activeRide.status == NEW then 
                        case state.data.activeRide.disabilityTag, state.data.activeRide.notifiedCustomer of   
                          Just ST.HEAR_IMPAIRMENT, false -> {primaryText : getString CUSTOMER_HAS_HEARING_IMPAIRMENT, secondaryText : getString PLEASE_CHAT_AND_AVOID_CALLS, imageUrl : fetchImage FF_ASSET "ny_ic_poor_hearing", videoUrl : "" , mediaType : "", videoId : ""}
                          Just ST.BLIND_AND_LOW_VISION, false -> {primaryText : getString CUSTOMER_HAS_LOW_VISION, secondaryText : getString PLEASE_CALL_AND_AVOID_CHATS, imageUrl : fetchImage FF_ASSET "ic_accessibility_vision", videoUrl : "", mediaType : "", videoId : ""}
                          Just ST.LOCOMOTOR_DISABILITY, false -> {primaryText : getString CUSTOMER_HAS_LOW_MOBILITY, secondaryText : getString PLEASE_GO_TO_EXACT_PICKUP,  imageUrl : fetchImage FF_ASSET "ny_ic_disability_purple", videoUrl : "", mediaType : "", videoId : ""}
                          Just ST.OTHER_DISABILITY, false -> {primaryText : getString CUSTOMER_HAS_DISABILITY, secondaryText : getString PLEASE_ASSIST_THEM_IF_NEEDED, imageUrl : fetchImage FF_ASSET "ny_ic_disability_purple", videoUrl : "", mediaType : "", videoId : ""}
                          Nothing, false -> {primaryText : getString CUSTOMER_HAS_DISABILITY, secondaryText : getString PLEASE_ASSIST_THEM_IF_NEEDED, imageUrl : fetchImage FF_ASSET "ny_ic_disability_purple", videoUrl : "", mediaType : "", videoId : ""}
                      -- else if state.data.activeRide.isDriverArrived then
                          -- case state.data.activeRide.disabilityTag of   
                          Just ST.HEAR_IMPAIRMENT, true -> {primaryText : getString CUSTOMER_HAS_HEARING_IMPAIRMENT, secondaryText : getString MESSAGE_THEM_AT_PICKUP, imageUrl : fetchImage FF_ASSET "ny_ic_poor_hearing", videoUrl : "", mediaType : "", videoId : ""}
                          Just ST.BLIND_AND_LOW_VISION, true -> {primaryText : getString CUSTOMER_HAS_LOW_VISION, secondaryText : getString SOUND_HORN_ONCE_AT_PICKUP, imageUrl : fetchImage FF_ASSET "ic_accessibility_vision", videoUrl : "", mediaType : "", videoId : ""}
                          Just ST.LOCOMOTOR_DISABILITY, true -> {primaryText : getString CUSTOMER_HAS_LOW_MOBILITY, secondaryText : getString HELP_WITH_THEIR_MOBILITY_AID, imageUrl : fetchImage FF_ASSET "ny_ic_disability_purple", videoUrl : "", mediaType : "", videoId : ""}
                          Just ST.OTHER_DISABILITY, true -> {primaryText : getString CUSTOMER_HAS_DISABILITY, secondaryText : getString PLEASE_ASSIST_THEM_IF_NEEDED, imageUrl : fetchImage FF_ASSET "ny_ic_disability_purple", videoUrl : "", mediaType : "", videoId : ""}
                          Nothing, true -> {primaryText : getString CUSTOMER_HAS_DISABILITY, secondaryText : getString PLEASE_ASSIST_THEM_IF_NEEDED, imageUrl : fetchImage FF_ASSET "ny_ic_disability_purple", videoUrl : "", mediaType : "", videoId : ""}
                      else 
                        case state.data.activeRide.disabilityTag of   
                          Just ST.HEAR_IMPAIRMENT -> {primaryText : getString CUSTOMER_HAS_HEARING_IMPAIRMENT, secondaryText : getString PLEASE_HELP_THEM_AS_YOU_CAN, imageUrl : fetchImage FF_ASSET "ny_ic_poor_hearing", videoUrl : "" , mediaType : "", videoId : ""}
                          Just ST.BLIND_AND_LOW_VISION -> {primaryText : getString CUSTOMER_HAS_LOW_VISION, secondaryText : getString PLEASE_HELP_THEM_AS_YOU_CAN, imageUrl : fetchImage FF_ASSET "ic_accessibility_vision", videoUrl : "", mediaType : "", videoId : ""}
                          Just ST.LOCOMOTOR_DISABILITY -> {primaryText : getString CUSTOMER_HAS_LOW_MOBILITY, secondaryText : getString PLEASE_HELP_THEM_AS_YOU_CAN, imageUrl : fetchImage FF_ASSET "ny_ic_disability_purple", videoUrl : "", mediaType : "", videoId : ""}
                          Just ST.OTHER_DISABILITY -> {primaryText : getString CUSTOMER_HAS_DISABILITY, secondaryText : getString PLEASE_HELP_THEM_AS_YOU_CAN, imageUrl : fetchImage FF_ASSET "ny_ic_disability_purple", videoUrl : "", mediaType : "", videoId : ""}
                          Nothing -> {primaryText : getString CUSTOMER_HAS_DISABILITY, secondaryText : getString PLEASE_HELP_THEM_AS_YOU_CAN, imageUrl : fetchImage FF_ASSET "ny_ic_disability_purple", videoUrl : "", mediaType : "", videoId : ""}

getRideCompletedConfig :: ST.HomeScreenState -> RideCompletedCard.Config 
getRideCompletedConfig state = let 
  config = RideCompletedCard.config
  autoPayBanner = state.props.autoPayBanner
  autoPayStatus = state.data.paymentState.autoPayStatus
  payerVpa = state.data.endRideData.payerVpa
  disability = state.data.endRideData.disability /= Nothing
  showDriverBottomCard = state.data.config.rideCompletedCardConfig.showSavedCommission || isJust state.data.endRideData.tip
  viewOrderConfig = [ {condition : autoPayBanner == DUE_LIMIT_WARNING_BANNER, elementView :  RideCompletedCard.BANNER },
                      {condition : autoPayStatus == ACTIVE_AUTOPAY && payerVpa /= "", elementView :  RideCompletedCard.QR_VIEW },
                      {condition : not (autoPayStatus == ACTIVE_AUTOPAY) && state.data.config.subscriptionConfig.enableSubscriptionPopups && (getValueToLocalNativeStore SHOW_SUBSCRIPTIONS == "true"), elementView :  RideCompletedCard.NO_VPA_VIEW },
                      {condition : autoPayBanner /= DUE_LIMIT_WARNING_BANNER, elementView :  RideCompletedCard.BANNER },
                      {condition : disability, elementView :  RideCompletedCard.BADGE_CARD },
                      {condition : showDriverBottomCard, elementView :  RideCompletedCard.DRIVER_BOTTOM_VIEW}
                    ]
  pspIcon = (Const.getPspIcon payerVpa)
  config' = config{
    primaryButtonConfig {
      width = MATCH_PARENT,
      margin = MarginTop 0,
      textConfig {
        text = getString FARE_COLLECTED
      }
    },
    topCard {
      title = getString COLLECT_VIA_UPI_QR_OR_CASH,
      finalAmount = state.data.endRideData.finalAmount,
      initalAmount = state.data.endRideData.finalAmount,
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
        visible = GONE
      },
      topPill{
        visible = disability,
        text = getString PURPLE_RIDE,
        textColor = Color.white900,
        background = Color.blueMagenta
    },
      bottomText = getString RIDE_DETAILS
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
        text = getString YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT,
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
    showContactSupportPopUp = state.props.showContactSupportPopUp,
    driverUpiQrCard {
      text = getString GET_DIRECTLY_TO_YOUR_BANK_ACCOUNT,
      id = "renderQRViewOnRideComplete",
      vpa = payerVpa,
      vpaIcon = fetchImage FF_ASSET pspIcon,
      collectCashText = getString OR_COLLECT_CASH_DIRECTLY
    },
    noVpaCard {
      title = getString SETUP_AUTOPAY_TO_ACCEPT_PAYMENT,
      collectCashText = getString COLLECT_CASH_DIRECTLY
    },
    accessibility = DISABLE,
    theme = LIGHT,
    isPrimaryButtonSticky = true,
    bannerConfig = autopayBannerConfig state false,
    viewsByOrder = map (_.elementView) (DA.filter (_.condition) viewOrderConfig)
  }
  in config'

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
    title = getString RATE_YOUR_RIDE_WITH1 <> " " <> state.data.endRideData.riderName <> " " <>  getString RATE_YOUR_RIDE_WITH2,
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
gotoCounterStrings popupType = case popupType of 
  MORE_GOTO_RIDES -> PopupReturn { primaryText : getString MORE_GOTO_RIDE_COMING
                            , secondaryText : getString MORE_GOTO_RIDE_COMING_DESC
                            , imageURL : "ny_ic_goto_more_rides,"
                            , buttonText : getString OKAY
                            }
  REDUCED 0 -> PopupReturn { primaryText : getString GOTO_REDUCED_TO_ZERO
                            , secondaryText : getString DUE_TO_MULTIPLE_CANCELLATIONS <> " 0."
                            , imageURL : "ny_ic_gotodriver_zero,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_near.png"
                            , buttonText : getString OK_GOT_IT
                            }
  REDUCED n -> PopupReturn { primaryText : getString GOTO_REDUCED_TO <> " " <> show n
                            , secondaryText : getString DUE_TO_MULTIPLE_CANCELLATIONS <> " " <> show  n <> "."
                            , imageURL : "ny_ic_gotodriver_one"
                            , buttonText : getString OK_GOT_IT
                            }
  VALIDITY_EXPIRED -> PopupReturn { primaryText : getString VALIDITY_EXPIRED_STR
                            , secondaryText : getString VALIDITY_EXPIRED_DESC
                            , imageURL : "ny_ic_validity_expired,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_near.png"
                            , buttonText : getString OK_GOT_IT
                            }
  REACHED_HOME -> PopupReturn { primaryText : getString GOTO_LOC_REACHED
                            , secondaryText : getString YOU_ARE_ALMOST_AT_LOCATION
                            , imageURL : "ny_ic_goto_arrived,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_near.png"
                            , buttonText : getString OK_GOT_IT
                            }
  NO_POPUP_VIEW -> PopupReturn { primaryText : "" , secondaryText : "" , imageURL : "" , buttonText : "" }

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
        , corners = (Corners 24.0 true true false false)
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
