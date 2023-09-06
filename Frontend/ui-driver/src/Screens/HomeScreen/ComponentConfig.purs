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

import Common.Types.App (LazyCheck(..))
import Common.Types.App as CommonTypes
import Components.Banner as Banner
import Components.ChatView as ChatView
import Components.InAppKeyboardModal as InAppKeyboardModal
import Components.MakePaymentModal as MakePaymentModal
import Components.PopUpModal as PopUpModal
import Components.RateCard as RateCard
import Components.RequestInfoCard as RequestInfoCard
import Components.RideActionModal as RideActionModal
import Components.SelectListModal as SelectListModal
import Components.StatsModel as StatsModel
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Debug (spy)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Suggestions (getSuggestionsfromKey)
import Font.Size as FontSize
import Font.Style as FontStyle
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink, isYesterday)
import Helpers.Utils (getMerchantVehicleSize)
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((<>))
import Prelude (unit, ($), (-), (/), (<), (<=), (<>), (==), (>=), (||), show, map, (&&), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..))
import PrestoDOM.Types.DomAttributes as PTD
import Screens.Types as ST
import Services.API (PaymentBreakUp(..), PromotionPopupConfig(..))
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Styles.Colors as Color

--------------------------------- rideActionModalConfig -------------------------------------
rideActionModalConfig :: ST.HomeScreenState -> RideActionModal.Config
rideActionModalConfig state = let
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
    isDriverArrived = state.data.activeRide.isDriverArrived,
    notifiedCustomer = state.data.activeRide.notifiedCustomer,
    currentStage = state.props.currentStage,
    unReadMessages = state.props.unReadMessages,
    specialLocationTag = state.data.activeRide.specialLocationTag,
    waitTime = state.data.activeRide.waitingTime,
    isChatOpened = state.props.isChatOpened,
    requestedVehicleVariant = state.data.activeRide.requestedVehicleVariant
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
        imageUrl = "ny_ic_driver_gender_banner,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_driver_gender_banner.png",
        isBanner = true
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
      imageUrl = "ny_ic_aadhaar_logo,https://assets.juspay.in/nammayatri/images/driver/ny_ic_aadhaar_logo.png"
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
    topTitle = Just ob.heading,
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
    , margin = MarginVertical 0 20
    , visibility = true
    , background = Color.white900
    , strokeColor = Color.white900
  }
}

offerConfigParams :: ST.HomeScreenState -> PromotionPopupConfig
offerConfigParams state = PromotionPopupConfig $ {
  title : getString LIMITED_TIME_OFFER,
  description : getString JOIN_THE_UNLIMITED_PLAN,
  imageUrl : "ny_ic_limited_time_offer,",
  buttonText : getString JOIN_NOW,
  heading : getString NAMMA_YATRI_PLANS
}

------------------------------------ cancelConfirmationConfig -----------------------------
cancelConfirmationConfig :: ST.HomeScreenState -> PopUpModal.Config
cancelConfirmationConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    gravity = CENTER,
    margin = MarginHorizontal 24 24 ,
    buttonLayoutMargin = Margin 16 0 16 20 ,
    primaryText {
      text = case state.data.activeRide.specialLocationTag of
              Nothing -> getString FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES
              Just specialLocationTag -> getString $ getCancelAlertText $ HU.getSpecialZoneConfig  "cancelText" (Just specialLocationTag)
    , margin = Margin 16 24 16 24 },
    secondaryText {visibility = GONE},
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
      imageUrl = if state.data.activeRide.specialLocationTag == Nothing || HU.getRequiredTag "" state.data.activeRide.specialLocationTag == Nothing then "ic_cancel_prevention," <> (getAssetStoreLink FunctionCall) <> "ny_ic_cancel_prevention.png"
                  else HU.getSpecialZoneConfig "cancelConfirmImage" (state.data.activeRide.specialLocationTag)
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
    margin = (Margin 24 164 24 164), 
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
      imageUrl = "ny_rc_deactivated," <> (getAssetStoreLink FunctionCall) <> "ny_rc_deactivated.png", 
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
    , suggestionsList = if (state.data.messagesSize == (show $ (DA.length state.data.messages) - 1) || state.data.messagesSize == "-1") then getDriverSuggestions state else []
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
  }
  in chatViewConfig'

getDriverSuggestions :: ST.HomeScreenState -> Array String
getDriverSuggestions state = case (DA.length state.data.suggestionsList == 0), (DA.length state.data.messages == 0 ) of
                                  true, true -> if (state.data.activeRide.isDriverArrived || state.data.activeRide.notifiedCustomer) then getSuggestionsfromKey "driverInitialAP" else getSuggestionsfromKey "driverInitialBP"
                                  true, false -> if (showSuggestions state) then (if (state.data.activeRide.isDriverArrived || state.data.activeRide.notifiedCustomer) then getSuggestionsfromKey "driverDefaultAP" else getSuggestionsfromKey "driverDefaultBP") else []
                                  false, false -> state.data.suggestionsList
                                  false, true -> getSuggestionsfromKey "driverDefaultAP" 

showSuggestions :: ST.HomeScreenState -> Boolean
showSuggestions state = do
  case (DA.last state.data.messages) of 
    Just value -> if value.sentBy == "Driver" then false else true
    Nothing -> true

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
      imageUrl : "ic_driver_status_offline,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_driver_status_offline.png",
      textColor : Color.white900
    },
    {
        status : ST.Silent,
        background : Color.blue800,
        imageUrl : "ic_driver_status_silent,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_driver_status_silent.png",
        textColor : Color.white900
    },
    {
      status : ST.Online,
        background : Color.darkMint,
        imageUrl : "ic_driver_status_online,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_driver_status_online.png",
        textColor : Color.white900
    }
]
getCancelAlertText :: String -> STR
getCancelAlertText key = case key of
  "ZONE_CANCEL_TEXT_PICKUP" -> ZONE_CANCEL_TEXT_PICKUP
  "ZONE_CANCEL_TEXT_DROP" -> ZONE_CANCEL_TEXT_DROP
  _ -> FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES

mapRouteConfig :: String -> String -> JB.MapRouteConfig
mapRouteConfig srcIcon destIcon = {
    sourceSpecialTagIcon : srcIcon
  , destSpecialTagIcon : destIcon
  , vehicleSizeTagIcon : (getMerchantVehicleSize unit)
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
      imageUrl = "ny_ic_bonus,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_bonus.png",
      height = V 122,
      width = V 116
    }
  , buttonConfig {
      text = getString GOT_IT
    }
  }
  in requestInfoCardConfig'

waitTimeInfoCardConfig :: LazyCheck -> RequestInfoCard.Config
waitTimeInfoCardConfig _ = let
  config = RequestInfoCard.config
  requestInfoCardConfig' = config{
    title {
      text = getString WAIT_TIMER
    }
  , primaryText {
      text = getString HOW_LONG_WAITED_FOR_PICKUP,
      padding = Padding 16 16 0 0
    }
  , secondaryText {
      text = getString CUSTOMER_WILL_PAY_FOR_EVERY_MINUTE,
      visibility = VISIBLE,
      padding = PaddingLeft 16
    }
  , imageConfig {
      imageUrl = "ny_ic_waiting_auto,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_ride_completed",
      height = V 130,
      width = V 130,
      padding = Padding 0 4 1 0
    }
  , buttonConfig {
      text = getString GOT_IT,
      padding = PaddingVertical 16 20
    }
  }
  in requestInfoCardConfig'


makePaymentState :: ST.HomeScreenState -> MakePaymentModal.MakePaymentModalState
makePaymentState state = 
  let payableAndGST = EHC.formatCurrencyWithCommas (show state.data.paymentState.payableAndGST) in {
    title : getString GREAT_JOB,
    description : getDescription state,
    description2 : ( case getValueToLocalStore LANGUAGE_KEY of
                          "EN_US" -> "To continue using Yatri Sathi, please complete your payment for " <> state.data.paymentState.date
                          "HI_IN" -> "यात्री साथी का उपयोग जारी रखने के लिए, कृपया "<> state.data.paymentState.date <>" के लिए अपना भुगतान पूरा करें"
                          "KN_IN" -> "ಯಾತ್ರಿ ಸತಿ ಬಳಸುವುದನ್ನು ಮುಂದುವರಿಸಲು, ದಯವಿಟ್ಟು "<> state.data.paymentState.date <> " ಕ್ಕೆ ನಿಮ್ಮ ಪಾವತಿಯನ್ನು ಪೂರ್ಣಗೊಳಿಸಿ"
                          "TA_IN" -> "யாத்ரி சாத்தியைத் தொடர்ந்து பயன்படுத்த, "<> state.data.paymentState.date <> " க்கு உங்கள் கட்டணத்தைச் செலுத்தவும்"
                          "BN_IN" -> "Yatri Sathi ব্যবহার চালিয়ে যেতে, অনুগ্রহ করে " <> state.data.paymentState.date <> " -এর জন্য আপনার অর্থপ্রদান সম্পূর্ণ করুন"
                          _       -> "To continue using Yatri Sathi, please complete your payment for " <> state.data.paymentState.date
                      ),
    okButtontext : ( case getValueToLocalStore LANGUAGE_KEY of
                          "EN_US" -> "Pay ₹" <> payableAndGST <> " now"
                          "HI_IN" -> "अभी ₹" <> payableAndGST <>" का भुगतान करें"
                          "KN_IN" -> "ಈಗ ₹"<> payableAndGST <>" ಪಾವತಿಸಿ"
                          "TA_IN" -> "இப்போது ₹" <> payableAndGST <> " செலுத்துங்கள்"
                          "BN_IN" -> "এখন " <> payableAndGST <> " পে করুন"
                          _       -> "Pay ₹" <> payableAndGST <> " now"
                      ),
    cancelButtonText : if (JB.withinTimeRange "14:00:00" "10:00:00" (EHC.convertUTCtoISC(EHC.getCurrentUTC "") "HH:mm:ss")
                            && not state.data.paymentState.laterButtonVisibility) then Nothing else Just $ getString LATER,
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
                        "EN_US" -> case (isYesterday state.data.paymentState.dateObj) of
                                  true -> (("You have completed <b>"<> (show state.data.paymentState.rideCount)) <> (if state.data.paymentState.rideCount == 1 then " Ride</b> yesterday!" else " Rides</b> yesterday!"))
                                  false -> ("You have completed <b>"<> (show state.data.paymentState.rideCount) <> ((if state.data.paymentState.rideCount == 1 then " Ride</b> on " else " Rides</b> on ") <> state.data.paymentState.date))
                        "HI_IN" -> if (isYesterday state.data.paymentState.dateObj) then "आपने कल <b>"<> (show state.data.paymentState.rideCount) <> " सवारी</b> पूरी कर लीं!" else 
                                    "आपने " <> state.data.paymentState.date <>  " को "<> (show state.data.paymentState.rideCount) <> " सवारी</b> पूरी कर लीं!"
                        "BN_IN" -> if (isYesterday state.data.paymentState.dateObj) then "আপনি গতকাল "<> (show state.data.paymentState.rideCount) <>"টি রাইড সম্পূর্ণ করেছেন" else 
                                    "আপনি " <> state.data.paymentState.date <>" তারিখে " <> (show state.data.paymentState.rideCount) <> "টি রাইড সম্পূর্ণ করেছেন"
                        "TA_IN" -> "நீங்கள் நேற்று "<> (show state.data.paymentState.rideCount) <>" சவாரிகளை முடித்துவிட்டீர்கள்!"
                        "KN_IN" -> "ನೀವು ನಿನ್ನೆ "<> (show state.data.paymentState.rideCount) <>" ರೈಡ್‌ಗಳನ್ನು ಪೂರ್ಣಗೊಳಿಸಿದ್ದೀರಿ!"
                        _       -> case (isYesterday state.data.paymentState.dateObj) of
                                    true -> (("You have completed <b>"<> (show state.data.paymentState.rideCount)) <> (if state.data.paymentState.rideCount == 1 then " Ride</b> yesterday!" else " Rides yesterday!"))
                                    false -> ("You have completed <b>"<> (show state.data.paymentState.rideCount) <> ((if state.data.paymentState.rideCount == 1 then " Ride</b> on" else " Rides on") <> state.data.paymentState.date))

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
    config' = config
      {
        backgroundColor = "#269574",
        title = (getString SETUP_AUTOPAY_NOW_TO_GET_SPECIAL_DISCOUNTS),
        titleColor = Color.white900,
        actionText = (getString SETUP_NOW),
        actionTextColor = Color.white900,
        imageUrl = "ny_ic_autopay_setup_banner,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_autopay_setup_banner.png",
        isBanner = state.props.autoPayBanner,
        imageHeight = if configureImage then (V 75) else (V 95),
        imageWidth = if configureImage then (V 98) else (V 118),
        actionTextStyle = if configureImage then FontStyle.Body3 else FontStyle.ParagraphText,
        titleStyle = if configureImage then FontStyle.Body4 else FontStyle.Body7
      }
  in config'