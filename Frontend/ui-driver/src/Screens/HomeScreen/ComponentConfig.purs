{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.ComponentConfig where

import Language.Strings (getString)
import Prelude(unit, show, ($), (-), (/), (<), (<=), (<>), (==), (>=), (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Visibility(..),Padding(..))
import Components.SelectListModal as SelectListModal
import Components.Banner as Banner
import Language.Strings
import Common.Types.App (LazyCheck(..))
import Components.ChatView as ChatView
import Components.InAppKeyboardModal as InAppKeyboardModal
import Components.PopUpModal as PopUpModal
import Components.RideActionModal as RideActionModal
import Components.StatsModel as StatsModel
import Components.ChatView as ChatView
import Components.RequestInfoCard as RequestInfoCard
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Helpers.Utils as HU
import Language.Types (STR(..))
import Prelude ((<>), otherwise)
import PrestoDOM.Types.DomAttributes as PTD
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore)
import JBridge as JB
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Engineering.Helpers.Suggestions (getSuggestionsfromKey)
import Font.Style as FontStyle
import Helpers.Utils (getMerchantVehicleSize)



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
    waitTime = state.data.activeRide.waitingTime
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
    , suggestionsList = if (state.data.messagesSize == (show $ (DA.length state.data.messages) - 1) || state.data.messagesSize == "-1") then getDriverSuggestions state else getSuggestionsfromKey "driverDefaultBP" 
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
        text = if (state.props.otpIncorrect) then (getString ENTERED_WRONG_OTP) else (getString OTP_LIMIT_EXCEEDED),
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


----------------------------------------------gotoKnowMoreConfig----------------------------------------------------------

-- gotoKnowMoreConfig :: LazyCheck -> RequestInfoCard.Config
-- gotoKnowMoreConfig _ = let
--   config = RequestInfoCard.config
--   gotoKnowMoreConfig' = config{
--     title {
--       text = "Know More"
--     }
--   , primaryText {
--       text = " This feature will be applicable if you are at least 3 km away from your current location."
--     }
--   , secondaryText {
--       text = "Location preferences are valid for only 30 minutes upon activation. ",
--       visibility = VISIBLE
--     }
--   , buttonConfig {
--       text = "Go Back" 
--     }
--   }
--   in gotoKnowMoreConfig'

gotoKnowMoreConfig :: ST.HomeScreenState-> PopUpModal.Config
gotoKnowMoreConfig state = let
    config = PopUpModal.config
    popUpConfig' = config {
      optionButtonOrientation = "VERTICAL",
      buttonLayoutMargin = Margin 16 0 16 20,
      gravity = CENTER,
      margin = MarginHorizontal 20 20,
      cornerRadius = PTD.Corners 15.0 true true true true,
      primaryText{ text = "Know More"},
      secondaryText{text = "This feature will be applicable if you are at least 3 km away from your current location.\n\nLocation preferences are valid for only 30 minutes upon activation.",
      margin = (Margin 0 16 0 20),
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
    in popUpConfig'

-------------------------------------------------DriverRequestPopuop------------------------------------------

gotoRequestPopupConfig :: ST.HomeScreenState -> PopUpModal.Config
gotoRequestPopupConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
      gravity = CENTER,
      optionButtonOrientation = "VERTICAL",
      buttonLayoutMargin = Margin 16 0 16 20,
      margin = MarginHorizontal 20 20, 
      primaryText {
        text = strings.primaryText
      , margin = Margin 16 20 16 10},
      secondaryText{
        text = strings.secondaryText
      , margin = Margin 0 0 0 20 },
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
  in popUpConfig'
    where (PopupReturn strings) = gotoCounterStrings 4

newtype PopupReturn = PopupReturn {
  primaryText :: String,
  secondaryText :: String,
  imageURL :: String,
  buttonText :: String
} 

gotoCounterStrings :: Int -> PopupReturn
gotoCounterStrings val
  | val == 1 = PopupReturn { primaryText : "More “Go To” rides coming!"
                            , secondaryText : "Please stay ONLINE; we are fetching more ride requests towards your “Go To” location"
                            , imageURL : "ny_ic_goto_more_rides,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_near.png"
                            , buttonText : "Okay"
                            }
  | val == 2 =  PopupReturn { primaryText : "“Go To” reduced to 1"
                            , secondaryText : "Due to multiple cancellations, the count has been reduced to 1. "
                            , imageURL : "ny_ic_gotodriver_1,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_near.png"
                            , buttonText : "Ok, Got it"
                            }
  | val == 3 =  PopupReturn { primaryText : "“Go To” reduced to zero"
                            , secondaryText : "Due to multiple cancellations, the count has been reduced to 0. "
                            , imageURL : "ny_ic_gotodriver_zero,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_near.png"
                            , buttonText : "Ok, Got it"
                            }
  | otherwise =  PopupReturn { primaryText : "Validity Expired!"
                            , secondaryText : "Your 30-minute validity has ended. We are unable to fulfill your request at this time."
                            , imageURL : "ny_ic_validity_expired,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_near.png"
                            , buttonText : "Go Home"
                            }   

--------------------------------------------------------------------------gotoCancellationPrevention---------------------------------------------------------
gotoCancellationPreventionConfig :: ST.HomeScreenState-> PopUpModal.Config
gotoCancellationPreventionConfig state = let
    config = PopUpModal.config
    popUpConfig' = config {
      optionButtonOrientation = "VERTICAL",
      buttonLayoutMargin = Margin 16 0 16 20,
      gravity = BOTTOM,
      cornerRadius = PTD.Corners 15.0 true true false false,
      primaryText{ 
        text = "“Go To” may be reduced",
        margin = Margin 0 0 0 0 
      },
      secondaryText{text = "Cancelation of a “Go To” ride will \n reduce “Go To” options for you!",
      margin = (Margin 0 16 0 20),
      color = Color.black600},
      option1 {
        text = "Cancel Anyway",
        margin = MarginHorizontal 16 16,
        color = Color.yellow900,
        background = Color.black900,
        strokeColor = Color.white900,
        width = MATCH_PARENT
      },
      option2 {
        text = getString GO_BACK,
        margin = MarginHorizontal 16 16,
        color = Color.black650,
        background = Color.white900,
        strokeColor = Color.white900,
        width = MATCH_PARENT
      },
      coverImageConfig {
        imageUrl = "ny_ic_gotodriver_zero,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_near.png"
      , visibility = VISIBLE
      , margin = Margin 16 20 16 24
      , width = MATCH_PARENT
      , height = V 270
      }
    }
    in popUpConfig'
------------------------------------------------------------------------------gotoLocInRange------------------------------------------------------------------------------------
gotoLocInRangeConfig :: ST.HomeScreenState-> PopUpModal.Config
gotoLocInRangeConfig state = let
    config = PopUpModal.config
    popUpConfig' = config {
      optionButtonOrientation = "VERTICAL",
      buttonLayoutMargin = Margin 16 0 16 20,
      gravity = CENTER,
      margin = MarginHorizontal 20 20,
      cornerRadius = PTD.Corners 15.0 true true true true,
      primaryText{ text = "You are very close to the\n “Go-To” location"},
      secondaryText{text = " \"Go-To\" is applicable for locations \n which are at least 3km away from \n your current location.",
      margin = (Margin 0 16 0 20),
      color = Color.black600},
      option1 {
        text = getString GO_BACK,
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
    in popUpConfig'
