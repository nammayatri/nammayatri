{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverProfileScreen.ComponentConfig where

import Components.PopUpModal as PopUpModal
import Components.GenericHeader as GenericHeader
import Components.PrimaryEditText as PrimaryEditText
import Components.CheckListView as CheckListView
import Screens.DriverProfileScreen.Controller
import Language.Strings
import PrestoDOM
import Common.Types.App (LazyCheck)
import Components.PopUpModal as PopUpModal
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Engineering.Helpers.Utils as EHU
import Prelude (unit, (/=), (<>), (==))
import Screens.Types as ST
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Data.Maybe(fromMaybe, Maybe(..), isJust)
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Prelude ((<>), (||),(&&),(==),(<), not, (>))
import Engineering.Helpers.Commons as EHC
import Data.String as DS
import Data.Array as DA
import Components.InAppKeyboardModal.View as InAppKeyboardModal
import Components.InAppKeyboardModal.Controller as InAppKeyboardModalController
import PrestoDOM.Types.DomAttributes  (Corners(..))
import Prelude
import Screens.DriverProfileScreen.Controller
import Effect (Effect)
import Helpers.Utils (getPeriod, fetchImage, FetchImageFrom(..))
import Font.Style (Style(..))
import ConfigProvider

logoutPopUp :: ST.DriverProfileScreenState -> PopUpModal.Config
logoutPopUp  state = let
  config' = PopUpModal.config
  popUpConfig' = config' {
    primaryText {text = (getString LOGOUT)},
    secondaryText {text = (getString ARE_YOU_SURE_YOU_WANT_TO_LOGOUT)},
    option1 {text = (getString GO_BACK), enableRipple = true},
    option2 {text = (getString LOGOUT), enableRipple = true}
  }
  in popUpConfig'

genericHeaderConfig :: ST.DriverProfileScreenState -> GenericHeader.Config
genericHeaderConfig state = let
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_back"
      , height = (V 25)
      , width = (V 25)
      , margin = (Margin 8 8 8 8)
      , layoutMargin = Margin 4 4 4 4
      , enableRipple = true
      }
    , padding = (PaddingVertical 5 5)
    , textConfig {
        text = if state.props.updateLanguages then (getString LANGUAGES_SPOKEN) else (getString SETTINGS)
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

genericHeaderConfigManageVehicle :: ST.DriverProfileScreenState -> GenericHeader.Config
genericHeaderConfigManageVehicle state = let
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , height = (V 25)
      , width = (V 25)
      , margin = (Margin 8 8 8 8)
      , layoutMargin = Margin 8 8 8 8
      , enableRipple = true
      }
    , padding = (PaddingVertical 5 5)
    , textConfig {
        text = getString MANAGE_VEHICLE
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

primaryEditTextConfig :: ST.DriverProfileScreenState -> PrimaryEditText.Config
primaryEditTextConfig state = let
  config = PrimaryEditText.config
  primaryEditTextConfig' = config
    { editText
      { singleLine = true
        , pattern = case state.props.detailsUpdationType of
                      Just ST.VEHICLE_AGE -> Just "[0-9]*,2"
                      Just ST.VEHICLE_NAME -> Just "[a-zA-Z0-9]*,30"
                      _ ->  Just "[a-zA-Z0-9]*,30"
        , text = ""
        , placeholder = ""
      }
    , height = V 54
    , margin = Margin 16 24 16 0
    , stroke = ("1,"<> Color.blue800)
    , id = (EHC.getNewIDWithTag "UpdateDetailsEditText")
    , topLabel
      { text =  case state.props.detailsUpdationType of
                  Just ST.VEHICLE_AGE -> (getString HOW_OLD_IS_YOUR_VEHICLE)
                  Just ST.VEHICLE_NAME -> (getString ENTER_NAME_OF_VEHICLE)
                  _ -> ""
      , color = Color.greyTextColor
      }
    }
  in primaryEditTextConfig'

driverGenericHeaderConfig :: ST.DriverProfileScreenState -> GenericHeader.Config
driverGenericHeaderConfig state = let
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , margin = Margin 12 12 12 12
      }
    , padding = PaddingVertical 5 5
    , textConfig {
        text = if state.props.showGenderView then getString GENDER else getString ALTERNATE_NUMBER
      , color = Color.black900
      }
    }
  in genericHeaderConfig'

primaryButtonConfig :: ST.DriverProfileScreenState -> PrimaryButton.Config
primaryButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = getString UPDATE
      , color = Color.primaryButtonColor}
      , margin = Margin 10 0 10 10
      , cornerRadius = 10.0
      , background = Color.black900
      , height = (V 48)
      , id = "DriverProfilePrimaryButton"
      , enableRipple = true
      , rippleColor = Color.rippleShade
      , isClickable = (state.props.updateLanguages && DA.length (getSelectedLanguages state) > 0)|| ( state.props.showGenderView && isJust state.data.genderTypeSelect && state.data.driverGender /= state.data.genderTypeSelect) || (state.props.alternateNumberView && (DS.length (fromMaybe "" state.data.driverEditAlternateMobile))==10 && state.props.checkAlternateNumber && state.data.driverAlternateNumber /= state.data.driverEditAlternateMobile)
      , alpha = if (state.props.updateLanguages && DA.length (getSelectedLanguages state) > 0) || (state.props.showGenderView && isJust state.data.genderTypeSelect && state.data.driverGender /= state.data.genderTypeSelect) || (state.props.alternateNumberView && DS.length(fromMaybe "" state.data.driverEditAlternateMobile)==10 && state.props.checkAlternateNumber && state.data.driverAlternateNumber /= state.data.driverEditAlternateMobile) then 1.0 else 0.7
      }
  in primaryButtonConfig'


updateButtonConfig :: ST.DriverProfileScreenState -> PrimaryButton.Config
updateButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = getString UPDATE
      , color = Color.primaryButtonColor}
      , margin = MarginHorizontal 10 10
      , cornerRadius = 10.0
      , background = Color.black900
      , height = (V 48)
      , isClickable = state.props.btnActive
      , alpha = if state.props.btnActive then 1.0 else 0.5
      , id = "DriverProfileScreenUpdatePrimaryButton"
      }
  in primaryButtonConfig'

downloadQRConfig :: ST.DriverProfileScreenState -> PrimaryButton.Config
downloadQRConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = getString DOWNLOAD_QR
      , color = Color.primaryButtonColor}
      , margin = MarginHorizontal 10 10
      , cornerRadius = 10.0
      , background = Color.black900
      , height = (V 48)
      , id = "downloadQRPrimaryButton"
      }
  in primaryButtonConfig'


shareOptionButtonConfig :: ST.DriverProfileScreenState -> PrimaryButton.Config
shareOptionButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = getString SHARE_OPTIONS
      , color = Color.black900
      , textStyle = Tags}
      , isPrefixImage = true
      , prefixImageConfig {
          imageUrl = fetchImage COMMON_ASSET "ny_ic_share"
        , margin = MarginRight 8
        , padding = PaddingTop 4
      }
      , margin = MarginHorizontal 10 10
      , cornerRadius = 10.0
      , background = Color.white900
      , height = (V 48)
      , id = "shareOptionPrimaryButton"
      }
  in primaryButtonConfig'


alternatePrimaryEditTextConfig :: ST.DriverProfileScreenState -> PrimaryEditText.Config
alternatePrimaryEditTextConfig state = let
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { singleLine = true
          , pattern = Just "[0-9]*,10"
          , color = Color.black800
          , margin = MarginHorizontal 10 10
          , focused = state.props.mNumberEdtFocused
        }
      , showConstantField = true
      , topLabel
        {
text = ""
        , color = Color.black800
        , visibility = GONE
        }
      , type = "number"
      , errorLabel
        { text = if state.props.numberExistError then getString NUMBER_ALREADY_EXIST_ERROR else getString PLEASE_ENTER_A_VALID_10_DIGIT_NUMBER
        , margin = MarginTop 1
        }
      , constantField {
          color = if state.props.mNumberEdtFocused then Color.black800 else Color.grey900
        , padding = PaddingBottom 1
        }
      , showErrorLabel = not state.props.checkAlternateNumber || state.props.numberExistError
      , margin = Margin 10 10 10 0
      , background = Color.white900
      , id = EHC.getNewIDWithTag "alternateMobileNumber"
      }
    in primaryEditTextConfig'

removeAlternateNumberConfig :: ST.DriverProfileScreenState -> PopUpModal.Config
removeAlternateNumberConfig state = let
    config = PopUpModal.config
    popUpConfig' = config {
      primaryText {
        text = getString REMOVE_ALTERNATE_NUMBER
      },
      secondaryText {
        text = getString ARE_YOU_SURE_YOU_WANT_TO_REMOVE_YOUR_ALTERNATE_MOBILE_NUMBER
      , color = Color.black700
        },
      option1 {
        text = getString CANCEL
      , color = Color.black900
      , strokeColor = Color.black700
      },
      option2 {
        text = getString YES_REMOVE_IT
      , background = Color.red
      , color = Color.white900
      , strokeColor = Color.red}
    }
  in popUpConfig'

enterOtpState :: ST.DriverProfileScreenState -> InAppKeyboardModalController.InAppKeyboardModalState
enterOtpState state = let
      config' = InAppKeyboardModalController.config
      inAppModalConfig' = config'{
        modalType = if state.props.otpAttemptsExceeded then ST.NONE else ST.OTP
      , showResendOtpButton = true
      , otpIncorrect = if state.props.otpAttemptsExceeded then false else state.props.otpIncorrect
      , otpAttemptsExceeded = state.props.otpAttemptsExceeded
      , inputTextConfig {
        text = state.props.alternateMobileOtp
      , focusIndex = state.props.enterOtpFocusIndex
      },
      headingConfig {
        text = getString ENTER_OTP
      },
      errorConfig {
        text = if state.props.otpIncorrect then getString WRONG_OTP else ""
      , visibility = if state.props.otpIncorrect || state.props.otpAttemptsExceeded then VISIBLE else GONE
      , margin = MarginBottom 8
      },
      subHeadingConfig {
        text = getString OTP_SENT_TO <> fromMaybe "" state.data.driverEditAlternateMobile
      , color = Color.black800
      , margin = MarginBottom 8
      , visibility = if not state.props.otpIncorrect then VISIBLE else GONE
      },
      imageConfig {
          alpha = if DS.length state.props.alternateMobileOtp < 4 || state.props.otpIncorrect then 0.3 else 1.0
      }
      }
      in inAppModalConfig'

checkListConfig :: ST.DriverProfileScreenState -> CheckListView.Config
checkListConfig state = let
  config = CheckListView.config
  checkListConfig' = config
    {
        optionsProvided = state.data.languageList
        , isSelected = false
        , index = 0
    }
 in checkListConfig'


primaryButtonConfig1 :: ST.DriverProfileScreenState -> PrimaryButton.Config
primaryButtonConfig1 state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = (getString UPDATE)
      , color = Color.primaryButtonColor}
      , margin = (Margin 10 0 10 0)
      , cornerRadius = 10.0
      , background = Color.black900
      , height = (V 48)
      , alpha = 1.0
      , id = "DriverProfileScreenPrimaryButton1"
      }
  in primaryButtonConfig'

activateAndDeactivateRcPopUpConfig :: forall w. (Action -> Effect Unit) ->  ST.DriverProfileScreenState -> PopUpModal.Config
activateAndDeactivateRcPopUpConfig push state =
  let
    config' = PopUpModal.config
    popUpConfig' =
      config'
        { primaryText { text = if state.data.isRCActive then (getString DEACTIVATE_RC) <> " - " <> state.data.rcNumber <> "?" else (getString ACTIVATE_RC) <>" - " <> state.data.rcNumber <> "?"}
        , buttonLayoutMargin = (MarginHorizontal 16 16)
        , dismissPopup = true
        , optionButtonOrientation = "VERTICAL"
        , secondaryText { text = if state.data.isRCActive then (getString CONFIRMATION_FOR_DEACTIVATING_RC) else (getString CONFIRMATION_FOR_ACTIVATING_RC) <>(getString THIS_WILL_DEACTIVATE_CURRENTLY_ACTIVE_RC), color = Color.black700}
        , option1 {
          text = if state.data.isRCActive then (getString YES_DEACTIVATE) else (getString YES_ACTIVATE)
        , width = MATCH_PARENT
        , color = state.data.config.primaryTextColor
        , strokeColor = state.data.config.primaryButtonBackground
        , background = state.data.config.primaryButtonBackground
        , padding = (PaddingVertical 10 10)
        }
        , option2 {
            text = (getString CANCEL)
          , width = MATCH_PARENT
          , background = Color.white900
          , strokeColor = Color.white900
          , color = Color.black650
          , margin = (MarginBottom 16)
          }
        }
  in
    popUpConfig'

paymentInfoPopUpConfig :: forall w. (Action -> Effect Unit) ->  ST.DriverProfileScreenState -> PopUpModal.Config
paymentInfoPopUpConfig push state =
  let
    config' = PopUpModal.config
    popUpConfig' =
      config'
        { 
         buttonLayoutMargin = Margin 16 24 16 20 ,
         padding = PaddingTop 24,
         backgroundClickable = true,
         dismissPopup = true,
         primaryText {
          visibility = GONE },
         secondaryText {
            visibility = GONE },
         option1 {
           text = getString GOT_IT
         , background = Color.black900
         , color = Color.yellow900
         },
         option2 {
           visibility = false
         },
        listViewArray = [getString USE_THIS_QR_TO_COLLECT_PAYMENT , getString AMOUNT_WILL_DEPOSITED_TO_BANK_ACCOUNT ]
        }
  in
    popUpConfig'

callDriverPopUpConfig :: forall w. (Action -> Effect Unit) ->  ST.DriverProfileScreenState -> PopUpModal.Config
callDriverPopUpConfig push state =
  let
    config' = PopUpModal.config
    popUpConfig' =
      config'
        { primaryText { text = (getString (CALL_DRIVER))}
        , gravity = CENTER
        , buttonLayoutMargin = (MarginHorizontal 16 16)
        , dismissPopup = true
        , backgroundColor = Color.transparent
        , cornerRadius = (Corners 24.0 true true true true)
        , optionButtonOrientation = "VERTICAL"
        , secondaryText { text = (getString CONNECT_CALL_ANONYMOUSLY), color = Color.black700}
        , option1 {
          text = (getString PLACE_CALL_REQUEST)
        , width = MATCH_PARENT
        , color = Color.yellow900
        , strokeColor = Color.black900
        , background = Color.black900
        , padding = (PaddingVertical 10 10)
        }
        , option2 {
            text = (getString GO_BACK)
          , width = MATCH_PARENT
          , background = Color.white900
          , strokeColor = Color.white900
          , margin = MarginTop 14
          , color = Color.black650
          , padding = (PaddingBottom 12)
          }
        }
  in
    popUpConfig'

addRCButtonConfig :: ST.DriverProfileScreenState -> PrimaryButton.Config
addRCButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = getString ADD_VEHICLE
      , color = Color.blue900}
      , margin = (Margin 16 15 16 0)
      , cornerRadius = 10.0
      , background = EHU.getColorWithOpacity 12 Color.blue900
      , height = (V 60)
      , id = "AddRCPrimaryButton" 
      }
  in primaryButtonConfig'

addRCButtonConfigs :: ST.DriverProfileScreenState -> PrimaryButton.Config
addRCButtonConfigs state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = getString MANAGE_VEHICLE
      , color = Color.black700}
      , margin = (Margin 16 12 16 18)
      , cornerRadius = 10.0
      , background = EHU.getColorWithOpacity 12 Color.blue900
      , height = (V 60)
      , id = "AddRCPrimaryButton" 
      }
  in primaryButtonConfig'

deleteRcPopUpConfig :: ST.DriverProfileScreenState -> PopUpModal.Config
deleteRcPopUpConfig state =
  let
    config' = PopUpModal.config
    popUpConfig' =
      config'
        { primaryText { text = (getString DELETE_RC) <>"- " <> state.data.rcNumber <> "?"}
        , buttonLayoutMargin = (MarginHorizontal 16 16)
        , dismissPopup = true
        , optionButtonOrientation = "VERTICAL"
        , secondaryText { text = (getString CONFIRMATION_FOR_DELETING_RC)  , color = Color.black700}
        , option1 {
          text = (getString YES_DELETE)
        , width = MATCH_PARENT
        , color = Color.white900
        , strokeColor = Color.red
        , background = Color.red
        }
        , option2 {
            text = (getString CANCEL)
          , width = MATCH_PARENT
          , background = Color.white900
          , strokeColor = Color.white900
          , color = Color.black650
          , margin = (MarginBottom 16)
          }
        }
  in
    popUpConfig'

driverBLockedPopup :: ST.DriverProfileScreenState -> PopUpModal.Config
driverBLockedPopup state = 
  PopUpModal.config {
    gravity = CENTER,
    backgroundClickable = false,
    optionButtonOrientation = "VERTICAL",
    buttonLayoutMargin = Margin 16 0 16 20,
    margin = MarginHorizontal 25 25, 
    primaryText {
      text = getString $ BLOCKED_TILL (EHC.convertUTCtoISC state.data.blockedExpiryTime "hh:mm A") (EHC.convertUTCtoISC state.data.blockedExpiryTime "DD-MM-YYYY")
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
    cornerRadius = Corners 15.0 true true true true,
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET "ny_ic_account_blocked"
    , visibility = VISIBLE
    , margin = Margin 16 16 16 16
    , width = MATCH_PARENT
    , height = V 250
    }
  }

getChipRailArray :: Int -> String -> Array String -> String -> Array ST.ChipRailData
getChipRailArray lateNightTrips lastRegistered lang totalDistanceTravelled =
  let
    alive = getPeriod lastRegistered
    appData = (getAppConfig appConfig).appData
  in
    ( if lateNightTrips > 0 then
        [ { mainTxt: show lateNightTrips
          , subTxt: getString LATE_NIGHT_TRIPS
          }
        ]
      else
        []
    ) <>
    ( [ { mainTxt: if alive.periodType == "new" then "" else (show alive.period) <> " " <> alive.periodType
          , subTxt: "on " <> appData.name
          }
        ]
    )<>
    ( if DA.length lang > 0 then
            [ { mainTxt: show (DA.length lang)
              , subTxt: getString LANGUAGES_SPOKEN
              }
            ]
          else
            []
    )<>
    (
      [ { mainTxt: totalDistanceTravelled
        , subTxt: getString $ TRAVELLED_ON_APP "TRAVELLED_ON_APP"
        }
      ]
    )
