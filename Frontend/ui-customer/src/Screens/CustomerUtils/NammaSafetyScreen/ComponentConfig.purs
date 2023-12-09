{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NammaSafetyScreen.ComponentConfig where

import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Data.Array (any, length, null)
import Engineering.Helpers.Commons as EHC
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (($), (<>), (==), (>), (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (NammaSafetyScreenState, NammaSafetyStage(..))
import Styles.Colors as Color

startNSOnboardingButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
startNSOnboardingButtonConfig state = 
  PrimaryButton.config
    { textConfig{ text = (getButtonString state.props.currentStage) }
    , isClickable = true
    , visibility = if state.data.hasCompletedSafetySetup || state.props.onRide then GONE else VISIBLE
    , margin = (Margin 16 0 16 24 )
    }

continueNextStepButtonConfig:: NammaSafetyScreenState -> PrimaryButton.Config
continueNextStepButtonConfig state = 
  PrimaryButton.config
    { textConfig{ text = getString $ if state.props.currentStage == SetPersonalSafetySettings then  FINISH_SETUP else CONTINUE }
    , isClickable = true
    , visibility = VISIBLE
    , margin = (Margin 16 0 16 24 )
    }

editEmergencyContactsBtnConfig :: NammaSafetyScreenState -> PrimaryButton.Config
editEmergencyContactsBtnConfig state = 
  PrimaryButton.config
    { textConfig{ text = (getString EDIT) , color = Color.blue900
    }
    , isClickable = true
    , height = MATCH_PARENT
    , width = WRAP_CONTENT
    , margin = (MarginLeft 9)
    , gravity = CENTER
    , background = Color.white900
    }

cancelSOSBtnConfig :: NammaSafetyScreenState -> PrimaryButton.Config
cancelSOSBtnConfig state = 
  PrimaryButton.config
    { textConfig{ text = getString MARK_RIDE_AS_SAFE , color = Color.black900
    }
    , isClickable = true
    , height = V 48
    , width = MATCH_PARENT
    , margin = (MarginTop 10)
    , gravity = CENTER
    , background = Color.white900
    }

goBackBtnConfig :: NammaSafetyScreenState -> PrimaryButton.Config
goBackBtnConfig state = 
  PrimaryButton.config
    { textConfig{ text = getString GO_BACK_ , color = Color.black900
                }
    , isClickable = true
    , height = V 48
    , width = MATCH_PARENT
    , margin = (Margin 16 10 16 24)
    , gravity = CENTER
    , background = Color.white900
    }


getButtonString :: NammaSafetyStage -> String
getButtonString stage = getString $ case stage of
  SetTriggerCustomerSupport ->  ADD_EMERGENCY_CONTACTS
  SetPersonalSafetySettings ->  FINISH_SETUP
  _ ->  START_SETUP

genericHeaderConfig :: String -> NammaSafetyScreenState -> GenericHeader.Config 
genericHeaderConfig title state = 
  GenericHeader.config
    {
      height = WRAP_CONTENT
    , width = MATCH_PARENT
    , background = Color.transparent
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = if any (_ == state.props.currentStage)[ActivateNammaSafety, TriggeredNammaSafety] then "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png" else "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , margin = (Margin 12 14 12 12)
      , visibility = if state.props.currentStage == NammaSafetyVideoRecord then GONE else VISIBLE
      } 
    , textConfig {
        text = title
      , color = if any (_ == state.props.currentStage)[ActivateNammaSafety, TriggeredNammaSafety, NammaSafetyVideoRecord] then Color.white900 else Color.black800
      , margin = if state.props.currentStage == NammaSafetyVideoRecord then MarginLeft 16 else Margin 0 0 0 0
      }
    , suffixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_close_white,https://assets.juspay.in/nammayatri/images/common/ny_ic_close_white.png"
      , margin = (Margin 12 14 12 12)
      , visibility = if state.props.currentStage == NammaSafetyVideoRecord then VISIBLE else GONE
      }
    , padding = (Padding 0 5 0 5)
    }
  



activateSoSButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
activateSoSButtonConfig state = 
  PrimaryButton.config
    { textConfig{ text = getString ACTIVATE_SOS , color = Color.black900}
    , isClickable = true
    , margin = (Margin 16 0 16 8 )
    , background = Color.white900
    }

dismissSoSButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
dismissSoSButtonConfig state = 
  PrimaryButton.config
    { textConfig{ text = getString DISMISS , color = Color.white900}
    , isClickable = true
    , margin = (Margin 16 4 16 0 )
    , background = Color.black900
    , stroke = ("1," <> Color.white900)
    }

contactListPrimaryButtonConfig :: Int -> PrimaryButton.Config
contactListPrimaryButtonConfig count =
  let
    config' = PrimaryButton.config
    primaryButtonConfig' =
      config'
        { textConfig
          { text = if (count > 0) then (getString CONFIRM_EMERGENCY_CONTACTS) else (getString SELECT_CONTACTS)
          , color = Color.yellow900
          }
        , alpha = if (count > 0) then 1.0 else 0.6
        , background =  Color.black900
        , isClickable = if (count > 0) then true else false
        , id = "ContactListPrimaryButton"
        }
  in
    primaryButtonConfig'

--------------------------------------------------- primaryButtonConfig -----------------------------------------------------
addContactButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
addContactButtonConfig state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { textConfig
          { text = if null state.data.emergencyContactsData.contactsList then (getString ADD_EMERGENCY_CONTACTS) else (getString ADD_ANOTHER_CONTACT)
          }
        , isClickable = true
        , width = if EHC.os == "IOS" then (V 360) else (MATCH_PARENT)
        , margin = (MarginBottom 24)
        , visibility = if ((length state.data.emergencyContactsData.contactsList) == 3) then GONE else VISIBLE
        }
  in
    primaryButtonConfig'


--------------------------------------------------- removeContactPopUpModelConfig -----------------------------------------------------
removeContactPopUpModelConfig :: NammaSafetyScreenState -> PopUpModal.Config
removeContactPopUpModelConfig state =
  let
    config' = PopUpModal.config

    popUpConfig' =
      config'
        { primaryText { text = (getString REMOVE) <> " " <> state.data.emergencyContactsData.removedContactDetail.name }
        , secondaryText { text = (getString ARE_YOU_SURE_YOU_WANT_TO_REMOVE_CONTACT) }
        , option1
          { text = (getString CANCEL_)
          , strokeColor = Color.black700
          }
        , option2
          { text = (getString YES_REMOVE)
          , background = Color.red
          , color = Color.white900
          , strokeColor = Color.red
          }
        , backgroundClickable = false
        , buttonLayoutMargin = MarginBottom if EHC.os == "IOS" then 0 else 24
        }
  in
    popUpConfig'

  
confirmPopUpModelConfig :: NammaSafetyScreenState -> PopUpModal.Config
confirmPopUpModelConfig state = 
  PopUpModal.config {
      cornerRadius = Corners 15.0 true true true true
      , margin = MarginHorizontal 16 16
      , padding = Padding 16 16 16 16
      , gravity = CENTER
      , backgroundColor =  Color.black9000
      , backgroundClickable = false
      , buttonLayoutMargin = MarginBottom 0
      , optionButtonOrientation = "VERTICAL"

    ,primaryText {
        text = getString ACTIVATE_NAMMA_SAFETY_POPUP_TITLE              
      , margin = Margin 16 16 16 0
      , visibility = VISIBLE
      , color = Color.black800
     },
      option1 {
        text =  getString ACTIVATE_NAMMA_SAFETY_POPUP_ACTION
                 
      , color = Color.yellow900
      , background = Color.black900
      , visibility =true
      , margin = MarginTop 16
      , width =  MATCH_PARENT
      },
    secondaryText {
      text = getString ACTIVATE_NAMMA_SAFETY_POPUP_DESC
      , color = Color.black700
      , margin = Margin 16 4 16 0
      , visibility = VISIBLE 
      },
    option2 { 
      visibility = true
      , text = getString DISMISS
      , color = Color.black650
      , background = Color.white900
      , strokeColor = Color.white900
      , width = MATCH_PARENT
      , margin = (Margin 0 0 0 0)
    }
    }

getStringBasedOnMode :: STR -> Boolean ->  String
getStringBasedOnMode str enableSafetyPlus = 
    getString $ if enableSafetyPlus 
      then case str of 
        NAMMA_SAFETY_WILL_ENABLE_ACCESS -> NAMMA_SAFETY_WILL_ENABLE_ACCESS_PLUS
        WHEN_YOU_START_EMERGENCY_SOS -> WHEN_YOU_START_EMERGENCY_SOS_PLUS
        ABOUT_SOS_DESC -> ABOUT_SOS_DESC_PLUS
        NAMMA_SAFETY_MEASURES -> NAMMA_SAFETY_MEASURES_PLUS
        ABOUT_SOS -> ABOUT_SOS_PLUS
        SHARE_INFO_WITH_EMERGENCY_CONTACTS_DESC -> SHARE_INFO_WITH_EMERGENCY_CONTACTS_DESC_PLUS
        SAFETY_MEASURE_2 -> SAFETY_MEASURE_2_PLUS
        ABOUT_SOS_9 -> ABOUT_SOS_9_PLUS
        NAMMA_SAFETY_IS_SET_UP -> NAMMA_SAFETY_IS_SET_UP_PLUS
        ACTIVATE_NAMMA_SAFETY_POPUP_TITLE -> ACTIVATE_NAMMA_SAFETY_POPUP_TITLE_PLUS
        ACTIVATE_NAMMA_SAFETY_POPUP_DESC -> ACTIVATE_NAMMA_SAFETY_POPUP_DESC_PLUS
        ABOUT_SOS_12 -> ABOUT_SOS_12_PLUS
        SAFETY_MEASURE_5 -> SAFETY_MEASURE_5_PLUS
        SHARE_LOCATION_AND_RIDE_DETAILS_WITH_NAMMA_YATRI_SUPPORT_TEAM -> SHARE_LOCATION_AND_RIDE_DETAILS_WITH_NAMMA_YATRI_SUPPORT_TEAM_PLUS
        NAMMA_SAFETY -> NAMMA_SAFETY_PLUS
        TRIGGER_ALERT_TO_NAMMA_YATRI_SUPPORT -> TRIGGER_ALERT_TO_NAMMA_YATRI_SUPPORT_PLUS
        TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_TITLE -> TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_TITLE_PLUS
        SAFETY_GUIDELINES_6 -> SAFETY_GUIDELINES_6_PLUS
        ENABLE_NIGHT_TIME_SAFETY_ALERTS_DESC -> ENABLE_NIGHT_TIME_SAFETY_ALERTS_DESC_PLUS
        LEARN_ABOUT_NAMMA_SAFETY -> LEARN_ABOUT_NAMMA_SAFETY_PLUS
        _ -> str
    else str