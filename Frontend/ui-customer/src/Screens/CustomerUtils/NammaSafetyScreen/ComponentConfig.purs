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
import Data.Array (any, null)
import Engineering.Helpers.Commons as EHC
import Font.Style (Style(..))
import Helpers.Utils as HU
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Prelude (($), (<>), (==), (>), (||), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (NammaSafetyScreenState, NammaSafetyStage(..))
import Styles.Colors as Color

startNSOnboardingButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
startNSOnboardingButtonConfig state =
  PrimaryButton.config
    { textConfig { text = getButtonString state.props.currentStage }
    , visibility = visibility'
    , margin = Margin 16 0 16 24
    }
  where
  visibility' = if state.data.hasCompletedSafetySetup || state.props.onRide then GONE else VISIBLE

continueNextStepButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
continueNextStepButtonConfig state =
  PrimaryButton.config
    { textConfig
      { text = text'
      }
    , margin = Margin 16 0 16 24
    }
  where
  text' =
    getString
      if state.props.currentStage == SetPersonalSafetySettings then
        FINISH_SETUP
      else
        CONTINUE

editEmergencyContactsBtnConfig :: NammaSafetyScreenState -> PrimaryButton.Config
editEmergencyContactsBtnConfig state =
  PrimaryButton.config
    { textConfig
      { text = getString if null state.data.contactsList then ADD_CONTACTS else EDIT
      , color = Color.blue900
      , textStyle = Body2
      }
    , height = WRAP_CONTENT
    , width = WRAP_CONTENT
    , margin = MarginLeft 9
    , gravity = LEFT
    , background = Color.white900
    }

cancelSOSBtnConfig :: NammaSafetyScreenState -> PrimaryButton.Config
cancelSOSBtnConfig state =
  PrimaryButton.config
    { textConfig
      { text = getString MARK_RIDE_AS_SAFE
      , color = Color.white900
      }
    , margin = MarginTop 10
    , stroke = "1," <> Color.white900
    }

goBackBtnConfig :: NammaSafetyScreenState -> PrimaryButton.Config
goBackBtnConfig state =
  PrimaryButton.config
    { textConfig
      { text = getString GO_BACK_
      , color = Color.black900
      }
    , margin = Margin 16 10 16 24
    , background = Color.white900
    }

getButtonString :: NammaSafetyStage -> String
getButtonString stage =
  getString
    $ case stage of
        SetTriggerCustomerSupport -> ADD_EMERGENCY_CONTACTS
        SetPersonalSafetySettings -> FINISH_SETUP
        _ -> START_SETUP

genericHeaderConfig :: String -> NammaSafetyScreenState -> GenericHeader.Config
genericHeaderConfig title state =
  GenericHeader.config
    { height = WRAP_CONTENT
    , background = Color.transparent
    , prefixImageConfig
      { imageUrl = prefixImage
      , margin = Margin 12 14 12 12
      , visibility = boolToVisibility $ not $ state.props.currentStage == NammaSafetyVideoRecord
      }
    , textConfig
      { text = title
      , color = textColor
      , margin = MarginLeft if state.props.currentStage == NammaSafetyVideoRecord then 16 else 0
      }
    , suffixImageConfig
      { imageUrl = "ny_ic_close_white"
      , margin = Margin 12 14 12 12
      , visibility = boolToVisibility $ state.props.currentStage == NammaSafetyVideoRecord
      }
    , padding = PaddingHorizontal 5 5
    }
  where
  prefixImage =
    HU.fetchImage HU.FF_ASSET
      if any (_ == state.props.currentStage) [ ActivateNammaSafety, TriggeredNammaSafety ] then
        "ny_ic_chevron_left_white"
      else
        "ny_ic_chevron_left"

  textColor =
    if any (_ == state.props.currentStage) [ ActivateNammaSafety, TriggeredNammaSafety, NammaSafetyVideoRecord ] then
      Color.white900
    else
      Color.black800

activateSoSButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
activateSoSButtonConfig state =
  PrimaryButton.config
    { textConfig { text = getString ACTIVATE_SOS, color = Color.black900 }
    , margin = Margin 16 0 16 8
    , background = Color.white900
    }

dismissSoSButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
dismissSoSButtonConfig state =
  PrimaryButton.config
    { textConfig { text = getString DISMISS, color = Color.white900 }
    , margin = Margin 16 4 16 0
    , stroke = "1," <> Color.white900
    }

contactListPrimaryButtonConfig :: Int -> PrimaryButton.Config
contactListPrimaryButtonConfig count =
  PrimaryButton.config
    { textConfig
      { text = if count > 0 then getString CONFIRM_EMERGENCY_CONTACTS else getString SELECT_CONTACTS
      , color = Color.yellow900
      }
    , alpha = if count > 0 then 1.0 else 0.6
    , isClickable = if count > 0 then true else false
    , id = "ContactListPrimaryButton"
    }

--------------------------------------------------- removeContactPopUpModelConfig -----------------------------------------------------
removeContactPopUpModelConfig :: NammaSafetyScreenState -> PopUpModal.Config
removeContactPopUpModelConfig state =
  PopUpModal.config
    { primaryText { text = getString REMOVE <> " " <> state.data.removedContactDetail.name }
    , secondaryText { text = getString ARE_YOU_SURE_YOU_WANT_TO_REMOVE_CONTACT }
    , option1
      { text = getString CANCEL_
      , strokeColor = Color.black700
      }
    , option2
      { text = getString YES_REMOVE
      , background = Color.red
      , color = Color.white900
      , strokeColor = Color.red
      }
    , backgroundClickable = false
    , buttonLayoutMargin = MarginBottom if EHC.os == "IOS" then 0 else 24
    }

confirmPopUpModelConfig :: NammaSafetyScreenState -> PopUpModal.Config
confirmPopUpModelConfig state =
  PopUpModal.config
    { cornerRadius = Corners 15.0 true true true true
    , margin = MarginHorizontal 16 16
    , padding = Padding 16 16 16 16
    , gravity = CENTER
    , backgroundColor = Color.black9000
    , backgroundClickable = false
    , buttonLayoutMargin = MarginBottom 0
    , optionButtonOrientation = "VERTICAL"
    , primaryText
      { text = getStringBasedOnMode ACTIVATE_NAMMA_SAFETY_POPUP_TITLE $ isSafetyPlus state
      , margin = Margin 16 16 16 0
      }
    , option1
      { text = getString ACTIVATE_NAMMA_SAFETY_POPUP_ACTION
      , color = Color.yellow900
      , background = Color.black900
      , margin = MarginTop 16
      , width = MATCH_PARENT
      }
    , secondaryText
      { text = getStringBasedOnMode ACTIVATE_NAMMA_SAFETY_POPUP_DESC $ isSafetyPlus state
      , color = Color.black700
      , margin = Margin 16 4 16 0
      }
    , option2
      { text = getString DISMISS
      , color = Color.black650
      , background = Color.white900
      , strokeColor = Color.white900
      , width = MATCH_PARENT
      , margin = MarginLeft 0
      }
    }

goBackButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
goBackButtonConfig state =
  PrimaryButton.config
    { textConfig { text = getString GO_BACK_, color = Color.black700 }
    , margin = Margin 16 0 16 24
    , background = Color.white900
    , stroke = "1," <> Color.black700
    }

getStringBasedOnMode :: STR -> Boolean -> String
getStringBasedOnMode str enableSafetyPlus =
  getString
    $ if enableSafetyPlus then case str of
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
        ABOUT_SOS_12 -> ABOUT_SOS_12_PLUS "ABOUT_SOS_12_PLUS"
        NAMMA_SAFETY -> NAMMA_SAFETY_PLUS
        SAFETY_GUIDELINES_6 -> SAFETY_GUIDELINES_6_PLUS
        ENABLE_NIGHT_TIME_SAFETY_ALERTS_DESC -> ENABLE_NIGHT_TIME_SAFETY_ALERTS_DESC_PLUS
        LEARN_ABOUT_NAMMA_SAFETY -> LEARN_ABOUT_NAMMA_SAFETY_PLUS
        EDIT_ACTIONS -> EDIT_ACTIONS_PLUS
        ALMOST_DONE_DESC -> ALMOST_DONE_DESC_PLUS
        NIGHT_SAFETY_DESC -> NIGHT_SAFETY_DESC_PLUS
        ACTIVATE_NAMMA_SAFETY_WILL_ENABLE_ACCESS -> ACTIVATE_NAMMA_SAFETY_WILL_ENABLE_ACCESS_PLUS
        _ -> str
      else
        str

isSafetyPlus :: NammaSafetyScreenState -> Boolean
isSafetyPlus state = state.data.safetyConfig.enableSupport || state.props.enableLocalPoliceSupport
