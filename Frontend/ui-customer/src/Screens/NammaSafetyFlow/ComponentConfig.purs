{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.ComponentConfig where

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
import Screens.Types (NammaSafetyScreenState, NammaSafetyStage(..), SafetySetupStage(..))
import Styles.Colors as Color

startNSOnboardingButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
startNSOnboardingButtonConfig state =
  PrimaryButton.config
    { textConfig { text = getString START_SETUP }
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
      if state.props.setupStage == SetPersonalSafetySettings then
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
    { textConfig { text = getString CANCEL_, color = Color.black900 }
    , margin = Margin 16 24 16 24
    , stroke = "1," <> Color.white900
    , background = Color.white900
    }

goToDrillButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
goToDrillButtonConfig state =
  PrimaryButton.config
    { textConfig
      { text = getString TEST_SAFETY_DRILL
      }
    , margin = Margin 16 0 16 24
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
      { text = getString ACTIVATE_NAMMA_SAFETY_POPUP_TITLE
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
      { text = getString ACTIVATE_NAMMA_SAFETY_POPUP_DESC
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
    { textConfig
      { text = getString GO_BACK_
      , color = Color.black700
      }
    , margin = Margin 16 0 16 24
    , background = Color.white900
    , stroke = "1," <> Color.black700
    }

startTestDrillButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
startTestDrillButtonConfig state =
  PrimaryButton.config
    { textConfig
      { text = getString START_TEST_DRILL
      , color = Color.black900
      }
    , margin = MarginVertical 48 24
    , background = Color.white900
    }

