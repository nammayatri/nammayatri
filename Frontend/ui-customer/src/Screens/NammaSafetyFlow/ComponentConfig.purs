{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.ComponentConfig where

import Components.PopUpModal as PopUpModal
import Components.PopupWithCheckbox.Controller as PopupWithCheckboxController
import Components.PrimaryButton as PrimaryButton
import Engineering.Helpers.Commons as EHC
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Prelude (map, not, ($), (<>), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (NammaSafetyScreenState, SafetySetupStage(..), IndividualRideCardState)
import Services.API (RideShareOptions(..))
import Styles.Colors as Color
import Components.SourceToDestination as SourceToDestination
import Data.Maybe (Maybe(..))
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..), Gravity(..))

startNSOnboardingButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
startNSOnboardingButtonConfig state =
  PrimaryButton.config
    { textConfig { text = getString START_SETUP }
    , visibility = boolToVisibility $ not state.data.hasCompletedSafetySetup
    , margin = Margin 16 0 16 24
    , id = "ScreenStartOnboardingButton"
    , enableRipple = true
    }

continueNextStepButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
continueNextStepButtonConfig state =
  PrimaryButton.config
    { textConfig
      { text = text'
      }
    , margin = Margin 16 0 16 24
    , id = "SetupScreenContinueNextStepButton"
    , enableRipple = true
    }
  where
  text' =
    getString
      if state.props.setupStage == SetPersonalSafetySettings then
        FINISH_SETUP
      else
        CONTINUE

cancelSOSBtnConfig :: NammaSafetyScreenState -> PrimaryButton.Config
cancelSOSBtnConfig state =
  PrimaryButton.config
    { textConfig
      { text = getString MARK_RIDE_AS_SAFE
      , color = Color.white900
      }
    , id = "CancelSosButton"
    , margin = MarginTop 10
    , stroke = "1," <> Color.white900
    , enableRipple = true
    }

activateSoSButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
activateSoSButtonConfig state =
  PrimaryButton.config
    { textConfig { text = getString ACTIVATE_SOS, color = Color.black900 }
    , margin = Margin 16 0 16 8
    , background = Color.white900
    , id = "SafetyScreenActivateSosButton"
    , enableRipple = true
    }

dismissSoSButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
dismissSoSButtonConfig state =
  PrimaryButton.config
    { textConfig { text = getString CANCEL_, color = Color.black900 }
    , margin = Margin 16 24 16 24
    , stroke = "1," <> Color.white900
    , background = Color.white900
    , id = "SafetyScreenDismissSosButton"
    , enableRipple = true
    }

goToDrillButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
goToDrillButtonConfig state =
  PrimaryButton.config
    { textConfig
      { text = getString TEST_SAFETY_DRILL
      }
    , margin = Margin 16 0 16 24
    , id = "SafetyScreenGoToDrillButton"
    , enableRipple = true
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
      , enableRipple = true
      }
    , option2
      { text = getString YES_REMOVE
      , background = Color.red
      , color = Color.white900
      , strokeColor = Color.red
      , enableRipple = true
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
      , enableRipple = true
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
      , enableRipple = true
      }
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
    , enableRipple = true
    , id = "SafetyScreenStartTestDrillButton"
    }

shareTripPopupConfig :: NammaSafetyScreenState -> PopupWithCheckboxController.Config
shareTripPopupConfig state =
  PopupWithCheckboxController.config
    { title = getString SHARE_TRIP_NOTIFICATONS
    , secondaryButtonText = getString SAVE
    , checkboxList = checkBoxData state
    , primaryButtonConfig = shareTripPopupBtnConfig state
    }

shareTripPopupBtnConfig :: NammaSafetyScreenState -> PrimaryButton.Config
shareTripPopupBtnConfig state =
  PrimaryButton.config
    { textConfig
      { text = getString SAVE
      }
    , margin = MarginVertical 20 20
    , id = "SafetyScreenShareTripPopupButton"
    , enableRipple = true
    }

checkBoxData :: NammaSafetyScreenState -> Array PopupWithCheckboxController.CheckBoxOption
checkBoxData state =
  map
    ( \x ->
        { label: x.label
        , selected: x.type == state.data.shareOptionCurrent
        }
    )
    labelData

type Label = { label :: String, type :: RideShareOptions }

labelData :: Array Label
labelData =
  [ { label: getString ALWAYS
    , type: ALWAYS_SHARE
    }
  , { label: getString NIGHT_RIDES_SHARE
    , type: SHARE_WITH_TIME_CONSTRAINTS
    }
  , { label: getString NEVER
    , type: NEVER_SHARE
    }
  ]

pastRideSOSConfirmationPopConfig :: NammaSafetyScreenState -> PopUpModal.Config
pastRideSOSConfirmationPopConfig state = PopUpModal.config
        { optionButtonOrientation = "VERTICAL"
        , buttonLayoutMargin = Margin 24 0 24 20
        , gravity = CENTER
        , margin = MarginHorizontal 20 20
        , primaryText
          { text = "Safety Center"
          , margin = Margin 16 16 16 10
          }
        , secondaryText
          { text = "If you have issues with your recent ride please select the below option"
          , margin = MarginHorizontal 16 16
          }
        , option1
          { text = "I need help with my recent ride"
          , color = Color.black700
          , background = Color.white900
          , width = MATCH_PARENT
          , margin = MarginVertical 20 10
          , enableRipple = true
          }
        , option2
          { text = "Continue with safety settings"
          , color = Color.yellow900
          , background = Color.black900
          , strokeColor = Color.transparent
          , width = MATCH_PARENT
          , margin = MarginBottom 10
          , enableRipple = true
          }
        , cornerRadius = Corners 15.0 true true true true
        , coverImageConfig
          { visibility = GONE
          }
        }


sourceToDestinationConfig :: IndividualRideCardState -> SourceToDestination.Config
sourceToDestinationConfig ride = SourceToDestination.config {
      margin = (Margin 0 13 16 0)
    , width = MATCH_PARENT
    , lineMargin = (Margin 4 6 0 0)
    , id = Just $ "PostRideSafetySTDC_" <> ride.rideId
    , sourceMargin = (Margin 0 0 0 14)
    , sourceBackground = Color.transparent
    , sourceImageConfig {
        imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_green_circle"
      , margin = (MarginTop 5)
      }
    , sourceTextConfig {
        text = ride.source
      , padding = (Padding 0 0 0 0)
      , margin = (Margin 7 0 15 0)
      , color = Color.white900
      , textStyle = FontStyle.Body1
      , ellipsize = true
      , maxLines = 1
      }
    , destinationBackground = Color.transparent
    , destinationImageConfig {
        imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_red_circle"
      , margin = (MarginTop 4)
      }
    , destinationTextConfig {
        text = ride.destination
      , padding = (Padding 0 0 0 0)
      , margin = (Margin 7 0 15 0)
      , color = Color.white900
      , textStyle = FontStyle.Body1
      , maxLines = 1
      , ellipsize = true
      }
    , overrideSeparatorCount = 2
    }
