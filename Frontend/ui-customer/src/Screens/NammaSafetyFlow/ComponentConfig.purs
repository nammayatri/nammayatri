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
import Components.GenericHeader as GenericHeader
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
import JBridge as JB
import Components.OptionsMenu as OptionsMenuConfig
import Common.Types.App as CTA
import Components.Safety.SafetyAudioRecording as SafetyAudioRecording

startNSOnboardingButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
startNSOnboardingButtonConfig state =
  PrimaryButton.config
    { textConfig { text = getString START_SETUP, accessibilityHint = "Start Setup Button" }
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
      , accessibilityHint = text' <> " Button"
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
      { text = getString if state.props.showTestDrill then DONE else MARK_RIDE_AS_SAFE
      , color = Color.white900
      , accessibilityHint = "Mark ride as safe button"
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
    { textConfig { text = getString if state.props.showTestDrill then DONE else CANCEL_, color = Color.white900 }
    , margin = Margin 16 0 16 16
    , stroke = "1," <> Color.white900
    , id = "SafetyScreenDismissSosButton"
    , enableRipple = true
    }

goToDrillButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
goToDrillButtonConfig state =
  PrimaryButton.config
    { textConfig
      { text = getString TEST_SAFETY_DRILL
      , accessibilityHint = "Test Safety Drill Button"
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
      , accessibilityHint = "Start Test Drill : Button"
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
    -- , secondaryButtonText = getString SAVE --check
    , checkboxList = checkBoxData state
    , primaryButtonConfig = shareTripPopupBtnConfig state
    , checkBoxType = PopupWithCheckboxController.Checkbox
    }

shareTripPopupBtnConfig :: NammaSafetyScreenState -> PrimaryButton.Config
shareTripPopupBtnConfig state =
  PrimaryButton.config
    { textConfig
      { text = getString SAVE
      , accessibilityHint = "Save : Button"
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

type Label
  = { label :: String, type :: RideShareOptions }

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
pastRideSOSConfirmationPopConfig state =
  PopUpModal.config
    { optionButtonOrientation = "VERTICAL"
    , buttonLayoutMargin = Margin 24 0 24 20
    , gravity = CENTER
    , margin = MarginHorizontal 20 20
    , primaryText
      { text = getString SAFETY_CENTER
      , margin = Margin 16 16 16 10
      }
    , secondaryText
      { text = getString RECENT_RIDE_ISSUE_DESC
      , margin = MarginHorizontal 16 16
      }
    , option1
      { text = getString I_NEED_HELP_WITH_MY_RECENT_RIDE
      , color = Color.black700
      , background = Color.white900
      , width = MATCH_PARENT
      , margin = MarginVertical 20 10
      , enableRipple = true
      }
    , option2
      { text = getString CONTINUE_WITH_SAFETY_SETTINGS
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
    , backgroundClickable = false
    }

sourceToDestinationConfig :: IndividualRideCardState -> SourceToDestination.Config
sourceToDestinationConfig ride =
  SourceToDestination.config
    { margin = (Margin 0 13 16 0)
    , width = MATCH_PARENT
    , lineMargin = (Margin 4 6 0 0)
    , id = Just $ "PostRideSafetySTDC_" <> ride.rideId
    , sourceMargin = (Margin 0 0 0 14)
    , sourceBackground = Color.transparent
    , sourceImageConfig
      { imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_green_circle"
      , margin = (MarginTop 5)
      }
    , sourceTextConfig
      { text = ride.source
      , padding = (Padding 0 0 0 0)
      , margin = (Margin 7 0 15 0)
      , color = Color.white900
      , textStyle = FontStyle.Body1
      , ellipsize = true
      , maxLines = 1
      }
    , destinationBackground = Color.transparent
    , destinationImageConfig
      { imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_red_circle"
      , margin = (MarginTop 4)
      }
    , destinationTextConfig
      { text = ride.destination
      , padding = (Padding 0 0 0 0)
      , margin = (Margin 7 0 15 0)
      , color = Color.white900
      , textStyle = FontStyle.Body1
      , maxLines = 1
      , ellipsize = true
      }
    , overrideSeparatorCount = 2
    }

genericHeaderConfig :: NammaSafetyScreenState -> GenericHeader.Config
genericHeaderConfig state =
  let
    genericHeaderConfig' =
      GenericHeader.config
        { height = WRAP_CONTENT
        , prefixImageConfig
          { height = V 25
          , width = V 25
          , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
          , visibility = VISIBLE
          , margin = Margin 8 8 8 8
          , layoutMargin = Margin 4 4 4 4
          , enableRipple = true
          }
        , textConfig
          { text = (getString SAFETY)
          , color = Color.darkCharcoal
          }
        , suffixImageConfig
          { visibility = GONE
          }
        , visibility = VISIBLE --titleVisibility
        }
  in
    genericHeaderConfig'
    
shareAudioButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
shareAudioButtonConfig state = let 
  disableButton = state.props.showTestDrill
  in PrimaryButton.config
    { textConfig
      { text = getString SHARE_WITH_SAFETY_TEAM
      , accessibilityHint = "Share Recorded Audio Button"
      , color = Color.blue800
      }
    , background = Color.transparent
    , margin = MarginHorizontal 16 16
    , id = "SafetyScreenShareAudioButton"
    , enableLoader = JB.getBtnLoader "SafetyScreenShareAudioButton"
    , enableRipple = true
    , alpha = if disableButton then 0.5 else 1.0
    , isClickable = not disableButton
    , visibility = boolToVisibility $ state.props.audioRecordingStatus == CTA.RECORDED 
    , viewbackground = Color.transparent
    }

optionsMenuConfig :: NammaSafetyScreenState -> OptionsMenuConfig.Config
optionsMenuConfig state = OptionsMenuConfig.config {
  menuItems = [
    {image : fetchImage FF_ASSET "ny_ic_issue_box", textdata : getString REPORT_SAFETY_ISSUE, action : "report_safety_issue", isVisible : not state.props.showTestDrill, color : Color.white900},
    {image : fetchImage COMMON_ASSET "ny_ic_safety_drill", textdata : getString START_TEST_DRILL, action : "start_test_drill", isVisible : not state.props.showTestDrill, color : Color.white900},
    {image : fetchImage COMMON_ASSET "ny_ic_board_menu", textdata : getString LEARN_ABOUT_NAMMA_SAFETY, action : "learn_about_safety", isVisible : true, color : Color.white900}
  ],
  backgroundColor = Color.blackLessTrans,
  menuBackgroundColor = Color.black900,
  gravity = RIGHT,
  menuExpanded = true,
  width = WRAP_CONTENT,
  marginRight = 16,
  itemHeight = V 50,
  itemPadding = Padding 16 16 16 16,
  cornerRadius = 4.0,
  enableAnim = true,
  showStroke = false
}

safetyAudioRecordingConfig :: NammaSafetyScreenState -> SafetyAudioRecording.Config
safetyAudioRecordingConfig state = {
  isAudioRecordingActive : state.props.isAudioRecordingActive,
  audioRecordingStatus : state.props.audioRecordingStatus,
  recordingTimer : state.props.recordingTimer,
  shareAudioButtonConfig : shareAudioButtonConfig state
}

defaultCallContactPopupConfig :: NammaSafetyScreenState -> PopupWithCheckboxController.Config
defaultCallContactPopupConfig state = PopupWithCheckboxController.config {
  title = getString DEFAULT_CALL_CONTACT,
  primaryOptionBackground = Color.blue600,
  contactList = state.data.emergencyContactsList,
  primaryButtonConfig {visibility = GONE},
  checkBoxType = PopupWithCheckboxController.Radio,
  headerBackground = Color.white900,
  titleStyle = FontStyle.Tags,
  showDismissButton = false,
  headerPadding = Padding 16 16 16 0
}
