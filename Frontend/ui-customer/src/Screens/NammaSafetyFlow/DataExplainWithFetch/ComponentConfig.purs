{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DataExplainWithFetch.ComponentConfig where

import Components.PopUpModal as PopUpModal
import Components.PopupWithCheckbox.Controller as PopupWithCheckboxController
import Components.PrimaryButton as PrimaryButton
import Components.GenericHeader as GenericHeader
import Engineering.Helpers.Commons as EHC
import Mobility.Prelude (boolToVisibility)
import Prelude (map, not, ($), (<>), (==), (&&), (/=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Services.API (RideShareOptions(..))
import Styles.Colors as Color
import Components.SourceToDestination as SourceToDestination
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (find)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..), Gravity(..))
import Components.BoxContainer as BoxContainer
import Components.DropDownWithHeader as DropDownWithHeader
import Components.PrimaryButton as PrimaryButton
import Components.InfoBox as InfoBox
import Screens.DataExplainWithFetch.Controller as DC
import Debug
import Screens.EmergencyContactsScreen.ScreenData (getRideOptionFromKey)

genericHeaderConfig :: ST.DataFetchScreenState -> GenericHeader.Config
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
          { text = state.data.headerValue
          , color = Color.darkCharcoal
          }
        , suffixImageConfig
          { visibility = GONE
          }
        , visibility = VISIBLE
        }
  in
    genericHeaderConfig'

primaryButtonConfig :: ST.DataFetchScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config

    screenStageConfig = DC.getStepConfig state

    primaryButtonConfig' =
      config
        { textConfig { text = if state.props.stageSetUpCompleted && screenStageConfig.primaryButtonAction /= "SafetyTestDrill" then (getString DONE) else screenStageConfig.primaryButtonText }
        , width = MATCH_PARENT
        , cornerRadius = 12.0
        , margin = (Margin 16 16 16 16)
        , id = "DataFetchScreenPrimaryButton"
        }
  in
    primaryButtonConfig'

boxContainerConfig :: ST.DataFetchScreenState -> ST.BoxContainerConfig -> String -> BoxContainer.Config
boxContainerConfig state config action =
  let
    screenStageConfig = DC.getStepConfig state
    toggleButtonConfig = case action of
      "UpdateUnExpectedEventChecks" -> getBooleanFromOptions state.data.unExpectedEventChecks
      "PostRideCheck" -> getBooleanFromOptions state.data.postRideCheck
      "NotifySafetyTeam" -> state.data.notifySafetyTeam
      "EmergencySOSShake" -> state.data.emergencySOSShake
      "AutomaticCallOnEmergency" -> state.data.autoCallDefaultContact
      _ -> false

    boxContainerConfig' =
      BoxContainer.config
        { title = config.title
        , subTitle = config.subTitle
        , noteText = config.noteText
        , noteImage = config.noteImageIcon
        , toggleButton = toggleButtonConfig
        , buttonAction = action
        }
  in
    boxContainerConfig'

getBooleanFromOptions :: RideShareOptions -> Boolean
getBooleanFromOptions options = case options of
  NEVER_SHARE -> false
  SHARE_WITH_TIME_CONSTRAINTS -> true
  ALWAYS_SHARE -> true

dropDownWithHeaderConfig :: ST.DataFetchScreenState -> ST.DropDownWithHeaderConfig -> String -> DropDownWithHeader.Config
dropDownWithHeaderConfig state config action =
  let
    screenStageConfig = DC.getStepConfig state

    selectedValueDropDown = case action of
      "UpdateUnExpectedEventChecks" -> state.data.unExpectedEventChecks
      "PostRideCheck" -> state.data.postRideCheck
      _ -> NEVER_SHARE

    dropDownWithHeaderConfig' =
      DropDownWithHeader.config
        { listVisibility = boolToVisibility $ state.props.dropDownAction == action && state.props.showDropDown
        , headerText = config.headerText
        , dropDownOptions = config.dropDownItems
        , selectedValue = getRideOptionFromKey selectedValueDropDown
        , dropDownAction = action
        }
  in
    dropDownWithHeaderConfig'

infoBoxConfig :: ST.DataFetchScreenState -> ST.NoteBoxConfig -> String -> InfoBox.Config
infoBoxConfig state config action =
  let
    defaultContact = fromMaybe state.data.defaultSelectedContact $ find (\contact -> contact.priority == 0) state.data.emergencyContactsList
    infoBoxConfig' =
      InfoBox.config
        { text = if action == "AutomaticCallOnEmergency" then defaultContact.name else config.noteText
        , icon = config.noteImageIcon
        , subTitle = config.noteSubTitle
        , backgroundColor = Color.blue600
        , margin = Margin 16 16 16 0
        , titleColor = if action == "SafetyTestDrill" then Color.blue800 else Color.black900
        , disabled = if action == "AutomaticCallOnEmergency" && not state.data.autoCallDefaultContact then true else false
        }
  in
    infoBoxConfig'
