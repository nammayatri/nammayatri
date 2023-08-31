{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NammaSafetyScreen.ComponentConfig
  where

import Common.Types.App

import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Array (any, length, null)
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons (os)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getCommonAssetStoreLink)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (not, show, ($), (<>), (==), (>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), padding, textFromHtml, visibility)
import Screens.Types (NammaSafetyScreenState, Stage(..))
import Styles.Colors as Color

startNSOnboardingButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
startNSOnboardingButtonConfig state = 
  PrimaryButton.config
    { textConfig{ text = (getButtonString state.props.currentStage) }
    , isClickable = true
    , visibility = if state.props.showOnboarding == true then VISIBLE else GONE
    , margin = (Margin 16 0 16 0 )
    }
  
skipNSOnboardingButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
skipNSOnboardingButtonConfig state = 
  PrimaryButton.config
    { textConfig{ text = (getString SKIP) , color = Color.black700}
    , isClickable = true
    , margin = (Margin 16 8 16 0 )
    , background = Color.white900
    , stroke = ("1," <> Color.black700)
    }
continueNextStepButtonConfig:: NammaSafetyScreenState -> PrimaryButton.Config
continueNextStepButtonConfig state = 
  PrimaryButton.config
    { textConfig{ text = if state.props.currentStage == SetPersonalSafetySettings then "Finish Setup" else "Continue" }
    , isClickable = true
    , visibility = if state.props.showOnboarding == true then VISIBLE else GONE
    , margin = (Margin 16 0 16 0 )
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
    { textConfig{ text = "Mark Ride As Safe" , color = Color.black900
    }
    , isClickable = true
    , height = MATCH_PARENT
    , width = MATCH_PARENT
    , margin = (MarginTop 20)
    , gravity = CENTER
    , background = Color.white900
    }



getButtonString :: Stage -> String
getButtonString stage = case stage of
  SetTriggerCustomerSupport ->  "Add Emergency Contacts"
  SetPersonalSafetySettings ->  "Finish Setup"
  _ ->  "Start Namma Safety Setup"

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
    { textConfig{ text = "Activate SOS" , color = Color.black900}
    , isClickable = true
    , margin = (Margin 16 0 16 16 )
    , background = Color.white900
    }

dismissSoSButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
dismissSoSButtonConfig state = 
  PrimaryButton.config
    { textConfig{ text = "Dismiss" , color = Color.white900}
    , isClickable = true
    , margin = (Margin 16 8 16 0 )
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
          , color = if (count > 0) then Color.yellow900 else Color.yellow800
          }
        , background = if (count > 0) then Color.black900 else Color.black600
        , isClickable = if (count > 0) then true else false
        }
  in
    primaryButtonConfig'

-- genericHeaderConfig :: NammaSafetyScreenState -> GenericHeader.Config
-- genericHeaderConfig state =
--   let
--     config = GenericHeader.config

--     genericHeaderConfig' =
--       config
--         { height = WRAP_CONTENT
--         , prefixImageConfig
--           { height = V 25
--           , width = V 25
--           , imageUrl = "ny_ic_chevron_left," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_left.png"
--           , margin = (Margin 12 12 12 12)
--           }
--         , padding = (Padding 0 5 0 5)
--         , textConfig
--           { text = if state.props.emergencyContactsProps.showContactList then (show (length state.data.emergencyContactsData.contactsList) <> "/3 " <> (getString CONTACTS_SELECTED)) else  (getString EMERGENCY_CONTACTS)
--           , color = Color.darkDescriptionText
--           }
--         , suffixImageConfig
--           { visibility = GONE
--           }
--         }
--   in
--     genericHeaderConfig'

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
        , width = if os == "IOS" then (V 360) else (MATCH_PARENT)
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
        , buttonLayoutMargin = MarginBottom if os == "IOS" then 0 else 24
        }
  in
    popUpConfig'

  
