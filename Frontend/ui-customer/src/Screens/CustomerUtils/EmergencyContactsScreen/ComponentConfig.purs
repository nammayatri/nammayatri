module Screens.CustomerUtils.EmergencyContactsScreen.ComponentConfig where

import Common.Types.App

import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Data.Array (length, null)
import Effect (Effect)
import Engineering.Helpers.Commons (os)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((<>), (==))
import PrestoDOM (Length(..), Margin(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, height, margin, padding, text, textSize, width, imageUrl, visibility, stroke)
import Screens.Types (EmergencyContactsScreenState)
import Styles.Colors as Color
import Data.Show (show)
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)

--------------------------------------------------- genericHeaderConfig -----------------------------------------------------
genericHeaderConfig :: EmergencyContactsScreenState -> GenericHeader.Config
genericHeaderConfig state =
  let
    config = GenericHeader.config

    genericHeaderConfig' =
      config
        { height = WRAP_CONTENT
        , prefixImageConfig
          { height = V 25
          , width = V 25
          , imageUrl = "ny_ic_chevron_left," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_left.png"
          , margin = (Margin 12 12 12 12)
          }
        , padding = (Padding 0 5 0 5)
        , textConfig
          { text = if state.props.showContactList then (show (length state.data.contactsList) <> "/3 " <> (getString CONTACTS_SELECTED)) else  (getString EMERGENCY_CONTACTS)
          , accessibilityHint = if state.props.showContactList then (show (length state.data.contactsList) <> " Of 3 " <> (getString CONTACTS_SELECTED)) else  (getString EMERGENCY_CONTACTS)
          , color = Color.darkDescriptionText
          }
        , suffixImageConfig
          { visibility = GONE
          }
        }
  in
    genericHeaderConfig'

--------------------------------------------------- primaryButtonConfig -----------------------------------------------------
primaryButtonConfig :: EmergencyContactsScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { textConfig
          { text = if null state.data.contactsList then (getString ADD_EMERGENCY_CONTACTS) else (getString ADD_ANOTHER_CONTACT)
          , accessibilityHint = (if null state.data.contactsList then (getString ADD_EMERGENCY_CONTACTS) else (getString ADD_ANOTHER_CONTACT)) <> " : Button"
          }
        , isClickable = true
        , width = if os == "IOS" then (V 360) else (MATCH_PARENT)
        , margin = (MarginBottom 24)
        , visibility = if ((length state.data.contactsList) == 3) then GONE else VISIBLE
        }
  in
    primaryButtonConfig'


--------------------------------------------------- removeContactPopUpModelConfig -----------------------------------------------------
removeContactPopUpModelConfig :: EmergencyContactsScreenState -> PopUpModal.Config
removeContactPopUpModelConfig state =
  let
    config' = PopUpModal.config

    popUpConfig' =
      config'
        { primaryText { text = (getString REMOVE) <> " " <> state.data.removedContactDetail.name }
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
