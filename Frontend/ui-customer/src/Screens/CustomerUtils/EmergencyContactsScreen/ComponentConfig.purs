module Screens.CustomerUtils.EmergencyContactsScreen.ComponentConfig where

import Common.Types.App

import Components.DropDownWithHeader as DropDownWithHeader
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Data.Array (length, null)
import Data.Show (show)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Prelude (not, (<>), (==), ($), (&&), (>), (<), (||))
import PrestoDOM (Length(..), Margin(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, height, margin, padding, text, textSize, width, imageUrl, visibility, stroke)
import Screens.EmergencyContactsScreen.ScreenData as EMData
import Screens.Types (EmergencyContactsScreenState, DropDownWithHeaderConfig, NewContacts)
import Styles.Colors as Color
import Components.PrimaryEditText as PrimaryEditText
import Storage (KeyStore(..), getValueToLocalStore)

--------------------------------------------------- genericHeaderConfig -----------------------------------------------------
genericHeaderConfig :: EmergencyContactsScreenState -> GenericHeader.Config
genericHeaderConfig state =
  let
    config = GenericHeader.config
    titleText = case null state.data.emergencyContactsList, state.props.showContactList of 
                    _, true -> show (length state.data.selectedContacts) <> "/3 " <> (getString CONTACTS_SELECTED)
                    true, false -> getString TRUSTED_CONTACT
                    false, false -> getString TRUSTED_CONTACT
    genericHeaderConfig' =
      config
        { height = WRAP_CONTENT
        , prefixImageConfig
          { height = V 25
          , width = V 25
          , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
          , margin = Margin 8 8 8 8 
          , layoutMargin = Margin 4 4 4 4
          , enableRipple = true
          }
        , padding = (Padding 0 5 0 5)
        , textConfig
          { text = titleText
          , accessibilityHint = if state.props.showContactList then (show (length state.data.emergencyContactsList) <> " Of 3 " <> (getString CONTACTS_SELECTED)) else  (getString TRUSTED_CONTACT)
          , color = Color.darkCharcoal
          }
        , suffixImageConfig
          { visibility = GONE
          }
        }
  in
    genericHeaderConfig'

primaryEditTextConfig :: EmergencyContactsScreenState -> PrimaryEditText.Config
primaryEditTextConfig state = let
    config = PrimaryEditText.config
    isOwnNumber = state.data.manualContactNumber == getValueToLocalStore MOBILE_NUMBER
    errorMessage = if isOwnNumber 
                   then getString CANNOT_ADD_OWN_NUMBER
                   else getString INVALID_MOBILE_NUMBER
    primaryEditTextConfig' = config
      { editText
        { color = Color.black800
        , singleLine = true
        -- , placeholder = "9999......"
        , textStyle = FontStyle.SubHeading3
        , pattern = Just "[0-9]*,10"
        , accessibilityHint = "Enter Number"
        -- , text = state.data.
        }
      , background = Color.white900
      , topLabel
        { text = "Enter Number"
        , color = Color.black800
        , textStyle = FontStyle.Body3
        }
      , stroke = ("1,"<> Color.black500)
      , type = "number"
      , margin = (Margin 16 16 16 0)
      , id = (EHC.getNewIDWithTag "TrustedNumberPET")
      , errorLabel
        { text = errorMessage
        , margin = (MarginTop 1)
        }
      , showErrorLabel = not state.props.validManualContact || isOwnNumber
      , width = MATCH_PARENT
      }
    in primaryEditTextConfig'

primaryEditTextConfigName :: EmergencyContactsScreenState -> PrimaryEditText.Config
primaryEditTextConfigName state = let
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { color = Color.black800
        , singleLine = true
        -- , placeholder = "Enter Name"
        , textStyle = FontStyle.SubHeading3
        , pattern = Just "[a-zA-Z0-9'‘’. ]*,30"
        , accessibilityHint = "Enter Name"
        -- , text = state.data.placeName
        }
      , background = Color.white900
      , type = "text"
      , topLabel
        { text = "Enter Name"
        , color = Color.black800
        , textStyle = FontStyle.Body3
        }
      , stroke = ("1,"<> Color.black500)
      , margin = (Margin 16 16 16 16)
      , id = (EHC.getNewIDWithTag "TrustedNamePET")
      , errorLabel
        { text = (getString INVALID_MOBILE_NUMBER)
        , margin = (MarginTop 1)
        }
      , showErrorLabel = false
      , width = MATCH_PARENT
      }
    in primaryEditTextConfig'

primaryButtonConfigManualContactDummy :: EmergencyContactsScreenState -> PrimaryButton.Config
primaryButtonConfigManualContactDummy state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' =
      config
        { textConfig
          { text = (getString SAVE)
          -- , accessibilityHint = (if null state.data.selectedContacts then "Add Contacts" else if conditionForPrimaryButtonText then "Next" else if defaultContactCondition then "Done" else (getString CONFIRM_EMERGENCY_CONTACTS)) <> " : Button"
          }
        , isClickable = false
        , width = (MATCH_PARENT)
        , margin = (Margin 16 16 16 16)
        , id = "ConfirmEmergencyContactsButtonDumm"
        , enableRipple = true
        , rippleColor = Color.rippleShade
        , visibility = INVISIBLE
        }
  in
    primaryButtonConfig'

primaryButtonConfigManualContact :: EmergencyContactsScreenState -> PrimaryButton.Config
primaryButtonConfigManualContact state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' =
      config
        { textConfig
          { text = (getString SAVE)
          -- , accessibilityHint = (if null state.data.selectedContacts then "Add Contacts" else if conditionForPrimaryButtonText then "Next" else if defaultContactCondition then "Done" else (getString CONFIRM_EMERGENCY_CONTACTS)) <> " : Button"
          }
        , isClickable = state.props.validManualContact
        , width = (MATCH_PARENT)
        , margin = (Margin 16 16 16 16)
        , id = "ConfirmEmergencyContactsButtonManual"
        , enableRipple = true
        , rippleColor = Color.rippleShade
        }
  in
    primaryButtonConfig'

--------------------------------------------------- primaryButtonConfig -----------------------------------------------------
primaryButtonConfig :: EmergencyContactsScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config
    conditionForPrimaryButtonText = state.props.saveEmergencyContacts && not state.props.getDefaultContacts && length state.data.selectedContacts > 0
    defaultContactCondition = state.props.getDefaultContacts
    primaryButtonConfig' =
      config
        { textConfig
          { text = if null state.data.selectedContacts then (getString ADD_CONTACTS) else if conditionForPrimaryButtonText then (getString NEXT) else if defaultContactCondition then (getString DONE) else (getString CONFIRM_EMERGENCY_CONTACTS)
          , accessibilityHint = (if null state.data.selectedContacts then "Add Contacts" else if conditionForPrimaryButtonText then "Next" else if defaultContactCondition then "Done" else (getString CONFIRM_EMERGENCY_CONTACTS)) <> " : Button"
          }
        , isClickable = true
        , width = (MATCH_PARENT)
        , margin = (MarginBottom 0)
        , id = "ConfirmEmergencyContactsButton"
        , enableRipple = true
        , rippleColor = Color.rippleShade
        }
  in
    primaryButtonConfig'


dropDownWithHeaderConfig :: EmergencyContactsScreenState -> NewContacts -> DropDownWithHeader.Config
dropDownWithHeaderConfig state contact =
  let
    listVisibile = state.data.selectedContact.number == contact.number && state.props.showDropDown
    dropDownWithHeaderConfig' =
      DropDownWithHeader.config
        { listVisibility = boolToVisibility listVisibile
        , headerText = ""
        , selectedValue = contact.shareTripWithEmergencyContactOption
        , boxPadding = Padding 0 0 0 0
        , boxBackground = Color.white900
        , margin = if listVisibile then MarginVertical 12 8 else MarginTop 12
        , selectedContact = contact
        , dropDownOptions = [EMData.alwaysShareRideOptionEM, EMData.shareWithTimeContraintsRideOptionEM, EMData.neverShareRideOptionEM]
        }
  in
    dropDownWithHeaderConfig'

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
        , buttonLayoutMargin = MarginBottom if EHC.os == "IOS" then 0 else 24
        }
  in
    popUpConfig'
