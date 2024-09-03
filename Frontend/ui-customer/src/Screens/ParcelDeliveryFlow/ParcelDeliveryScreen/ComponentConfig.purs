module Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.ComponentConfig where

import Prelude
import Components.GenericHeader.Controller as GenericHeader
import Components.PrimaryButton.Controller as PrimaryButton
import Components.SeparatorView.View as SeparatorView
import Components.SourceToDestination.Controller as SourceToDestination
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.PopUpModal as PopUpModal
import Components.SelectListModal as CancelRidePopUpConfig
import ConfigProvider
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Font.Style (Style(..))
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..))
import Screens.Types as ST
import Styles.Colors as Color
import Services.API as API
import Data.Array as DA
import Data.String as DS
import PrestoDOM.Types.DomAttributes (Corners(..))

primaryButtonConfig :: ST.ParcelDeliveryScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
          { text = "Book For" 
          , color = Color.yellow900
          , height = V 40
          }
      , gravity = CENTER
      , margin = (MarginHorizontal 16 16)
      , isClickable = true -- state.data.bookingId /= ""
      -- , alpha = if state.data.bookingId == "" then 0.5 else 1.0
      , id = "GoHomeButton"
      , enableRipple = true
      , rippleColor = Color.rippleShade
      }
  in
    primaryButtonConfig'

genericHeaderConfig :: ST.ParcelDeliveryScreenState -> GenericHeader.Config
genericHeaderConfig _state = let
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , width = WRAP_CONTENT
    , background = Color.white900
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , margin = (Margin 12 12 12 12)
      }
    , textConfig {
        text = "Delivery Details"
      , color = Color.black800
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , padding = (Padding 0 5 0 0)
    }
  in genericHeaderConfig'


deliveryPickupDetialsModalConfig :: ST.ParcelDeliveryScreenState -> PopUpModal.Config
deliveryPickupDetialsModalConfig state = 
  let
      config' = PopUpModal.config
      config = getAppConfig appConfig
      isSenderModal = state.data.currentStage == ST.SENDER_DETAILS
      (API.DeliveryDetails deliveryDetails) = state.data.deliveryDetailsInfo
      (API.PersonLocationAndInstruction details) = if isSenderModal then deliveryDetails.senderDetails else deliveryDetails.receiverDetails
      (API.InstructionAndAddress address) = details.address
      isSelected = if isSenderModal then deliveryDetails.initiatedAs == API.Sender else deliveryDetails.initiatedAs == API.Receiver
      checkBoxDetailsText = if isSenderModal then getString I_AM_THE_SENDER else getString I_AM_THE_RECEIVER
      isClickable = details.name /= "" && details.phoneNumber /= "" && address.extras /= ""
      popUpConfig' =
        config'
          { primaryText { 
              text = (if isSenderModal then getString SENDER else getString RECEIVER) <> " " <> getString DETAILS 
            , gravity = LEFT
            , padding = (Padding 0 0 0 0)
            , margin = (Margin 0 0 0 0)
            }
          , step { 
            text = if isSenderModal then "Step 1/2" else "Step 2/2"
          , visibility = VISIBLE
          }
          , backgroundClickable = false
          , dismissPopup = false
          , padding = (Padding 24 16 12 16)
          , secondaryText { 
            text = if isSenderModal then getString HELP_US_PROVIDE_SMOOTH_PICKUP else getString HELP_US_PROVIDE_SMOOTH_DROP 
          , gravity = LEFT
          , padding = (Padding 0 0 0 0)
          , margin = (Margin 0 4 0 0)
          }
          , option1
            { background = config.popupBackground
            , strokeColor = config.primaryBackground
            , color = config.primaryBackground
            , text = "Back"
            , isClickable = not isSenderModal
            , enableRipple = true
            , margin = Margin 0 20 12 0
            , width = (V 158)
            }
          , option2
            { color = config.primaryTextColor
            , strokeColor = config.primaryBackground
            , background = config.primaryBackground
            , text = if isSenderModal then "Next" else "Confirm" 
            , enableRipple = true
            , margin = Margin 0 20 0 0
            , width = (V 158)
            , isClickable = isClickable
            }
          , deliveryDetailsConfig { 
            visibility = VISIBLE,
            margin  = Margin 0 20 0 0,
            personNameDetails = personNameDetailsConfig (API.PersonLocationAndInstruction details) isSenderModal,
            mobileNumberDetails = mobileNumberDetailsConfig (API.PersonLocationAndInstruction details) isSenderModal,
            addressDetails = addressDetailsConfig (API.PersonLocationAndInstruction details) isSenderModal,
            instructionDetails = instructionDetailsConfig (API.PersonLocationAndInstruction details) isSenderModal,
            checkBoxDetails = {text : checkBoxDetailsText , isSelected : isSelected}
            }
          }
    in
      popUpConfig'
  where
    personNameDetailsConfig :: API.PersonLocationAndInstruction -> Boolean -> PrimaryEditTextController.Config
    personNameDetailsConfig details' isSender = 
      let config = PopUpModal.dummyDeliveryPrimaryText
          (API.PersonLocationAndInstruction details) = details'
      in
        config {
          editText { text = details.name, placeholder = "Name" },
          topLabel { text = if isSender then getString SENDER else getString RECEIVER <> " " <> getString NAME <> "*" },
          textImage { visibility = VISIBLE },
          margin = Margin 0 0 0 8
        }
    mobileNumberDetailsConfig :: API.PersonLocationAndInstruction -> Boolean  -> PrimaryEditTextController.Config
    mobileNumberDetailsConfig details' isSender = 
      let config = PopUpModal.dummyDeliveryPrimaryText
          (API.PersonLocationAndInstruction details) = details'
      in
        config {
          editText { text = details.phoneNumber, placeholder = "9999999999", pattern = Just ("[0-9]*,10") },
          topLabel { text = getString $ PHONE $ if isSender then getString SENDER else getString RECEIVER <> "*"  },
          textImage { visibility = VISIBLE } 
        }
    addressDetailsConfig :: API.PersonLocationAndInstruction -> Boolean  -> PrimaryEditTextController.Config
    addressDetailsConfig details' isSender = 
      let config = PopUpModal.dummyDeliveryPrimaryText
          (API.PersonLocationAndInstruction details) = details'
          (API.InstructionAndAddress address) = details.address
      in
        config {
          editText { text = address.extras, placeholder = "Apt #1, 1st Block, Locality" },
          topLabel { text = getString $ BUILDING_OR_FLAT $ (if isSender then getString SENDER else getString RECEIVER) <> "*"  }
        }
    instructionDetailsConfig :: API.PersonLocationAndInstruction -> Boolean  -> PrimaryEditTextController.Config
    instructionDetailsConfig details' isSender = 
      let config = PopUpModal.dummyDeliveryPrimaryText
          (API.PersonLocationAndInstruction details) = details'
          (API.InstructionAndAddress address) = details.address
      in
        config {
          height = (V 90),
          editText { text = fromMaybe "" address.instruction, placeholder = "Instruction", singleLine = false },
          topLabel { text =if isSender then getString PICKUP else getString DROP  <> " " <> getString OPTIONAL_INSTRUCTION <> "*" }
        }