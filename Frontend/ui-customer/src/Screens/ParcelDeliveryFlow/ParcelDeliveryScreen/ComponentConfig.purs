module Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.ComponentConfig where

import Prelude
import Data.String.CodeUnits (slice)
import Data.Array as DA
import Data.String as DS
import Components.ChooseVehicle.Controller as ChooseVehicle
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
import Helpers.Utils as HU
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Services.API as API
import Styles.Colors as Color

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
      , imageUrl = HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_chevron_left"
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
      (API.PersonLocationAndInstruction editDetails) = state.props.editDetails
      (API.InstructionAndAddress editAddress) = editDetails.address
      isSelected = if isSenderModal then deliveryDetails.initiatedAs == API.Sender else deliveryDetails.initiatedAs == API.Receiver
      checkBoxDetailsText = if isSenderModal then getString I_AM_THE_SENDER else getString I_AM_THE_RECEIVER
      isClickable = validateInput state.props.editDetails
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
            , isClickable = true
            , enableRipple = true
            , margin = Margin 0 20 12 0
            , width = MATCH_PARENT
            }
          , option2
            { color = config.primaryTextColor
            , strokeColor = config.primaryBackground
            , background = config.primaryBackground
            , text = if isSenderModal then "Next" else "Confirm" 
            , enableRipple = true
            , margin = Margin 0 20 0 0
            , width = MATCH_PARENT
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
    validateInput :: API.PersonLocationAndInstruction -> Boolean
    validateInput (API.PersonLocationAndInstruction details) = 
      let
        (API.InstructionAndAddress address) = details.address
        firstLetter = slice 0 1 details.phoneNumber
        isValidNumber = firstLetter >= "6" && firstLetter <= "9"
      in
        details.name /= "" && DS.length details.phoneNumber == 10 && isValidNumber && address.extras /= ""
    personNameDetailsConfig :: API.PersonLocationAndInstruction -> Boolean -> PrimaryEditTextController.Config
    personNameDetailsConfig details' isSender = 
      let config = PopUpModal.dummyDeliveryPrimaryText
          (API.PersonLocationAndInstruction details) = details'
      in
        config {
          editText { text = details.name, placeholder = "Name", pattern = Just "[^\n]*,40" },
          topLabel { text = (if isSender then getString SENDER else getString RECEIVER) <> " " <> getString NAME },
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
          topLabel { text = (getString $ PHONE $ if isSender then getString SENDER else getString RECEIVER) },
          textImage { visibility = VISIBLE } 
        }
    addressDetailsConfig :: API.PersonLocationAndInstruction -> Boolean  -> PrimaryEditTextController.Config
    addressDetailsConfig details' isSender = 
      let config = PopUpModal.dummyDeliveryPrimaryText
          (API.PersonLocationAndInstruction details) = details'
          (API.InstructionAndAddress address) = details.address
      in
        config {
          editText { text = address.extras, placeholder = "Apt #1, 1st Block, Locality", pattern = Just "[^\n]*,50" },
          topLabel { text = (getString $ BUILDING_OR_FLAT $ if isSender then getString SENDER else getString RECEIVER) <> "*"  }
        }
    instructionDetailsConfig :: API.PersonLocationAndInstruction -> Boolean  -> PrimaryEditTextController.Config
    instructionDetailsConfig details' isSender = 
      let config = PopUpModal.dummyDeliveryPrimaryText
          (API.PersonLocationAndInstruction details) = details'
          (API.InstructionAndAddress address) = details.address
      in
        config {
          height = (V 90),
          editText { text = fromMaybe "" address.instruction, placeholder = "Instruction", singleLine = false, pattern = Just "[^\n]*,100" },
          topLabel { text = (if isSender then getString PICKUP else getString DROP  <> " " <> getString OPTIONAL_INSTRUCTION) }
        }
        
chooseVehicleConfig :: ST.ParcelDeliveryScreenState -> ChooseVehicle.Config
chooseVehicleConfig state =
  let
    config = ChooseVehicle.config
    selectedParcelEstimate = state.data.parcelQuoteList
    chooseVehicleConfig' = config
      { vehicleImage = HU.getVehicleVariantImage selectedParcelEstimate.vehicleVariant ST.RIGHT_VIEW
      , isSelected = true
      , vehicleVariant = selectedParcelEstimate.vehicleVariant
      , vehicleType = selectedParcelEstimate.vehicleType
      , capacity = selectedParcelEstimate.capacity
      , price = selectedParcelEstimate.price
      , isCheckBox = false
      , isEnabled = true
      , index = selectedParcelEstimate.index
      , activeIndex = selectedParcelEstimate.activeIndex
      , id = selectedParcelEstimate.id
      , maxPrice = selectedParcelEstimate.maxPrice
      , basePrice = selectedParcelEstimate.basePrice
      , showInfo = selectedParcelEstimate.showInfo
      , searchResultType = selectedParcelEstimate.searchResultType
      , isBookingOption = false
      , pickUpCharges = selectedParcelEstimate.pickUpCharges 
      , layoutMargin = Margin 16 0 16 0
      , tollCharge = selectedParcelEstimate.tollCharge
      , serviceTierName = selectedParcelEstimate.serviceTierName
      , serviceTierShortDesc = selectedParcelEstimate.serviceTierShortDesc
      , airConditioned = selectedParcelEstimate.airConditioned
      , extraFare = selectedParcelEstimate.extraFare
      , fareInfoDescription = selectedParcelEstimate.fareInfoDescription
      , isNightShift = selectedParcelEstimate.isNightShift
      , nightChargeTill = selectedParcelEstimate.nightChargeTill
      , nightChargeFrom = selectedParcelEstimate.nightChargeFrom
      , driverAdditions = selectedParcelEstimate.driverAdditions
      , showEditButton = true
      -- , editBtnText = getString CHANGE
      , validTill = selectedParcelEstimate.validTill
      , hasTollCharges = selectedParcelEstimate.hasTollCharges
      , hasParkingCharges = selectedParcelEstimate.hasParkingCharges
      , singleVehicle = true
      }
  in chooseVehicleConfig'
