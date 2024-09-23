module Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.ComponentConfig where

import Prelude
import Data.String.CodeUnits (slice)
import Data.Array as DA
import Data.String as DS
import Components.ChooseVehicle.Controller as ChooseVehicle
import Components.GenericHeader.Controller as GenericHeader
import Components.PrimaryButton.Controller as PrimaryButton
import Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.Controller (Action(..), ScreenOutput, eval, validateInput)
import Storage (getValueToLocalStore, KeyStore(..))
import Common.Types.App as App
import Components.SeparatorView.View as SeparatorView
import Components.PrimaryEditText as PrimaryEditText
import Font.Style as FontStyle
import Engineering.Helpers.Commons as EHC
import Components.SourceToDestination.Controller as SourceToDestination
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.PopUpModal as PopUpModal
import Components.SelectListModal as CancelRidePopUpConfig
import Components.RateCard as RateCard
import ConfigProvider
import Data.Maybe (Maybe(..), isJust, maybe, fromMaybe, isNothing)
import Font.Style (Style(..))
import Helpers.Utils as HU
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), fontStyle)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Services.API as API
import Styles.Colors as Color
import Data.String.CodeUnits (slice)
import Services.API as API
import Data.Array as DA
import Data.String as DS
import PrestoDOM.Types.DomAttributes (Corners(..))
import Debug (spy)

primaryButtonConfig :: ST.ParcelDeliveryScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
          { text = (getString BOOK_FOR) <> " " <> state.data.parcelQuoteList.price
          , color = Color.yellow900
          , height = V 40
          }
      , gravity = CENTER
      , margin = (MarginHorizontal 16 16)
      , isClickable = true -- state.data.bookingId /= ""
      -- , alpha = if isJust state.data.selectedQuote then 1.0 else 0.5
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
        text = getString DELIVERY_DETAILS
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
      details = if isSenderModal then state.data.senderDetails else state.data.receiverDetails
      isSelected = if isSenderModal then state.data.initiatedAs == API.Sender else state.data.initiatedAs == API.Receiver
      checkBoxDetailsText = if isSenderModal then getString I_AM_THE_SENDER else getString I_AM_THE_RECEIVER
      isClickable = validateInput state
      address = decodeAddress' $ if isSenderModal then state.data.sourceAddress else state.data.destinationAddress
      addressArea = fromMaybe (fromMaybe "" ((DS.split (DS.Pattern ",") (address)) DA.!! 0)) $ if isSenderModal then state.data.sourceAddress.area else state.data.destinationAddress.area
      showUserNameAndNumber = state.props.showUserNameAndNumber
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
            , text = getString BACK
            , isClickable = true
            , enableRipple = true
            , margin = Margin 0 20 12 0
            , width = MATCH_PARENT
            }
          , option2
            { color = config.primaryTextColor
            , strokeColor = config.primaryBackground
            , background = config.primaryBackground
            , text = if state.props.isEditModal then getString CONFIRM else if isSenderModal then getString NEXT else getString CONFIRM
            , enableRipple = true
            , margin = Margin 0 20 0 0
            , width = MATCH_PARENT
            , isClickable = isClickable
            }
          , deliveryDetailsConfig { 
            visibility = VISIBLE,
            margin  = Margin 0 20 0 0,
            isSource = isSenderModal,
            personNameDetails = personNameDetailsConfig details isSenderModal showUserNameAndNumber,
            mobileNumberDetails = mobileNumberDetailsConfig details isSenderModal showUserNameAndNumber,
            addressDetails = addressDetailsConfig details isSenderModal,
            instructionDetails = instructionDetailsConfig details isSenderModal,
            checkBoxDetails = {text : checkBoxDetailsText , isSelected : isSelected, visibility : ((isSenderModal && state.data.initiatedAs == API.Sender) || (state.data.currentStage == ST.RECEIVER_DETAILS && state.data.initiatedAs == API.Receiver) || state.data.initiatedAs == API.SomeoneElse)},
            locationTitle = addressArea,
            locationDetails = address
            }
          }
    in
      popUpConfig'
  where
    personNameDetailsConfig :: ST.PersonDeliveryDetails -> Boolean -> Boolean -> PrimaryEditTextController.Config
    personNameDetailsConfig details isSender showUserNameAndNumber = 
      let config = PopUpModal.dummyDeliveryPrimaryText
          userName = getValueToLocalStore USER_NAME
      in
        config {
          editText { text = if showUserNameAndNumber then userName else details.name, placeholder = "Name", pattern = Just "[A-Za-z ]*,30" },
          topLabel { text = if isSender then getString SENDER_NAME else getString RECEIVER_NAME },
          textImage { visibility = VISIBLE },
          margin = Margin 0 0 0 8,
          id = (EHC.getNewIDWithTag $ "personName"),
          errorLabel { text = "Please enter a valid name" }
        }
    mobileNumberDetailsConfig :: ST.PersonDeliveryDetails -> Boolean -> Boolean -> PrimaryEditTextController.Config
    mobileNumberDetailsConfig details isSender showUserNameAndNumber = 
      let config = PopUpModal.dummyDeliveryPrimaryText
          phoneNo = getValueToLocalStore MOBILE_NUMBER
      in
        config {
          editText { text = if showUserNameAndNumber then phoneNo else details.phone, placeholder = "9999999999", pattern = Just ("[0-9]*,10") },
          topLabel { text = if isSender then getString SENDER_PHONE else getString RECEIVER_PHONE },
          textImage { visibility = VISIBLE } ,
          id = (EHC.getNewIDWithTag $ "mobileNumber")
          ,type = "number"
          , errorLabel { text = "Please enter a valid mobile number" }
        }
    addressDetailsConfig :: ST.PersonDeliveryDetails -> Boolean  -> PrimaryEditTextController.Config
    addressDetailsConfig details isSender = 
      let config = PopUpModal.dummyDeliveryPrimaryText
      in
        config {
          editText { text = details.extras, placeholder = "Apt #1, 1st Block, Locality", pattern = Just "[^\n]*,50" },
          topLabel { text = if isSender then getString SENDERS_BUILDING_FLAT else getString RECEIVERS_BUILDING_FLAT },
          id = (EHC.getNewIDWithTag $ "addressDetails" ),
          errorLabel { text = "Please enter a valid address" }
        }
    instructionDetailsConfig :: ST.PersonDeliveryDetails -> Boolean  -> PrimaryEditTextController.Config
    instructionDetailsConfig details isSender = 
      let config = PopUpModal.dummyDeliveryPrimaryText
      in
        config {
          height = (V 90),
          margin = MarginVertical 20 20,
          editText { text = fromMaybe "" details.instructions, placeholder = "Instruction", singleLine = false, pattern = Just "[^\n]*,100" },
          topLabel { text = if isSender then getString PICKUP_INSTRUCTIONS else getString DROP_INSTRUCTIONS },
          id = (EHC.getNewIDWithTag "instructionDetails")
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
      -- , editBtnText = getString CHANGE
      , validTill = selectedParcelEstimate.validTill
      , hasTollCharges = selectedParcelEstimate.hasTollCharges
      , hasParkingCharges = selectedParcelEstimate.hasParkingCharges
      , singleVehicle = true
      }
  in chooseVehicleConfig'

decodeAddress' :: ST.Address -> String
decodeAddress' address =
  if (DA.all isNothing [address.city, address.area, address.country, address.building, address.door, address.street, address.city, address.areaCode, address.ward]) then
      ""
    else if (DS.trim (fromMaybe "" address.city) == "" && DS.trim (fromMaybe "" address.area) == "" && DS.trim (fromMaybe "" address.street) == "" && DS.trim (fromMaybe "" address.door) == "" && DS.trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (DS.trim (fromMaybe "" address.area) == "" && DS.trim (fromMaybe "" address.street) == "" && DS.trim (fromMaybe "" address.door) == "" && DS.trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (DS.trim (fromMaybe "" address.street) == "" && DS.trim (fromMaybe "" address.door) == "" && DS.trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (DS.trim (fromMaybe "" address.door) == "" && DS.trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (DS.trim (fromMaybe "" address.door) == "") then
      ((fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else
      ((fromMaybe "" address.door) <> ", " <> (fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))

rateCardConfig :: ST.ParcelDeliveryScreenState -> RateCard.Config
rateCardConfig state =
  RateCard.config 
  { isNightShift = state.data.rateCard.isNightShift
    , currentRateCardType = state.data.rateCard.currentRateCardType
    , onFirstPage = state.data.rateCard.onFirstPage
    , showDetails = state.data.config.searchLocationConfig.showRateCardDetails
    , description = if state.data.rateCard.isNightShift then (getString $ NIGHT_TIME_CHARGES state.data.rateCard.nightChargeFrom state.data.rateCard.nightChargeTill) else (getString $ DAY_TIME_CHARGES state.data.rateCard.nightChargeTill state.data.rateCard.nightChargeFrom)
    , buttonText = Just if state.data.rateCard.currentRateCardType == App.DefaultRateCard then (getString GOT_IT) else (getString GO_BACK_)
    , title = getString RATE_CARD
    , fareList = state.data.rateCard.extraFare 
    , driverAdditions = state.data.rateCard.driverAdditions
    , otherOptions = otherOptions $ (not DA.null state.data.rateCard.driverAdditions) && state.data.config.searchLocationConfig.showDriverAdditions
    , fareInfoDescription = state.data.rateCard.fareInfoDescription
    , additionalStrings = 
        (if state.data.config.searchLocationConfig.showDriverAdditions then 
          [ {key : "DRIVER_ADDITIONS_OPTIONAL", val : getString DRIVER_ADDITIONS_OPTIONAL}
          , {key : "THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC", val : getString THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC}
          , {key : "DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE", val : getString DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE}
          ] 
        else []) 
      <> (if state.data.config.searchLocationConfig.showAdditionalChargesText then 
          [ {key : "TOLL_OR_PARKING_CHARGES", val : getString TOLL_OR_PARKING_CHARGES}
          , {key : "TOLL_CHARGES", val : getString TOLL_CHARGES}
          , {key : "TOLL_CHARGES_DESC", val : getString TOLL_CHARGES_DESC}
          , {key : "PARKING_CHARGE", val : getString PARKING_CHARGE}
          , {key : "PARKING_CHARGES_DESC", val : getString PARKING_CHARGES_DESC}
          ] 
          else []) 
      <> (if state.data.rateCard.serviceTierName == Just "Auto" && state.data.config.searchLocationConfig.showChargeDesc then [{key : "CHARGE_DESCRIPTION", val : (getString ERNAKULAM_LIMIT_CHARGE)}] else [])
      }
  where
  otherOptions :: Boolean -> Array App.FareList
  otherOptions showAdditions =
    (if showAdditions then [ { key: "DRIVER_ADDITIONS", val: (getString DRIVER_ADDITIONS) } ] else [])
    <> (if state.data.config.searchLocationConfig.showAdditionalChargesText then [{key : "TOLL_OR_PARKING_CHARGES", val : getString TOLL_OR_PARKING_CHARGES }]else [])