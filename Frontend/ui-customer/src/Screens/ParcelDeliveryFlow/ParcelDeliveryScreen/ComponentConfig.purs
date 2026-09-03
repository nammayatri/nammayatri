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
import Data.Foldable as DF
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
          { text = if state.data.currentStage == ST.DELIVERY_INSTRUCTIONS then getString SEND_NOW else getString BOOK_FOR <> " " <> state.data.parcelQuoteList.price
          , color = Color.yellow900
          , height = V 40
          }
      -- , gravity = CENTER
      , margin = (Margin 16 10 16 10)
      , isClickable = state.data.currentStage == ST.FINAL_DETAILS || state.data.currentStage == ST.DELIVERY_INSTRUCTIONS
      , id = (getString $ if state.data.currentStage == ST.DELIVERY_INSTRUCTIONS then SEND_NOW else BOOK_FOR) <> "PrimaryButton"
      , enableRipple = true
      , rippleColor = Color.rippleShade
      }
  in
    primaryButtonConfig'

genericHeaderConfig :: ST.ParcelDeliveryScreenState -> GenericHeader.Config
genericHeaderConfig state = let
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
        text = getString $ if state.data.currentStage == ST.DELIVERY_INSTRUCTIONS then DELIVERY_STR else DELIVERY_DETAILS
      , color = Color.black800
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , padding = (Padding 0 5 0 0)
    }
  in genericHeaderConfig'

getRoleConfig :: ST.ParcelDeliveryScreenState -> { details :: ST.PersonDeliveryDetails, checkBoxDetailsText :: String, headerText :: String, stepInfo :: String, helpText :: String, address :: ST.Address, addressArea :: Maybe String, isSelected :: Boolean, personNameLabel :: String, phoneLabel :: String, addressLabel :: String, instructionLabel :: String }
getRoleConfig state = 
  if state.data.currentStage == ST.SENDER_DETAILS
  then 
    { details: state.data.senderDetails
    , checkBoxDetailsText: getString I_AM_THE_SENDER
    , headerText: getString SENDER <> " " <> getString DETAILS
    , stepInfo: getString STEP <> "1/2"
    , helpText: getString HELP_US_PROVIDE_SMOOTH_PICKUP
    , address: state.data.sourceAddress
    , addressArea: state.data.sourceAddress.area
    , isSelected: state.data.initiatedAs == API.Sender
    , personNameLabel: getString SENDER_NAME
    , phoneLabel: getString SENDER_PHONE
    , addressLabel: getString SENDERS_BUILDING_FLAT
    , instructionLabel: getString PICKUP_INSTRUCTIONS
    }
  else
    { details: state.data.receiverDetails
    , checkBoxDetailsText: getString I_AM_THE_RECEIVER
    , headerText: getString RECEIVER <> " " <> getString DETAILS
    , stepInfo: getString STEP <> "2/2"
    , helpText: getString HELP_US_PROVIDE_SMOOTH_DROP
    , address: state.data.destinationAddress
    , addressArea: state.data.destinationAddress.area
    , isSelected: state.data.initiatedAs == API.Receiver
    , personNameLabel: getString RECEIVER_NAME
    , phoneLabel: getString RECEIVER_PHONE
    , addressLabel: getString RECEIVERS_BUILDING_FLAT
    , instructionLabel: getString DROP_INSTRUCTIONS
    }

deliveryPickupDetialsModalConfig :: ST.ParcelDeliveryScreenState -> PopUpModal.Config
deliveryPickupDetialsModalConfig state =
  let
    config' = PopUpModal.config
    appConfig' = getAppConfig appConfig

    roleConfig = getRoleConfig state

    decodedAddress = decodeAddress' roleConfig.address
    addressArea = fromMaybe (fromMaybe "" $ DS.split (DS.Pattern ",") decodedAddress DA.!! 0) roleConfig.addressArea

    popUpConfig' =
      config'
        { primaryText
          { text = roleConfig.headerText
          , gravity = LEFT
          , padding = Padding 0 0 0 0
          , margin = Margin 0 0 0 0
          }
        , headerInfo
          { text = roleConfig.stepInfo
          , visibility = VISIBLE
          }
        , backgroundClickable = false
        , dismissPopup = false
        , padding = Padding 24 16 12 16
        , secondaryText
          { text = roleConfig.helpText
          , gravity = LEFT
          , padding = Padding 0 0 0 0
          , margin = Margin 0 4 0 0
          }
        , option1
          { background = appConfig'.popupBackground
          , strokeColor = appConfig'.primaryBackground
          , color = appConfig'.primaryBackground
          , text = getString BACK
          , isClickable = true
          , enableRipple = true
          , margin = Margin 0 20 12 0
          , width = MATCH_PARENT
          }
        , option2
          { color = appConfig'.primaryTextColor
          , strokeColor = appConfig'.primaryBackground
          , background = appConfig'.primaryBackground
          , text = if state.props.isEditModal then getString CONFIRM else if state.data.currentStage == ST.SENDER_DETAILS then getString NEXT else getString CONFIRM
          , enableRipple = true
          , margin = Margin 0 20 0 0
          , width = MATCH_PARENT
          , isClickable = validateInput state
          }
        , deliveryDetailsConfig
          { visibility = VISIBLE
          , margin = Margin 0 20 0 0
          , isSource = state.data.currentStage == ST.SENDER_DETAILS
          , personNameDetails = personNameDetailsConfig roleConfig.details roleConfig.personNameLabel
          , mobileNumberDetails = mobileNumberDetailsConfig roleConfig.details roleConfig.phoneLabel
          , addressDetails = addressDetailsConfig roleConfig.details roleConfig.addressLabel
          , instructionDetails = instructionDetailsConfig roleConfig.details roleConfig.instructionLabel
          , checkBoxDetails = 
              { text: roleConfig.checkBoxDetailsText
              , isSelected: roleConfig.isSelected
              , visibility: 
                  (state.data.initiatedAs == API.Sender && state.data.currentStage == ST.SENDER_DETAILS) ||
                  (state.data.initiatedAs == API.Receiver && state.data.currentStage == ST.RECEIVER_DETAILS) ||
                  state.data.initiatedAs == API.SomeoneElse
              }
          , locationTitle = addressArea
          , locationDetails = decodedAddress
          }
        }
  in
    popUpConfig'
  where
    personNameDetailsConfig :: ST.PersonDeliveryDetails -> String -> PrimaryEditTextController.Config
    personNameDetailsConfig details label = 
      let config = PopUpModal.dummyDeliveryPrimaryText
      in config
        { editText { text = details.name, placeholder = getString ENTER_A_NAME, pattern = Just "[A-Za-z ]*,30" }
        , topLabel { text = label }
        , textImage { visibility = VISIBLE }
        , margin = Margin 0 0 0 8
        , id = EHC.getNewIDWithTag "personName"
        , errorLabel { text = getString PLEASE_ENTER_A_VALID_NAME }
        }

    mobileNumberDetailsConfig :: ST.PersonDeliveryDetails -> String -> PrimaryEditTextController.Config
    mobileNumberDetailsConfig details label = 
      let config = PopUpModal.dummyDeliveryPrimaryText
      in config
        { editText { text = details.phone, placeholder = "9999999999", pattern = Just "[0-9]*,10" }
        , topLabel { text = label }
        , textImage { visibility = VISIBLE }
        , id = EHC.getNewIDWithTag "mobileNumber"
        , type = "number"
        , errorLabel { text = getString PLEASE_ENTER_A_VALID_MOBILE_NUMBER }
        }

    addressDetailsConfig :: ST.PersonDeliveryDetails -> String -> PrimaryEditTextController.Config
    addressDetailsConfig details label = 
      let config = PopUpModal.dummyDeliveryPrimaryText
      in config
        { editText { text = details.extras, placeholder = getString ENTER_A_ADDRESS, pattern = Just "[^\n]*,50" }
        , topLabel { text = label }
        , id = EHC.getNewIDWithTag "addressDetails"
        , errorLabel { text = getString PLEASE_ENTER_A_VALID_ADDRESS }
        }

    instructionDetailsConfig :: ST.PersonDeliveryDetails -> String -> PrimaryEditTextController.Config
    instructionDetailsConfig details label = 
      let config = PopUpModal.dummyDeliveryPrimaryText
      in config
        { height = V 90
        , margin = MarginVertical 20 20
        , editText { text = fromMaybe "" details.instructions, placeholder = getString OPTIONAL_INSTRUCTION, singleLine = false, pattern = Just "[^\n]*,100" }
        , topLabel { text = label }
        , id = EHC.getNewIDWithTag "instructionDetails"
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
      , id = selectedParcelEstimate.id <> "_delivery"
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
      , validTill = selectedParcelEstimate.validTill
      , hasTollCharges = selectedParcelEstimate.hasTollCharges
      , hasParkingCharges = selectedParcelEstimate.hasParkingCharges
      , singleVehicle = true
      }
  in chooseVehicleConfig'

joinNonEmpty :: Array (Maybe String) -> String
joinNonEmpty mbParts =
    let parts = map (fromMaybe "") mbParts
        trimmedPart = map DS.trim parts
        filteredPart = DA.filter (\part -> part /= "") trimmedPart
    in DF.intercalate ", " $ filteredPart

decodeAddress' :: ST.Address -> String
decodeAddress' address =
  joinNonEmpty
    [ address.door
    , address.building
    , address.street
    , address.area
    , address.city
    , address.state
    , address.country
    ]
  
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
