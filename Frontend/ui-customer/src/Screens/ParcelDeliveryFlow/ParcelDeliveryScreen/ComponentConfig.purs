module Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.ComponentConfig where

import Data.String.CodeUnits (slice)
import Data.Array as DA
import Data.String as DS
import Common.Types.App as CTA
import Components.ChooseVehicle.Controller as ChooseVehicle
import Components.GenericHeader.Controller as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton.Controller as PrimaryButton
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.RateCard.Controller as RateCard
import Components.SeparatorView.View as SeparatorView
import Components.SelectListModal as CancelRidePopUpConfig
import ConfigProvider
import Data.Maybe (Maybe(..), isJust, maybe, fromMaybe)
import Font.Style (Style(..))
import Helpers.Utils as HU
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Resources.LocalizableV2.Strings (getEN)
import Screens.Types as ST
import Services.API as API
import Styles.Colors as Color

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
      , gravity = CENTER
      , margin = (MarginHorizontal 16 16)
      , isClickable = state.data.currentStage == ST.FINAL_DETAILS
      , id = getEN (if state.data.currentStage == ST.DELIVERY_INSTRUCTIONS then SEND_NOW else BOOK_FOR) <> "PrimaryButton"
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
      , validTill = selectedParcelEstimate.validTill
      , hasTollCharges = selectedParcelEstimate.hasTollCharges
      , hasParkingCharges = selectedParcelEstimate.hasParkingCharges
      , singleVehicle = true
      }
  in chooseVehicleConfig'

rateCardConfig :: ST.ParcelDeliveryScreenState -> RateCard.Config
rateCardConfig state =
  RateCard.config 
    { isNightShift = state.data.rateCard.isNightShift
    , currentRateCardType = state.data.rateCard.currentRateCardType
    , onFirstPage = state.data.rateCard.onFirstPage
    , showDetails = state.data.config.searchLocationConfig.showRateCardDetails
    , description = if state.data.rateCard.isNightShift then (getString $ NIGHT_TIME_CHARGES state.data.rateCard.nightChargeFrom state.data.rateCard.nightChargeTill) else (getString $ DAY_TIME_CHARGES state.data.rateCard.nightChargeTill state.data.rateCard.nightChargeFrom)
    , buttonText = Just if state.data.rateCard.currentRateCardType == CTA.DefaultRateCard then (getString GOT_IT) else (getString GO_BACK_)
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
  otherOptions :: Boolean -> Array CTA.FareList
  otherOptions showAdditions =
    (if showAdditions then [ { key: "DRIVER_ADDITIONS", val: (getString DRIVER_ADDITIONS) } ] else [])
    <> (if state.data.config.searchLocationConfig.showAdditionalChargesText then [{key : "TOLL_OR_PARKING_CHARGES", val : getString TOLL_OR_PARKING_CHARGES }]else [])