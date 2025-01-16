module Screens.BookingOptionsScreen.Controller where

import Components.ChooseVehicle as ChooseVehicle
import Components.PrimaryButton as PrimaryButton
import Components.PopUpModal as PopUpModal
import Data.Array (filter, length, (!!), find)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Log (trackAppScreenRender)
import Prelude (class Show, map, pure, show, unit, discard, void, (<>), (==), not, ($), (>), (<$>),(&&))
import PrestoDOM (Eval, update, continue, exit, continueWithCmd, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (BookingOptionsScreenState, VehicleP, RidePreference)
import Common.Types.App  as CTA
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Helpers.Utils (getVehicleVariantImage, contactSupportNumber)
import Language.Strings (getString)
import Language.Types (STR(..))
import Effect.Unsafe (unsafePerformEffect)
import JBridge as JB
import Services.API as API
import Components.RateCard.Utils (getFareBreakupList)
import Components.RateCard as RateCard
import Common.RemoteConfig.Utils (tipConfigData)
import RemoteConfig as RC
import Data.Foldable as DF
import Storage (getValueToLocalStore, KeyStore(..))
import Helpers.Utils as HU
import Screens.Types as ST

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "BookingOptionsScreen"
    BackPressed -> pure unit
    _ -> pure unit

data Action
  = BackPressed
  | AfterRender
  | PrimaryButtonAC PrimaryButton.Action
  | ToggleRidePreference RidePreference
  | UpdateACAvailability Boolean
  | CallSupport
  | ShowACVideoPopup
  | TopAcDriverAction PopUpModal.Action
  | OpenLink String
  | ToggleRentalRide 
  | ToggleIntercityRide
  | ToggleLocalRide
  | UpdateRateCard API.GetDriverRateCardRes
  | ShowRateCard ST.RidePreference
  | RateCardAction RateCard.Action
  | RateCardBannerClick
  | ServiceTierInfoVideo API.ServiceTierType

data ScreenOutput
  = GoBack BookingOptionsScreenState
  | ChangeRidePreference BookingOptionsScreenState RidePreference
  | ToggleACAvailability BookingOptionsScreenState Boolean
  | ToggleRentalIntercityRide BookingOptionsScreenState
  | ExitToRateCardScreen BookingOptionsScreenState

eval :: Action -> BookingOptionsScreenState -> Eval Action ScreenOutput BookingOptionsScreenState
eval BackPressed state = 
  if state.props.acExplanationPopup then continue state { props { acExplanationPopup = false } }
  else if state.props.showRateCard then continue state { props { showRateCard = false }}
  else exit $ GoBack state
eval (ToggleRidePreference service) state = 
  if service.isUsageRestricted then do
    void $ pure $ JB.toast $ getString $ SET_THE_AC_ON_TO_ENABLE service.name
    update state
  else updateAndExit state $ ChangeRidePreference state service

eval ToggleRentalRide state = updateAndExit state { props {canSwitchToRental = not <$> state.props.canSwitchToRental} } $ ToggleRentalIntercityRide state { props {canSwitchToRental = not <$> state.props.canSwitchToRental} }

eval ToggleIntercityRide state = updateAndExit state {props {canSwitchToInterCity = not <$> state.props.canSwitchToInterCity}} $ ToggleRentalIntercityRide state {props {canSwitchToInterCity = not <$> state.props.canSwitchToInterCity}}

eval ToggleLocalRide state = updateAndExit state {props {canSwitchToIntraCity = not <$> state.props.canSwitchToIntraCity}} $ ToggleRentalIntercityRide state {props {canSwitchToIntraCity = not <$> state.props.canSwitchToIntraCity}}

eval (UpdateACAvailability acServiceToggle) state = exit $ ToggleACAvailability state $ not acServiceToggle

eval ShowACVideoPopup state = continue state { props { acExplanationPopup = not state.props.acExplanationPopup && state.data.config.rateCardScreen.showYoutubeVideo} }

eval (TopAcDriverAction action) state = do
  let acVideoLink = "https://www.youtube.com/watch?v=MbgxZkqxPLQ"
      newState = state { props { acExplanationPopup = false } }
  case action of
    PopUpModal.DismissPopup -> continue newState
    PopUpModal.OnButton2Click -> continue newState
    PopUpModal.OnCoverImageClick -> continueWithCmd newState [pure $ OpenLink acVideoLink]
    PopUpModal.OnButton1Click -> continueWithCmd newState [pure $ OpenLink acVideoLink]
    _ -> continue state

eval (OpenLink link) state = continueWithCmd state [ do 
  void $ JB.openUrlInApp link
  pure AfterRender
  ]

eval CallSupport state = do
  void $ pure $ unsafePerformEffect $ contactSupportNumber ""
  continue state

eval (UpdateRateCard rateCardResp) state = do
  let ridePreferences = map (\item -> 
      let rateCardOb = getRateCardOb item.serviceTierType rateCardResp
      in item { rateCardData = rateCardOb.rateCardData,
                perKmRate = rateCardOb.perKmRate,
                farePolicyHour = rateCardOb.farePolicyHour
              }) state.data.ridePreferences
  continue state { data { ridePreferences = ridePreferences  }, props { rateCardLoaded = true, peakTime = isJust $ find (\item -> item.farePolicyHour == Just API.Peak) ridePreferences } }

eval (ShowRateCard pref) state = do
  let mbPref = find (\item -> item.serviceTierType == pref.serviceTierType) state.data.ridePreferences
  case mbPref of
    Just ridePreference -> do
      case ridePreference.rateCardData of
        Just rateCardData -> 
          continue state { data { rateCard  {
              onFirstPage = false,
              serviceTierName = Just ridePreference.name,
              currentRateCardType = CTA.DefaultRateCard,
              driverAdditions = rateCardData.driverAdditions,
              fareInfoDescription = rateCardData.fareInfo,
              isNightShift = rateCardData.isNightShift,
              nightChargeTill = rateCardData.nightChargeEnd,
              nightChargeFrom = rateCardData.nightChargeStart,
              extraFare = rateCardData.fareList
            }} , props {showRateCard = state.data.config.rateCardScreen.showRateCard } }
        Nothing -> continue state
    Nothing -> continue state

eval (RateCardAction RateCard.Close) state = continue state { props { showRateCard = false } , data{rateCard{onFirstPage = false,currentRateCardType = CTA.DefaultRateCard}}}

eval (RateCardAction RateCard.BackPressed) state = continueWithCmd state [pure $ (RateCardAction RateCard.Close)]

eval (RateCardAction RateCard.NoAction) state = continue state

eval (RateCardAction RateCard.GoToDefaultStart) state = continue state { data{rateCard{currentRateCardType = CTA.DefaultRateCard}}}

eval (RateCardAction RateCard.GoToDriverAddition) state = continue state { data{rateCard{currentRateCardType = CTA.DriverAddition,onFirstPage = true}}}

eval (RateCardAction RateCard.GoToTollOrParkingCharges) state = continue state { data{rateCard{currentRateCardType = CTA.TollOrParkingCharges,onFirstPage = true}}}

eval RateCardBannerClick state = exit $ ExitToRateCardScreen state

eval (ServiceTierInfoVideo serviceTierType) state = do
  case serviceTierType of
    API.DELIVERY_BIKE ->
      let parcelConfig = RC.getParcelConfig "lazy"
      in continueWithCmd state [pure $ OpenLink parcelConfig.introductionVideo]
    _ -> continue state
  
eval _ state = update state

downgradeOptionsConfig :: Array VehicleP -> String -> ChooseVehicle.Config
downgradeOptionsConfig vehicles vehicleType =
  ChooseVehicle.config
    { vehicleImage = getVehicleVariantImage vehicleType
    , isCheckBox = true
    , vehicleVariant = vehicleType
    , isBookingOption = true
    , capacity = getVehicleCapacity vehicleType
    , isSelected = (fromMaybe dummyVehicleP $ (filter (\item -> item.vehicleName == vehicleType) vehicles) !! 0).isSelected
    }

getVehicleCapacity :: String -> String
getVehicleCapacity vehicleType = case vehicleType of
  "TAXI" -> getString ECONOMICAL <> " · " <> "4 " <> getString PEOPLE
  "SUV" -> getString SPACIOUS <> " · " <> "6 " <> getString PEOPLE
  "BIKE" -> getString ECONOMICAL <> " · " <> "1 " <> getString PEOPLE
  "AMBULANCE_TAXI" -> "Ambulance Taxi" <> " · " <> "4 " <> getString PEOPLE
  "AMBULANCE_TAXI_OXY" -> "Ambulance Taxi Oxy" <> " · " <> "4 " <> getString PEOPLE
  "AMBULANCE_AC" -> "Ambulance AC" <> " · " <> "4 " <> getString PEOPLE
  "AMBULANCE_AC_OXY" -> "Ambulance AC Oxy" <> " · " <> "4 " <> getString PEOPLE
  "AMBULANCE_VENTILATOR" -> "Ambulance Ventilator" <> " · " <> "4 " <> getString PEOPLE
  "SUV_PLUS" -> "AC, " <> getString SPACIOUS <> " · " <> "6 " <> getString PEOPLE
  "HERITAGE_CAB" -> "AC, " <> "Signature Cabs" <> " · " <> "4 " <> getString PEOPLE
  _ -> getString COMFY <> " · " <> "4 " <> getString PEOPLE

dummyVehicleP :: VehicleP
dummyVehicleP =
  { vehicleName: ""
  , isSelected: false
  }

getRateCardOb :: API.ServiceTierType -> API.GetDriverRateCardRes -> RateCardOb
getRateCardOb serviceTierType (API.GetDriverRateCardRes rateCardResp) = do
  let rateCardRespItem = find (\(API.RateCardRespItem item) -> item.serviceTierType == serviceTierType) rateCardResp
      tips = fromMaybe 0 $ DF.maximum $ tipConfigData (getValueToLocalStore DRIVER_LOCATION) $ HU.getVehicleMapping serviceTierType
  case rateCardRespItem of
    Just (API.RateCardRespItem rateCardRespItem) -> 
      { 
        perKmRate : Just rateCardRespItem.perKmRate.amount, 
        farePolicyHour : Just rateCardRespItem.farePolicyHour,
        rateCardData : Just $ getFareBreakupList rateCardRespItem.rateCardItems tips
      }
    Nothing -> {
        perKmRate : Nothing,
        farePolicyHour : Nothing,
        rateCardData : Nothing
    }

type RateCardOb = {
  perKmRate :: Maybe Number,
  farePolicyHour :: Maybe API.FarePolicyHour,
  rateCardData :: Maybe CTA.BreakupList
}