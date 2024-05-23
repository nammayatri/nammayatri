module Screens.BookingOptionsScreen.Controller where

import Components.ChooseVehicle as ChooseVehicle
import Components.PrimaryButton as PrimaryButton
import Components.PopUpModal as PopUpModal
import Data.Array (filter, length, (!!), find)
import Data.Maybe (Maybe(..), fromMaybe)
import Log (trackAppScreenRender)
import Prelude (class Show, map, pure, show, unit, discard, void, (<>), (==), not, ($), (>))
import PrestoDOM (Eval, update, continue, exit, continueWithCmd)
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
import Data.Foldable as DF
import Storage (getValueToLocalStore, KeyStore(..))

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
  | UpdateRateCard API.GetDriverRateCardRes
  | ShowRateCard Int
  | RateCardAction RateCard.Action

data ScreenOutput
  = GoBack BookingOptionsScreenState
  | ChangeRidePreference BookingOptionsScreenState RidePreference
  | ToggleACAvailability BookingOptionsScreenState Boolean

eval :: Action -> BookingOptionsScreenState -> Eval Action ScreenOutput BookingOptionsScreenState
eval BackPressed state = 
  if state.props.acExplanationPopup then continue state { props { acExplanationPopup = false } }
  else exit $ GoBack state

eval (ToggleRidePreference service) state = 
  if service.isUsageRestricted then do
    void $ pure $ JB.toast $ getString $ SET_THE_AC_ON_TO_ENABLE service.name
    update state
  else exit $ ChangeRidePreference state service

eval (UpdateACAvailability acServiceToggle) state = exit $ ToggleACAvailability state $ not acServiceToggle

eval ShowACVideoPopup state = continue state { props { acExplanationPopup = not state.props.acExplanationPopup } }

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
  let ridePreferences = map (\item -> item {
      rateCardData = getRateCardData item.serviceTierType rateCardResp
    }) state.data.ridePreferences
  continue state { data { ridePreferences = ridePreferences  } }

eval (ShowRateCard index) state = case state.data.ridePreferences !! index of
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
          }} , props {showRateCard = true } }
      Nothing -> continue state
  Nothing -> continue state

eval (RateCardAction RateCard.Close) state = continue state { props { showRateCard = false } , data{rateCard{onFirstPage = false,currentRateCardType = CTA.DefaultRateCard}}}

eval (RateCardAction RateCard.BackPressed) state = continue state { props { showRateCard = false } ,data{rateCard{onFirstPage = false,currentRateCardType = CTA.DefaultRateCard}}}

eval (RateCardAction RateCard.NoAction) state = continue state

eval (RateCardAction RateCard.GoToDefaultStart) state = continue state { data{rateCard{currentRateCardType = CTA.DefaultRateCard}}}

eval (RateCardAction RateCard.GoToDriverAddition) state = continue state { data{rateCard{currentRateCardType = CTA.DriverAddition,onFirstPage = true}}}

eval (RateCardAction RateCard.GoToTollOrParkingCharges) state = continue state { data{rateCard{currentRateCardType = CTA.TollOrParkingCharges,onFirstPage = true}}}

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
  _ -> getString COMFY <> " · " <> "4 " <> getString PEOPLE

dummyVehicleP :: VehicleP
dummyVehicleP =
  { vehicleName: ""
  , isSelected: false
  }

getRateCardData :: API.ServiceTierType -> API.GetDriverRateCardRes -> Maybe CTA.BreakupList
getRateCardData serviceTierType (API.GetDriverRateCardRes rateCardResp) = do
  let rateCardRespItem = find (\(API.RateCardRespItem item) -> item.serviceTierType == serviceTierType) rateCardResp
      tips = fromMaybe 0 $ DF.maximum $ tipConfigData (getValueToLocalStore DRIVER_LOCATION) $ getVehicleMapping serviceTierType
  case rateCardRespItem of
    Just (API.RateCardRespItem rateCardRespItem) -> Just $ getFareBreakupList rateCardRespItem.rateCardItems tips
    Nothing -> Nothing

getVehicleMapping :: API.ServiceTierType -> String
getVehicleMapping serviceTierType = case serviceTierType of
  API.COMFY -> "SEDAN"
  API.ECO -> "HATCHBACK"
  API.PREMIUM -> "SUV"
  API.SUV_TIER -> "SUV"
  API.AUTO_RICKSHAW -> "AUTO_RICKSHAW"
  API.HATCHBACK_TIER -> "HATCHBACK"
  API.SEDAN_TIER -> "SEDAN"
  API.TAXI -> "TAXI"
  API.TAXI_PLUS -> "TAXI_PLUS"
  API.RENTALS -> "RENTALS"
  API.INTERCITY -> "INTERCITY"