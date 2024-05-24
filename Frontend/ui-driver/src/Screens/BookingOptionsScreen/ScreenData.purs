module Screens.BookingOptionsScreen.ScreenData where

import Screens.Types (BookingOptionsScreenState, RidePreference)
import Services.API as API
import Data.Maybe (Maybe(..))
import Common.Types.App as CTA

initData :: BookingOptionsScreenState
initData =
  { data:
      { vehicleType: ""
      , vehicleNumber: ""
      , vehicleName: ""
      , vehicleCapacity: 0
      , downgradeOptions: []
      , ridePreferences: []
      , defaultRidePreference: defaultRidePreferenceOption
      , canSwitchToRental: false
      , canSwitchToInterCity: false
      , airConditioned: Nothing
      , rateCard : dummyRateCard
      }
  , props:
      { isBtnActive: false
      , downgraded: false
      , canSwitchToRental: false
      , acExplanationPopup: false
      , fromDeepLink : false
      , showRateCard : false
      }
  }

defaultRidePreferenceOption :: RidePreference
defaultRidePreferenceOption =
  { airConditioned: Nothing
  , driverRating: Nothing
  , isDefault: false
  , isSelected: false
  , longDescription: Nothing
  , luggageCapacity: Nothing
  , name: ""
  , seatingCapacity: Nothing
  , serviceTierType: API.AUTO_RICKSHAW
  , shortDescription: Nothing
  , vehicleRating: Nothing
  , isUsageRestricted: false
  , priority: 0
  , rateCardData : Nothing
  }

dummyRateCard :: CTA.RateCard
dummyRateCard =
  {
    additionalFare : 0,
    currentRateCardType : CTA.DefaultRateCard,
    onFirstPage:false,
    baseFare : 0,
    createdTime : "",
    driverAdditions : [],
    extraFare : [],
    fareInfoDescription : [],
    isNightShift : false,
    nightChargeTill : "",
    nightChargeFrom : "",
    waitingTimeInfo : { freeMinutes: "", charge: "" },
    serviceTierName : Nothing
  }