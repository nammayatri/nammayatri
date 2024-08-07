module Screens.DriverProfileScreenCommon.Controller where

import Prelude
import Screens.DriverProfileScreenCommon.ScreenData (DriverProfileScreenCommonState(..), DriverProfileScreenCommonData(..))
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Services.Common.Backend
import Engineering.Helpers.Utils as EHU
import Engineering.Helpers.Commons as EHC
import Services.Common.API
import Data.Array as DA
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Array (cons)
import Debug (spy)

data ScreenOutput = GoToBack

data Action = GetDriverProfileAPIResponseAction (DriverProfileRes)
            | NoAction
            | PrevImg
            | NextImg
            | GoBack

val (GetDriverProfileAPIResponseAction respList) state = do 
  _ <- pure $ EHU.toggleLoader false
  continue state{data = getDriverProfile respList}

val (PrevImg) state = do 
  void $ pure $ spy "prevClick" state
  continue state{data{ imgIdx = if state.data.imgIdx /= 0 then state.data.imgIdx - 1 else 0 }}

val (NextImg) state = do 
  void $ pure $ spy "NextClick" state
  continue state{data{ imgIdx = if DA.length state.data.profileImages - 1 /= state.data.imgIdx then state.data.imgIdx + 1 else state.data.imgIdx }}

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = pure unit

eval :: Action -> DriverProfileScreenCommonState -> Eval Action ScreenOutput DriverProfileScreenCommonState

eval PrevImg state = do
  void $ pure $ spy "prevClick" state
  continue state{data{ imgIdx = if state.data.imgIdx /= 0 then state.data.imgIdx - 1 else 0 }}

eval NextImg state = do 
  void $ pure $ spy "NextClick" state
  continue state{data{ imgIdx = if DA.length state.data.profileImages - 1 /= state.data.imgIdx then state.data.imgIdx + 1 else state.data.imgIdx }}

eval GoBack state = exit $ GoToBack

eval _ state = update state

getProfileImages :: Maybe String -> Array String -> Array String
getProfileImages arr1 arr2 = 
  case arr1 of
    Nothing -> arr2
    Just arr3 -> cons arr3 arr2

getDriverProfile :: (DriverProfileRes) -> DriverProfileScreenCommonData
getDriverProfile (DriverProfileRes details) = {
    certificates : EHC.convertTo2DArray details.certificates,
    homeTown : details.homeTown,
    driverName : details.driverName,
    aboutMe : details.aboutMe,
    drivingSince : details.drivingSince,
    onboardedAt : details.onboardedAt,
    pledges : details.pledges,
    driverStats : details.driverStats,
    languages : details.languages,
    aspirations : details.aspirations,
    vehicleNum : details.vehicleNum,
    vechicleVariant : details.vechicleVariant,
    vehicleTags : details.vehicleTags,
    profileImage : details.profileImage,
    images : details.images,
    profileImages : getProfileImages details.profileImage details.images,
    topReviews : details.topReviews,
    imgIdx : 0
  }

getVariant :: Variant -> String
getVariant variant = do
  case variant of
    SEDAN -> "Sedan"
    SUV -> "Suv"
    HATCHBACK -> "Hatchback"
    AUTO_RICKSHAW -> "Auto Rickshaw"
    TAXI -> "Taxi"
    TAXI_PLUS -> "Taxi Plus"
    PREMIUM_SEDAN -> "Premium Sedan"
    BLACK -> "Black"
    BLACK_XL -> "Black Xl"
    BIKE -> "Bike"
    AMBULANCE_TAXI -> "Ambulance Taxi"
    AMBULANCE_TAXI_OXY -> "Ambulance Taxi Oxy"
    AMBULANCE_AC -> "Ambulance Ac"
    AMBULANCE_AC_OXY -> "Ambulance Ac Oxy"
    AMBULANCE_VENTILATOR -> "Ambulance Ventilator"
    SUV_PLUS -> "Suv Plus"

getPillText :: String -> String
getPillText pill = do
  case pill of
    "Expert Driving" -> "🚗   Expert Driving"
    "Clean Vehicle" -> "🧼   Clean Vehicle"
    "Skilled Navigator" -> "🧭   Skilled Navigator"
    "Safe Ride" -> "✅   Safe Ride"
    "Polite Driver" -> "🤝   Polite Driver"
    "On Time" -> "🕙   On Time"
    "A/C not turned on" -> "🥵   A/C not turned on"
    "Late pickup/ arrival" -> "⏰   A/C not turned on"
    "Asked for more fare" -> "💸   Asked for more fare"
    "Unhygienic vehicle" -> "💩   Unhygienic vehicle"
    "Rash driving" -> "❗   Rash driving"
    "Rude driver" -> "😡   Rude driver"
    "Training" -> "✅   Training"
    "Financial" -> "🪙   Financial"
    "Safety" -> "🚦   Safety"
    _ -> ""