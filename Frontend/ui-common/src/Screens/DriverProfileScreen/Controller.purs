module Screens.DriverProfileScreenCommon.Controller where

import Prelude
import Screens.DriverProfileScreenCommon.ScreenData (DriverProfileScreenCommonState(..), DriverProfileScreenCommonData(..))
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Services.Common.Backend
import Mobility.Prelude (startsWith)
import Engineering.Helpers.Utils as EHU
import Engineering.Helpers.Commons 
import Services.Common.API
import Data.Array as DA
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Array (cons)
import Debug (spy)
import JBridge as JB
import Effect.Uncurried(runEffectFn1)
import Engineering.Helpers.Commons (getNewIDWithTag, screenWidth)
import Data.String (joinWith, null)
import Language.Strings (getString)
import Language.Types (STR(..)) as STR
import Helpers.Utils (emitTerminateApp, isParentView)
import Effect (Effect)
import Common.Types.App (LazyCheck(..))
import Constants as Constants
import Data.Lens ((^.))
import Engineering.Helpers.Accessor

data ScreenOutput = GoToBack

data Action = GetDriverProfileAPIResponseAction (DriverProfileRes)
            | NoAction
            | PrevImg
            | NextImg
            | GoBack

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = pure unit

getImage :: DriverProfileScreenCommonState -> String
getImage state = joinWith "" $ (DA.mapWithIndex(\index item -> if index == state.data.imgIdx then item else "")state.data.displayImages)

renderBase64 :: String -> Effect Unit
renderBase64 image = do
  if not (null image) then void $ runEffectFn1 JB.displayBase64Image JB.displayBase64ImageConfig {source = image, id = getNewIDWithTag "add_image_component_images" , scaleType =  "FIT_CENTER", adjustViewBounds = false, inSampleSize = 2} else pure unit
  pure unit

eval :: Action -> DriverProfileScreenCommonState -> Eval Action ScreenOutput DriverProfileScreenCommonState

eval (GetDriverProfileAPIResponseAction (DriverProfileRes resp)) state = do 
  let response = resp.response
      updatedState = state{data = getDriverProfile response}
  continueWithCmd updatedState [do 
    let image = getImage updatedState
    renderBase64 image
    pure NoAction
  ]

eval (PrevImg) state = do 
  let updatedState = state{data{ imgIdx = if state.data.imgIdx /= 0 then state.data.imgIdx - 1 else 0 }}
  continueWithCmd updatedState [do 
    let image = getImage updatedState
    renderBase64 image
    pure NoAction
  ]

eval (NextImg) state = do 
  let updatedState = state{data{ imgIdx = if DA.length state.data.displayImages - 1 /= state.data.imgIdx then state.data.imgIdx + 1 else state.data.imgIdx }}
  continueWithCmd updatedState [do 
    let image = getImage updatedState
    renderBase64 image
    pure NoAction
  ]

eval GoBack state = do
  if isParentView FunctionCall 
    then do 
      let mBPayload = getGlobalPayload Constants.globalPayload
      case mBPayload of 
        Just globalPayload -> case globalPayload ^. _payload ^. _view_param of
          Just screen -> if startsWith "driverprofile" screen then do
                            void $ pure $ emitTerminateApp Nothing true
                            continue state
                            else exit $ GoToBack
          _ -> exit $ GoToBack
        Nothing -> exit $ GoToBack
    else exit $ GoToBack

eval _ state = update state

getProfileImages :: Maybe String -> Array String -> Array String
getProfileImages arr1 arr2 = 
  case arr1 of
    Nothing -> arr2
    Just arr3 -> cons arr3 arr2

getDriverProfile :: (DriverProfile) -> DriverProfileScreenCommonData
getDriverProfile (DriverProfile details) = {
    certificates : convertTo2DArray details.certificates,
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
    displayImages : getProfileImages details.profileImage details.images,
    topReviews : details.topReviews,
    imgIdx : 0,
    shimmerView : false
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
    EV_AUTO_RICKSHAW -> "EV Auto Rickshaw"
    HERITAGE_CAB -> "Heritage Cab"
    _ -> ""

getPillText :: String -> String
getPillText pill = do
  case pill of
    "Expert Driving" -> "🚗   " <> getString STR.EXPERT_DRIVING
    "Clean Vehicle" -> "🧼   " <> getString STR.CLEAN_VEHICLE
    "Clean Auto" -> "🧼   " <> getString STR.CLEAN_AUTO
    "Clean Cab" -> "🧼   " <> getString STR.CLEAN_CAB
    "Skilled Navigator" -> "🧭   " <> getString STR.SKILLED_NAVIGATOR
    "Safe Ride" -> "✅   " <> getString STR.SAFE_RIDE
    "Polite Driver" -> "🤝   " <> getString STR.POLITE_DRIVER
    "On Time" -> "🕙   " <> getString STR.ON_TIME
    "A/C not turned on" -> "🥵   " <> getString STR.AC_NOT_TURNED_ON
    "Late pickup/ arrival" -> "⏰   " <> getString STR.LATE_PICK_UP_ARRIVAL
    "Asked for more fare" -> "💸   " <> getString STR.ASKED_FOR_MORE_FARE
    "Unhygienic vehicle" -> "💩   " <> getString STR.UNHYGIENIC_VEHICLE
    "Rash driving" -> "❗   " <> getString STR.RASH_DRIVING
    "Rude driver" -> "😡   " <> getString STR.RUDE_DRIVER
    "Training" -> "✅   " <> getString STR.TRAINING
    "Financial" -> "🪙   " <> getString STR.FINANCIAL
    "Safety" -> "🚦   " <> getString STR.SAFETY
    _ -> pill

getAspirationsText :: String -> String
getAspirationsText pill = do
  case pill of
    "Kid's Education" -> "📕    " <> getString STR.KIDS_EDUCATION
    "Buy New Vehicle" -> "🚙    " <> getString STR.BUY_NEW_VEHICLE
    "Buy New Home" -> "🏠    " <> getString STR.BUY_NEW_HOME
    _ -> "🤞    " <> pill