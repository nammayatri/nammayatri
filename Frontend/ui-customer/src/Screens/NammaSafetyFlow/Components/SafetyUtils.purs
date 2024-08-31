module Screens.NammaSafetyFlow.Components.SafetyUtils where

import Prelude (map, (<>), (==), ($), (&&), not, (||), (<#>), (/=))
import Screens.Types (NewContacts, NammaSafetyScreenState(..))
import Data.Array (filter, length, uncons, find)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import Services.API as API
import Storage (getValueToLocalStore, KeyStore(..))
import DecodeUtil (decodeForeignAnyImpl, parseJSON)

getDefaultPriorityList :: Array NewContacts -> Array NewContacts
getDefaultPriorityList contacts =
  let
    splitContacts = uncons contacts
    priorityAlreadySet = length (filter (\contact -> contact.priority == 0) contacts) == 1
  in
    case priorityAlreadySet, splitContacts of
      true, _ -> contacts
      false, Nothing -> contacts
      false, Just { head, tail } -> do
        [ head { priority = 0 } ] <> map (\item -> item{priority = 1}) tail

getVehicleDetails :: NammaSafetyScreenState -> String
getVehicleDetails state = 
  case state.data.lastRideDetails of
    Nothing -> state.data.vehicleDetails
    Just rideDetails -> rideDetails.vehicleNumber

getPrimaryContact :: NammaSafetyScreenState -> Maybe NewContacts
getPrimaryContact state = 
  find (\contact -> contact.priority == 0) $ getDefaultPriorityList state.data.emergencyContactsList

showNightSafetyFlow :: Maybe Boolean -> Maybe String -> Maybe String -> Int -> Int -> Boolean
showNightSafetyFlow hasNightIssue rideStartTime rideEndTime safetyCheckStartSeconds safetyCheckEndSeconds = (hasNightIssue == Just false) && ((checkTimeConstraints rideStartTime) || ( checkTimeConstraints rideEndTime))
  where 
    checkTimeConstraints = checkSafetyTimeConstraints safetyCheckStartSeconds safetyCheckEndSeconds 

checkSafetyTimeConstraints :: Int -> Int -> Maybe String -> Boolean
checkSafetyTimeConstraints safetyCheckStartSeconds safetyCheckEndSeconds = maybe false (\time -> JB.withinTimeRange safetyCheckStart safetyCheckEnd $ EHC.convertUTCtoISC time "HH:mm:ss")
  where
    safetyCheckStart = EHC.convertUTCtoISC (EHC.parseSecondsOfDayToUTC safetyCheckStartSeconds) "HH:mm:ss"
    safetyCheckEnd = EHC.convertUTCtoISC (EHC.parseSecondsOfDayToUTC safetyCheckEndSeconds) "HH:mm:ss"
    
type PostRideSettingCache = {
  enablePostRideSafetyCheck :: Boolean,
  safetyCheckStartSeconds :: Int,
  safetyCheckEndSeconds :: Int
}

checkIfFollowEnabled :: API.RideShareOptions -> Maybe Int -> Maybe Int -> Boolean
checkIfFollowEnabled shareOption mbSafetyCheckStartSeconds mbSafetyCheckEndSeconds = do
  let currentTime = EHC.convertUTCtoISC (EHC.getCurrentUTC "") "HH:mm:ss"
  case shareOption of
    API.NEVER_SHARE -> false
    API.ALWAYS_SHARE -> true
    API.SHARE_WITH_TIME_CONSTRAINTS -> checkTimeConstraints (Just currentTime) || checkTimeConstraints (Just currentTime)
  where
    checkTimeConstraints = checkSafetyTimeConstraints (fromMaybe 0 mbSafetyCheckStartSeconds) (fromMaybe 0 mbSafetyCheckEndSeconds)

getPostRideCheckSettingsFromCache :: String -> Maybe PostRideSettingCache
getPostRideCheckSettingsFromCache cache = 
  let
    savedValue = getValueToLocalStore POST_RIDE_CHECK_SETTINGS
    (parsedValue :: Maybe PostRideSettingCache) = 
         (decodeForeignAnyImpl (parseJSON savedValue)) 
  in
    parsedValue