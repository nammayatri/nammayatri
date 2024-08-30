module Screens.NammaSafetyFlow.Components.SafetyUtils where

import Prelude (map, (<>), (==), ($), (&&), not, (||), (<#>), (/=))
import Screens.Types (NewContacts, NammaSafetyScreenState(..))
import Data.Array (filter, length, uncons, find)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Engineering.Helpers.Commons as EHC
import JBridge as JB

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
showNightSafetyFlow hasNightIssue rideStartTime rideEndTime safetyCheckStartSeconds safetyCheckEndSeconds = (hasNightIssue == Just false) && (checkTimeConstraints rideStartTime || checkTimeConstraints rideEndTime)
  where 
    checkTimeConstraints = checkSafetyTimeConstraints safetyCheckStartSeconds safetyCheckEndSeconds 

checkSafetyTimeConstraints :: Int -> Int -> Maybe String -> Boolean
checkSafetyTimeConstraints safetyCheckStartSeconds safetyCheckEndSeconds = maybe false (\time -> JB.withinTimeRange safetyCheckStart safetyCheckEnd $ EHC.convertUTCtoISC time "HH:mm:ss")
  where
    safetyCheckStart = EHC.convertUTCtoISC (EHC.parseSecondsOfDayToUTC safetyCheckStartSeconds) "HH:mm:ss"
    safetyCheckEnd = EHC.convertUTCtoISC (EHC.parseSecondsOfDayToUTC safetyCheckEndSeconds) "HH:mm:ss"
    