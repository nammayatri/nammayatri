module Screens.NammaSafetyFlow.Components.SafetyUtils where

import Prelude (map, (<>), (==))
import Screens.Types (NewContacts, NammaSafetyScreenState(..))
import Data.Array (filter, length, uncons)
import Data.Maybe (Maybe(..))

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