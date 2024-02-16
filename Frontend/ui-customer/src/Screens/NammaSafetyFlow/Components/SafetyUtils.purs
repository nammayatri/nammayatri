module Screens.NammaSafetyFlow.Components.SafetyUtils where

import Prelude
import Screens.Types (NewContacts)
import Data.Array 
import Debug
import Data.Maybe

getDefaultPriorityList :: Array NewContacts -> Array NewContacts
getDefaultPriorityList contacts =
  let
    splitContacts = uncons contacts
    priorityAlreadySet = length (filter (\contact -> contact.priority == 0) contacts) == 1
    _ = spy "EC log priorityAlreadySet" priorityAlreadySet
    _ = spy "EC log splitContacts" splitContacts
    -- _ = spy "EC log []" ([ head { priority = 0 } ] <> map (\item -> item{priority = 1}) tail)
  in
    case priorityAlreadySet, splitContacts of
      true, _ -> contacts
      false, Nothing -> contacts
      false, Just { head, tail } -> do
        let _ = spy "EC log []" ([ head { priority = 0 } ] <> map (\item -> item{priority = 1}) tail)
        [ head { priority = 0 } ] <> map (\item -> item{priority = 1}) tail