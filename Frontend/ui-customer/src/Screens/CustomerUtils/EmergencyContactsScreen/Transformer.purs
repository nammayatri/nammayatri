module Screens.EmergencyContactsScreen.Transformer where

import Prelude (map)
import Screens.Types (Contacts, NewContacts)
import Data.Maybe (Maybe(..))
import Screens.EmergencyContactsScreen.ScreenData (shareWithTimeContraintsRideOption)

getContactList :: Array Contacts -> Array NewContacts
getContactList contacts = map (\x -> getContact x) contacts

getContact :: Contacts -> NewContacts
getContact contact =
  { isSelected: false
  , name: contact.name
  , number: contact.number
  , enableForFollowing: false
  , enableForShareRide: false
  , shareTripWithEmergencyContactOption: shareWithTimeContraintsRideOption
  , onRide: false
  , priority: 1
  , contactPersonId : Nothing
  , isFollowing: Nothing
  , notifiedViaFCM : Nothing
}
