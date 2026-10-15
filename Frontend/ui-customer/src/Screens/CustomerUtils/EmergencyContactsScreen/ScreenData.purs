module Screens.EmergencyContactsScreen.ScreenData where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign.Object (empty)
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.Types (EmergencyContactsScreenState(..), DropDownOptions)
import Services.API as API
import Data.Maybe (Maybe(..))

initData :: EmergencyContactsScreenState
initData =
  { data:
      { emergencyContactsList: []
      , storedContactsList: []
      , selectedContacts: []
      , selectedContact:
          { isSelected: false
          , name: ""
          , number: ""
          , enableForFollowing: false
          , enableForShareRide: false
          , shareTripWithEmergencyContactOption: neverShareRideOption
          , onRide: false
          , priority: 1
          , contactPersonId: Nothing
          , isFollowing: Nothing
          , notifiedViaFCM : Nothing
          }
      , searchResult: []
      , prestoListArrayItems: []
      , loadMoreDisabled: true
      , manualContactName : ""
      , manualContactNumber : ""
      , offsetForEmergencyContacts: 0
      , limitForEmergencyContacts: 20
      , removedContactDetail:
          { isSelected: false
          , name: ""
          , number: ""
          , enableForFollowing: false
          , enableForShareRide: false
          , shareTripWithEmergencyContactOption: neverShareRideOption
          , onRide: false
          , priority: 1
          , notifiedViaFCM : Nothing
          , contactPersonId: Nothing
          , isFollowing: Nothing
          }
      , editedText: ""
      , logField: empty
      }
  , props:
      { showContactList: false
      , showInfoPopUp: false
      , fromSosFlow: false
      , validManualContact : true
      , validManualName : false
      , showAddContactOptions : false
      , addContactsManually : false
      , fromNewSafetyFlow: false
      , saveEmergencyContacts: false
      , getDefaultContacts: false
      , showDropDown: false
      , appName: ""
      , isKeyBoardOpen : false
      }
  }

neverShareRideOption :: DropDownOptions
neverShareRideOption = { key: API.NEVER_SHARE, value: getString NEVER_SHARE_LN }

neverShareRideOptionEM :: DropDownOptions
neverShareRideOptionEM = { key: API.NEVER_SHARE, value: getString NEVER_SHARE_EM }

alwaysShareRideOption :: DropDownOptions
alwaysShareRideOption = { key: API.ALWAYS_SHARE, value: getString ALWAYS_SHARE_LN }

alwaysShareRideOptionEM :: DropDownOptions
alwaysShareRideOptionEM = { key: API.ALWAYS_SHARE, value: getString ALWAYS_SHARE_EM }

shareWithTimeContraintsRideOption :: DropDownOptions
shareWithTimeContraintsRideOption = { key: API.SHARE_WITH_TIME_CONSTRAINTS, value: getString SHARE_WITH_TIME_CONSTRAINTS_LN }

shareWithTimeContraintsRideOptionEM :: DropDownOptions
shareWithTimeContraintsRideOptionEM = { key: API.SHARE_WITH_TIME_CONSTRAINTS, value: getString SHARE_WITH_TIME_CONSTRAINTS_EM }

getRideOptionFromKey :: API.RideShareOptions -> DropDownOptions
getRideOptionFromKey key = case key of
  API.NEVER_SHARE -> neverShareRideOption
  API.ALWAYS_SHARE -> alwaysShareRideOption
  API.SHARE_WITH_TIME_CONSTRAINTS -> shareWithTimeContraintsRideOption

getRideOptionFromKeyEM :: API.RideShareOptions -> DropDownOptions
getRideOptionFromKeyEM key = case key of
  API.NEVER_SHARE -> neverShareRideOptionEM
  API.ALWAYS_SHARE -> alwaysShareRideOptionEM
  API.SHARE_WITH_TIME_CONSTRAINTS -> shareWithTimeContraintsRideOptionEM
