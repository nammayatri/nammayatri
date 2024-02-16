module Screens.EmergencyContactsScreen.ScreenData where

import Prelude

import Foreign.Object (empty)
import Screens.Types (EmergencyContactsScreenState(..))

initData :: EmergencyContactsScreenState
initData = {
    data: {
      emergencyContactsList : []
    , storedContactsList : []
    , selectedContacts : []
    , searchResult: []
    , prestoListArrayItems: []
    , loadMoreDisabled: true
    , offsetForEmergencyContacts : 0
    , limitForEmergencyContacts : 20
    , removedContactDetail : { isSelected :false
                              , name : ""
                              , number : ""
                              , enableForFollowing: false
                              , priority: 1
                              }
    , editedText : ""
    , logField : empty
    },
    props:{
        showContactList : false
      , showInfoPopUp : false  
      , fromSosFlow : false
    }
}