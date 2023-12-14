module Screens.EmergencyContactsScreen.ScreenData where

import Prelude

import Foreign.Object (empty)
import Screens.Types (EmergencyContactsScreenState(..), NammaSafetyStage(..))

initData :: EmergencyContactsScreenState
initData = {
    data: {
      contactInfoState : []
    , contactsCount : 0
    , contactsList : []
    , contactsNewList : []
    , contactsUpdatedNewList: []
    , prestoListArrayItems: []
    , loadMoreDisabled: true
    , offsetForEmergencyContacts : 0
    , limitForEmergencyContacts : 20
    , removedContactDetail : { isSelected :false
                              , name : ""
                              , number : ""
                              }
    , editedText : ""
    , logField : empty
    },
    props:{
        showContactList : false
      , showInfoPopUp : false  
    }
}