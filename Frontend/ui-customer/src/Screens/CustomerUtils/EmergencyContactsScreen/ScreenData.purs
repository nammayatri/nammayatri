module Screens.EmergencyContactsScreen.ScreenData where

import Prelude
import Screens.Types (EmergencyContactsScreenState(..))
import Common.Types.App (PopUpStatus(..))
import Data.Maybe (Maybe(..)) as Mb

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
    , popUpConfig : {
        status : OPEN
      , actionType : Mb.Nothing
    }
    },
    props:{
        showContactList : false
      , showInfoPopUp : false  
    }
}