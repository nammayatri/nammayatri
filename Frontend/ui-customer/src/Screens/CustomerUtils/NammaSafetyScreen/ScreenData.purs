{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NammaSafetyScreen.ScreenData where

import PrestoDOM (LetterSpacing(..))
import Screens.Types (NammaSafetyScreenState, Stage(..))

initData :: NammaSafetyScreenState
initData = {
    data: {
      emergencyContacts : [{name : "A B", number : "95645645"}],
      shareToEmergencyContacts : false,
      nightTimeSafety : false,
      triggerNYSupport : false,
      hasCompletedSafetySetup : false,
      emergencyContactsData : {
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
      }
    },
    props: {
      showOnboarding : true,
      currentStage : NammaSafetyDashboard,
      emergencyContactsProps : {
        showContactList : false
        , showInfoPopUp : false  
      }
    }
}
