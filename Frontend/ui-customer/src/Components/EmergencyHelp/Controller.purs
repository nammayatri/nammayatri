{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.EmergencyHelp.Controller where

import Prelude
import Styles.Types

import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Components.PopUpModal.Controller as PopUpModalController
import MerchantConfig.Types (AppConfig)
import PrestoDOM (Eval, continue, exit, continueWithCmd)
import Screens.Types (Contact)
import Styles.Colors as Color
import ConfigProvider
import Constants

data Action = NoAction 
            | CallPolicePopup
            | ContactSupportPopup
            | CallSuccessfulPopup
            | CallContactPopUp Contact
            | CallPolice PopUpModalController.Action
            | ContactSupport PopUpModalController.Action
            | CallEmergencyContact PopUpModalController.Action
            | CallSuccessful PopUpModalController.Action
            | GenericHeaderAC GenericHeaderController.Action
            | StoreContacts
            | AddedEmergencyContacts

type EmergencyHelpModelState = {
     showContactSupportPopUp :: Boolean,
     showCallPolicePopUp :: Boolean, 
     showCallContactPopUp :: Boolean,
     showCallSuccessfulPopUp :: Boolean,
     emergencyContactData :: Array Contact,
     currentlySelectedContact :: Contact,
     config :: AppConfig
}

config :: EmergencyHelpModelState 
config = {
     showContactSupportPopUp : false
   , showCallPolicePopUp : false
   , showCallContactPopUp : false
   , showCallSuccessfulPopUp : false
   , emergencyContactData : []
   , currentlySelectedContact : selectedContactData
   , config : getAppConfig appConfig
}

selectedContactData ::  Contact
selectedContactData = 
  { name : "", phoneNo : "" } 

contactColorsList :: Array (Array Color)
contactColorsList = [
    [Color.yellow900, Color.black800],
    [Color.blue800, Color.white900],
    [Color.orange800, Color.black800]
]
