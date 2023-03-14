module Components.EmergencyHelp.Controller where
import Components.PopUpModal.Controller as PopUpModalController
import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Screens.Types (Contact)
import Prelude
import PrestoDOM (Eval, continue, exit, continueWithCmd)
import Styles.Types
import Styles.Colors as Color

data Action = NoAction 
            | CallPolicePopup
            | ContactSupportPopup
            | CallPolice PopUpModalController.Action
            | ContactSupport PopUpModalController.Action
            | GenericHeaderAC GenericHeaderController.Action

type EmergencyHelpModelState = {
     showContactSupportPopUp :: Boolean,
     showCallPolicePopUp :: Boolean, 
     emergencyContactData :: Array Contact
}

config :: EmergencyHelpModelState 
config = {
     showContactSupportPopUp : false
   , showCallPolicePopUp : false
   , emergencyContactData : []
}

contactColorsList :: Array (Array Color)
contactColorsList = [
    [Color.yellow900, Color.black800],
    [Color.blue800, Color.white900],
    [Color.orange800, Color.black800]
]
