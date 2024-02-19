{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.PopupWithCheckbox.Controller where

import Prelude

import Components.PrimaryButton as PrimaryButton
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.Types (NewContacts)

data Action = DismissPopup
            | ClickPrimaryButton PrimaryButton.Action
            | ClickSecondaryButton
            | ToggleSelect Int
            | ContactAction ContactCircle.Action

type Config = {
    title :: String,
    description :: String,
    secondaryButtonText :: String,
    secondaryButtonVisibliity :: Boolean,
    checkboxList :: Array CheckBoxOption,
    contactList :: Array NewContacts,
    primaryButtonConfig :: PrimaryButton.Config,
    secondaryButtonImage :: String
}

type CheckBoxOption = {
    label :: String,
    selected :: Boolean
}

config :: Config
config = 
  { title : "",
    description : "",
    secondaryButtonText : "",
    secondaryButtonVisibliity : false,
    checkboxList : [],
    contactList : [],
    primaryButtonConfig : PrimaryButton.config,
    secondaryButtonImage : ""
  }