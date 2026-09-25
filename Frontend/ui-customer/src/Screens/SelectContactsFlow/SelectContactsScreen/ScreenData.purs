{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SelectContactsFlow.SelectContactsScreen.ScreenData where

import Prelude (Unit)
import Data.Maybe (Maybe(..))
import Screens.Types (NewContacts, NewContactsProp)
import ConfigProvider

initData :: SelectContactsScreenState
initData =
  { data : {
      contacts : [],
      searchResult : [],
      prestoListContacts : [],
      selectedContacts : [],
      alreadySelectedContacts : [],
      contactSelectionLimit : 3
    },
    props : {
      showContacts : false
    }
  }

type SelectContactsScreenState = {
  data :: SelectContactsScreenData,
  props :: SelectContactsScreenProps
}

type SelectContactsScreenData = {
  contacts :: Array NewContacts,
  searchResult :: Array NewContacts,
  prestoListContacts :: Array NewContactsProp,
  selectedContacts :: Array NewContacts,
  alreadySelectedContacts :: Array NewContacts,
  contactSelectionLimit :: Int
}

type SelectContactsScreenProps = {
  showContacts :: Boolean
}