module Components.ContactList.Controller where

import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.GenericHeader.Controller as GenericHeaderController
import Prelude
import Screens.Types(NewContacts)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import Data.String as DS
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Data.Array (filter, sortBy)
import Data.String (Pattern(..), contains)

data Action = Click NewContacts
            | NoAction
            | BackPressed
            | PrimaryButtonActionController PrimaryButtonController.Action
            | GenericHeaderActionController GenericHeaderController.Action
            | ContactSelected NewContacts
            | ContactTextChanged String
            | PrimaryEditTextAction PrimaryEditTextController.Action
            | ClearText

type ContactsState = {
  contactsData :: Array NewContacts,
  count :: Int,
  contactList :: Array NewContacts,
  editedText :: String
}

aToz :: Array String
aToz = [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ]

sortedContactData :: String -> Array NewContacts -> Array NewContacts
sortedContactData x config = sortBy (\a b -> compare (a.name) (b.name)) (filter (\item -> DS.take 1 item.name == x) config)

searchContacts :: String -> Array NewContacts -> Array NewContacts
searchContacts value config =
  let
    searchResultName = filter (\item -> (contains (Pattern (DS.toLower value)) (DS.toLower item.name))) config
    searchResultNumber = filter (\item -> (contains (Pattern (DS.toLower value)) (DS.toLower item.number))) config
  in
    if (value == "") then
      config
    else if (searchResultName == []) then
      searchResultNumber
    else
      searchResultName