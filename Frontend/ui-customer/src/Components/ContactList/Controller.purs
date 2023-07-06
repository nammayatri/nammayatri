 module Components.ContactList.Controller where

-- import Components.PrimaryButton.Controller as PrimaryButtonController
-- import Components.PrimaryButton as PrimaryButton
-- import Components.GenericHeader.Controller as GenericHeaderController
-- import Prelude
-- import Screens.Types(NewContacts, NewContactsProp)
-- import PrestoDOM (Eval, continue, continueWithCmd, ScrollState(..), exit, updateAndExit)
-- import Data.String as DS
-- import Components.PrimaryEditText.Controller as PrimaryEditTextController
-- import Data.Array (filter, sortBy)
-- import Data.String (Pattern(..), contains)
-- import PrestoDOM.Types.Core (class Loggable, toPropValue)

-- data Action = Click NewContacts
--             | NoAction
--             | BackPressed
--             | ContactListPrimaryButtonActionController PrimaryButtonController.Action
--             | ContactListGenericHeaderActionController GenericHeaderController.Action
--             | ContactListContactSelected NewContacts
--             | ContactListContactTextChanged String
--             | ContactListPrimaryEditTextAction PrimaryEditTextController.Action
--             | ContactListClearText
--             | SContactListcroll String
--             | ContactListScrollStateChanged ScrollState

-- type ContactsState = {
--   contactsData :: Array NewContacts,
--   count :: Int,
--   contactList :: Array NewContacts,
--   editedText :: String,
--   offsetForEmergencyContacts :: Int,
--   limitForEmergencyContacts :: Int,
--   prestoListArrayItems ::Array NewContactsProp,
--   loadMoreDisabled :: Boolean
-- }

-- aToz :: Array String
-- aToz = [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ]

-- sortedContactData :: Array NewContacts -> Array NewContacts
-- sortedContactData config = sortBy (\a b -> compare (a.name) (b.name)) config


-- searchContacts :: String -> Array NewContacts -> Array NewContacts
-- searchContacts value config =
--   let
--     searchResultName = filter (\item -> (contains (Pattern (DS.toLower value)) (DS.toLower item.name))) config
--     searchResultNumber = filter (\item -> (contains (Pattern (DS.toLower value)) (DS.toLower item.number))) config
--   in
--     if (value == "") then
--       config
--     else if (searchResultName == []) then
--       searchResultNumber
--     else
--       searchResultName