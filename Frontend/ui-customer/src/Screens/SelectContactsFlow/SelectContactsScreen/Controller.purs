{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.SelectContactsFlow.SelectContactsScreen.Controller where

import Prelude
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)
import Screens.SelectContactsFlow.SelectContactsScreen.ScreenData
import Prelude (class Show, bind, compare, discard, map, not, pure, unit, ($), (&&), (*), (+), (-), (/), (/=), (<), (<=), (<>), (==), (>), (>=), (||))
import PrestoDOM (Eval, update, ScrollState, continue, continueWithCmd, exit)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.NewContact.Controller as NewContactController
import JBridge (hideKeyboardOnNavigation)
import Engineering.Helpers.Utils as EHU
import Screens.Types (Contacts, NewContacts, NewContactsProp)
import Data.Array (catMaybes, delete, dropEnd, elem, filter, head, last, length, mapWithIndex, nubByEq, null, slice, snoc, sortBy, tail, updateAt, (!!), mapMaybe, deleteBy, unionBy)
import Helpers.Utils (contactPermission, setEnabled, setRefreshing, setText)
import Screens.EmergencyContactsScreen.Transformer (getContactList)
import Language.Strings (getString)
import Language.Types (STR(..))
import Styles.Colors as Color
import Engineering.Helpers.Commons (getNewIDWithTag, flowRunner, liftFlow)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String as DS
import Data.Array as DA
import Data.Set as DSet
import Data.Int (fromString, toNumber)
import Types.App (defaultGlobalState)
import Effect.Aff (launchAff)
import Engineering.Helpers.Utils (loaderText, terminateLoader, toggleLoader)
import Engineering.Helpers.LogEvent (logEvent)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Data.Either (Either(..))
import Storage (KeyStore(..), getValueToLocalStore)
import Data.Tuple(Tuple(..))
import Mobility.Prelude (startsWith)
import Debug (spy)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

data Action
  = AfterRender
  | ContactsCallback (Array Contacts)
  | KeyboardCallback String
  | NoAction
  | BackPressAction
  | ContactListGenericHeaderActionController GenericHeader.Action
  | ContactListClearText
  | ContactTextChanged String
  | PrimaryButtonAC PrimaryButton.Action
  | NewContactActionController NewContactController.Action

data ScreenOutput
  = BackPressed
  | ExecuteCallback SelectContactsScreenState

eval :: forall a. Action -> SelectContactsScreenState -> Eval Action ScreenOutput SelectContactsScreenState

eval (ContactListGenericHeaderActionController GenericHeader.PrefixImgOnClick) state = continueWithCmd state [ do pure BackPressAction ]

eval AfterRender state = do
  continueWithCmd state
    [ do
        _ <-
          launchAff $ flowRunner defaultGlobalState
            $ do
                _ <- loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
                _ <- toggleLoader true
                liftFlow $ contactPermission unit
        pure NoAction
    ]

eval (ContactListClearText) state =
  continueWithCmd state
    [ do
        _ <- (pure $ setText (getNewIDWithTag "contactEditText") "")
        pure NoAction
    ]

eval (ContactTextChanged value) state = do
  let
    newArray = findContactsWithPrefix value state.data.contacts

    prestoList = contactListTransformerProp newArray
  continue state { data { searchResult = newArray, prestoListContacts = prestoList } }

eval BackPressAction state = exit BackPressed

eval (PrimaryButtonAC PrimaryButton.OnClick) state = 
  exit $ ExecuteCallback state

eval (NewContactActionController (NewContactController.ContactSelected index)) state = do
  let _ = spy "debug contact ContactSelected index" index
  case state.data.searchResult !! index of
    Just (contact :: NewContacts) -> do
      let item = (getValidContact contact){ isSelected = not contact.isSelected }
      if (length state.data.selectedContacts) >= state.data.contactSelectionLimit && not contact.isSelected then do
        _ <- pure $ EHU.showToast $ getString LIMIT_REACHED
        continue state
      else if DS.length item.number /= 10 then do
        _ <- pure $ EHU.showToast (getString INVALID_CONTACT_FORMAT)
        continue state
      else do
        let contactToUpdate = contact { isSelected = not contact.isSelected }
            updatedSelectedContacts = if item.isSelected then snoc state.data.selectedContacts contactToUpdate else deleteBy (\a b -> a.name == b.name && a.number == b.number) contact state.data.selectedContacts
            updatedContactsList = updateAt index contactToUpdate state.data.searchResult
        case updatedContactsList of
          Nothing -> continue state
          Just updatedArray -> do
            let contactPropValue = contactTransformerProp contactToUpdate
                updatedPrestoList = updateAt index contactPropValue state.data.prestoListContacts
            case updatedPrestoList of
              Nothing -> continue state
              Just list -> do
                let contactList =
                      map
                      ( \contactItem ->
                          if contactItem.name == contact.name && contactItem.number == contact.number then contactToUpdate else contactItem
                      )
                      state.data.contacts
                continue state { data { selectedContacts = updatedSelectedContacts, contacts = contactList, searchResult = updatedArray, prestoListContacts = list } } 
    Nothing -> do
      _ <- pure $ EHU.showToast (getString INVALID_CONTACT_FORMAT)
      continue state

eval (ContactsCallback allContacts) state = do
  let
    flag = case last allContacts of
      Just contact -> if (contact.name == "beckn_contacts_flag") && (contact.number == "true") then "true" else "NA" -- TODO :: Need to refactor @Chakradhar
      Nothing -> "false"

    updatedContactList = case (last allContacts) of
      Just contact -> if (contact.name == "beckn_contacts_flag") then dropEnd 1 allContacts else allContacts -- TODO :: Need to refactor @Chakradhar
      Nothing -> allContacts
  if flag == "false" then do
    _ <- pure $ EHU.showToast (getString PLEASE_ENABLE_CONTACTS_PERMISSION_TO_PROCEED)
    goBack state
  else if (null updatedContactList) then do
    _ <- pure $ EHU.showToast (getString NO_CONTACTS_FOUND_ON_THE_DEVICE_TO_BE_ADDED)
    goBack state
  else do
    let
      userMobileNumber = getValueToLocalStore MOBILE_NUMBER
      isUserMobileNumberNotPresent = elem userMobileNumber ["", "(null)", "__failed"]
      filteredContacts =
        mapMaybe
          ( \contactItem -> do
              let formattedContact = getFormattedContact contactItem
              if formattedContact.number /= userMobileNumber || isUserMobileNumberNotPresent
                then do 
                  Just $ case getContactFromEMList formattedContact state.data.contacts of
                    Nothing -> formattedContact
                    Just contact -> formattedContact { isSelected = true, priority = contact.priority, enableForFollowing = contact.enableForFollowing }
                else Nothing
          )
          $ getContactList updatedContactList

      unionNewContacts = nubByEq (\a b -> a.number == b.number) filteredContacts
      updateWithSelected = updateSelectedContacts unionNewContacts state.data.selectedContacts
      updatedSelectedContactFlag = map (\contact -> contact{ isSelected = true }) state.data.selectedContacts

      bufferCardDataPrestoList = contactListTransformerProp updateWithSelected
    if null updateWithSelected then do
      _ <- pure $ EHU.showToast (getString NO_CONTACTS_LEFT_ON_DEVICE_TO_ADD)
      goBack state
    else do
      let
        newState = state { data { contacts = updateWithSelected
                                , searchResult = updateWithSelected
                                , prestoListContacts = bufferCardDataPrestoList 
                                , selectedContacts = updatedSelectedContactFlag 
                                , alreadySelectedContacts = updatedSelectedContactFlag }
                         , props{ showContacts = true } }
      removeLoader newState
  where
  removeLoader updatedState =
    continueWithCmd updatedState
      [ do
          void $ terminateLoader ""
          _ <-
            launchAff $ flowRunner defaultGlobalState
              $ void $ toggleLoader false
          pure NoAction
      ]
  goBack updatedState =
    continueWithCmd updatedState
      [ do
          void $ terminateLoader ""
          _ <-
            launchAff $ flowRunner defaultGlobalState
              $ void $ toggleLoader false
          pure BackPressAction
      ]

eval _ state = continue state

------------------------------------ Helpers -----------------------------------
getFormattedContact :: NewContacts -> NewContacts
getFormattedContact contact =
  let
    eiRegexPattern = regex "\\D" global

    formattedNumber = case eiRegexPattern of
      Right regexPattern -> replace regexPattern "" contact.number
      Left _ -> contact.number
  in
    contact { number = formattedNumber }

getContactFromEMList :: NewContacts -> Array NewContacts -> Maybe NewContacts
getContactFromEMList contact contactsList = head $ filter (\x -> ((if DS.length contact.number == 10 then "" else "91") <> x.number <> x.name == contact.number <> contact.name)) contactsList

contactListTransformerProp :: Array NewContacts -> Array NewContactsProp
contactListTransformerProp = map \contact -> contactTransformerProp contact

contactTransformerProp :: NewContacts -> NewContactsProp
contactTransformerProp contact =
  { name: toPropValue $ contact.name
  , number: toPropValue $ contact.number
  , contactBackgroundColor: toPropValue $ if contact.isSelected then Color.grey900 else Color.white900
  , visibilitySelectedImage: toPropValue if contact.isSelected then "visible" else "gone"
  , visibilityUnSelectedImage: toPropValue $ if contact.isSelected then "gone" else "visible"
  }

findContactsWithPrefix :: String -> Array NewContacts -> Array NewContacts
findContactsWithPrefix prefix arr =
  let
    filteredResultWithStartsWith =
      filter
        ( \contact ->
            let
              nameParts = DS.split (DS.Pattern " ") (DS.toLower contact.name)
            in
              DA.any (\part -> startsWith (DS.toLower prefix) part) nameParts
        )
        arr
  in
    if null filteredResultWithStartsWith then
      filter (\contact -> DS.contains (DS.Pattern $ DS.toLower prefix) (DS.toLower contact.name)) arr
    else
      sortBy (compareByMatchPercentage prefix) filteredResultWithStartsWith
  where 
    compareByMatchPercentage pre a b = compare (calculateMatchPercentage pre b.name) (calculateMatchPercentage pre a.name)

    calculateMatchPercentage :: String -> String -> Number
    calculateMatchPercentage searchString name =
      let
        searchLength = DS.length searchString
        nameLength = DS.length name
        unmatchedLength = nameLength - searchLength
      in
        100.0 - (toNumber unmatchedLength / toNumber nameLength) * 100.0

getValidContact :: NewContacts -> NewContacts
getValidContact contact =
  if ((DS.length contact.number) > 10 && (DS.length contact.number) <= 12 && ((DS.take 1 contact.number) == "0" || (DS.take 2 contact.number) == "91")) then
    contact { number = DS.drop ((DS.length contact.number) - 10) contact.number }
  else
    contact

updateSelectedContacts :: forall a. Eq String => Array NewContacts -> Array NewContacts -> Array NewContacts
updateSelectedContacts contacts selectedContacts =
  let selectedKeys = map (\contact -> Tuple contact.name contact.number) selectedContacts -- Extract name-user pairs from selectedContacts
  in map (\contact -> 
              if DA.elem (Tuple contact.name contact.number) selectedKeys 
              then contact { isSelected = true } 
              else contact
           ) contacts