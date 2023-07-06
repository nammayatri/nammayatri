module Screens.EmergencyContactsScreen.Controller where

import Prelude (bind , class Show, pure, unit, ($), discard, (/=), (&&), (>=), (==), map, (<), (||), not, (-), (<>), (>), (<=), Unit, show, (+))
import Screens.Types (EmergencyContactsScreenState, Contacts)
import PrestoDOM (Eval, continue, exit, continueWithCmd, updateAndExit, ScrollState(..))
import PrestoDOM.Types.Core (class Loggable)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.NewContact.Controller as NewContactController
import Log (printLog, trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import JBridge (toast, loaderText, toggleLoader)
import Screens.Types (EmergencyContactsScreenState , ContactDetail, NewContacts, NewContactsProp)
import Helpers.Utils (storeCallBackContacts, parseNewContacts, contactPermission, setText', toString, setEnabled, setRefreshing)
import Data.Array (length, filter, snoc, elem, null, unionBy, elem, head, tail, catMaybes, (!!), take, last, slice, union)
import Log (printLog)
import Screens.EmergencyContactsScreen.Transformer (getContactList)
-- import Components.ContactList as ContactListController
import Language.Strings (getString)
import Language.Types (STR(..))
import Debug (spy)
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Styles.Types
import Styles.Colors as Color
import Components.PopUpModal.Controller as PopUpModal
import Engineering.Helpers.Commons (getNewIDWithTag, flowRunner)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String as DS
import Data.Int (fromString)
import Presto.Core.Types.Language.Flow (Flow)
import Types.App (GlobalState, defaultGlobalState)
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons (clearTimer, flowRunner, getNewIDWithTag, os, strToBool)
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.String (split, Pattern(..), Replacement(..), replaceAll)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Data.Ord (min)


instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen EMERGENCY_CONTACS_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen EMERGENCY_CONTACS_SCREEN)
      trackAppEndScreen appId (getScreen EMERGENCY_CONTACS_SCREEN)
    GenericHeaderActionController act -> case act of
      GenericHeader.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen EMERGENCY_CONTACS_SCREEN) "generic_header_action" "back_icon_onclick"
        trackAppEndScreen appId (getScreen EMERGENCY_CONTACS_SCREEN)
      _ -> pure unit
    PrimaryButtonActionControll act -> case act of
      PrimaryButton.OnClick -> trackAppActionClick appId (getScreen EMERGENCY_CONTACS_SCREEN) "contacts_list" "primary_btn_onclick"
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen EMERGENCY_CONTACS_SCREEN) "contacts_list" "primary_btn_noaction"
    RemoveButtonClicked contacts -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "remove_button_click_action"
    NoAction -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "no_action"
    ContactsCallback contacts -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "contacts_callback"
    ContactListGenericHeaderActionController act -> case act of
      GenericHeader.PrefixImgOnClick -> trackAppActionClick appId (getScreen EMERGENCY_CONTACS_SCREEN) "generic_header_action" "back_icon_onclick"
      _ -> pure unit
    ContactTextChanged value -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "contact_text_changed"
    ContactListClearText -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "clear_text"
    ContactListContactSelected item -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "contacts_callback"
    ContactListPrimaryButtonActionController onclick-> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "primary_button_action_controller"
    ContactListScroll value -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "contact_text_changed"
    CheckingContactList -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "checking_contact_list"
    PopUpModalAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "pop_up_modal_action_on_button1_click"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "pop_up_modal_action_on_button2_click"
      _ -> pure unit
    FetchContacts -> trackAppActionClick appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "fetch_contacts"
    LoadMoreContacts -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "loading more contacts"
    _ -> pure unit
data Action = GenericHeaderActionController GenericHeader.Action
            | PrimaryButtonActionControll PrimaryButton.Action
            | RemoveButtonClicked NewContacts
            | BackPressed
            | AfterRender
            | NoAction
            | ContactsCallback (Array Contacts)
            | CheckingContactList
            | PopUpModalAction PopUpModal.Action
            | FetchContacts
            | LoadMoreContacts
            | ContactListPrimaryButtonActionController PrimaryButton.Action
            | ContactListGenericHeaderActionController GenericHeader.Action
            | ContactListContactSelected NewContacts
            | ContactTextChanged String
            | ContactListPrimaryEditTextAction PrimaryEditTextController.Action
            | ContactListClearText
            | ContactListScroll String
            | ContactListScrollStateChanged ScrollState
            | NewContactActionController NewContactController.Action

data ScreenOutput = GoToHomeScreen
                  | PostContacts EmergencyContactsScreenState
                  | GetContacts EmergencyContactsScreenState

eval :: Action -> EmergencyContactsScreenState -> Eval Action ScreenOutput EmergencyContactsScreenState
eval (PrimaryButtonActionControll PrimaryButton.OnClick) state = continueWithCmd state
      [do
        _ <- launchAff $ flowRunner defaultGlobalState $ do
                _ <- loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
                _ <- toggleLoader true
                pure unit

        _ <- pure $ contactPermission unit
        pure NoAction
      ]

eval (PopUpModalAction PopUpModal.OnButton2Click) state = do
  let newContacts = filter (\x -> x.number <> x.name /= state.data.removedContactDetail.number <> state.data.removedContactDetail.name) state.data.contactsList
  contactsInString <- pure $ toString newContacts
  _ <- pure $ setValueToLocalStore CONTACTS contactsInString
  exit $ PostContacts state{data{contactsList = newContacts}}

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state{props{showInfoPopUp = false}}

eval (ContactsCallback allContacts) state = do
  let flag = case last allContacts of
              Just contact ->  if (contact.name == "beckn_contacts_flag") && (contact.number == "true") then "true" else "NA" -- TODO :: Need to refactor @Chakradhar
              Nothing -> "false"
      updatedContactList = case (last allContacts) of
              Just contact ->  if (contact.name == "beckn_contacts_flag") then take ((length allContacts) - 1) allContacts else allContacts -- TODO :: Need to refactor @Chakradhar
              Nothing -> allContacts
  if(flag == "false") then do
    _ <- pure $ toast (getString PERMISSION_DENIED)
    continueWithCmd state
      [do
        _ <- launchAff $ flowRunner defaultGlobalState $ do
            _ <- toggleLoader false
            pure unit
        pure NoAction
      ]
  else if (null updatedContactList) then do
    _ <- pure $ toast (getString NO_CONTACTS_FOUND_ON_DEVICE_TO_ADD)
    continueWithCmd state
      [do
        _ <- launchAff $ flowRunner defaultGlobalState $ do
            _ <- toggleLoader false
            pure unit
        pure NoAction
      ]
  else do
    _ <- pure $ spy "xyz allContacts" allContacts
    let newContacts = getContactList updatedContactList
    localContacts <- pure $ getValueToLocalStore CONTACTS
    contactsInJson <- pure $ parseNewContacts localContacts
    let filteredContacts = filter (\x -> filter (\y -> (validContact x.number) <> x.name == (validContact y.number) <> y.name) contactsInJson == [] ) newContacts
    let unionNewContacts = uniqueContacts [] filteredContacts
    if (null unionNewContacts) then do
      _ <- pure $ toast (getString NO_CONTACTS_LEFT_ON_DEVICE_TO_ADD)
      continueWithCmd state
        [do
          _ <- launchAff $ flowRunner defaultGlobalState $ do
              _ <- toggleLoader false
              pure unit
          pure NoAction
        ]
    else do
      continueWithCmd state{data{contactsList = newContacts, contactsNewList = unionNewContacts, contactsUpdatedNewList = newContacts, contactsCount = 0}, props{showContactList = true}}
        [do
          _ <- launchAff $ flowRunner defaultGlobalState $ do
              _ <- toggleLoader false
              pure unit
          pure LoadMoreContacts
        ]
    where
      validContact :: String -> String
      validContact contact =
        if ((DS.length contact) > 10 && (DS.length contact) <= 12 && ((DS.take 1 contact) == "0" || (DS.take 2 contact) == "91")) then
          DS.drop ((DS.length contact) - 10) contact
        else contact


eval (ContactListGenericHeaderActionController (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval (RemoveButtonClicked contactDetail) state = continue state{props{showInfoPopUp = true}, data{removedContactDetail = contactDetail}}

eval BackPressed state =
  if(state.props.showContactList) then do
    let newState = state { data = state.data { editedText = "", contactsNewList = map (\x ->
      if (x.isSelected == true) then x { isSelected = not (x.isSelected) }
      else x
    ) state.data.contactsNewList
    }, props = state.props { showContactList = false } }
    continue newState
  else if (state.props.showInfoPopUp == true) then
    continue state{props{showInfoPopUp = false}}
  else
    exit $ GoToHomeScreen

eval (ContactListGenericHeaderActionController GenericHeader.PrefixImgOnClick) state = do
  let newState = state { data = state.data { editedText = "", contactsNewList = map (\x ->
      if (x.isSelected == true) then x { isSelected = not (x.isSelected) }
      else x
    ) state.data.contactsNewList
  }, props = state.props { showContactList = false } }
  continue newState

-- eval (PrimaryButtonActionControl PrimaryButton.OnClick) state = do
--   continue state{ data{ offsetForEmergencyContacts = state.data.offsetForEmergencyContacts + state.data.limitForEmergencyContacts, limitForEmergencyContacts = state.data.limitForEmergencyContacts + state.data.limitForEmergencyContacts}} 

eval (ContactListScroll value) state = do
  let firstIndex = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!0)))
  let visibleItems = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!1)))
  let totalItems = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!2)))
  let loadMoreButton = if (totalItems == (firstIndex + visibleItems) && totalItems /= 0 && totalItems /= visibleItems) then true else false
  if loadMoreButton  then continueWithCmd state [do pure LoadMoreContacts]
  else continue state { data{loadMoreDisabled = loadMoreButton}}

eval LoadMoreContacts state = do
  let contactsList = sliceContacts state
  let bufferCardDataPrestoList = ((contactListTransformerProp (contactsList)))
  let loaderBtnDisabled = if(length (contactsList)== 0) then true else false
  let offsetForContacts = if (loaderBtnDisabled) then state.data.offsetForEmergencyContacts else state.data.offsetForEmergencyContacts + length contactsList
  continue $ state { data {prestoListArrayItems = union (state.data.prestoListArrayItems) (bufferCardDataPrestoList) , loadMoreDisabled = loaderBtnDisabled, offsetForEmergencyContacts = offsetForContacts}}


eval (ContactTextChanged value) state = do
  let newArray = findContactsWithPrefix value state.data.contactsList
  continueWithCmd state{ data { editedText = value , contactsUpdatedNewList = newArray, offsetForEmergencyContacts = 0, prestoListArrayItems = []} }
    [do
      _ <- launchAff $ flowRunner defaultGlobalState $ do
          _ <- toggleLoader false
          pure unit
      pure LoadMoreContacts
    ]

eval (ContactListClearText) state = continueWithCmd state { data { editedText = "" } }
  [do
    _ <- (setText' (getNewIDWithTag "contactEditText") "")
    pure NoAction
  ]

eval (NewContactActionController (NewContactController.ContactSelected index)) state = do
  let contact = fromMaybe {isSelected : false , name : "" , number : ""} (state.data.contactsUpdatedNewList !! index)
  _ <- pure $ spy "xyz contact" contact
  -- continue state
  let item = if ((DS.length contact.number) > 10 && (DS.length contact.number) <= 12 && ((DS.take 1 contact.number) == "0" || (DS.take 2 contact.number) == "91")) then
    contact {number =  DS.drop ((DS.length contact.number) - 10) contact.number}
  else contact
  if ((state.data.contactsCount >= (3 - (length state.data.contactsList))) && (item.isSelected == false)) then do
    _ <- pure $ toast (getString MAXIMUM_CONTACTS_LIMIT_REACHED)
    continue state
  else if((DS.length item.number) /= 10 || (fromMaybe 0 (fromString (DS.take 1 item.number)) < 6)) then do
    _ <- pure $ toast (getString SELECTED_CONTACT_IS_INVALID)
    continue state
  else do
    let newState = state { data = state.data { contactsNewList = map (\x ->
      if ((x.number <> x.name  == contact.number <> contact.name) && (state.data.contactsCount < (3 - (length state.data.contactsList)) || x.isSelected == true)) then x { isSelected = not (x.isSelected) }
      else x
      ) state.data.contactsNewList
    }}
    let selectedContactsCount = spy "contactsCount" (length $ filter (\x -> x.isSelected) newState.data.contactsNewList)
    _ <- pure $ spy "xyz selectedContactsCount" selectedContactsCount
    continue newState{data{contactsCount = selectedContactsCount}}

eval (ContactListPrimaryButtonActionController PrimaryButton.OnClick) state = do
  let selectedContacts = (filter (\x -> x.isSelected) state.data.contactsNewList) <> state.data.contactsList
  let validSelectedContacts = (map (\contact ->
    if ((DS.length contact.number) > 10 && (DS.length contact.number) <= 12 && ((DS.take 1 contact.number) == "0" || (DS.take 2 contact.number) == "91")) then
      contact {number =  DS.drop ((DS.length contact.number) - 10) contact.number}
    else contact
  ) selectedContacts)
  contactsInString <- pure $ toString validSelectedContacts
  _ <- pure $ setValueToLocalStore CONTACTS contactsInString
  updateAndExit state $ PostContacts state{data{editedText = "", contactsList = validSelectedContacts}, props{showContactList = false}}

eval CheckingContactList state = do
  contacts <- pure $ getValueToLocalStore CONTACTS
  _ <- pure $ spy "CheckingContactList" contacts
  if (contacts /= "") then do
    contactsInJson <- pure $ parseNewContacts contacts
    continue state{data{contactsList = contactsInJson}}
    else do
      continue state

eval FetchContacts state =
  updateAndExit state $ GetContacts state

eval _ state = continue state

startsWith :: String -> String -> Boolean
startsWith prefix str = DS.take (DS.length prefix) (DS.toLower str) == (DS.toLower prefix)

findContactsWithPrefix :: String -> Array NewContacts -> Array NewContacts
findContactsWithPrefix prefix arr = filter (\contact -> startsWith prefix contact.name) arr

contactColorsList :: Array (Array Color)
contactColorsList = [
    [Color.yellow900, Color.black800],
    [Color.blue800, Color.white900],
    [Color.orange800, Color.black800]
]

uniqueContacts :: Array NewContacts -> Array NewContacts -> Array NewContacts
uniqueContacts result contacts =
  case head contacts of
    Just contact' ->
      case elem contact' result of
        true  -> uniqueContacts result (fromMaybe [] (tail contacts))
        false -> uniqueContacts (result <> (catMaybes [head contacts])) (fromMaybe [] (tail contacts))
    Nothing      -> result
uniqueContacts result [] = result

contactListTransformerProp :: Array NewContacts -> Array NewContactsProp 
contactListTransformerProp contactList =(map (\(contact) -> {
  name: toPropValue (contact.name),
  number: toPropValue (contact.number),
  isSelected: toPropValue (contact.isSelected)
})(contactList))

contactListTransformer :: Array NewContacts -> Array NewContacts 
contactListTransformer contactList = (map (\(contact)->{
  name: contact.name,
  number: contact.number,
  isSelected: contact.isSelected
})(contactList))

sliceContacts :: EmergencyContactsScreenState -> Array NewContacts
sliceContacts config = do
  let tempLastIndex= config.data.limitForEmergencyContacts + config.data.offsetForEmergencyContacts
  let lastIndex = if ((length (config.data.contactsUpdatedNewList)) < tempLastIndex) then length (config.data.contactsUpdatedNewList) else tempLastIndex 
  slice config.data.offsetForEmergencyContacts lastIndex config.data.contactsUpdatedNewList