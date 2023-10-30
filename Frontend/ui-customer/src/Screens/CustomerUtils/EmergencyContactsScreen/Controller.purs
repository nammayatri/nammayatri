module Screens.EmergencyContactsScreen.Controller where

import Prelude (bind, compare, class Show, pure, unit, ($), discard, (/=), (&&), (>=), (==), map, (<), (||), not, (-), (<>), (>), (<=), Unit, show, (+))
import Screens.Types (EmergencyContactsScreenState, Contacts)
import PrestoDOM (Eval, continue, exit, continueWithCmd, updateAndExit, ScrollState(..))
import PrestoDOM.Types.Core (class Loggable)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.NewContact.Controller as NewContactController
import Log (printLog, trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import JBridge (toast)
import Screens.Types (EmergencyContactsScreenState , ContactDetail, NewContacts, NewContactsProp)
import Data.Array (length, sortBy, filter, snoc, elem, null, unionBy, elem, head, tail, catMaybes, (!!), take, last, slice, union)
import Helpers.Utils (storeCallBackContacts, parseNewContacts, contactPermission, setText, toStringJSON, setEnabled, setRefreshing)
import Log (printLog)
import Screens.EmergencyContactsScreen.Transformer (getContactList)
import Language.Strings (getString)
import Language.Types (STR(..))
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
import Engineering.Helpers.Utils (loaderText, toggleLoader, fromProp)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)

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
            | RefreshScreen
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
                  | Refresh EmergencyContactsScreenState

eval :: Action -> EmergencyContactsScreenState -> Eval Action ScreenOutput EmergencyContactsScreenState
eval (PrimaryButtonActionControll PrimaryButton.OnClick) state = continueWithCmd state
      [do
        _ <- pure $ setRefreshing (getNewIDWithTag "EmergencyContactTag")false
        pure $ setEnabled (getNewIDWithTag "EmergencyContactTag") false
        _ <- launchAff $ flowRunner defaultGlobalState $ do
                _ <- loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
                _ <- toggleLoader true
                pure unit

        _ <- pure $ contactPermission unit
        let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_add_emergency_contact_click"
        pure NoAction
      ]

eval (ContactListScrollStateChanged scrollState) state = continue state

eval (PopUpModalAction PopUpModal.OnButton2Click) state = do
  let newContacts = filter (\x -> x.number <> x.name /= state.data.removedContactDetail.number <> state.data.removedContactDetail.name) state.data.contactsList
  let newPrestoList = map (\x -> if ((fromProp x.number) <> (fromProp x.name) == state.data.removedContactDetail.number <> state.data.removedContactDetail.name) then contactTransformerProp state.data.removedContactDetail{isSelected = false} else x) state.data.prestoListArrayItems
  contactsInString <- pure $ toStringJSON newContacts
  _ <- pure $ setValueToLocalStore CONTACTS contactsInString
  exit $ PostContacts state{data{contactsList = newContacts, prestoListArrayItems = newPrestoList}}

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state{props{showInfoPopUp = false}}

eval (ContactsCallback allContacts) state = do
  let flag = case last allContacts of
              Just contact ->  if (contact.name == "beckn_contacts_flag") && (contact.number == "true") then "true" else "NA" -- TODO :: Need to refactor @Chakradhar
              Nothing -> "false"
      updatedContactList = case (last allContacts) of
              Just contact ->  if (contact.name == "beckn_contacts_flag") then take ((length allContacts) - 1) allContacts else allContacts -- TODO :: Need to refactor @Chakradhar
              Nothing -> allContacts
  if(flag == "false") then do
    _ <- pure $ toast (getString PLEASE_ENABLE_CONTACTS_PERMISSION_TO_PROCEED)
    continueWithCmd state
      [do
        _ <- launchAff $ flowRunner defaultGlobalState $ do
            _ <- toggleLoader false
            pure unit
        pure NoAction
      ]
  else if (null updatedContactList) then do
    _ <- pure $ toast (getString NO_CONTACTS_FOUND_ON_THE_DEVICE_TO_BE_ADDED)
    continueWithCmd state
      [do
        _ <- launchAff $ flowRunner defaultGlobalState $ do
            _ <- toggleLoader false
            pure unit
        pure NoAction
      ]
  else do
    let newContacts = sortedContactData $ getContactList updatedContactList
    localContacts <- pure $ getValueToLocalStore CONTACTS
    contactsInJson <- pure $ parseNewContacts localContacts
    let filteredContacts = map (\x -> if(contactIsSelected x contactsInJson) then x{isSelected = true} else x ) newContacts
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
      continueWithCmd state{data{contactsNewList = unionNewContacts, contactsUpdatedNewList = unionNewContacts, contactsCount = length contactsInJson}, props{showContactList = true}}
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


eval (RemoveButtonClicked contactDetail) state = continue state{props{showInfoPopUp = true}, data{removedContactDetail = contactDetail}}

eval RefreshScreen state = do
  continueWithCmd state
      [do
        _ <- pure $ setRefreshing (getNewIDWithTag "EmergencyContactTag") false
        _ <- pure $ contactPermission unit
        pure NoAction
  ]

eval BackPressed state =
  if(state.props.showContactList) then do
    localContacts <- pure $ getValueToLocalStore CONTACTS
    contactsInJson <- pure $ parseNewContacts localContacts
    let filteredContacts = map (\x -> if(contactIsSelected x contactsInJson) then x{isSelected = true} else x {isSelected = false} ) state.data.contactsNewList
    let unionNewContacts = uniqueContacts [] filteredContacts
    continueWithCmd state{data{contactsNewList = unionNewContacts, prestoListArrayItems = [], offsetForEmergencyContacts = 0, contactsUpdatedNewList = unionNewContacts, editedText = "", contactsCount = length contactsInJson, contactsList = contactsInJson}, props{showContactList = false}}
        [do
          _ <- launchAff $ flowRunner defaultGlobalState $ do
              _ <- toggleLoader false
              pure unit
          pure LoadMoreContacts
        ]
  else if state.props.showInfoPopUp then
    continue state{props{showInfoPopUp = false, showContactList = false}}
  else
    exit $ GoToHomeScreen

eval (ContactListGenericHeaderActionController GenericHeader.PrefixImgOnClick) state = do
  if(state.props.showContactList) then do
    localContacts <- pure $ getValueToLocalStore CONTACTS
    contactsInJson <- pure $ parseNewContacts localContacts
    let filteredContacts = map (\x -> if(contactIsSelected x contactsInJson) then x{isSelected = true} else x {isSelected = false} ) state.data.contactsNewList
    let unionNewContacts = uniqueContacts [] filteredContacts
    continueWithCmd state{data{contactsNewList = unionNewContacts, prestoListArrayItems = [], offsetForEmergencyContacts = 0, contactsUpdatedNewList = unionNewContacts, editedText = "", contactsCount = length contactsInJson, contactsList = contactsInJson}, props{showContactList = false}}
        [do
          _ <- launchAff $ flowRunner defaultGlobalState $ do
              _ <- toggleLoader false
              pure unit
          pure LoadMoreContacts
        ]
  else if state.props.showInfoPopUp then
    continue state{props{showInfoPopUp = false, showContactList = false}}
  else
    exit $ GoToHomeScreen


eval (ContactListScroll value) state = do
  let firstIndex = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!0)))
  let visibleItems = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!1)))
  let totalItems = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!2)))
  let canScrollUp = if firstIndex == 0 then false else true
  let loadMoreButton = if (totalItems == (firstIndex + visibleItems) && totalItems /= 0 && totalItems /= visibleItems) then true else false
  _ <- if canScrollUp then (pure $ setEnabled (getNewIDWithTag "EmergencyContactTag") false) else  (pure $ setEnabled (getNewIDWithTag "EmergencyContactTag") true)
  if loadMoreButton  then continueWithCmd state [do pure LoadMoreContacts]
  else continue state { data{loadMoreDisabled = loadMoreButton}}

eval LoadMoreContacts state = do
  let contactsList = sliceContacts state
  let bufferCardDataPrestoList = ((contactListTransformerProp (contactsList)))
  let loaderBtnDisabled = if(length (contactsList)== 0) then true else false
  let offsetForContacts = if (loaderBtnDisabled) then state.data.offsetForEmergencyContacts else state.data.offsetForEmergencyContacts + length contactsList
  continue $ state { data {prestoListArrayItems = union (state.data.prestoListArrayItems) (bufferCardDataPrestoList) , loadMoreDisabled = loaderBtnDisabled, offsetForEmergencyContacts = offsetForContacts}}


eval (ContactTextChanged value) state = do
  let newArray = findContactsWithPrefix value state.data.contactsNewList
  continueWithCmd state{ data { editedText = value , contactsUpdatedNewList = newArray, offsetForEmergencyContacts = 0, prestoListArrayItems = []} }
    [do
      _ <- launchAff $ flowRunner defaultGlobalState $ do
          _ <- toggleLoader false
          pure unit
      pure LoadMoreContacts
    ]

eval (ContactListClearText) state = continueWithCmd state { data { editedText = "" } }
  [do
    _ <- (pure $ setText (getNewIDWithTag "contactEditText") "")
    pure NoAction
  ]

eval (NewContactActionController (NewContactController.ContactSelected index)) state = do
  let contact = fromMaybe {isSelected : false , name : "" , number : ""} (state.data.contactsUpdatedNewList !! index)
  let item = if ((DS.length contact.number) > 10 && (DS.length contact.number) <= 12 && ((DS.take 1 contact.number) == "0" || (DS.take 2 contact.number) == "91")) then
    contact {number =  DS.drop ((DS.length contact.number) - 10) contact.number}
  else contact
  if (((length state.data.contactsList) >= 3) && not item.isSelected ) then do
    _ <- pure $ toast $ getString LIMIT_REACHED_3_OF_3_EMERGENCY_CONTACTS_ALREADY_ADDED
    continue state
  else if((DS.length item.number) /= 10 || (fromMaybe 0 (fromString (DS.take 1 item.number)) < 6)) then do
    _ <- pure $ toast (getString INVALID_CONTACT_FORMAT)
    continue state
  else do
    let contactListState = if not contact.isSelected then state{ data {contactsList = (snoc state.data.contactsList item{isSelected = true}) } } else state { data {contactsList = filter (\x -> ((if DS.length contact.number == 10 then "" else "91") <> x.number <> x.name  /= contact.number <> contact.name)) state.data.contactsList}}
    let newState = contactListState { data = contactListState.data { contactsNewList = map (\x ->
      if ((x.number <> x.name  == contact.number <> contact.name)) then x { isSelected = not (x.isSelected) }
      else x
      ) contactListState.data.contactsNewList
    }}
    let updatedNewState = newState { data = newState.data { contactsUpdatedNewList = map (\x ->
      if ((x.number <> x.name  == contact.number <> contact.name)) then x { isSelected = not (x.isSelected) }
      else x
      ) newState.data.contactsUpdatedNewList
    }}
    let contactPropValue = contactTransformerProp contact{isSelected = not contact.isSelected}
    let updatedPrestoList = updatedNewState { data = updatedNewState.data { prestoListArrayItems = map (\x ->
      if ((x.number == contactPropValue.number && x.name  == contactPropValue.name)) then contactPropValue
      else x
      ) updatedNewState.data.prestoListArrayItems
    }}
    updateAndExit state $ Refresh updatedPrestoList{data{contactsCount = length updatedPrestoList.data.contactsList}}

eval (ContactListPrimaryButtonActionController PrimaryButton.OnClick) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_emergency_contact_added"
  let selectedContacts = filter (_.isSelected) state.data.contactsNewList
  let validSelectedContacts = (map (\contact ->
    if ((DS.length contact.number) > 10 && (DS.length contact.number) <= 12 && ((DS.take 1 contact.number) == "0" || (DS.take 2 contact.number) == "91")) then
      contact {number =  DS.drop ((DS.length contact.number) - 10) contact.number}
    else contact
  ) selectedContacts)
  contactsInString <- pure $ toStringJSON validSelectedContacts
  _ <- pure $ setValueToLocalStore CONTACTS contactsInString
  updateAndExit state $ PostContacts state{data{editedText = "", contactsList = validSelectedContacts, prestoListArrayItems = [], offsetForEmergencyContacts = 0}, props{showContactList = false}}

eval CheckingContactList state = do
  contacts <- pure $ getValueToLocalStore CONTACTS
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

sortedContactData :: Array NewContacts -> Array NewContacts
sortedContactData config = sortBy (\a b -> compare (a.name) (b.name)) config

contactListTransformerProp :: Array NewContacts -> Array NewContactsProp 
contactListTransformerProp contactList =(map (\(contact) -> {
  name: toPropValue (contact.name),
  number: toPropValue (contact.number),
  isSelected: toPropValue (contact.isSelected),
  contactBackgroundColor: toPropValue (if contact.isSelected then Color.grey900 else Color.white900),
  visibilitySelectedImage: toPropValue (if contact.isSelected then "visible" else "gone"),
  visibilityUnSelectedImage: toPropValue (if contact.isSelected then "gone" else "visible"),
  isSelectImage: toPropValue(if contact.isSelected then "ny_ic_selected_icon" else "ny_ic_outer_circle") 
})(contactList))

contactTransformerProp :: NewContacts -> NewContactsProp 
contactTransformerProp contact = {
  name: toPropValue (contact.name),
  number: toPropValue (contact.number),
  isSelected: toPropValue (contact.isSelected),
  contactBackgroundColor : toPropValue (if contact.isSelected then Color.grey900 else Color.white900),
  visibilitySelectedImage: toPropValue (if contact.isSelected then "visible" else "gone"),
  visibilityUnSelectedImage: toPropValue (if contact.isSelected then "gone" else "visible"),
  isSelectImage: toPropValue(if contact.isSelected then "ny_ic_selected_icon" else "ny_ic_outer_circle") 
}

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

contactIsSelected :: NewContacts -> Array NewContacts -> Boolean
contactIsSelected contact tempContactList = do
  let ele = filter(\x -> ((if DS.length contact.number == 10 then "" else "91") <> x.number <> x.name  == contact.number <> contact.name)) tempContactList
  length ele == 1