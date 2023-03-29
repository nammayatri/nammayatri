module Screens.EmergencyContactsScreen.Controller where

import Prelude (bind , class Show, pure, unit, ($), discard, (/=), (&&), (>=), (==), map, (<), (||), not, (-), (<>), (>), (<=), Unit, show)
import Screens.Types (EmergencyContactsScreenState, Contacts)
import PrestoDOM (Eval, continue, exit, continueWithCmd, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Log (printLog, trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import JBridge (toast, loaderText, toggleLoader)
import Screens.Types (EmergencyContactsScreenState , ContactDetail, NewContacts)
import Helpers.Utils (storeCallBackContacts, parseNewContacts, contactPermission, setText', toString)
import Data.Array (length, filter, snoc, elem, null, unionBy, elem, head, tail, catMaybes, (!!), take, last)
import Log (printLog)
import Screens.EmergencyContactsScreen.Transformer (getContactList)
import Components.ContactList as ContactListController
import Language.Strings (getString)
import Language.Types (STR(..))
import Debug.Trace (spy)
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Styles.Types
import Styles.Colors as Color
import Components.PopUpModal.Controller as PopUpModal
import Engineering.Helpers.Commons (getNewIDWithTag, flowRunner)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String as DS
import Data.Int (fromString)
import Presto.Core.Types.Language.Flow (Flow)
import Types.App (GlobalState)
import Effect.Aff (launchAff_)
import Engineering.Helpers.Commons (clearTimer, flowRunner, getNewIDWithTag, os)
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.String (split, Pattern(..), Replacement(..), replaceAll)

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
    ContactListAction act -> case act of
      ContactListController.GenericHeaderActionController genericHeader-> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "generic_header_action"
      ContactListController.ContactTextChanged value -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "contact_text_changed"
      ContactListController.ClearText -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "clear_text"
      ContactListController.ContactSelected item -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "contacts_callback"
      ContactListController.PrimaryButtonActionController onclick-> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "primary_button_action_controller"
      _ -> pure unit
    CheckingContactList -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "checking_contact_list"
    PopUpModalAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "pop_up_modal_action_on_button1_click"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "pop_up_modal_action_on_button2_click"
      _ -> pure unit
    FetchContacts -> trackAppActionClick appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "fetch_contacts"
    _ -> pure unit
data Action = GenericHeaderActionController GenericHeader.Action
            | PrimaryButtonActionControll PrimaryButton.Action
            | RemoveButtonClicked NewContacts
            | BackPressed
            | AfterRender
            | NoAction
            | ContactsCallback (Array Contacts)
            | ContactListAction ContactListController.Action
            | CheckingContactList
            | PopUpModalAction PopUpModal.Action
            | FetchContacts

data ScreenOutput = GoToHomeScreen
                  | PostContacts EmergencyContactsScreenState
                  | GetContacts EmergencyContactsScreenState
eval :: Action -> EmergencyContactsScreenState -> Eval Action ScreenOutput EmergencyContactsScreenState
eval (PrimaryButtonActionControll PrimaryButton.OnClick) state = continueWithCmd state
      [do
        _ <- launchAff_ $ flowRunner $ do
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
  updateAndExit state $ PostContacts state{data{contactsList = newContacts}, props{showInfoPopUp = false}}

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
        _ <- launchAff_ $ flowRunner $ do
            _ <- toggleLoader false
            pure unit
        pure NoAction
      ]
  else if (null updatedContactList) then do
    _ <- pure $ toast (getString NO_CONTACTS_FOUND_ON_DEVICE_TO_ADD)
    continueWithCmd state
      [do
        _ <- launchAff_ $ flowRunner $ do
            _ <- toggleLoader false
            pure unit
        pure NoAction
      ]
  else do
    let newContacts = getContactList updatedContactList
    localContacts <- pure $ getValueToLocalStore CONTACTS
    contactsInJson <- pure $ parseNewContacts localContacts
    let filteredContacts = filter (\x -> filter (\y -> (validContact x.number) <> x.name == (validContact y.number) <> y.name) contactsInJson == [] ) newContacts
    let unionNewContacts = uniqueContacts [] filteredContacts
    if (null unionNewContacts) then do
      _ <- pure $ toast (getString NO_CONTACTS_LEFT_ON_DEVICE_TO_ADD)
      continueWithCmd state
        [do
          _ <- launchAff_ $ flowRunner $ do
              _ <- toggleLoader false
              pure unit
          pure NoAction
        ]
    else do
      continueWithCmd state{data{contactInfoState = updatedContactList, contactsNewList = unionNewContacts, contactsCount = 0}, props{showContactList = true}}
        [do
          _ <- launchAff_ $ flowRunner $ do
              _ <- toggleLoader false
              pure unit
          pure NoAction
        ]
    where
      validContact :: String -> String
      validContact contact =
        if ((DS.length contact) > 10 && (DS.length contact) <= 12 && ((DS.take 1 contact) == "0" || (DS.take 2 contact) == "91")) then
          DS.drop ((DS.length contact) - 10) contact
        else contact


eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

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

eval (ContactListAction(ContactListController.GenericHeaderActionController GenericHeader.PrefixImgOnClick)) state = do
  let newState = state { data = state.data { editedText = "", contactsNewList = map (\x ->
      if (x.isSelected == true) then x { isSelected = not (x.isSelected) }
      else x
    ) state.data.contactsNewList
  }, props = state.props { showContactList = false } }
  continue newState

eval (ContactListAction(ContactListController.ContactTextChanged value)) state = continue state { data { editedText = value } }

eval (ContactListAction ContactListController.ClearText) state = continueWithCmd state { data { editedText = "" } }
  [do
    _ <- (setText' (getNewIDWithTag "contactEditText") "")
    pure NoAction
  ]

eval (ContactListAction(ContactListController.ContactSelected contact)) state = do
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
    continue newState{data{contactsCount = selectedContactsCount}}

eval (ContactListAction(ContactListController.PrimaryButtonActionController PrimaryButton.OnClick)) state = do
  let selectedContacts = (filter (\x -> x.isSelected) state.data.contactsNewList) <> state.data.contactsList
  let validSelectedContacts = (map (\contact ->
    if ((DS.length contact.number) > 10 && (DS.length contact.number) <= 12 && ((DS.take 1 contact.number) == "0" || (DS.take 2 contact.number) == "91")) then
      contact {number =  DS.drop ((DS.length contact.number) - 10) contact.number}
    else contact
  ) selectedContacts)
  contactsInString <- pure $ toString validSelectedContacts
  _ <- pure $ setValueToLocalStore CONTACTS contactsInString
  _ <- pure $ toast (getString EMERGENCY_CONTACS_ADDED_SUCCESSFULLY)
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

