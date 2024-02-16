module Screens.EmergencyContactsScreen.Controller where

import Prelude (class Show, bind, compare, discard, map, not, pure, unit, ($), (&&), (+), (-), (/=), (<), (<=), (<>), (==), (>), (>=), (||))
import PrestoDOM (Eval, ScrollState, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.NewContact.Controller as NewContactController
import Log (trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenEvent, trackAppScreenRender)
import Screens (ScreenName(..), getScreen)
import JBridge (toast, hideKeyboardOnNavigation)
import Screens.Types (Contacts, EmergencyContactsScreenState, NewContacts, NewContactsProp)
import Data.Array (catMaybes, delete, dropEnd, elem, filter, head, last, length, mapWithIndex, nub, null, slice, snoc, sortBy, tail, updateAt, (!!))
import Helpers.Utils (contactPermission, setEnabled, setRefreshing, setText)
import Screens.EmergencyContactsScreen.Transformer (getContactList)
import Language.Strings (getString)
import Language.Types (STR(..))
import Styles.Colors as Color
import Components.PopUpModal.Controller as PopUpModal
import Engineering.Helpers.Commons (getNewIDWithTag, flowRunner, liftFlow)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String as DS
import Data.Int (fromString)
import Types.App (defaultGlobalState)
import Effect.Aff (launchAff)
import Engineering.Helpers.Utils (loaderText, terminateLoader, toggleLoader)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import Screens.NammaSafetyFlow.Components.ContactsList as ContactsList
import Screens.NammaSafetyFlow.Components.SafetyUtils (getDefaultPriorityList)
import Mobility.Prelude (boolToInt)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Data.Either (Either(..))

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
    NoAction -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "no_action"
    ContactsCallback contacts -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "contacts_callback"
    ContactListGenericHeaderActionController act -> case act of
      GenericHeader.PrefixImgOnClick -> trackAppActionClick appId (getScreen EMERGENCY_CONTACS_SCREEN) "generic_header_action" "back_icon_onclick"
      _ -> pure unit
    ContactTextChanged value -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "contact_text_changed"
    ContactListClearText -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "clear_text"
    ContactListContactSelected item -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "contacts_callback"
    PrimaryButtonAC onclick-> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "primary_button_action_controller"
    ContactListScroll value -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "contact_text_changed"
    PopUpModalAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "pop_up_modal_action_on_button1_click"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "pop_up_modal_action_on_button2_click"
      _ -> pure unit
    FetchContacts -> trackAppActionClick appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "fetch_contacts"
    LoadMoreContacts -> trackAppScreenEvent appId (getScreen EMERGENCY_CONTACS_SCREEN) "in_screen" "loading more contacts"
    _ -> pure unit

data Action = GenericHeaderActionController GenericHeader.Action
            | PrimaryButtonActionControll PrimaryButton.Action
            | BackPressed
            | AfterRender
            | NoAction
            | RefreshScreen
            | ContactsCallback (Array Contacts)
            | PopUpModalAction PopUpModal.Action
            | FetchContacts
            | LoadMoreContacts
            | PrimaryButtonAC PrimaryButton.Action
            | ContactListGenericHeaderActionController GenericHeader.Action
            | ContactListContactSelected NewContacts
            | ContactTextChanged String
            | ContactListPrimaryEditTextAction PrimaryEditTextController.Action
            | ContactListClearText
            | AddContacts
            | ContactListScroll String
            | ContactListScrollStateChanged ScrollState
            | NewContactActionController NewContactController.Action
            | ContactListAction ContactsList.Action

data ScreenOutput = GoToSafetyScreen EmergencyContactsScreenState
                  | PostContacts EmergencyContactsScreenState Boolean
                  | GetContacts EmergencyContactsScreenState
                  | Refresh EmergencyContactsScreenState

eval :: Action -> EmergencyContactsScreenState -> Eval Action ScreenOutput EmergencyContactsScreenState

eval (PrimaryButtonActionControll PrimaryButton.OnClick) state = 
  if null state.data.emergencyContactsList 
    then continueWithCmd state [pure AddContacts]
    else updateAndExit state $ PostContacts state{data{selectedContacts = state.data.emergencyContactsList}} true

eval AddContacts state = continueWithCmd state
      [do
        _ <- pure $ setRefreshing (getNewIDWithTag "EmergencyContactTag")false
        pure $ setEnabled (getNewIDWithTag "EmergencyContactTag") false
        _ <- launchAff $ flowRunner defaultGlobalState $ do
                _ <- loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
                _ <- toggleLoader true
                liftFlow $ contactPermission unit
        logEvent state.data.logField "ny_user_add_emergency_contact_click"
        pure NoAction
      ]

eval (PopUpModalAction PopUpModal.OnButton2Click) state = do
  let newContacts = delete state.data.removedContactDetail state.data.emergencyContactsList
  exit $ PostContacts state{data{selectedContacts = getDefaultPriorityList newContacts}} false

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state{props{showInfoPopUp = false}}

eval (ContactsCallback allContacts) state = do
  let flag = case last allContacts of
              Just contact ->  if (contact.name == "beckn_contacts_flag") && (contact.number == "true") then "true" else "NA" -- TODO :: Need to refactor @Chakradhar
              Nothing -> "false"
      updatedContactList = case (last allContacts) of
              Just contact ->  if (contact.name == "beckn_contacts_flag") then dropEnd 1 allContacts else allContacts -- TODO :: Need to refactor @Chakradhar
              Nothing -> allContacts
  if flag == "false"  then do
    _ <- pure $ toast (getString PLEASE_ENABLE_CONTACTS_PERMISSION_TO_PROCEED)
    removeLoader
  else if (null updatedContactList) then do
    _ <- pure $ toast (getString NO_CONTACTS_FOUND_ON_THE_DEVICE_TO_BE_ADDED)
    removeLoader
  else do
    let filteredContacts = map (\contactItem -> do 
                                let formattedContact = getFormattedContact contactItem
                                case getContactFromEMList formattedContact state.data.emergencyContactsList of
                                  Nothing -> formattedContact
                                  Just contact -> formattedContact{isSelected = true, priority = contact.priority, enableForFollowing = contact.enableForFollowing}) $ getContactList updatedContactList
        unionNewContacts = nub filteredContacts
        bufferCardDataPrestoList = contactListTransformerProp unionNewContacts
    if null unionNewContacts then do
      _ <- pure $ toast (getString NO_CONTACTS_LEFT_ON_DEVICE_TO_ADD)
      removeLoader
    else do
      let newState = state{data{storedContactsList = unionNewContacts, searchResult = unionNewContacts, prestoListArrayItems = bufferCardDataPrestoList, selectedContacts = state.data.emergencyContactsList}, props{showContactList = true}}
      continue newState
  where
    removeLoader =     
      continueWithCmd state
      [do
        _ <- terminateLoader ""
        pure NoAction
      ]

eval BackPressed state =
  if state.props.showContactList then do
    continue state{data{ editedText = ""}, props{showContactList = false}}
  else if state.props.showInfoPopUp then
    continue state{props{showInfoPopUp = false, showContactList = false}}
  else
    exit $ GoToSafetyScreen state

eval (ContactListGenericHeaderActionController GenericHeader.PrefixImgOnClick) state = continueWithCmd state [do pure BackPressed]

eval (ContactTextChanged value) state = do
  let newArray = findContactsWithPrefix value state.data.storedContactsList
      prestoList = contactListTransformerProp newArray
  continue state{ data { editedText = value , searchResult = newArray, offsetForEmergencyContacts = 0, prestoListArrayItems = prestoList} }

eval (ContactListClearText) state = continueWithCmd state { data { editedText = "" } }
  [do
    _ <- (pure $ setText (getNewIDWithTag "contactEditText") "")
    pure NoAction
  ]

eval (NewContactActionController (NewContactController.ContactSelected index)) state = do
  let contact = getValidContact $ fromMaybe {isSelected : false , name : "" , number : "", enableForFollowing: false, priority : 0} (state.data.searchResult !! index)
  let item = (getValidContact contact){isSelected = not contact.isSelected}
  if (length state.data.selectedContacts) >= 3 && not contact.isSelected then do
    _ <- pure $ toast $ getString LIMIT_REACHED_3_OF_3_EMERGENCY_CONTACTS_ALREADY_ADDED
    continue state
  else if((DS.length item.number) /= 10 || (fromMaybe 0 (fromString (DS.take 1 item.number)) < 6)) then do
    _ <- pure $ toast (getString INVALID_CONTACT_FORMAT)
    continue state
  else do
    let contactToUpdate = contact{isSelected = not contact.isSelected}
        updatedSelectedContacts = if item.isSelected then snoc state.data.selectedContacts contactToUpdate else delete contact state.data.selectedContacts
        updatedContactsList = updateAt index contactToUpdate state.data.searchResult
    case updatedContactsList of
      Nothing -> continue state
      Just updatedArray -> do 
        let contactPropValue = contactTransformerProp contactToUpdate
            updatedPrestoList = updateAt index contactPropValue state.data.prestoListArrayItems
        case updatedPrestoList of
          Nothing -> continue state
          Just list -> do
            let contactList = map (\contactItem ->
              if contactItem  == contact then contactToUpdate else contactItem) state.data.storedContactsList
            continue state{data{selectedContacts = updatedSelectedContacts, storedContactsList = contactList, searchResult = updatedArray ,prestoListArrayItems = list}}

eval (PrimaryButtonAC PrimaryButton.OnClick) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_emergency_contact_added"
  let validSelectedContacts = (mapWithIndex (\index contact ->
    if ((DS.length contact.number) > 10 && (DS.length contact.number) <= 12 && ((DS.take 1 contact.number) == "0" || (DS.take 2 contact.number) == "91")) then
      contact {number =  DS.drop ((DS.length contact.number) - 10) contact.number, priority = boolToInt $ index /= 0}
    else contact{priority = boolToInt $ index /= 0}
  ) state.data.selectedContacts)
  _ <- pure $ hideKeyboardOnNavigation true
  updateAndExit state $ PostContacts state{data{editedText = "", selectedContacts = getDefaultPriorityList validSelectedContacts}, props{showContactList = false}} false

eval FetchContacts state =
  updateAndExit state $ GetContacts state

eval (ContactListAction (ContactsList.RemoveButtonClicked contactDetail)) state = continue state{props{showInfoPopUp = true}, data{removedContactDetail = contactDetail}}

eval (ContactListAction (ContactsList.ContactCardClicked index)) state = do
  let
    newContactsList =
      mapWithIndex
        ( \i contact ->
            if i == index then
              contact { priority = 0 }
            else
              contact { priority = 1 }
        )
        state.data.emergencyContactsList
  continue state { data { emergencyContactsList = newContactsList } }

eval (ContactListAction (ContactsList.AddContacts)) state = continueWithCmd state [pure AddContacts]

eval _ state = continue state

startsWith :: String -> String -> Boolean
startsWith prefix str = DS.take (DS.length prefix) (DS.toLower str) == (DS.toLower prefix)

findContactsWithPrefix :: String -> Array NewContacts -> Array NewContacts
findContactsWithPrefix prefix arr = filter (\contact -> startsWith prefix contact.name) arr


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
contactListTransformerProp = map \contact -> contactTransformerProp contact

contactTransformerProp :: NewContacts -> NewContactsProp 
contactTransformerProp contact = {
  name: toPropValue $ contact.name,
  number: toPropValue $ contact.number,
  contactBackgroundColor : toPropValue $ if contact.isSelected then Color.grey900 else Color.white900,
  visibilitySelectedImage : toPropValue if contact.isSelected then "visible" else "gone",
  visibilityUnSelectedImage : toPropValue $ if contact.isSelected then "gone" else "visible"
}

contactListTransformer :: Array NewContacts -> Array NewContacts 
contactListTransformer contactList = (map (\(contact)->{
  name: contact.name,
  number: contact.number,
  isSelected: contact.isSelected,
  enableForFollowing: false,
  priority : contact.priority
})(contactList))

sliceContacts :: EmergencyContactsScreenState -> Array NewContacts
sliceContacts config = do
  let tempLastIndex= config.data.limitForEmergencyContacts + config.data.offsetForEmergencyContacts
  let lastIndex = if ((length (config.data.searchResult)) < tempLastIndex) then length (config.data.searchResult) else tempLastIndex 
  slice config.data.offsetForEmergencyContacts lastIndex config.data.searchResult

getContactFromEMList :: NewContacts -> Array NewContacts -> Maybe NewContacts
getContactFromEMList contact emergencyContactsList =  head $ filter (\x -> ((if DS.length contact.number == 10 then "" else "91") <> x.number <> x.name  == contact.number <> contact.name)) emergencyContactsList

getValidContact :: NewContacts -> NewContacts
getValidContact contact = if ((DS.length contact.number) > 10 && (DS.length contact.number) <= 12 && ((DS.take 1 contact.number) == "0" || (DS.take 2 contact.number) == "91")) 
    then contact {number =  DS.drop ((DS.length contact.number) - 10) contact.number}
    else contact
getFormattedContact :: NewContacts -> NewContacts
getFormattedContact contact = 
  let eiRegexPattern = regex "\\D" global
      formattedNumber = case eiRegexPattern of 
                          Right regexPattern -> replace regexPattern "" contact.number
                          Left _ -> contact.number
  in contact{number = formattedNumber}