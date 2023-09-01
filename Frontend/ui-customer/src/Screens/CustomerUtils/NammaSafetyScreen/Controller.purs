{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NammaSafetyScreen.Controller where

import Common.Types.App (LazyCheck(..)) as Lazy
import Components.GenericHeader.Controller as GenericHeaderController
import Components.NewContact.Controller as NewContactController
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.StepsHeaderModel.Controller as StepsHeaderModelController
import Data.Array (catMaybes, elem, filter, head, last, length, null, slice, snoc, sortBy, tail, take, union, (!!))
import Data.Int (fromString)
import Data.Lens.Lens.Product (_1)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.String as DS
import Data.String.CodeUnits (charAt)
import Debug (spy)
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, os, setText)
import Engineering.Helpers.Utils (loaderText, toggleLoader)
import Helpers.Utils (contactPermission, parseNewContacts, setEnabled, setRefreshing, toString)
import JBridge (firebaseLogEvent, hideKeyboardOnNavigation, minimizeApp, setupCamera, showDialer, toast, toggleBtnLoader)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog, trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, bind, compare, discard, map, not, pure, show, unit, void, ($), (&&), (-), (<=), (<>), (==), (>), (||), (+), (/=), (>=), (<))
import PrestoDOM (Eval, LetterSpacing(..), ScrollState, continue, continueWithCmd, exit, toPropValue, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (Contacts, EmergencyContactsData, NammaSafetyScreenState, NewContacts, NewContactsProp, Stage(..))
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalNativeStore, setValueToLocalStore)
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import Types.EndPoint (emergencyContacts)

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
        AfterRender -> trackAppScreenRender appId "screen" (getScreen NAMMASAFETY_ONBOARDING_SCREEN)
        StepsHeaderModelAC _ -> trackAppActionClick appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN) "steps_header_modal" "backpressed"
        BackPressed -> trackAppBackPress appId (getScreen HOME_SCREEN)
        GenericHeaderAC act -> case act of 
          GenericHeaderController.PrefixImgOnClick -> do 
            _ <- pure $ spy "calll log" "" 
            trackAppActionClick appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN) "generic_header_action" "back_icon"
            trackAppEndScreen appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN)
          GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN) "generic_header_action" "forward_icon"
        StartNammaSafetyOnboarding act -> case act of
          PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN) "start_onboarding" "primary button"
          PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN) "no_action" "primary button"
        GoToNextStep act -> case act of
          PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN) "next_step_onboard" "primary button"
          PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN) "no_action" "primary button"
        _ -> trackAppScreenRender appId "screen" (getScreen NAMMASAFETY_ONBOARDING_SCREEN)
        

data ScreenOutput = GoBack
                  | PostContacts NammaSafetyScreenState
                  | GetContacts NammaSafetyScreenState
                  | Refresh NammaSafetyScreenState

data Action = BackPressed
             | AfterRender
             | AfterRenderVideo
             | NoAction
             | RefreshScreen
             | StepsHeaderModelAC StepsHeaderModelController.Action
             | GenericHeaderAC GenericHeaderController.Action
             | GenericHeaderACEdu GenericHeaderController.Action
             | StartNammaSafetyOnboarding PrimaryButtonController.Action
             | GoToNextStep PrimaryButtonController.Action
             | SkipToNextStep PrimaryButtonController.Action
             | EditEmergencyContacts PrimaryButtonController.Action
             | ShowAboutNammaSafety
             | SwitchToStage Stage
             | ToggleSwitch Stage
             | ActivateSOS PrimaryButtonController.Action
             | CallForSupport String
             | ContactsCallback (Array Contacts)
             | CheckingContactList
             | PopUpModalAction PopUpModal.Action
             | FetchContacts
             | LoadMoreContacts
             | ContactListPrimaryButtonActionController PrimaryButtonController.Action
             | ContactListGenericHeaderActionController GenericHeaderController.Action
             | ContactListContactSelected NewContacts
             | ContactTextChanged String
             | ContactListPrimaryEditTextAction PrimaryEditTextController.Action
             | ContactListClearText
             | ContactListScroll String
             | ContactListScrollStateChanged ScrollState
             | NewContactActionController NewContactController.Action
             | RemoveButtonClicked NewContacts
             | AddEmergencyContacts PrimaryButtonController.Action
             | AddContacts
             | UpdateEmergencySettings
             

eval :: Action -> NammaSafetyScreenState -> Eval Action ScreenOutput NammaSafetyScreenState

eval (AddEmergencyContacts PrimaryButtonController.OnClick) state = continueWithCmd state [ pure AddContacts ]

eval AddContacts state = continueWithCmd state{props{currentStage = EmergencyContactsStage}} [ do
        _ <- pure $ spy "calll1" ""
        _ <- pure $ setRefreshing (getNewIDWithTag "EmergencyContactTag")false
        pure $ setEnabled (getNewIDWithTag "EmergencyContactTag") false
        _ <- launchAff $ flowRunner defaultGlobalState $ do
                _ <- loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
                _ <- toggleLoader true
                pure unit

        _ <- pure $ contactPermission unit
        pure NoAction
    ]

-- eval AfterRenderVideo state = do
--     _ <- pure $ spy "AfterRenderVideo" "VideoCamView"
--     continue state

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
      continueWithCmd state{data{emergencyContactsData{contactsNewList = unionNewContacts, contactsUpdatedNewList = unionNewContacts, contactsCount = length contactsInJson}}, props{emergencyContactsProps{showContactList = true}}}
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

eval (StepsHeaderModelAC StepsHeaderModelController.OnArrowClick) state = continueWithCmd state [ do pure $ BackPressed ]

eval (GenericHeaderAC (GenericHeaderController.PrefixImgOnClick)) state =  continueWithCmd state [ do pure $ BackPressed ]

eval (ToggleSwitch stage) state = do
  _ <- pure $ spy "modify" stage
  case stage of
    SetTriggerCustomerSupport   -> continue state{data{triggerNYSupport = not state.data.triggerNYSupport}}
    SetNightTimeSafetyAlert     -> continue state{data{nightTimeSafety = not state.data.nightTimeSafety}}
    SetDefaultEmergencyContacts -> if length state.data.emergencyContactsData.contactsList /= 0 
                                      then continue state{data{shareToEmergencyContacts = not state.data.shareToEmergencyContacts}}
                                   else continueWithCmd state [ pure AddContacts ]
    _                           -> continue state
eval (GenericHeaderACEdu (GenericHeaderController.PrefixImgOnClick)) state = continue state{ props {currentStage = AboutNammaSafety}}

eval AfterRender state = continue state

eval (ActivateSOS PrimaryButtonController.OnClick) state = continue state{props{currentStage = TriggeredNammaSafety}}

eval (StartNammaSafetyOnboarding PrimaryButtonController.OnClick) state = continue state {props {currentStage = SetDefaultEmergencyContacts}}

eval (EditEmergencyContacts PrimaryButtonController.OnClick) state = continueWithCmd state [ pure AddContacts ]

eval (SwitchToStage stage) state = continue state {props {currentStage = stage}}

eval (CallForSupport callTo) state = do
  void <- pure $ showDialer (if callTo == "police" then "112" else "123232") false
  continue state

eval (ShowAboutNammaSafety) state = continue state {props {currentStage = AboutNammaSafety, showOnboarding = false}}

eval (GoToNextStep PrimaryButtonController.OnClick) state = do
  _ <- pure $ spy "currentStage" state.props.currentStage
  case state.props.currentStage of
    SetTriggerCustomerSupport ->  continue state {props {currentStage = SetNightTimeSafetyAlert}}
    SetNightTimeSafetyAlert ->  continue state {props {currentStage = SetPersonalSafetySettings}}
    SetDefaultEmergencyContacts ->  continue state {props {currentStage = SetTriggerCustomerSupport}}
    SetPersonalSafetySettings -> continue state{props {currentStage = NammaSafetyDashboard}, data{hasCompletedSafetySetup = true}}
    _ -> continue state

eval (SkipToNextStep PrimaryButtonController.OnClick) state = do
  _ <- pure $ spy "currentStage" state.props.currentStage
  case state.props.currentStage of
    SetTriggerCustomerSupport ->  continue state {props {currentStage = SetNightTimeSafetyAlert}}
    SetNightTimeSafetyAlert ->  continue state {props {currentStage = SetDefaultEmergencyContacts}}
    SetDefaultEmergencyContacts ->  continue state {props {currentStage = SetPersonalSafetySettings}}
    SetPersonalSafetySettings -> continue state
    _ -> continue state

eval (ContactListScroll value) state = do
  let firstIndex = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!0)))
  let visibleItems = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!1)))
  let totalItems = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!2)))
  let canScrollUp = if firstIndex == 0 then false else true
  let loadMoreButton = if (totalItems == (firstIndex + visibleItems) && totalItems /= 0 && totalItems /= visibleItems) then true else false
  _ <- if canScrollUp then (pure $ setEnabled (getNewIDWithTag "EmergencyContactTag") false) else  (pure $ setEnabled (getNewIDWithTag "EmergencyContactTag") true)
  if loadMoreButton  then continueWithCmd state [do pure LoadMoreContacts]
  else continue state { data{ emergencyContactsData{loadMoreDisabled = loadMoreButton}}}

eval LoadMoreContacts state = do
  let contactsList = sliceContacts state.data.emergencyContactsData
  let bufferCardDataPrestoList = ((contactListTransformerProp (contactsList)))
  let loaderBtnDisabled = if(length (contactsList)== 0) then true else false
  let offsetForContacts = if (loaderBtnDisabled) then state.data.emergencyContactsData.offsetForEmergencyContacts else state.data.emergencyContactsData.offsetForEmergencyContacts + length contactsList
  continue $ state { data{ emergencyContactsData {prestoListArrayItems = union (state.data.emergencyContactsData.prestoListArrayItems) (bufferCardDataPrestoList) , loadMoreDisabled = loaderBtnDisabled, offsetForEmergencyContacts = offsetForContacts}}}


eval (ContactTextChanged value) state = do
  let newArray = findContactsWithPrefix value state.data.emergencyContactsData.contactsNewList
  continueWithCmd state{ data{ emergencyContactsData  { editedText = value , contactsUpdatedNewList = newArray, offsetForEmergencyContacts = 0, prestoListArrayItems = []} }}
    [do
      _ <- launchAff $ flowRunner defaultGlobalState $ do
          _ <- toggleLoader false
          pure unit
      pure LoadMoreContacts
    ]

eval (ContactListClearText) state = continueWithCmd state { data{ emergencyContactsData  { editedText = "" } }}
  [do
    _ <- (pure $ setText (getNewIDWithTag "contactEditText") "")
    pure NoAction
  ]

eval (PopUpModalAction PopUpModal.OnButton2Click) state = do
  let newContacts = filter (\x -> x.number <> x.name /= state.data.emergencyContactsData.removedContactDetail.number <> state.data.emergencyContactsData.removedContactDetail.name) state.data.emergencyContactsData.contactsList
  contactsInString <- pure $ toString newContacts
  _ <- pure $ setValueToLocalStore CONTACTS contactsInString
  exit $ PostContacts state{data{emergencyContactsData{contactsList = newContacts}}}

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state{props{emergencyContactsProps{showInfoPopUp = false}}}

eval (NewContactActionController (NewContactController.ContactSelected index)) state = do
  let contactsData = state.data.emergencyContactsData
  let contact = fromMaybe {isSelected : false , name : "" , number : ""} (contactsData.contactsUpdatedNewList !! index)
  let item = if ((DS.length contact.number) > 10 && (DS.length contact.number) <= 12 && ((DS.take 1 contact.number) == "0" || (DS.take 2 contact.number) == "91")) then
    contact {number =  DS.drop ((DS.length contact.number) - 10) contact.number}
  else contact
  if (((length contactsData.contactsList) >= 3) && (item.isSelected == false)) then do
    _ <- pure $ toast $ getString LIMIT_REACHED_3_OF_3_EMERGENCY_CONTACTS_ALREADY_ADDED
    continue state
  else if((DS.length item.number) /= 10 || (fromMaybe 0 (fromString (DS.take 1 item.number)) < 6)) then do
    _ <- pure $ toast (getString INVALID_CONTACT_FORMAT)
    continue state
  else do
    let contactListState = if(contact.isSelected == false) then state{ data {emergencyContactsData{contactsList = (snoc contactsData.contactsList item{isSelected = true}) }} } else state { data {emergencyContactsData{contactsList = filter (\x -> ((if DS.length contact.number == 10 then "" else "91") <> x.number <> x.name  /= contact.number <> contact.name)) state.data.emergencyContactsData.contactsList}}}
    let newState = contactListState { data {emergencyContactsData =  contactListState.data.emergencyContactsData { contactsNewList = map (\x ->
      if ((x.number <> x.name  == contact.number <> contact.name)) then x { isSelected = not (x.isSelected) }
      else x
      ) contactListState.data.emergencyContactsData.contactsNewList
    }}}
    let updatedNewState = newState { data = newState.data{emergencyContactsData { contactsUpdatedNewList = map (\x ->
      if ((x.number <> x.name  == contact.number <> contact.name)) then x { isSelected = not (x.isSelected) }
      else x
      ) newState.data.emergencyContactsData.contactsUpdatedNewList
    }}}
    let contactPropValue = contactTransformerProp contact{isSelected = not contact.isSelected}
    let updatedPrestoList = updatedNewState { data = updatedNewState.data {emergencyContactsData { prestoListArrayItems = map (\x ->
      if ((x.number == contactPropValue.number && x.name  == contactPropValue.name)) then contactPropValue
      else x
      ) updatedNewState.data.emergencyContactsData.prestoListArrayItems
    }}}
    updateAndExit state $ Refresh updatedPrestoList{data{emergencyContactsData{contactsCount = length updatedPrestoList.data.emergencyContactsData.contactsList}}}

eval (ContactListPrimaryButtonActionController PrimaryButtonController.OnClick) state = do
  let selectedContacts = filter (\x -> x.isSelected) state.data.emergencyContactsData.contactsNewList
  let validSelectedContacts = (map (\contact ->
    if ((DS.length contact.number) > 10 && (DS.length contact.number) <= 12 && ((DS.take 1 contact.number) == "0" || (DS.take 2 contact.number) == "91")) then
      contact {number =  DS.drop ((DS.length contact.number) - 10) contact.number}
    else contact
  ) selectedContacts)
  contactsInString <- pure $ toString validSelectedContacts
  _ <- pure $ setValueToLocalStore CONTACTS contactsInString
  updateAndExit state $ PostContacts state{data{emergencyContactsData{editedText = "", contactsList = validSelectedContacts, prestoListArrayItems = [], offsetForEmergencyContacts = 0}}, props{emergencyContactsProps{showContactList = false}}}

eval CheckingContactList state = do
  contacts <- pure $ getValueToLocalStore CONTACTS
  if (contacts /= "") then do
    contactsInJson <- pure $ parseNewContacts contacts
    continue state{data{ emergencyContactsData {contactsList = contactsInJson}}}
    else do
      continue state

eval FetchContacts state =
  updateAndExit state $ GetContacts state

eval (RemoveButtonClicked contactDetail) state = continue state{props{emergencyContactsProps{showInfoPopUp = true}}, data{emergencyContactsData{removedContactDetail = contactDetail}}}

eval (BackPressed) state = do
    case state.props.currentStage of 
      NammaSafetyDashboard -> exit $ GoBack
      AboutNammaSafety -> continue state{props{currentStage = NammaSafetyDashboard}}
      SetTriggerCustomerSupport -> continue state{props{currentStage = NammaSafetyDashboard}}
      SetNightTimeSafetyAlert -> continue state{props{currentStage = NammaSafetyDashboard}}
      SetDefaultEmergencyContacts -> continue state{props{currentStage = NammaSafetyDashboard}}
      SetPersonalSafetySettings -> continue state{props{currentStage = NammaSafetyDashboard}}
      EduNammaSafetyMeasures -> continue state{props{currentStage = AboutNammaSafety}}
      EduNammaSafetyGuidelines -> continue state{props{currentStage = AboutNammaSafety}}
      EduNammaSafetyAboutSOS -> continue state{props{currentStage = AboutNammaSafety}}
      ActivateNammaSafety -> exit $ GoBack
      TriggeredNammaSafety -> exit $ GoBack
      NammaSafetyVideoRecord -> continue state{props{currentStage = TriggeredNammaSafety}}
      EmergencyContactsStage -> continue state{props{currentStage = NammaSafetyDashboard}}
      _ -> continue state

eval (_) state = continue state

-- eval (VerifyOTPButtonAction PrimaryButtonController.OnClick) state = do
--     _ <- pure $ hideKeyboardOnNavigation true
--     _ <- pure $ clearCountDownTimer state.data.timerID
--     updateAndExit state $ GoToAccountSetUp state

-- eval (MobileNumberEditTextAction (PrimaryEditTextController.TextChanged id value)) state = do
--     _ <- if length value == 10 then do
--             pure $ hideKeyboardOnNavigation true
--             else pure unit
--     let isValidMobileNumber = case (charAt 0 value) of
--                                     Just a -> if a=='0' || a=='1' || a=='2' || a=='3' || a=='4' then false
--                                                 else if a=='5' then
--                                                     if value=="5000500050" then true else false
--                                                         else true
--                                     Nothing -> true
--     if (length value == 10 && isValidMobileNumber) then do
--         _ <- pure $ firebaseLogEvent "ny_user_mobnum_entry"
--         pure unit
--         else pure unit
--     let newState = state { props = state.props { isValidMobileNumber = isValidMobileNumber
--                                         , btnActiveMobileNumber = if (length value == 10 && isValidMobileNumber) then true else false}
--                                         , data = state.data { mobileNumber = if length value <= 10 then value else state.data.mobileNumber}}
--     continue newState

-- eval (MobileNumberEditTextAction (PrimaryEditTextController.FocusChanged boolean)) state = continue state { props{ mNumberEdtFocused = boolean}}

-- eval (OTPEditTextAction (PrimaryEditTextController.FocusChanged boolean)) state = continue state { props{ otpEdtFocused = boolean}}

-- eval (OTPEditTextAction (PrimaryEditTextController.TextChanged id value)) state = do
--     let newState = state { props = state.props { btnActiveOTP = if length value == 4 then true else false, letterSpacing = PX if value == "" then 1.0 else 6.0, wrongOTP = if state.props.wrongOTP && value == "" then true else false}
--                   , data = state.data { otp = if length value <= 4 then value else state.data.otp }}
--     if length value == 4 then do
--         pure $ hideKeyboardOnNavigation true
--         _ <- pure $ clearCountDownTimer state.data.timerID
--         updateAndExit newState $ GoToAccountSetUp newState
--     else
--         continue newState

-- eval (GenericHeaderActionController (GenericHeaderController.PrefixImgOnClick )) state = continueWithCmd state [ do pure $ BackPressed ]

-- eval Resend state = do
--     let newState = state {data{attempts = if (state.data.attempts > 0) then state.data.attempts - 1 else state.data.attempts},props{resendEnable = false}}
--     if state.data.attempts == 0 then do
--         _ <- pure $ toast (getString OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER)
--         _ <- pure $ toggleBtnLoader "" false
--         continue newState{props{enterOTP = false}}
--       else do 
--         _ <- pure $ toast (getString OTP_RESENT_SUCCESSFULLY)
--         exit $ ResendOTP newState

-- eval (AutoFill otp) state = do
--     _ <- pure $ firebaseLogEvent "ny_user_otp_autoread"
--     let newState = state {props = state.props {isReadingOTP = if length otp == 4 then false else true ,capturedOtp = otp}, data = state.data { otp = if (length otp <= 4) then otp else state.data.otp}}
--     updateAndExit newState $ GoToAccountSetUp (newState)
   

-- eval (CountDown seconds id status timerID) state = do
--         _ <- pure $ printLog "timer" $ show seconds
--         if status == "EXPIRED" then do
--             _ <- pure $ clearCountDownTimer state.data.timerID
--             let newState = state{data{timer = 30, timerID = ""},props = state.props{resendEnable = true}}
--             continue newState
--         else
--             continue $ state{data{timer = seconds, timerID=timerID},props = state.props{resendEnable = false}}
-- eval (SetToken id )state = do
--   _ <- pure $ spy "SetTokenSetToken" id
--   _ <- pure $ setValueToLocalNativeStore FCM_TOKEN  id
--   continue state

-- eval (SetPhoneNumber number )state = continue state {props { editTextVal = number, mNumberEdtFocused = true}}

-- eval ContinueCommand state = exit $ GoToOTP state{data{timer = 30, timerID = ""},props = state.props{btnActiveOTP = false, resendEnable = false}}

getContactList :: Array Contacts -> Array NewContacts
getContactList contacts = map (\x -> getContact x) contacts

getContact :: Contacts -> NewContacts
getContact contact = {
    isSelected : false
  , name : contact.name
  , number : contact.number
}

contactIsSelected :: NewContacts -> Array NewContacts -> Boolean
contactIsSelected contact tempContactList = do
  let ele = filter(\x -> ((if DS.length contact.number == 10 then "" else "91") <> x.number <> x.name  == contact.number <> contact.name)) tempContactList
  length ele == 1

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

sliceContacts :: EmergencyContactsData -> Array NewContacts
sliceContacts config = do
  let tempLastIndex= config.limitForEmergencyContacts + config.offsetForEmergencyContacts
  let lastIndex = if ((length (config.contactsUpdatedNewList)) < tempLastIndex) then length (config.contactsUpdatedNewList) else tempLastIndex 
  slice config.offsetForEmergencyContacts lastIndex config.contactsUpdatedNewList


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

findContactsWithPrefix :: String -> Array NewContacts -> Array NewContacts
findContactsWithPrefix prefix arr = filter (\contact -> startsWith prefix contact.name) arr

startsWith :: String -> String -> Boolean
startsWith prefix str = DS.take (DS.length prefix) (DS.toLower str) == (DS.toLower prefix)