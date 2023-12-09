{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NammaSafetyScreen.Controller where


import Components.GenericHeader.Controller as GenericHeaderController
import Components.NewContact.Controller as NewContactController
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.StepsHeaderModel.Controller as StepsHeaderModelController
import Data.Array (catMaybes, elem, filter, head, last, length, null, slice, snoc, sortBy, tail, take, union, (!!))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (Pattern(..), split)
import Data.String as DS
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, os, setText, liftFlow, encodeURIData)
import Engineering.Helpers.Utils (loaderText, toggleLoader, uploadMultiPartData, uploadMultiPartDataIOS)
import Helpers.Utils (clearCountDownTimer, contactPermission, parseNewContacts, setEnabled, setRefreshing, toStringJSON, requestCameraAndMicrophonePermissions)
import JBridge (askRequestedPermissions, openUrlInApp, showDialer, stopRecord, toast)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog, trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenRender)
import Prelude (class Show, bind, compare, discard, map, not, pure, show, unit, void, ($), (&&), (-), (<=), (<>), (==), (>), (||), (+), (/=), (>=), (<))
import Presto.Core.Types.Language.Flow (delay)
import PrestoDOM (Eval, ScrollState, continue, continueWithCmd, exit, toPropValue, updateAndExit)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (Contacts, EmergencyContactsData, NammaSafetyScreenState, NewContacts, NewContactsProp, RecordingState(..), NammaSafetyStage(..))
import Services.API (ContactDetails(..), GetEmergencySettingsRes(..))
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Styles.Colors as Color
import Styles.Types (Color)
import Types.App (defaultGlobalState)
import Types.EndPoint (updateSosVideo)
import Data.Time.Duration (Milliseconds(..))

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
        StepsHeaderModelAC _ -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "steps_header_modal" "backpressed"
        BackPressed -> trackAppBackPress appId (getScreen NAMMASAFETY_SCREEN)
        GenericHeaderAC act -> case act of 
          GenericHeaderController.PrefixImgOnClick -> do
            trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "generic_header_action" "back_icon"
            trackAppEndScreen appId (getScreen NAMMASAFETY_SCREEN)
          GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "generic_header_action" "forward_icon"
        StartNammaSafetyOnboarding act -> case act of
          PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "start_onboarding" "primary button"
          PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "no_action" "primary button"
        GoToNextStep act -> case act of
          PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "next_step_onboard" "primary button"
          PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "no_action" "primary button"
        _ -> trackAppScreenRender appId "screen" (getScreen NAMMASAFETY_SCREEN)
        

data ScreenOutput = GoBack
                  | PostContacts NammaSafetyScreenState
                  -- | GetContacts NammaSafetyScreenState
                  | Refresh NammaSafetyScreenState
                  | PostEmergencySettings NammaSafetyScreenState
                  | CreateSOS NammaSafetyScreenState
                  | UpdateAction NammaSafetyScreenState
                  | UpdateSafe NammaSafetyScreenState
                  | GoToEmergencyContactScreen NammaSafetyScreenState

data Action = BackPressed
             | NoAction
             | StepsHeaderModelAC StepsHeaderModelController.Action
             | GenericHeaderAC GenericHeaderController.Action
             | StartNammaSafetyOnboarding PrimaryButtonController.Action
             | GoToNextStep PrimaryButtonController.Action
             | EditEmergencyContacts PrimaryButtonController.Action
             | SwitchToStage NammaSafetyStage
             | ToggleSwitch NammaSafetyStage
             | ActivateSOS PrimaryButtonController.Action
             | CallForSupport String
             | UpdateSosId String
             | ContactsCallback (Array Contacts)
             | CheckingContactList
             | PopUpModalAction PopUpModal.Action
             | FetchContacts
             | LoadMoreContacts
             | ContactListPrimaryButtonActionController PrimaryButtonController.Action
             | VideoShared PrimaryButtonController.Action
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
             | UpdateEmergencySettings GetEmergencySettingsRes
             | ToggleRecord
             | ConfirmSOSActivate PopUpModal.Action
             | MarkRideAsSafe PrimaryButtonController.Action
             | CountDown Int String String String
             | VideoStatusCallBack String String
             | DismissSOS PrimaryButtonController.Action
             | ChangeRecordingState RecordingState
             | ShareVideo
             | ActivateSoSAndCallPolice
             | ShareSilentSos (Maybe String)
             | UploadMultiPartCallback String String

eval :: Action -> NammaSafetyScreenState -> Eval Action ScreenOutput NammaSafetyScreenState

eval (AddEmergencyContacts PrimaryButtonController.OnClick) state = continueWithCmd state [ pure AddContacts ]

eval AddContacts state = exit $ GoToEmergencyContactScreen state

eval (UpdateSosId sosId) state = continue state{data{sosId = sosId}, 
                                            props{ currentStage = if checkForContactsAndSupportDisabled state
                                                                  then ActivateNammaSafety
                                                                  else TriggeredNammaSafety}}

eval (CountDown seconds id status timerID) state = do
        _ <- pure $ printLog "timer" $ show seconds
        if status == "EXPIRED" then do
          let newState = state{props{timerValue = 15, timerId = ""}} 
          _ <- pure $ clearCountDownTimer state.props.timerId
          _ <- pure $ stopRecord unit
          continue newState
        else continue state{props{timerValue = seconds, timerId=timerID}}

eval (UpdateEmergencySettings (GetEmergencySettingsRes response)) state = do
  let contacts = map (\(ContactDetails item) -> {
          number: item.mobileNumber,
          name: item.name,
          isSelected: true
        }) response.defaultEmergencyNumbers
  continue state {data {
          hasCompletedSafetySetup = response.hasCompletedSafetySetup,
          shareToEmergencyContacts = response.shareEmergencyContacts,
          nightSafetyChecks = response.nightSafetyChecks,
          triggerSupport = response.triggerSupport, emergencyContactsData{contactsList = contacts}}, props{enableLocalPoliceSupport = response.enablePoliceSupport, localPoliceNumber = fromMaybe "" response.localPoliceNumber}}

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

eval (ToggleRecord) state = do
  if state.props.recordingState == RECORDING
    then do
      _ <- pure $ stopRecord unit
      _ <- pure $ clearCountDownTimer state.props.timerId
      continue state
  -- else if state.props.recordingState == SHARING
  --   then do
  --     _ <- pure $ clearCountDownTimer state.props.timerId
  --     continueWithCmd state {props{recordingState = NOT_RECORDING}}[
  --      do pure $ SwitchToStage TriggeredNammaSafety
  --     ]
  else do
    continue state {props { recordingState = RECORDING }}

eval (ChangeRecordingState recordingState) state = continue state {props { recordingState = recordingState }}

eval (GenericHeaderAC (GenericHeaderController.SuffixImgOnClick)) state =  do
    _ <- pure $ clearCountDownTimer state.props.timerId
    continueWithCmd state{props{timerValue = 15, recordingState = NOT_RECORDING}} [ do pure $ SwitchToStage TriggeredNammaSafety ]

eval (VideoStatusCallBack status uri) state = do
  case status of 
    "VIDEO_RECORDED" -> do 
        if state.props.recordingState == RECORDING then do
          continueWithCmd state{props { recordingState = SHARING }, data{videoPath = uri}} [do
              void $ launchAff $ flowRunner defaultGlobalState $ do
                push <- liftFlow $ getPushFn Nothing "NammaSafetyScreen"
                loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
                toggleLoader true
                delay (Milliseconds 1000.0)
                if os == "IOS" then do
                  void $ liftFlow $ uploadMultiPartDataIOS uri (updateSosVideo state.data.sosId) "Video" "video" "fileUrl" push UploadMultiPartCallback
                  toggleLoader false
                else do
                  res <- liftFlow $ uploadMultiPartData uri (updateSosVideo state.data.sosId) "Video" "video" "fileUrl"
                  toggleLoader false
                  if state.props.enableLocalPoliceSupport 
                    then do
                          liftFlow $ push $ ShareSilentSos $ Just res
                  else liftFlow $ push $ if res /= "" then ChangeRecordingState SHARED else BackPressed
              pure $ NoAction
          ]
        else continue state
    _ ->  do
      continue state

eval (ToggleSwitch stage) state = do
  if state.props.currentStage == NammaSafetyDashboard then do
    case stage of 
      SetTriggerCustomerSupport   -> exit $ PostEmergencySettings state{data{triggerSupport = not state.data.triggerSupport}}
      SetNightTimeSafetyAlert     -> exit $ PostEmergencySettings state{data{nightSafetyChecks = not state.data.nightSafetyChecks}}
      SetDefaultEmergencyContacts -> if length state.data.emergencyContactsData.contactsList /= 0 
                                        then exit $ PostEmergencySettings state{data{shareToEmergencyContacts = not state.data.shareToEmergencyContacts}}
                                     else continueWithCmd state [ pure AddContacts ]
      _                           -> continue state
  else do
    case stage of
      SetTriggerCustomerSupport   -> continue state{data{triggerSupport = not state.data.triggerSupport}}
      SetNightTimeSafetyAlert     -> continue state{data{nightSafetyChecks = not state.data.nightSafetyChecks}}
      SetDefaultEmergencyContacts -> if length state.data.emergencyContactsData.contactsList /= 0 
                                        then continue state{data{shareToEmergencyContacts = not state.data.shareToEmergencyContacts}}
                                    else continueWithCmd state [ pure AddContacts ]
      _                           -> continue state

eval (MarkRideAsSafe PrimaryButtonController.OnClick) state = exit $ UpdateSafe state

eval (ActivateSOS PrimaryButtonController.OnClick) state = continue state{props{confirmPopup = true}}

eval (StartNammaSafetyOnboarding PrimaryButtonController.OnClick) state = continue state {props {currentStage = SetDefaultEmergencyContacts}}

eval (EditEmergencyContacts PrimaryButtonController.OnClick) state = exit $ GoToEmergencyContactScreen state

eval (SwitchToStage stage) state = continue state {props {currentStage = stage}}

eval (CallForSupport callTo) state = do
  void <- pure $ showDialer (if callTo == "police" then "112" else "123232") false
  exit $ UpdateAction state{data{updateActionType = callTo}}

eval (DismissSOS PrimaryButtonController.OnClick) state = 
    if checkForContactsAndSupportDisabled state then exit $ UpdateSafe state
    else exit $ GoBack

eval (GoToNextStep PrimaryButtonController.OnClick) state = do
  case state.props.currentStage of
    SetTriggerCustomerSupport ->  continue state {props {currentStage = SetNightTimeSafetyAlert}}
    SetNightTimeSafetyAlert ->  continue state {props {currentStage = SetPersonalSafetySettings}}
    SetDefaultEmergencyContacts ->  continue state {props {currentStage = if state.data.safetyConfig.enableSupport then SetTriggerCustomerSupport else SetNightTimeSafetyAlert}}
    SetPersonalSafetySettings -> do
      if os == "IOS" then do
        void $ pure $ requestCameraAndMicrophonePermissions unit
      else
        void $ pure $ askRequestedPermissions ["android.permission.CAMERA", "android.permission.RECORD_AUDIO"]
      exit $ PostEmergencySettings state
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
  contactsInString <- pure $ toStringJSON newContacts
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
    let contactListState = if(contact.isSelected == false) 
            then state{ data {emergencyContactsData{contactsList = (snoc contactsData.contactsList item{isSelected = true}) }} } else state { data {emergencyContactsData{contactsList = filter (\x -> ((if DS.length contact.number == 10 then "" else "91") <> x.number <> x.name  /= contact.number <> contact.name)) state.data.emergencyContactsData.contactsList}}}
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
    continue updatedPrestoList{data{emergencyContactsData{contactsCount = length updatedPrestoList.data.emergencyContactsData.contactsList}}}

eval (ContactListPrimaryButtonActionController PrimaryButtonController.OnClick) state = do
  let selectedContacts = filter (\x -> x.isSelected) state.data.emergencyContactsData.contactsNewList
  let validSelectedContacts = (map (\contact ->
    if ((DS.length contact.number) > 10 && (DS.length contact.number) <= 12 && ((DS.take 1 contact.number) == "0" || (DS.take 2 contact.number) == "91")) then
      contact {number =  DS.drop ((DS.length contact.number) - 10) contact.number}
    else contact
  ) selectedContacts)
  contactsInString <- pure $ toStringJSON validSelectedContacts
  _ <- pure $ setValueToLocalStore CONTACTS contactsInString
  continue state
  -- updateAndExit state $ PostContacts state{data{emergencyContactsData{editedText = "", contactsList = validSelectedContacts, prestoListArrayItems = [], offsetForEmergencyContacts = 0}}, props{emergencyContactsProps{showContactList = false}}}

eval CheckingContactList state = do
  contacts <- pure $ getValueToLocalStore CONTACTS
  if (contacts /= "") then do
    contactsInJson <- pure $ parseNewContacts contacts
    continue state{data{ emergencyContactsData {contactsList = contactsInJson}}}
    else do
      continue state

eval FetchContacts state =
  -- updateAndExit state $ GetContacts state
  continue state

eval (RemoveButtonClicked contactDetail) state = continue state{props{emergencyContactsProps{showInfoPopUp = true}}, data{emergencyContactsData{removedContactDetail = contactDetail}}}

eval (BackPressed) state = do
    case state.props.currentStage of 
      NammaSafetyDashboard -> case state.props.onRide, state.data.sosId == "" of
                                true, true ->  continue state{props{currentStage = ActivateNammaSafety}} 
                                true, false ->  continue state{props{currentStage = if checkForContactsAndSupportDisabled state then ActivateNammaSafety else TriggeredNammaSafety}}
                                false, _   ->  exit $ GoBack
      AboutNammaSafety -> continue state{props{currentStage = if state.props.onRide then ActivateNammaSafety else NammaSafetyDashboard}}
      SetTriggerCustomerSupport -> continue state{props{currentStage = SetDefaultEmergencyContacts}}
      SetNightTimeSafetyAlert -> continue state{props{currentStage = if state.data.safetyConfig.enableSupport then SetTriggerCustomerSupport else SetDefaultEmergencyContacts}}
      SetDefaultEmergencyContacts -> continue state{props{currentStage = NammaSafetyDashboard}}
      SetPersonalSafetySettings -> continue state{props{currentStage = SetNightTimeSafetyAlert}}
      EduNammaSafetyMeasures -> continue state{props{currentStage = AboutNammaSafety}}
      EduNammaSafetyGuidelines -> continue state{props{currentStage = AboutNammaSafety}}
      EduNammaSafetyAboutSOS -> continue state{props{currentStage = AboutNammaSafety}}
      ActivateNammaSafety -> exit $ GoBack
      TriggeredNammaSafety -> exit $ GoBack
      NammaSafetyVideoRecord -> do
          _ <- pure $ clearCountDownTimer state.props.timerId
          continue state{props{currentStage = TriggeredNammaSafety, timerValue = 15, recordingState = NOT_RECORDING}}
      EmergencyContactsStage -> if state.data.hasCompletedSafetySetup || state.props.onRide
                                  then continue state{props{currentStage = NammaSafetyDashboard}}
                                else continue state{props{currentStage = SetDefaultEmergencyContacts}}
      _ -> continue state

eval (ConfirmSOSActivate (PopUpModal.OnButton1Click)) state = updateAndExit state{props{confirmPopup = false}} $ CreateSOS state{props{confirmPopup = false}}

eval (ConfirmSOSActivate (PopUpModal.OnButton2Click)) state = updateAndExit state{props{confirmPopup = false}} $ GoBack

eval (VideoShared PrimaryButtonController.OnClick) state = do
    _ <- pure $ clearCountDownTimer state.props.timerId
    continueWithCmd state{props{recordingState = NOT_RECORDING, timerValue = 15}} [ pure $ SwitchToStage TriggeredNammaSafety ]

eval ActivateSoSAndCallPolice state = do
  void <- pure $ showDialer "112" false
  if state.data.sosId == "" then
    exit $ CreateSOS state
  else continue state

eval (ShareSilentSos videoUri) state = continueWithCmd state [do
  let uriText = case videoUri of 
                    Just uri -> "\n\nSOS Video Link : " <> uri
                    Nothing  -> ""
      dataToEncode = encodeURIData $ "*SOS Alert*\n"
                            <> "I am in an emergency during Namma Yatri ride.\n\nHere are my details:\nUsername : "
                            <> getValueToLocalStore USER_NAME <>"\nPhone number : "
                            <> getValueToLocalStore MOBILE_NUMBER 
                            <>"\nRide Journey link - https://nammayatri.in/track/?id=" 
                            <> state.data.rideId <> uriText
  _ <- openUrlInApp $ "https://wa.me/"<> DS.trim state.props.localPoliceNumber <>"?text=" <> dataToEncode
  pure if isNothing videoUri then NoAction else BackPressed
 ]

eval (UploadMultiPartCallback fileType response) state = do
  continueWithCmd state [do
    pure if state.props.enableLocalPoliceSupport 
      then do
            ShareSilentSos $ Just response
    else if response /= "FAILED" then ChangeRecordingState SHARED else BackPressed
  ]
 
eval (_) state = continue state

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

contactColorsList :: Array (Array Color)
contactColorsList = [
    [Color.yellow900, Color.black800],
    [Color.blue800, Color.white900],
    [Color.orange800, Color.black800]
]

checkForContactsAndSupportDisabled :: NammaSafetyScreenState -> Boolean
checkForContactsAndSupportDisabled state = do
  (null state.data.emergencyContactsData.contactsList || not state.data.shareToEmergencyContacts) && not state.data.safetyConfig.enableSupport && not state.props.enableLocalPoliceSupport