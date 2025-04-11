{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RegistrationScreen.Controller where

import Common.Types.App (LazyCheck(..))
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButtonController
import Components.GenericHeader as GenericHeader
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Helpers.Utils (getStatus, contactSupportNumber, getLatestAndroidVersion, getHvErrorMsg)
import JBridge as JB
import Log (trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenEvent, trackAppScreenRender, trackAppTextInput)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (class Show, class Eq, bind, discard, pure, show, unit, ($), void, (>),(<), (+), (<>), (>=), (-), not, min, (==), (&&), (/=), when, (||), Unit (..), otherwise, (<$>))
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (RegisterationStep(..), RegistrationScreenState, StageStatus(..), HyperVergeKycResult(..))
import Services.Config (getSupportNumber, getWhatsAppSupportNo)
import Components.PrimaryEditText as PrimaryEditText
import Components.InAppKeyboardModal as InAppKeyboardModal
import Data.String as DS
import Data.String.Pattern as DSP
import Language.Strings (getString)
import Language.Types (STR(..))
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent, logEventWithMultipleParams)
import Foreign (Foreign, unsafeToForeign)
import Data.Eq.Generic (genericEq)
import Screens.Types as ST
import Data.Generic.Rep (class Generic)
import Data.Maybe as Mb
import Storage (getValueToLocalStore, KeyStore(..), setValueToLocalStore)
import Data.Array as DA
import Screens.RegistrationScreen.ScreenData as SD
import Components.OptionsMenu as OptionsMenu
import Components.BottomDrawerList as BottomDrawerList
import Effect.Uncurried (runEffectFn8)
import Effect.Class (liftEffect)
import Foreign.Generic (decodeJSON)
import Services.Backend as Remote
import Data.Function.Uncurried (runFn1)
import Data.Either(Either(..))
import Debug
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons as EHC
import Types.App (defaultGlobalState, GlobalState)
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (doAff, Flow)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Control.Monad.Except (runExcept)
import Services.API as API
import PrestoDOM.Core (getPushFn)
import Engineering.Helpers.Commons (liftFlow)
import Common.RemoteConfig (fetchRemoteConfigString)
import RemoteConfig
import DecodeUtil (decodeForeignObject)
import Engineering.Helpers.Events as EHE
import Helpers.Utils as HU
import Data.Tuple
import Control.Bind (join)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
   performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen REGISTRATION_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen REGISTRATION_SCREEN)
      trackAppEndScreen appId (getScreen REGISTRATION_SCREEN)
    NoAction -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "no_action"
    UpdateApkAction -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "update_apk_action"
    AppOnboardingNavBarAC act -> case act of
      AppOnboardingNavBar.GenericHeaderAC genericHeaderAction -> case genericHeaderAction of 
        GenericHeader.PrefixImgOnClick -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "generic_header_on_click"
        GenericHeader.SuffixImgOnClick -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "generic_header_on_click"
      AppOnboardingNavBar.Logout -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "onboarding_nav_bar_logout"
      AppOnboardingNavBar.PrefixImgOnClick -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "app_onboarding_nav_bar_prefix_img_on_click"
    RegistrationAction value -> trackAppScreenRender appId "screen" (getScreen REGISTRATION_SCREEN)
    PopUpModalLogoutAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal" "on_goback"
      PopUpModal.DismissPopup -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal" "dismiss_popup"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal" "call_support"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "countdown_updated"
      _ -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "no_action"
    PrimaryButtonAction act -> case act of 
      PrimaryButtonController.OnClick -> do
        trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "primary_button" "next_on_click"
        trackAppEndScreen appId (getScreen REGISTRATION_SCREEN)
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "primary_button" "no_action"
    PrimaryEditTextActionController act -> case act of
      PrimaryEditText.TextChanged id value -> trackAppTextInput appId (getScreen VEHICLE_DETAILS_SCREEN) "registration_number_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen VEHICLE_DETAILS_SCREEN) "registration_number_text_focus_changed" "primary_edit_text"
      PrimaryEditText.TextImageClicked -> trackAppTextInput appId (getScreen VEHICLE_DETAILS_SCREEN) "registration_number_text_image_clicked" "primary_edit_text"
    ReferralCodeTextChanged str -> pure unit
    EnterReferralCode val -> pure unit
    _ -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "no_action"

    
data ScreenOutput = GoBack 
                  | GoToUploadDriverLicense RegistrationScreenState 
                  | GoToUploadVehicleRegistration RegistrationScreenState (Array String)
                  | GoToPermissionScreen RegistrationScreenState
                  | LogoutAccount
                  | GoToOnboardSubscription RegistrationScreenState
                  | GoToHomeScreen RegistrationScreenState
                  | RefreshPage
                  | ReferralCode RegistrationScreenState
                  | DocCapture RegistrationScreenState RegisterationStep
                  | SelectLang RegistrationScreenState
                  | GoToAadhaarPANSelfieUpload RegistrationScreenState ST.HyperVergeKycResult
                  | GoToAppUpdatePopUpScreen RegistrationScreenState

data Action = BackPressed 
            | NoAction
            | AfterRender
            | RegistrationAction ST.StepProgress
            | PopUpModalLogoutAction PopUpModal.Action
            | ChangeVehicleAC PopUpModal.Action
            | VehicleMismatchAC PopUpModal.Action
            | PrimaryButtonAction PrimaryButtonController.Action
            | Refresh
            | ContactSupport
            | AppOnboardingNavBarAC AppOnboardingNavBar.Action
            | PrimaryEditTextActionController PrimaryEditText.Action 
            | ReferralCodeTextChanged String
            | EnterReferralCode Boolean
            | InAppKeyboardModalAction InAppKeyboardModal.Action
            | SupportClick Boolean
            | WhatsAppClick
            | CallButtonClick
            | ChooseVehicleCategory Int ST.VehicleCategory
            | ContinueButtonAction PrimaryButtonController.Action
            | ExpandOptionalDocs
            | OptionsMenuAction OptionsMenu.Action
            | BottomDrawerListAC BottomDrawerList.Action
            | CallHV String String
            | OnActivityResult String
            | CallWorkFlow String
            | UpdateApkAction
            | StoreDataAction String String
            | CallHVFlowAction ST.HyperVergeKycResult
            | InitFlowTxnIdAction
            
derive instance genericAction :: Generic Action _
instance eqAction :: Eq Action where
  eq _ _ = true

eval :: Action -> RegistrationScreenState -> Eval Action ScreenOutput RegistrationScreenState
eval AfterRender state = continue state

eval BackPressed state = do
  let newState = state {props {dontAllowHvRelaunch = false}}
  if newState.props.enterReferralCodeModal then continue newState { props = newState.props {enterOtpFocusIndex = 0, enterReferralCodeModal = false}, data {referralCode = ""} }
  else if newState.props.menuOptions then continue newState { props { menuOptions = false}}
  else if newState.props.logoutModalView then continue newState { props { logoutModalView = false}}
  else if newState.data.vehicleTypeMismatch then continue newState { data { vehicleTypeMismatch = false}}
  else if newState.props.confirmChangeVehicle then continue newState { props { confirmChangeVehicle = false}}
  else if newState.props.contactSupportModal == ST.SHOW then continue newState { props { contactSupportModal = ST.ANIMATING}}
  else if newState.props.manageVehicle then exit $ GoToHomeScreen newState
  else do
      void $ pure $ JB.minimizeApp ""
      continue newState 

eval (RegistrationAction step ) state = do
       let item = step.stage
           hvFlowIds = decodeForeignObject (getHVRemoteConfig $ fetchRemoteConfigString "app_configs") (hvConfigs $ JB.getAppName unit)
           _ = EHE.addEvent (EHE.defaultEventObject $ HU.getRegisterationStepClickEventName item)
       case item of 
          DRIVING_LICENSE_OPTION -> exit $ GoToUploadDriverLicense state
          VEHICLE_DETAILS_OPTION -> exit $ GoToUploadVehicleRegistration state step.rcNumberPrefixList
          GRANT_PERMISSION -> exit $ GoToPermissionScreen state
          SUBSCRIPTION_PLAN -> exit $ GoToOnboardSubscription state
          PROFILE_PHOTO -> if state.data.cityConfig.enableHvSdk then continueWithCmd state [ pure $ CallHV hvFlowIds.selfie_flow_id ""] else exit $ DocCapture state item
          AADHAAR_CARD -> if state.data.cityConfig.enableHvSdk then continueWithCmd state [ pure $ CallWorkFlow hvFlowIds.aadhaar_flow_id] else exit $ DocCapture state item
          PAN_CARD  -> if state.data.cityConfig.enableHvSdk then continueWithCmd state [ pure $ CallWorkFlow hvFlowIds.pan_flow_id] else exit $ DocCapture state item
          VEHICLE_PERMIT  -> exit $ DocCapture state item
          FITNESS_CERTIFICATE  -> exit $ DocCapture state item
          VEHICLE_INSURANCE -> exit $ DocCapture state item
          VEHICLE_PUC -> exit $ DocCapture state item
          _ -> continue state

eval (CallHV workFLowId inputJson) state = 
  continueWithCmd state {props {dontAllowHvRelaunch = true}}
    [ do
        let transactionId = JB.generateSessionId unit
        push <- getPushFn Mb.Nothing "RegistrationScreen"
        if EHC.os == "ANDROID" 
          then do
            void $ runEffectFn8 JB.initHVSdk state.data.accessToken workFLowId transactionId false (getDefaultAlpha2LanguageCode "") inputJson OnActivityResult push
            pure $ StoreDataAction transactionId workFLowId
          else pure NoAction
    ]


eval (CallWorkFlow flowId) state =
  continueWithCmd state {props {dontAllowHvRelaunch = true}}
    [ do
        void $ launchAff $ EHC.flowRunner defaultGlobalState
          $ do
              resp <- Remote.getLiveSelfie "APPROVED"
              push <- liftFlow $ getPushFn Mb.Nothing "RegistrationScreen"
              liftFlow $ case resp of
                Right (API.GetLiveSelfieResp response) -> do
                  let imagePath = runFn1 JB.decodeAndStoreImage response.image
                  case imagePath of
                    "FAILED" -> do
                      void $ pure $ JB.toast $ getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
                      push NoAction
                    _ -> do
                      push $ CallHV flowId $ "{\"selfie\":\"" <> imagePath <> "\"}"
                _ -> push  NoAction
        pure NoAction
    ]

eval (PopUpModalLogoutAction (PopUpModal.OnButton2Click)) state = continue $ (state {props {logoutModalView= false}})

eval (PopUpModalLogoutAction (PopUpModal.OnButton1Click)) state = exit LogoutAccount

eval (PopUpModalLogoutAction (PopUpModal.DismissPopup)) state = continue state {props {logoutModalView= false}}

eval (ChangeVehicleAC (PopUpModal.OnButton2Click)) state = continue $ (state {props {confirmChangeVehicle= false}})

eval (ChangeVehicleAC (PopUpModal.OnButton1Click)) state = continue state { data { vehicleCategory = Mb.Nothing}, props { menuOptions = false, confirmChangeVehicle= false}}

eval (ChangeVehicleAC (PopUpModal.DismissPopup)) state = continue state {props {confirmChangeVehicle= false}}

eval (VehicleMismatchAC (PopUpModal.OnButton2Click)) state = continue state { data { vehicleTypeMismatch = false}}

eval (VehicleMismatchAC (PopUpModal.OnButton1Click)) state = continue state { data { vehicleCategory = Mb.Nothing, vehicleTypeMismatch = false}}

eval (SupportClick show) state = continue state { props { contactSupportModal = if show then ST.SHOW else if state.props.contactSupportModal == ST.ANIMATING then ST.HIDE else state.props.contactSupportModal}}

eval (BottomDrawerListAC BottomDrawerList.Dismiss) state = continue state { props { contactSupportModal = ST.ANIMATING}}

eval (BottomDrawerListAC BottomDrawerList.OnAnimationEnd) state = continue state { props { contactSupportModal = if state.props.contactSupportModal == ST.ANIMATING then ST.HIDE else state.props.contactSupportModal}}

eval (BottomDrawerListAC (BottomDrawerList.OnItemClick item)) state = do
  case item.identifier of
    "whatsapp" -> continueWithCmd state [pure WhatsAppClick]
    "call" -> continueWithCmd state [pure CallButtonClick]
    _ -> continue state

eval WhatsAppClick state = continueWithCmd state [do
  let supportPhone = state.data.cityConfig.registration.supportWAN
      phone = "%0APhone%20Number%3A%20"<> state.data.phoneNumber
      dl = if (state.data.drivingLicenseStatus == ST.FAILED && state.data.enteredDL /= "__failed") then ("%0ADL%20Number%3A%20"<> state.data.enteredDL) else ""
      rc = if (state.data.vehicleDetailsStatus == ST.FAILED && state.data.enteredRC /= "__failed") then ("%0ARC%20Number%3A%20"<> state.data.enteredRC) else ""
  void $ JB.openUrlInApp $ "https://wa.me/" <> supportPhone <> "?text=Hi%20Team%2C%0AI%20would%20require%20help%20in%20onboarding%20%0A%E0%A4%AE%E0%A5%81%E0%A4%9D%E0%A5%87%20%E0%A4%AA%E0%A4%82%E0%A4%9C%E0%A5%80%E0%A4%95%E0%A4%B0%E0%A4%A3%20%E0%A4%AE%E0%A5%87%E0%A4%82%20%E0%A4%B8%E0%A4%B9%E0%A4%BE%E0%A4%AF%E0%A4%A4%E0%A4%BE%20%E0%A4%95%E0%A5%80%20%E0%A4%86%E0%A4%B5%E0%A4%B6%E0%A5%8D%E0%A4%AF%E0%A4%95%E0%A4%A4%E0%A4%BE%20%E0%A4%B9%E0%A5%8B%E0%A4%97%E0%A5%80" <> phone <> dl <> rc
  pure NoAction
  ]

eval CallButtonClick state = do
  void $ pure $ unsafePerformEffect $ contactSupportNumber ""
  continue state

eval (PrimaryButtonAction (PrimaryButtonController.OnClick)) state = do
  void $ pure if state.props.manageVehicle then unit else  unsafePerformEffect $ logEvent state.data.logField "ny_driver_complete_registration"
  when state.props.referralCodeSubmitted $ do
    let _ = unsafePerformEffect $ logEventWithMultipleParams state.data.logField "ny_driver_complete_registration_with_referral_code" $ [{key : "Referee Code", value : unsafeToForeign state.data.referralCode}]
    pure unit
  exit $ GoToHomeScreen state

eval Refresh state = updateAndExit state { props { refreshAnimation = true}} RefreshPage

eval (AppOnboardingNavBarAC (AppOnboardingNavBar.Logout)) state = continue state {props{menuOptions = not state.props.menuOptions}}

eval (OptionsMenuAction OptionsMenu.BackgroundClick) state = continue state{props{menuOptions = false}}

eval (OptionsMenuAction (OptionsMenu.ItemClick item)) state = do
  let newState = state{props{menuOptions = false}}
  case item of
    "logout" -> continue newState{ props { logoutModalView = true }} 
    "contact_support" -> continueWithCmd newState [pure $ SupportClick true]
    "change_vehicle" -> continue newState { props { confirmChangeVehicle = true}}
    "change_language" -> exit $ SelectLang newState
    "faqs" -> continue newState -- exit $ GoToFaqsScreen newState
    _ -> continue newState

eval (PrimaryEditTextActionController (PrimaryEditText.TextChanged id value)) state = continue state

eval (ReferralCodeTextChanged val) state = continue state{data { referralCode = val }, props {isValidReferralCode = true} }

eval (EnterReferralCode val ) state = if not val then do
                                        pure $ JB.toast $ getString COMPLETE_STEPS_TO_APPLY_REFERRAL
                                        continue state
                                      else continue state {props {enterReferralCodeModal = true, isValidReferralCode = true}}

eval (InAppKeyboardModalAction (InAppKeyboardModal.OnSelection key index)) state = do
  let
    referralCode = if (index + 1) > (DS.length state.data.referralCode) then (DS.take 6 (state.data.referralCode <> key))
                   else (DS.take index (state.data.referralCode)) <> key <> (DS.take 6 (DS.drop (index+1) state.data.referralCode))
    focusIndex = DS.length referralCode
    newState = state { props = state.props {enterOtpFocusIndex = focusIndex }, data{referralCode = referralCode} }
  continue newState
eval (InAppKeyboardModalAction (InAppKeyboardModal.OnClickBack text)) state = do
  let
    referralCode = if DS.length( text ) > 0 then (DS.take (DS.length ( text ) - 1 ) text) else ""
    focusIndex = DS.length referralCode
  continue state { props = state.props { enterOtpFocusIndex = focusIndex, isValidReferralCode = true }, data {referralCode = referralCode} }
eval (InAppKeyboardModalAction (InAppKeyboardModal.OnclickTextBox index)) state = do
  let focusIndex = min index (DS.length  state.data.referralCode)
      referralCode = DS.take index state.data.referralCode
  continue state { props = state.props { enterOtpFocusIndex = focusIndex, isValidReferralCode = true }, data {referralCode = referralCode} }
eval (InAppKeyboardModalAction (InAppKeyboardModal.BackPressed)) state = continue state { props = state.props {enterOtpFocusIndex = 0, enterReferralCodeModal = false}, data {referralCode = ""} }
eval (InAppKeyboardModalAction (InAppKeyboardModal.OnClickDone text)) state = exit $ ReferralCode state

eval ContactSupport state = continueWithCmd state [do
  let merchant = getMerchant FunctionCall
  _ <- case merchant of
    NAMMAYATRI -> contactSupportNumber "WHATSAPP"
    YATRI -> contactSupportNumber "WHATSAPP"
    YATRISATHI -> JB.openWhatsAppSupport $ getWhatsAppSupportNo $ show merchant
    _ -> pure $ JB.showDialer (getSupportNumber "") false
  pure NoAction
  ]

eval (ChooseVehicleCategory index item) state = do
  let _ = EHE.addEvent (EHE.defaultEventObject $ HU.getVehicleCategorySelectedEvent item)
  continue state { props { selectedVehicleIndex = Mb.Just index } }

eval (ContinueButtonAction PrimaryButtonController.OnClick) state = do
  case state.props.selectedVehicleIndex of
    Mb.Nothing -> continue state
    Mb.Just index -> do
      case (state.data.variantList) DA.!! index of
        Mb.Just vehicleType | state.props.manageVehicle -> continue state { data { vehicleCategory = Mb.Just vehicleType }, props { manageVehicleCategory = Mb.Just vehicleType} }
        Mb.Just vehicleType -> do
          void $ pure $ setValueToLocalStore VEHICLE_CATEGORY $ show vehicleType
          void $ pure $ setValueToLocalStore SHOW_SUBSCRIPTIONS 
            $ if DA.elem (show vehicleType) state.data.cityConfig.variantSubscriptionConfig.variantList 
                then "true"
              else "false"
          continue state { data { vehicleCategory = Mb.Just vehicleType } }
        Mb.Nothing -> continue state

eval ExpandOptionalDocs state = continue state { props { optionalDocsExpanded = not state.props.optionalDocsExpanded}}

eval (OnActivityResult bundle) state = continueWithCmd state {props {dontAllowHvRelaunch = false}} [do
  push <- getPushFn Mb.Nothing "RegistrationScreen"
  void $ launchAff $ EHC.flowRunner defaultGlobalState $ do
  -- bundle is the data returned from the activity
    case runExcept (decodeJSON bundle :: _ ST.HyperVergeKycResult) of
      Left _ -> do
        case runExcept (decodeJSON bundle :: _ ST.HvErrorCode) of 
          Left _ -> do
            callHvLoggerForDecodeFailure state.data.hvTxnId (Mb.Just "decode_failed") (Mb.Just "Failed to decode Hyperverge callback payload") state.data.hvFlowId (Mb.Just bundle)
            void $ pure $ JB.toast $ getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
          Right (ST.HvErrorCode errorCode) -> if errorCode.errorCode == 106
            then do
              void $ pure $ JB.toast $ getString WE_NEED_ACCESS_TO_YOUR_LOCATION
              callHvLoggerForDecodeFailure state.data.hvTxnId (Mb.Just "location_permission_error") (Mb.Just "Location Access Not provided") state.data.hvFlowId (Mb.Just bundle)
            else do
              void $ pure $ JB.toast $ getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
              callHvLoggerForDecodeFailure state.data.hvTxnId (Mb.Just "decode_failed") (Mb.Just "Failed to decode Hyperverge callback payload") state.data.hvFlowId (Mb.Just bundle)
        liftFlow $ push InitFlowTxnIdAction
      Right (ST.HyperVergeKycResult result) -> do
        let (Tuple failureReason docType) = getFailureReasonAndDoctype (HyperVergeKycResult result)
        case result.transactionId, state.data.hvTxnId of
          Mb.Just tId, _ ->  void $ Remote.updateHVSdkCallLog $ Remote.makeupdateHVSdkCallLogReq tId result.status state.data.hvFlowId failureReason docType (Mb.Just $ show result)
          _, Mb.Just tId ->  void $ Remote.updateHVSdkCallLog $ Remote.makeupdateHVSdkCallLogReq tId result.status state.data.hvFlowId failureReason docType (Mb.Just $ show result)
          _, _ -> void $ pure $ spy "Error: " "TxnId of Hyperverge sdk call not found so could not log to backend !!!!!!!"
        if result.status == (Mb.Just "auto_approved") || result.status == (Mb.Just "auto_declined") || result.status == (Mb.Just "needs_review")
          then liftFlow $ push $ CallHVFlowAction (ST.HyperVergeKycResult result)
        else
          case result.status of
            Mb.Just "user_cancelled" -> liftFlow $ push InitFlowTxnIdAction
            _ -> do
              if result.errorCode == (Mb.Just 106) then void $ pure $ JB.toast $ getString WE_NEED_ACCESS_TO_YOUR_LOCATION else void $ pure $ JB.toast $ getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
              liftFlow $ push InitFlowTxnIdAction 
      _ -> liftFlow $ push InitFlowTxnIdAction
    pure unit
  pure NoAction
]

eval UpdateApkAction state = exit $ GoToAppUpdatePopUpScreen state {props {dontAllowHvRelaunch = false}}

eval (StoreDataAction txnId flowId) state = continue state { data { hvFlowId = Mb.Just flowId, hvTxnId = Mb.Just txnId}}

eval InitFlowTxnIdAction state = continue state { data { hvFlowId = Mb.Nothing, hvTxnId = Mb.Nothing}}

eval (CallHVFlowAction result) state = exit $ GoToAadhaarPANSelfieUpload (state { data { hvFlowId = Mb.Nothing, hvTxnId = Mb.Nothing}}) result

eval _ state = update state


callHvLoggerForDecodeFailure :: Mb.Maybe String -> Mb.Maybe String -> Mb.Maybe String -> Mb.Maybe String -> Mb.Maybe String -> Flow GlobalState Unit
callHvLoggerForDecodeFailure txnId status mbFailureReason fId result = do
  let docType = join $ getDocTypeFromWorkFlowId <$> fId
  case txnId of
    Mb.Just tId -> void $ Remote.updateHVSdkCallLog $ Remote.makeupdateHVSdkCallLogReq tId status fId mbFailureReason docType result
    Mb.Nothing -> void $ pure $ spy "Error:" "Decode Failure of callback happened and TxnId of Hyperverge sdk call not found so could not log to backend !!!!!!!"
  where
    getDocTypeFromWorkFlowId :: String -> Mb.Maybe String
    getDocTypeFromWorkFlowId flowId 
       | DS.contains (DSP.Pattern "pan") flowId = Mb.Just "PanCard"
       | DS.contains (DSP.Pattern "aadhaar") flowId = Mb.Just "AadhaarCard"
       | DS.contains (DSP.Pattern "selfie") flowId = Mb.Just "ProfilePhoto"
       | otherwise = Mb.Nothing


getFailureReasonAndDoctype :: HyperVergeKycResult -> Tuple (Mb.Maybe String) (Mb.Maybe String)
getFailureReasonAndDoctype (HyperVergeKycResult result) = do
  case result.details of 
    Mb.Just (ST.LIVE_SELFIE (ST.LiveSelfie detail)) -> Tuple (getErrMsgForHV detail.errorCode) (Mb.Just "ProfilePhoto")
    Mb.Just (ST.PAN_DETAILS (ST.PanDetails detail)) -> Tuple (getErrMsgForHV detail.errorCode) (Mb.Just "PanCard")
    Mb.Just (ST.AADHAAR_DETAILS (ST.AadhaarCardDetails detail)) -> Tuple (getErrMsgForHV detail.errorCode) (Mb.Just "AadhaarCard")
    _ -> Tuple Mb.Nothing Mb.Nothing
  where
    getErrMsgForHV errCode = case result.status of
      Mb.Just "auto_declined" -> Mb.Just $ getHvErrorMsg errCode
      Mb.Just "auto_approved" -> Mb.Nothing
      Mb.Just "needs_review" -> Mb.Nothing
      Mb.Just "user_cancelled" -> Mb.Just "User cancelled"
      Mb.Just "error" -> result.errorMessage
      Mb.Just _ -> Mb.Just "Unknown Status received in OnActivity callback"
      Mb.Nothing -> Mb.Just "Null Status received in OnActivity callback"
    
getStatusValue :: String -> StageStatus
getStatusValue value = case value of
  "VALID" -> COMPLETED
  "PENDING" -> IN_PROGRESS
  "FAILED" -> FAILED
  "NO_DOC_AVAILABLE" -> NOT_STARTED
  "INVALID" -> FAILED
  "LIMIT_EXCEED" -> FAILED
  "MANUAL_VERIFICATION_REQUIRED" -> MANUAL_VERIFICATION_REQUIRED
  _ -> NOT_STARTED


getDefaultAlpha2LanguageCode :: String -> String
getDefaultAlpha2LanguageCode _ = do
  let language = JB.getKeyInSharedPrefKeys "LANGUAGE_KEY"
  case language of
    "BN_IN" -> "bn"
    "HI_IN" -> "hi"
    "KN_IN" -> "kn"
    "ML_IN" -> "ml"
    "TA_IN" -> "ta"
    "TE_IN" -> "te"
    "FR_FR" -> "fr"
    _ -> "en"

getStatus :: ST.RegisterationStep -> ST.RegistrationScreenState -> ST.StageStatus
getStatus step state = 
  case step of
    ST.GRANT_PERMISSION -> state.data.permissionsStatus
    -- ST.SUBSCRIPTION_PLAN -> state.data.subscriptionStatus  //don't check from frontend
    _ -> do
          let documentStatusArr = state.data.documentStatusList
              vehicleDoc = [ ST.VEHICLE_PERMIT, ST.FITNESS_CERTIFICATE, ST.VEHICLE_INSURANCE, ST.VEHICLE_PUC, ST.VEHICLE_DETAILS_OPTION]
              findStatus = if step `DA.elem` vehicleDoc 
                          then DA.find (\docStatus -> docStatus.docType == step && filterCondition docStatus) documentStatusArr
                          else DA.find (\docStatus -> docStatus.docType == step) documentStatusArr
          case findStatus of
            Mb.Nothing -> ST.NOT_STARTED
            Mb.Just docStatus -> docStatus.status
  where filterCondition item = (state.data.vehicleCategory == item.verifiedVehicleCategory) || (Mb.isNothing item.verifiedVehicleCategory && item.vehicleType == state.data.vehicleCategory)