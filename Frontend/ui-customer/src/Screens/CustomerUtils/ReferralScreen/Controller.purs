{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReferralScreen.Controller where

import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.Referral as ReferralComponent
import Data.String as DS
import JBridge (hideKeyboardOnNavigation, shareTextMessage, showKeyboard, copyToClipboard)
import Engineering.Helpers.Utils as EHU
import Log (trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenRender, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, bind, discard, not, pure, void, ($), (==), unit, (<>), (&&),(>=), (<))
import PrestoDOM (class Loggable, Eval, update, continue, continueWithCmd, exit)
import Screens (ScreenName(..), getScreen)
import Screens.Types (ReferralScreenState, ReferralStage(..))
import Helpers.Referral (generateReferralLink)
import Storage (KeyStore(..), getValueToLocalStore)
import Data.Array (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Components.PopUpModal as PopUpModal
import Storage (KeyStore(..), setValueToLocalStore)
import Engineering.Helpers.Commons as EHC
import Language.Strings (getString)
import Language.Types (STR(..))
import DecodeUtil ( getAnyFromWindow)
import Data.Function.Uncurried (runFn3)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen REFERRAL_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen REFERRAL_SCREEN)
      trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
    ContinueButtonAC act -> case act of
      PrimaryButton.OnClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "continue_primary_button" "continue"
      PrimaryButton.NoAction -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "continue_primary_button" "no_action"
    GoToHomeButtonAC act -> case act of
      PrimaryButton.OnClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "go_to_home_primary_button" "go_to_home"
      PrimaryButton.NoAction -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "go_to_home_primary_button" "no_action"
    ReferralEditText act -> case act of
      PrimaryEditText.TextChanged _ _ -> trackAppTextInput appId (getScreen REFERRAL_SCREEN) "referral_code_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen REFERRAL_SCREEN) "referral_code_text_focus_changed" "primary_edit_text"
      PrimaryEditText.TextImageClicked -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "referral_code_text_image_onclick" "primary_edit_text"
    GenericHeaderAC act -> case act of
      GenericHeader.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen REFERRAL_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
      GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "generic_header_action" "forward_icon"
    ExpandReference -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "in_screen" "referral_program_drop_down"
    EditReferralCode act -> case act of
      PrimaryButton.OnClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "edit_referral_primary_button" "edit_referral"
      PrimaryButton.NoAction -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "edit_referral_primary_button" "no_action"
    _ -> pure unit

data Action
  = AfterRender
  | ContinueButtonAC PrimaryButton.Action
  | ReferralEditText PrimaryEditText.Action
  | GenericHeaderAC GenericHeader.Action
  | ExpandReference
  | BackPressed
  | GoToHomeButtonAC PrimaryButton.Action
  | EditReferralCode PrimaryButton.Action
  | GenericHeaderActionController GenericHeader.Action
  | ReferralButton PrimaryButton.Action
  | ShareAndRefer
  | ShowQR
  | RenderQRCode
  | QRCodeAction PrimaryButton.Action
  | ReferralComponentAction ReferralComponent.Action
  | ReferredUserInfoAction
  | CopyToClipboard
  | NoAction

data ScreenOutput
  = UpdateReferral ReferralScreenState
  | GoToHome

eval :: Action -> ReferralScreenState -> Eval Action ScreenOutput ReferralScreenState
eval (ReferralEditText (PrimaryEditText.TextChanged id value)) state = do
  let
    refCode = DS.trim value
    btnActive = (DS.length refCode) == 6
    newState = state { referralCode = refCode, isInvalidCode = false }
  if btnActive then 
    void $ pure $ hideKeyboardOnNavigation true 
    else pure unit
  continue newState { btnActive = btnActive }

eval (ContinueButtonAC PrimaryButton.OnClick) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  exit $ UpdateReferral state

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [ do pure BackPressed ]

eval (GoToHomeButtonAC PrimaryButton.OnClick) state = continueWithCmd state [ do pure BackPressed ]

eval (EditReferralCode PrimaryButton.OnClick) state = continue state{ showThanks = false }

eval ExpandReference state = continue state { isExpandReference = not state.isExpandReference }

eval BackPressed state = do 
  if state.isExpandReference then continue state{ isExpandReference = not state.isExpandReference }
    else exit GoToHome

eval ShareAndRefer state = do
  let shareAppConfig = state.config.shareAppConfig
      title = shareAppConfig.title
      appName = fromMaybe state.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just 
      code = if (state.referralCode `elem` ["__failed", "(null)",""]) then "" else "Referral Code : " <> state.referralCode
      description = "Download " <> appName <> " now!."
      message = shareAppConfig.description <> "\n" <> description <> "\n" <> code <>  "\n" <> (generateReferralLink (getValueToLocalStore CUSTOMER_LOCATION) "share" "referral" "refer" state.referralCode)
  void $ pure $ shareTextMessage title message
  continue state

eval ShowQR state = continue state{ showQRCodePopUp = true }
 
eval (QRCodeAction PrimaryButton.OnClick) state = continue state{ showQRCodePopUp = false }

eval (ReferralButton PrimaryButton.OnClick) state = continue state{ referralComponentProps{ stage = ENTER_REFERRAL_CODE } }

eval ReferredUserInfoAction state = continue state{ referralComponentProps{ showReferredUserInfoPopup = true } }

eval (ReferralComponentAction componentAction) state =
  case componentAction of
    ReferralComponent.OnClickDone referralCode ->
      if DS.length referralCode >= 6 && DS.length referralCode < 10 then 
        continue state{ referralCode = referralCode, referralComponentProps{ applyButtonActive = true } }
      else
        continue state{ referralComponentProps{ applyButtonActive = false } }

    ReferralComponent.PopUpModalAction popUpAction ->
      case popUpAction of
        PopUpModal.OnButton1Click -> do
          case state.referralComponentProps.stage of
            INVALID_POPUP -> do
              void $ pure $ showKeyboard (EHC.getNewIDWithTag "RefferalCode")
              continue state{ referralComponentProps{ stage = ENTER_REFERRAL_CODE } }
            APPLIED_POPUP -> do
              void $ pure $ setValueToLocalStore REFERRAL_STATUS "REFERRED_NOT_TAKEN_RIDE"
              continue state{ referralComponentProps{ stage = NO_REFERRAL_STAGE } } 
            _ -> continue state{ referralComponentProps{ stage = NO_REFERRAL_STAGE } } 
        PopUpModal.OnButton2Click ->
          continue state{ referralComponentProps{ stage = NO_REFERRAL_STAGE } }
        _ -> continue state

    ReferralComponent.ApplyAction PrimaryButton.OnClick ->
      exit $ UpdateReferral state{ referralComponentProps{ applyButtonActive = false } }

    ReferralComponent.SkipAction PrimaryButton.OnClick ->
      continue state{ referralComponentProps{ stage = NO_REFERRAL_STAGE } }

    ReferralComponent.ReferredUserInfo PopUpModal.OnButton2Click ->
      continue state{ referralComponentProps{ showReferredUserInfoPopup = false } }
    
    ReferralComponent.ReferralProgramInfo PopUpModal.OnButton2Click -> 
      continue state{ referralComponentProps{ showReferralProgramInfoPopup = false } }

    _ -> continue state

eval CopyToClipboard state = 
  continueWithCmd state [ do
    _ <- pure $ copyToClipboard state.referralCode
    _ <- pure $ EHU.showToast (getString COPIED)
    pure NoAction
  ]

eval _ state = update state
