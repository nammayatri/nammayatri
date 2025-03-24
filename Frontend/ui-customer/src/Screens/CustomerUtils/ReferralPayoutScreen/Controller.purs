{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.ReferralPayoutScreen.Controller where

import Prelude
import Screens.ReferralPayoutScreen.ScreenData
import PrestoDOM
import PrestoDOM.Types.Core (class Loggable)
import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Helpers.Utils (emitTerminateApp, isParentView)
import Common.Types.App (LazyCheck(..))
import Data.Maybe (Maybe(..))
import Storage (KeyStore(..), setValueToLocalStore, setUserCity)
import Components.MenuButton.Controller (Action(..)) as MenuButtonController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Engineering.Helpers.Commons as EHC
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState)
import Control.Monad.Except.Trans (runExceptT)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Control.Transformers.Back.Trans (runBackT)
import JBridge (toast, copyToClipboard, shareTextMessage)
import Engineering.Helpers.Utils as EHU
import Language.Strings (getString)
import Language.Types (STR(..))
import DecodeUtil (getAnyFromWindow)
import Data.Function.Uncurried (runFn3)
import Helpers.Referral (generateReferralLink)
import Storage (KeyStore(..), getValueToLocalStore)
import Data.Maybe
import Data.Array (elem)
import Services.API

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen ABOUT_US_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen ABOUT_US_SCREEN)
      trackAppEndScreen appId (getScreen ABOUT_US_SCREEN)
    GenericHeaderActionController act -> case act of
      GenericHeaderController.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen ABOUT_US_SCREEN)
      GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "generic_header_action" "forward_icon"
    TermsAndConditions -> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "in_screen" "t_&_c"
    PrivacyPolicy -> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "in_screen" "privacy_policy"
    ShowDemoPopUp -> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "in_screen" "SwitchCityConfigs"
    _ -> pure unit

data Action
  = GenericHeaderActionController GenericHeaderController.Action
  | BackPressed
  | TermsAndConditions
  | AfterRender
  | NoAction
  | ShowDemoPopUp
  | PrivacyPolicy
  | ShowReferralFAQ
  | ShareLink
  | ShareQR
  | CloseQR
  | CloseReferralFAQ
  | CloseUPI
  | CopyToClipboard String
  | VpaTextChanged String
  | AddUPI
  | EditUPI
  | SheetStateChanged String
  | MenuButtonActionController MenuButtonController.Action
  | PrimaryButtonActionController PrimaryButtonController.Action
  | DonePrimaryButtonAC PrimaryButtonController.Action
  | VerifyVPA
  | ShowEarnings
  | HandlePayoutHistory PayoutHistoryResp

data ScreenOutput
  = GoToHomeScreen ReferralPayoutScreenState
  | VerifyVPAOut ReferralPayoutScreenState
  | AddVPAOut ReferralPayoutScreenState
  | EditVPAOut ReferralPayoutScreenState
  | ShowEarningsOut ReferralPayoutScreenState

eval :: Action -> ReferralPayoutScreenState -> Eval Action ScreenOutput ReferralPayoutScreenState
eval (GenericHeaderActionController (GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [ do pure BackPressed ]

eval ShareQR state = continue state { props { showShareAppQr = true } }

eval CloseQR state = continue state { props { showShareAppQr = false } }

eval ShowEarnings state = exit $ ShowEarningsOut $ state { props { isEarnings = true } }

eval AddUPI state = exit $ AddVPAOut state

eval EditUPI state = exit $ EditVPAOut state

eval (CopyToClipboard clipBoardData) state = do
  let
    _ = copyToClipboard clipBoardData

    _ = toast (getString COPIED)
  update state

eval ShowReferralFAQ state = continue state { props { showReferralFaq = true } }

eval ShareLink state = do
  let
    shareAppConfig = state.data.appConfig.shareAppConfig

    title = shareAppConfig.title

    appName = fromMaybe state.data.appConfig.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just

    code = if (state.data.referralCode `elem` [ "__failed", "(null)", "" ]) then "" else "Referral Code : " <> state.data.referralCode

    description = "Download " <> appName <> " now!."

    message = shareAppConfig.description <> "\n" <> description <> "\n" <> code <> "\n" <> (generateReferralLink (getValueToLocalStore CUSTOMER_LOCATION) "share" "referral" "refer" state.data.referralCode)
  void $ pure $ shareTextMessage title message
  continue state

eval CloseReferralFAQ state = continue state { props { showReferralFaq = false } }

eval (SheetStateChanged sheet) state = if sheet /= "5" then update state { props { showReferralFaq = true } } else continue state { props { showReferralFaq = false } }

eval BackPressed state =
  if state.props.isEarnings then
    continue state { props { isEarnings = false } }
  else if state.props.showShareAppQr then
    continue state { props { showShareAppQr = false } }
  else if state.props.showUPIPopUp then
    continue state { props { showUPIPopUp = false } }
  else if state.props.showReferralFaq then
    continue state { props { showReferralFaq = false } }
  else if isParentView FunctionCall then do
    void $ pure $ emitTerminateApp Nothing true
    continue state
  else
    exit $ GoToHomeScreen state

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = exit $ AddVPAOut state

eval (DonePrimaryButtonAC PrimaryButtonController.OnClick) state = continue state { props { showUpiSuccess = false } }

eval (HandlePayoutHistory (PayoutHistoryResp resp)) state = continue state { data { history = resp.history } }

eval _ state = update state
