{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AboutUsScreen.Controller where

import Prelude (class Show, pure, unit, bind, void, ($), discard, (==), (+))
import Screens.Types (AboutUsScreenState)
import PrestoDOM (Eval, update, continue, exit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Helpers.Utils (emitTerminateApp, isParentView)
import Common.Types.App (LazyCheck (..))
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
import JBridge (toast)
import Engineering.Helpers.Utils as EHU

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
    ShowDemoPopUp-> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "in_screen" "SwitchCityConfigs"
    _ -> pure unit

data Action = GenericHeaderActionController GenericHeaderController.Action
            | BackPressed
            | TermsAndConditions
            | AfterRender
            | ShowDemoPopUp
            | PrivacyPolicy
            | MenuButtonActionController MenuButtonController.Action
            | PrimaryButtonActionController PrimaryButtonController.Action

data ScreenOutput = GoToHomeScreen AboutUsScreenState
eval :: Action -> AboutUsScreenState -> Eval Action ScreenOutput AboutUsScreenState

eval (GenericHeaderActionController (GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval BackPressed state = 
  if isParentView FunctionCall 
    then do 
      void $ pure $ emitTerminateApp Nothing true
      continue state
    else exit $ GoToHomeScreen state{props {demoModePopup = false, enableDemoModeCount = 0}}

eval (MenuButtonActionController (MenuButtonController.OnClick config)) state = do
  void $ pure $ setValueToLocalStore CUSTOMER_LOCATION config.id
  void $ pure $ setValueToLocalStore LOCATION_MANUALLY_UPDATED "true"
  continueWithCmd state [do
    void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
      liftFlowBT EHU.reboot
      pure unit
    pure $ BackPressed
  ]

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = continue state

eval ShowDemoPopUp state = do
  if (state.props.enableDemoModeCount == 5) then do
    _ <- pure $ toast $ "You can now switch city!!"
    continue state {props {demoModePopup = true}}
    else continue state {props {enableDemoModeCount = state.props.enableDemoModeCount + 1}}

eval _ state = update state