{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AboutUsScreen.Controller where

import Prelude (class Show, pure, unit, discard, bind, (>=), (==), (||), ($), (&&), (+), (<>), (-), show)
import PrestoDOM (Eval, exit, continue, updateAndExit)
import Screens.Types (AboutUsScreenState)
import PrestoDOM.Types.Core (class Loggable)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Components.PopUpModal.Controller as PopUpModal
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Data.String (length)
import JBridge (toast)
import Storage (KeyStore(..), setValueToLocalStore, getValueToLocalStore)
import Data.Array (elem)

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of 
    AfterRender -> trackAppScreenRender appId "screen" (getScreen ABOUT_US_SCREEN)
    BackPressed flag -> do
      trackAppBackPress appId (getScreen ABOUT_US_SCREEN)
      if flag then trackAppScreenEvent appId (getScreen ABOUT_US_SCREEN) "in_screen" "backpress_in_demo_mode_popup"
        else trackAppEndScreen appId (getScreen ABOUT_US_SCREEN)
    ShowDemoPopUp -> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "in_screen" "show_demo_popup_onclick"
    PopUpModalDemoModeAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "popup_modal" "button_1_onclick"
      PopUpModal.OnButton2Click -> do
        trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "popup_modal" "button_2_onclick"
        trackAppEndScreen appId (getScreen ABOUT_US_SCREEN)
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "popup_modal" "image_onclick"
      PopUpModal.ETextController act -> case act of 
        PrimaryEditTextController.TextChanged valId newVal -> trackAppTextInput appId (getScreen ABOUT_US_SCREEN) "popup_modal_password_text_changed" "primary_edit_text"
        PrimaryEditTextController.FocusChanged _ -> trackAppTextInput appId (getScreen ABOUT_US_SCREEN) "popup_modal_password_text_focus_changed" "primary_edit_text"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "popup_modal" "no_action"
      PopUpModal.CountDown seconds status timerID -> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "popup_modal_cancel_confirmation" "countdown_onclick"
      PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen ABOUT_US_SCREEN) "popup_modal_action" "tip_clicked"
      PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen ABOUT_US_SCREEN) "popup_modal_action" "popup_dismissed"
      PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen ABOUT_US_SCREEN) "popup_modal_action" "secondary_text_clicked"
      PopUpModal.YoutubeVideoStatus _ -> trackAppScreenEvent appId (getScreen ABOUT_US_SCREEN) "popup_modal_action" "youtube_video_status"
      PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen ABOUT_US_SCREEN) "popup_modal_action" "option_with_html_clicked"
    TermsAndConditionAction -> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "in_screen" "t_&_c"
    NoAction -> trackAppScreenEvent appId (getScreen ABOUT_US_SCREEN) "in_screen" "no_action"
    
data ScreenOutput = GoBack | GoToHome AboutUsScreenState

data Action = BackPressed Boolean | NoAction | AfterRender | ShowDemoPopUp | PopUpModalDemoModeAction PopUpModal.Action | TermsAndConditionAction
eval :: Action -> AboutUsScreenState -> Eval Action ScreenOutput AboutUsScreenState
eval AfterRender state = continue state
eval ShowDemoPopUp state = do
  if (state.props.enableDemoModeCount == 4 && getValueToLocalStore DRIVER_STATUS == "true") then do
    _ <- pure $ toast $ "Demo mode activated, enter password to enable demo mode!!"
    continue state {props {demoModePopup = true}}
    else do
      -- _ <- pure $ toast $ "You are " <> show (4 - state.props.enableDemoModeCount) <> " steps away to enable demo mode!!"
      continue state {props {enableDemoModeCount = state.props.enableDemoModeCount + 1}}
eval (BackPressed flag) state =  if state.props.demoModePopup then continue state {props{enableConfirmPassword=false,demoModePopup = false}} else exit GoBack
eval NoAction state = continue state
eval (PopUpModalDemoModeAction (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ setValueToLocalStore IS_DEMOMODE_ENABLED  "true"
  updateAndExit state {props{enableConfirmPassword = false,demoModePopup = false}} $ GoToHome state {props{enableConfirmPassword = false,demoModePopup = false}}
eval (PopUpModalDemoModeAction (PopUpModal.OnButton1Click)) state = continue state --{props{enableConfirmPassword=false,demoModePopup = false}}
eval (PopUpModalDemoModeAction (PopUpModal.OnImageClick)) state = continue state {props{demoModePopup = false}}
eval (PopUpModalDemoModeAction (PopUpModal.ETextController (PrimaryEditTextController.TextChanged valId newVal))) state = do
  _ <- pure $ setValueToLocalStore DEMO_MODE_PASSWORD newVal
  continue state{ props{ enableConfirmPassword = (validateDemoMode newVal) }}
eval _ state = continue state

validateDemoMode :: String -> Boolean
validateDemoMode newVal = length newVal >= 7 && newVal `elem` ["7891234", "8917234", "9178234", "1789234","7891789","7891788", "7891567", "7891678"]
