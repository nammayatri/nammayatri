{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.AskPermissionScreen.Controller where

import Components.PrimaryButton.Controller as PrimaryButtonController
import Prelude (class Show, discard, pure, unit, void, ($), (&&), (+), (-), (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit)
import PrestoDOM.Types.Core (class Loggable)
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Screens.Types as ST
import JBridge as JB
import Components.StepsHeaderModel as SHM

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action
  = PrimaryButtonActionController PrimaryButtonController.Action
  | OverlayCallBack Boolean
  | LocationCallBack Boolean
  | BatteryPermissionCallBack Boolean
  | StepsHeaderModelAC SHM.Action
  | UpdateCurrentItem
  | BackPressed
  | NoAction

data ScreenOutput
  = HomeScreen

eval :: Action -> ST.AskPermissionScreenState -> Eval Action ScreenOutput ST.AskPermissionScreenState
eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state =
  continueWithCmd state
    [ do
        void
          $ liftEffect case state.props.currentStep of
              Just ST.LocationPermission -> JB.requestLocation unit
              Just ST.Overlay -> JB.checkOverlayPermission unit
              Just ST.Battery -> JB.requestBatteryPermission unit
              Just ST.Notifications -> JB.checkAndAskNotificationPermission true
              Just ST.AutoStart -> JB.requestAutoStartPermission unit
              Nothing -> JB.checkAndAskNotificationPermission true
        pure NoAction
    ]

eval BackPressed state =
  if state.props.backpressEnabled then
    exit $ HomeScreen
  else do
    void $ pure $ JB.minimizeApp ""
    continue state

eval (StepsHeaderModelAC SHM.OnArrowClick) state = continueWithCmd state [ pure BackPressed ]

eval (OverlayCallBack isGranted) state =
  if isGranted && state.props.currentStep == Just ST.Overlay then
    continueWithCmd state [ pure UpdateCurrentItem ]
  else
    continue state

eval (LocationCallBack isGranted) state =
  if isGranted && state.props.currentStep == Just ST.LocationPermission then
    continueWithCmd state [ pure UpdateCurrentItem ]
  else
    continue state

eval (BatteryPermissionCallBack isGranted) state =
  if isGranted && state.props.currentStep == Just ST.Battery then
    continueWithCmd state [ pure UpdateCurrentItem ]
  else
    continue state

eval UpdateCurrentItem state = case state.props.currentStep of
  Just step -> do
    let
      currentIndex = DA.findIndex (\item -> item == step) state.data.permissionList
    case currentIndex of
      Just index -> do
        let
          isLast = index == (DA.length state.data.permissionList) - 1

          nextItem = state.data.permissionList DA.!! (index + 1)
        case isLast, nextItem of
          true, _ -> exit $ HomeScreen
          false, Just item -> continue state { props { currentStep = Just item } }
          _, _ -> continue state
      Nothing -> continue state
  Nothing -> continue state

eval _ state = continue state
