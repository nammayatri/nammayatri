{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NotificationsScreen.Handler where

import Components.NotificationCard as NotificationCard
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (getState)
import Engineering.Helpers.Commons (liftFlow)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.List as PrestoList
import Screens.NotificationsScreen.Controller (ScreenOutput(..))
import Screens.NotificationsScreen.ScreenData as NotificationsScreenData
import Screens.NotificationsScreen.View as NotificationsScreen
import Screens.Types (AnimationState(..))
import Types.App (FlowBT, GlobalState(..), NAVIGATION_ACTIONS(..), NOTIFICATIONS_SCREEN_OUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import React.Navigation.Navigate (navigateToScreen)

notifications :: FlowBT String NOTIFICATIONS_SCREEN_OUTPUT
notifications = do
  (GlobalState state) <- getState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "NotificationsScreen"
  notificationListItem <- lift $ lift $ PrestoList.preComputeListItem $ NotificationCard.view push
  act <- lift $ lift $ navigateToScreen $ NotificationsScreen.screen state.notificationScreen { shimmerLoader = AnimatedIn } notificationListItem
  case act of
    RefreshScreen state -> App.BackT $ App.NoBack <$> pure (REFRESH_SCREEN state)
    LoaderOutput updatedState -> App.BackT $ App.NoBack <$> (pure $ LOAD_NOTIFICATIONS updatedState)
    GoBack -> do
      modifyScreenState $ NotificationsScreenStateType (\notificationScreen → NotificationsScreenData.initData)
      App.BackT $ pure App.GoBack
    GoToRidesScreen -> App.BackT $ App.BackPoint <$> (pure $ GO_RIDE_HISTORY_SCREEN)
    GoToHomeScreen -> App.BackT $ App.BackPoint <$> (pure $ GO_HOME_SCREEN )
    GoToProfileScreen -> App.BackT $ App.BackPoint <$> (pure $ GO_PROFILE_SCREEN )
    GoToReferralScreen -> App.BackT $ App.BackPoint <$> (pure $ GO_REFERRAL_SCREEN)
    GoToCurrentRideFlow -> do
      modifyScreenState $ NotificationsScreenStateType (\notificationScreen → NotificationsScreenData.initData)
      App.BackT $ App.NoBack <$> (pure $ CHECK_RIDE_FLOW_STATUS)
    SubscriptionScreen updatedState -> App.BackT $ App.NoBack <$> (pure $ NOTIFICATION_SCREEN_NAV GoToSubscription)