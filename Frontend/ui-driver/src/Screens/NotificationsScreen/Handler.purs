module Screens.NotificationsScreen.Handler where

import Components.NotificationCard as NotificationCard
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (getState)
import Engineering.Helpers.Commons (liftFlow)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import PrestoDOM.Core2 (getPushFn)
import PrestoDOM.List as PrestoList
import Screens.NotificationsScreen.Controller (ScreenOutput(..))
import Screens.NotificationsScreen.View as NotificationsScreen
import Screens.NotificationsScreen.ScreenData as NotificationsScreenData
import Screens.Types (AnimationState(..))
import Types.App (ScreenType(..), FlowBT, GlobalState(..), NOTIFICATIONS_SCREEN_OUTPUT(..))
import Types.ModifyScreenState (modifyScreenState)

notifications :: FlowBT String NOTIFICATIONS_SCREEN_OUTPUT
notifications = do
  (GlobalState state) <- getState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "NotificationsScreen"
  notificationListItem <- lift $ lift $ PrestoList.preComputeListItem $ NotificationCard.view push
  act <- lift $ lift $ runScreen $ NotificationsScreen.screen state.notificationScreen { shimmerLoader = AnimatedIn } notificationListItem
  case act of
    RefreshScreen state -> App.BackT $ App.NoBack <$> pure (REFRESH_SCREEN state)
    LoaderOutput updatedState -> App.BackT $ App.NoBack <$> (pure $ LOAD_NOTIFICATIONS updatedState)
    GoBack -> do
      modifyScreenState $ NotificationsScreenStateType (\notificationScreen → NotificationsScreenData.initData)
      App.BackT $ pure App.GoBack
