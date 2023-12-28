{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReferralScreen.Handler where


import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (isJust)
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.ReferralScreen.Controller (ScreenOutput(..), getReferralStage)
import Screens.ReferralScreen.ScreenData as ReferralScreenData
import Screens.ReferralScreen.ScreenData as ReferralScreenData
import Screens.ReferralScreen.View as ReferralScreen
import Screens.Types (ReferralType(..))
import Types.App (FlowBT, GlobalState(..), NAVIGATION_ACTIONS(..), REFERRAL_SCREEN_OUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Common.Types.App (LazyCheck(..))

referralScreen:: FlowBT String REFERRAL_SCREEN_OUTPUT
referralScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ ReferralScreen.screen state.referralScreen{ props{ stage = getReferralStage state.referralScreen } }
  case action of
    GoBack -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> ReferralScreenData.initData)
      App.BackT $ pure App.GoBack
    GoToHomeScreen updatedState -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_SCREEN_FROM_REFERRAL_SCREEN)
    GoToRidesScreen updatedState -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_RIDES_SCREEN_FROM_REFERRAL_SCREEN)
    GoToProfileScreen updatedState -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_PROFILE_SCREEN_FROM_REFERRAL_SCREEN)
    GoToNotifications updatedState -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_NOTIFICATION_SCREEN_FROM_REFERRAL_SCREEN)
    LinkReferralApi updatedState -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_FLOW_AND_COME_BACK updatedState)
    Refresh updatedState -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ REFRESH_LEADERBOARD)
    SubscriptionScreen updatedState -> do
      modifyScreenState $ ReferralScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ REFERRAL_SCREEN_NAV GoToSubscription)