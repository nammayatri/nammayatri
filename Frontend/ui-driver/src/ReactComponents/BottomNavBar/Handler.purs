module ReactComponents.BottomNavBar.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, ($), pure, (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
-- import Screens.DriverReferralScreen.Controller (ScreenOutput(..))
import Screens.DriverReferralScreen.View as DriverReferralScreen
import Types.App (FlowBT, GlobalState(..), DRIVER_REFERRAL_SCREEN_OUTPUT(..), ScreenType(..), NAVIGATION_ACTIONS(..))
import Types.ModifyScreenState (modifyScreenState)
import Screens.DriverReferralScreen.ScreenData as DriverReferralScreenData
import Engineering.Helpers.Commons (liftFlow)
import PrestoDOM.Core (getPushFn)
import Data.Maybe (Maybe(..))
import React.Navigation.Navigate (navigateToScreen)
import ReactComponents.BottomNavBar.View as BottomNavBarView
import ReactComponents.BottomNavBar.Controller (ScreenOutput(..), SCREEN_OUTPUT(..), navData)

-- bottomNavBarScreen :: FlowBT String SCREEN_OUTPUT
-- bottomNavBarScreen = do

-- push <- lift $ lift $ liftFlow $ getPushFn Nothing "bottomNavBarComponent"
-- act <- lift $ lift $ navigateToScreen $ BottomNavBarView.app push ()
-- case act of
--   GoToRidesScreen -> App.BackT $ App.BackPoint <$> (pure $ GO_RIDE_HISTORY_SCREEN)
--   GoToHomeScreen -> App.BackT $ App.BackPoint <$> (pure $ GO_HOME_SCREEN)
--   GoToProfileScreen -> App.BackT $ App.BackPoint <$> (pure $ GO_PROFILE_SCREEN)
--   GoToReferralScreen -> App.BackT $ App.BackPoint <$> (pure $ GO_REFERRAL_SCREEN)
--   SubscriptionScreen -> App.BackT $ App.NoBack <$> (pure $ GO_SUBSCRIPTION_SCREEN)
