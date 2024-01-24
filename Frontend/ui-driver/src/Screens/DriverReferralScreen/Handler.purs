module Screens.DriverReferralScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, ($), pure, (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.DriverReferralScreen.Controller (ScreenOutput(..))
import Screens.DriverReferralScreen.View as DriverReferralScreen
import Types.App (FlowBT, GlobalState(..), DRIVER_REFERRAL_SCREEN_OUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import Screens.DriverReferralScreen.ScreenData as DriverReferralScreenData

driverReferralScreen :: FlowBT String DRIVER_REFERRAL_SCREEN_OUTPUT
driverReferralScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ DriverReferralScreen.screen state.driverReferralScreen
  case act of
    GoBack -> do
      modifyScreenState $ DriverReferralScreenStateType (\driverReferralScreen -> DriverReferralScreenData.initData)
      App.BackT $ pure App.GoBack
    GoToDriverContestScreen updatedState -> do
      modifyScreenState $ DriverReferralScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ DRIVER_CONTEST_SCREEN)
    BottomNavBarFlow screenName -> App.BackT $ App.NoBack <$> (pure $ DRIVER_REFERRAL_SCREEN_NAV screenName)
