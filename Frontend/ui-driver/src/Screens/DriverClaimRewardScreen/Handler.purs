module Screens.DriverClaimRewardScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>) , discard)
import PrestoDOM.Core.Types.Language.Flow (runLoggableScreen)
import Screens.DriverClaimRewardScreen.Controller (ScreenOutput(..))
import Screens.DriverClaimRewardScreen.View as DriverClaimRewardScreen
import Types.App (DRIVER_CLAIM_REWARD_SCREEN_OUTPUT(..), FlowBT, GlobalState(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import Screens.DriverClaimRewardScreen.ScreenData as DriverClaimRewardScreenData

driverClaimRewardScreen :: FlowBT String DRIVER_CLAIM_REWARD_SCREEN_OUTPUT
driverClaimRewardScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runLoggableScreen $ DriverClaimRewardScreen.screen state.driverClaimRewardScreen
  case action of
    GoBack -> do
      modifyScreenState $ DriverClaimRewardScreenStateType (\driverClaimRewardScreen -> DriverClaimRewardScreenData.initData)
      App.BackT $ pure App.GoBack 