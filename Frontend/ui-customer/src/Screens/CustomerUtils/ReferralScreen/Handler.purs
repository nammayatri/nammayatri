module Screens.ReferralScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import JBridge (toggleLoader)
import ModifyScreenState (modifyScreenState)
import Prelude (bind, discard, ($), (<$>), pure, void)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.ReferralScreen.Controller (ScreenOutput(..))
import Screens.ReferralScreen.View as ReferralScreen
import Types.App (FlowBT, GlobalState(..), REFERRAL_SCREEN_OUPUT(..), ScreenType(..))

referralScreen :: FlowBT String REFERRAL_SCREEN_OUPUT
referralScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ ReferralScreen.screen state.referralScreen
  void $ lift $ lift $ toggleLoader false
  case act of
    UpdateReferral updatedState -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreenState -> updatedState)
      App.BackT $ App.NoBack <$> pure (UPDATE_REFERRAL updatedState.referralCode)
    GoToHome -> App.BackT $ App.NoBack <$> pure BACK_TO_HOME
