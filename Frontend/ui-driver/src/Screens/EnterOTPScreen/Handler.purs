module Screens.EnterOTPScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import Screens.EnterOTPScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.EnterOTPScreen.View as EnterOTPScreen
import Types.App (FlowBT, GlobalState(..), ENTER_OTP_SCREEN_OUTPUT(..),ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)

enterOTP :: FlowBT String ENTER_OTP_SCREEN_OUTPUT
enterOTP = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ EnterOTPScreen.screen state.enterOTPScreen
  case act of
    GoBack updatedState  -> do 
      modifyScreenState $ EnterOTPScreenType (\enterOTPScreen → updatedState)
      App.BackT $ pure App.GoBack
    Retry updatedState -> App.BackT $ App.NoBack <$> pure (RETRY updatedState)
    GoToHome updatedState -> App.BackT $ App.BackPoint <$> pure (DRIVER_INFO_API_CALL updatedState)