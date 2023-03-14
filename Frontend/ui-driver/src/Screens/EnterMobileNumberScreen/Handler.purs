module Screens.EnterMobileNumberScreen.Handler where
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>),discard)
import Screens.EnterMobileNumberScreen.Controller (ScreenOutput(..))
import Storage (KeyStore(..), setValueToLocalStore)
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.EnterMobileNumberScreen.View as EnterMobileNumberScreen
import Types.App (FlowBT, GlobalState(..),ScreenType(..), ENTER_MOBILE_NUMBER_SCREEN_OUTPUT(..))
import Types.ModifyScreenState (modifyScreenState)

enterMobileNumber :: FlowBT String ENTER_MOBILE_NUMBER_SCREEN_OUTPUT
enterMobileNumber = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ EnterMobileNumberScreen.screen state.mobileNumberScreen
  case act of
    GoBack -> do 
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber → enterMobileNumber { data { mobileNumber = ""}, props{btnActive = false}})
      App.BackT $ pure App.GoBack
    GoToNextScreen updatedState -> do
      _ <- setValueToLocalStore MOBILE_NUMBER_KEY updatedState.data.mobileNumber
      modifyScreenState $ EnterOTPScreenType (\enterOTPScreen → enterOTPScreen { data {mobileNo = updatedState.data.mobileNumber}})
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreenScreen → updatedState)
      App.BackT $ App.BackPoint <$> pure (GO_TO_ENTER_OTP updatedState)
