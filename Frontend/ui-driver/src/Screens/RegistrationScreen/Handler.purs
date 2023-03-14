module Screens.RegistrationScreen.Handler where

import Prelude (bind, pure, ($), (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.RegistrationScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.RegistrationScreen.View as RegistrationScreen
import Types.App (FlowBT, GlobalState(..), REGISTRATION_SCREENOUTPUT(..))


registration :: FlowBT String REGISTRATION_SCREENOUTPUT
registration = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ RegistrationScreen.screen state.registrationScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    GoToUploadDriverLicense -> App.BackT $ App.BackPoint <$> (pure $ UPLOAD_DRIVER_LICENSE)