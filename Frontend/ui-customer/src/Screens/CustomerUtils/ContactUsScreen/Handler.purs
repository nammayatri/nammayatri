module Screens.ContactUsScreen.Handler where

import Prelude (bind, ($), pure , (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.ContactUsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.ContactUsScreen.View as ContactUsScreen
import Types.App (FlowBT, GlobalState(..),CONTACT_US_SCREEN_OUTPUT(..))


contactUsScreen ::FlowBT String CONTACT_US_SCREEN_OUTPUT
contactUsScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ ContactUsScreen.screen state.contactUsScreen
  case act of
    GoBack ->  App.BackT $ pure App.GoBack 
    GoHome updatedState -> App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME_FROM_CONTACT updatedState)