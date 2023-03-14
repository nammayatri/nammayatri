module Screens.EnterMobileNumberScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, discard, ($), pure, (<$>))
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.EnterMobileNumberScreen.Controller (ScreenOutput(..))
import Screens.EnterMobileNumberScreen.View as EnterMobileNumberScreen
import Types.App (GlobalState(..), FlowBT, ScreenType(..),defaultGlobalState)
import ModifyScreenState (modifyScreenState)


enterMobileNumberScreen ::FlowBT String ScreenOutput
enterMobileNumberScreen = do
  (GlobalState state') <- getState
  let (GlobalState defaultGlobalState') = defaultGlobalState
  act <- lift $ lift $ runScreen $ EnterMobileNumberScreen.screen state'.enterMobileNumberScreen
  case act of
    GoToAccountSetUp state -> do 
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber ->  state) 
                    App.BackT $ App.BackPoint <$> pure act
    GoToOTP state -> do 
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber ->  state)
                    App.BackT  $ App.BackPoint <$> pure act 
    ResendOTP state -> do 
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber ->  state)
                    App.BackT  $ App.BackPoint <$> pure act 
    GoBack state -> do
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber ->  state{data{otp=""},props{wrongOTP=false}})
                    App.BackT  $ App.NoBack <$> pure act
    GoToChooseLanguage state -> do
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> defaultGlobalState'.enterMobileNumberScreen )
                    App.BackT  $ App.NoBack <$> pure act

-- REFERENCE TO UPDATE STATE GLOBALLY
-- modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen ->  state)