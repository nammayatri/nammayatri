module Screens.EmergencyContactsScreen.Handler where

import Prelude (bind, pure, ($), (<$>), discard)
import Engineering.Helpers.BackTrack (getState)
import Screens.EmergencyContactsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.EmergencyContactsScreen.View as EmergencyContactsScreen
import ModifyScreenState (modifyScreenState)
import Types.App (FlowBT, GlobalState(..), EMERGECY_CONTACTS_SCREEN_OUTPUT(..),ScreenType(..))


emergencyContactsScreen:: FlowBT String EMERGECY_CONTACTS_SCREEN_OUTPUT
emergencyContactsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ EmergencyContactsScreen.screen state.emergencyContactsScreen
  case action of
    GoToHomeScreen -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_FROM_EMERGENCY_CONTACTS)
    PostContacts updatedState -> do
      modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactsScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ POST_CONTACTS updatedState)
    GetContacts updatedState -> do
      modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactsScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GET_CONTACTS updatedState)
