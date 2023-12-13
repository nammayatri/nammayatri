module Screens.EmergencyContactsScreen.Handler where

import Prelude (bind, pure, ($), (<$>), discard)
import Engineering.Helpers.BackTrack (getState)
import Screens.EmergencyContactsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.EmergencyContactsScreen.View as EmergencyContactsScreen
import ModifyScreenState (modifyScreenState)
import Types.App (FlowBT, GlobalState(..), EMERGECY_CONTACTS_SCREEN_OUTPUT(..), ScreenType(..))
import Screens.Types (NewContacts)
import PrestoDOM.List as PrestoList
import Components.NewContact.View as NewContact
import Engineering.Helpers.Commons (liftFlow)
import PrestoDOM.Core (getPushFn)
import Data.Maybe (Maybe(..))

emergencyContactsScreen :: FlowBT String EMERGECY_CONTACTS_SCREEN_OUTPUT
emergencyContactsScreen = do
  (GlobalState state) <- getState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "EmergencyContactsScreen"
  listItemm <- lift $ lift $ PrestoList.preComputeListItem $ NewContact.view push listItem1
  action <- lift $ lift $ runScreen $ EmergencyContactsScreen.screen state.emergencyContactsScreen listItemm
  case action of
    GoToHomeScreen -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_FROM_EMERGENCY_CONTACTS)
    PostContacts updatedState -> do
      modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactsScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ POST_CONTACTS updatedState)
    GetContacts updatedState -> do
      modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactsScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GET_CONTACTS updatedState)
    Refresh updatedState -> App.BackT $ App.NoBack <$> (pure $ REFRESH_EMERGECY_CONTACTS_SCREEN updatedState)

listItem1 :: NewContacts
listItem1 =
  { name: ""
  , number: ""
  , isSelected: false
  }
