{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NammaSafetyScreen.Handler where

import Components.NewContact.View as NewContact
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (getState)
import Engineering.Helpers.Commons (liftFlow)
import ModifyScreenState (modifyScreenState)
import Prelude (bind, discard, ($), pure, (<$>))
import PrestoDOM (getPushFn)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import PrestoDOM.List as PrestoList
import Screens.NammaSafetyScreen.Controller (ScreenOutput(..))
import Screens.NammaSafetyScreen.View as NammaSafetyScreen
import Screens.Types (NewContacts)
import Types.App (FlowBT, GlobalState(..), ScreenType(..), defaultGlobalState, NAMMA_SAFETY_SCREEN_OUTPUT(..))
import Debug

nammaSafetyScreen ::FlowBT String NAMMA_SAFETY_SCREEN_OUTPUT
nammaSafetyScreen = do
  (GlobalState state') <- getState
  let (GlobalState defaultGlobalState') = defaultGlobalState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "NammaSafetyScreen"
  listItemm <- lift $ lift $ PrestoList.preComputeListItem $ NewContact.view push listItem1
  act <- lift $ lift $ runScreen $ NammaSafetyScreen.screen state'.nammaSafetyScreen listItemm
  case act of
    GoBack -> do
      _ <- pure $ spy "inside go back" act
      App.BackT  $ App.NoBack <$> (pure $ NS_GO_BACK)
    PostContacts updatedState -> do
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ UPDATE_CONTACTS updatedState)
    PostEmergencySettings updatedState-> do
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ POST_EMERGENCY_SETTINGS updatedState)
    -- GetContacts updatedState -> do
    --   modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> updatedState)
    --   App.BackT $ App.NoBack <$> (pure $ GET_CONTACTS updatedState)
    CreateSOS updatedState -> do
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ CREATE_SOS updatedState)
    UpdateAction updatedState -> do
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ UPDATE_ACTION updatedState)
    UpdateSafe updatedState -> do
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ UPDATE_AS_SAFE updatedState)
    Refresh updatedState -> do
      App.BackT $ App.NoBack <$> (pure $ NS_REFRESH updatedState)
    GoToEmergencyContactScreen updatedState -> do
      App.BackT $ App.NoBack <$> (pure $ GO_TO_EMERGENCY_CONTACT_SCREEN updatedState)


listItem1 :: NewContacts
listItem1 = {
  name: "",
  number: "",
  isSelected: false
}