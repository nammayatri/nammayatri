{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SelectContactsFlow.SelectContactsScreen.Handler where

import Prelude (bind, pure, void, unit, ($), (<$>))
import Presto.Core.Types.Language.Flow (doAff)
import Screens.SelectContactsFlow.SelectContactsScreen.View as SelectContactsScreen
import PrestoDOM.Core.Types.Language.Flow(runScreenWithNameSpace, initUIWithNameSpace, showScreenWithNameSpace)
import Types.App (FlowBT, GlobalState(..), SELECT_CONTACT_SCREEN_OUTPUT(..))
import Control.Monad.Except.Trans (lift)
import Effect.Class (liftEffect)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Data.Maybe (Maybe(..))
import PrestoDOM.Core (terminateUI)
import Presto.Core.Types.Language.Flow (getLogFields)
import Helpers.PrestoUtils
import Engineering.Helpers.Commons (liftFlow)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.List as PrestoList
import Components.NewContact.View as NewContact
import Screens.Types (NewContacts)
import Screens.EmergencyContactsScreen.ScreenData (neverShareRideOption)
import Screens.SelectContactsFlow.SelectContactsScreen.Controller (ScreenOutput(..))

selectContactsScreen :: FlowBT String SELECT_CONTACT_SCREEN_OUTPUT
selectContactsScreen = do
  (GlobalState state) ← getState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "SelectContactsScreen"
  listItemm <- lift $ lift $ PrestoList.preComputeListItem $ NewContact.view push listItem1

  _ <- lift $ lift $ doAff $ liftEffect $ initUIWithNameSpace "SelectContactsScreen" (getFragmentView "") 
  act <- lift $ lift $ runScreenWithNameSpace ( SelectContactsScreen.screen state.selectContactsScreen listItemm)
  _ <- lift $ lift $ doAff $ liftEffect $ terminateUI $ Just "SelectContactsScreen"
  case act of
    BackPressed -> do
      App.BackT $ App.NoBack <$> (pure $ SELECT_CONTACTS_BACK_PRESSED)
    ExecuteCallback updatedState -> do
      App.BackT $ App.NoBack <$> (pure $ EXECUTE_CALLBACK updatedState)

listItem1 :: NewContacts
listItem1 = {
  name: "",
  number: "",
  isSelected: false,
  enableForFollowing: false,
  enableForShareRide: false,
  shareTripWithEmergencyContactOption: neverShareRideOption,
  onRide: false,
  priority : 1,
  contactPersonId : Nothing,
  isFollowing: Nothing,
  notifiedViaFCM : Nothing
}