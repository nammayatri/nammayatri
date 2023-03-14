module Screens.AddNewAddressScreen.Handler where

import Prelude ( bind, ($), (<$>), pure, discard)
import Engineering.Helpers.BackTrack (getState)
import Screens.AddNewAddressScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.AddNewAddressScreen.View as AddNewAddressScreen
import Types.App (FlowBT, GlobalState(..), ADD_NEW_ADDRESS_SCREEN_OUTPUT(..), ScreenType(..))
import Components.SettingSideBar.Controller (Status(..))
import ModifyScreenState (modifyScreenState)


addNewAddressScreen :: FlowBT String ADD_NEW_ADDRESS_SCREEN_OUTPUT
addNewAddressScreen = do 
  (GlobalState state) <- getState 
  act <- lift $ lift $ runScreen $ AddNewAddressScreen.screen state.addNewAddressScreen 
  case act of 
    SearchPlace input updatedState ->  App.BackT $ App.NoBack <$> (pure $ SEARCH_ADDRESS input updatedState)
    AddLocation updatedState -> App.BackT $ App.NoBack <$> (pure $ ADD_LOCATION updatedState)
    UpdateLocationName state lat lng-> do
      modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreenState -> state)
      App.BackT $ App.BackPoint <$> pure (UPDATE_LOCATION_NAME_ADDRESS state lat lng)
    GoToFavourites -> App.BackT $ App.NoBack <$> (pure $ GO_TO_FAVOURITES)
    CheckLocServiceability updatedState locItemType -> App.BackT $ App.NoBack <$> (pure $ CHECK_LOCATION_SERVICEABILITY updatedState locItemType)
    GoToHome -> App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME_SCREEN_FLOW)

