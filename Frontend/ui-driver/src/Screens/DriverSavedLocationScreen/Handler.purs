module Screens.DriverSavedLocationScreen.Handler where

import Debug
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, ($), pure, (<$>), unit, Unit, discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.DriverSavedLocationScreen.Controller (ScreenOutput(..))
import Screens.DriverSavedLocationScreen.View as DriverSavedLocationScreen
import Types.App (DRIVE_SAVED_LOCATION_OUTPUT(..), ScreenType(..))
import Types.App (FlowBT, GlobalState(..))
import Types.ModifyScreenState (modifyScreenState)

driverSavedLocationScreen :: FlowBT String DRIVE_SAVED_LOCATION_OUTPUT
driverSavedLocationScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ DriverSavedLocationScreen.screen state.driverSavedLocationScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    CallAutoComplete searchVal updatedState -> do
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> pure (AUTO_COMPLETE searchVal updatedState)
    UpdateConfirmLocation updatedScreen -> do
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updatedScreen)
      App.BackT $ App.NoBack <$> pure (GET_LOCATION_NAME updatedScreen)
    SaveLocation updatedState -> do
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> pure (SAVE_LOCATION updatedState)
    GetPlaceNameAPI updatedState placeId -> do
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> pure (GET_PLACE_NAME updatedState placeId)
    DeleteLocation updatedState placeId -> do
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> pure (DELETE_PLACE updatedState placeId)
    UpdateHomeLocation updateState placeId -> do
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updateState)
      App.BackT $ App.NoBack <$> pure (UPDATE_HOME_LOCATION updateState placeId)
    ChangeView updatedState -> do
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> pure (CHANGE_VIEW)
    _ -> App.BackT $ pure App.GoBack
