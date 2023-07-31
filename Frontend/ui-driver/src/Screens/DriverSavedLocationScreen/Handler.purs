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
    __ <- pure $ spy "GlobalState" "_" 
    action <- lift $ lift $ runScreen $ DriverSavedLocationScreen.screen state.driverSavedLocationScreen
    case action of
        GoBack -> App.BackT $ pure App.GoBack 
        CallAutoComplete searchVal updatedState -> do
            modifyScreenState $ DriverSavedLocationScreenStateType (\screenState -> screenState)
            App.BackT $ App.NoBack <$> pure (AUTO_COMPLETE searchVal updatedState)
        _ ->  App.BackT $ App.BackPoint <$> pure EXIT_FROM_SCREEN

