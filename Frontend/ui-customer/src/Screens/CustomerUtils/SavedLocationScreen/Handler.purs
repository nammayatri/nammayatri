module Screens.SavedLocationScreen.Handler where

import Prelude ( bind, discard, ($), (<$>), pure)
import Engineering.Helpers.BackTrack (getState)
import Screens.SavedLocationScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.SavedLocationScreen.View as SavedLocationScreen
import Types.App (FlowBT, GlobalState(..), SAVED_LOCATION_SCREEN_OUTPUT(..))
import ModifyScreenState (modifyScreenState)
import Types.App(ScreenType(..))

savedLocationScreen :: FlowBT String SAVED_LOCATION_SCREEN_OUTPUT
savedLocationScreen = do 
  (GlobalState state) <- getState 
  act <- lift $ lift $ runScreen $ SavedLocationScreen.screen state.savedLocationScreen 
  case act of 
    AddLocation updatedState -> do 
      modifyScreenState $ SavedLocationScreenStateType (\savedLocationScreenState → updatedState)
      App.BackT $  App.BackPoint <$> ( pure $ ADD_NEW_LOCATION updatedState)
    DeleteLocation tagName -> App.BackT $ App.NoBack <$> (pure $ DELETE_LOCATION tagName)
    EditLocation cardState -> App.BackT $ App.BackPoint <$> (pure $ EDIT_LOCATION cardState)
    GoBack -> App.BackT $ App.NoBack <$> (pure $ GO_BACK_FROM_SAVED_LOCATION)