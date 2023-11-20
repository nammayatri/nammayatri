module Screens.ChooseCityScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, ($), pure, (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.ChooseCityScreen.Controller (ScreenOutput(..))
import Screens.ChooseCityScreen.View as ChooseCityScreen
import Types.App (CHOOSE_CITY_SCREEN_OUTPUT(..), FlowBT, GlobalState(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)


chooseCityScreen :: FlowBT String CHOOSE_CITY_SCREEN_OUTPUT
chooseCityScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ ChooseCityScreen.screen state.chooseCityScreen
  case act of
    SelectLanguageScreen -> App.BackT $ pure App.GoBack 
    WelcomeScreen -> App.BackT $ App.NoBack <$> (pure GoToWelcomeScreen)
    GetLatLong updateState -> do
      modifyScreenState $ ChooseCityScreenStateType (\chooseCityScreen -> updateState)
      App.BackT $ App.BackPoint <$> (pure $ GET_LAT_LONGS updateState)
    RefreshScreen updatedState -> do
      modifyScreenState $ ChooseCityScreenStateType (\chooseCityScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ REFRESH_SCREEN_CHOOSE_CITY updatedState)
