module Screens.ChooseCityScreen.Handler where

import Prelude (bind, ($), pure , (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.ChooseCityScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.ChooseCityScreen.View as ChooseCityScreen
import Types.App (FlowBT, GlobalState(..),CHOOSE_CITY_SCREEN_OUTPUT(..))


chooseCityScreen :: FlowBT String CHOOSE_CITY_SCREEN_OUTPUT
chooseCityScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ ChooseCityScreen.screen state.chooseCityScreen
  case act of
    SelectLanguageScreen -> App.BackT $ pure App.GoBack 
    WelcomeScreen -> App.BackT $ App.BackPoint <$> (pure GoToWelcomeScreen)
    