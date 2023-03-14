module Screens.TripDetailsScreen.Handler where

import Prelude ( bind , ($), pure, (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.TripDetailsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.TripDetailsScreen.View as TripDetailsScreen
import Types.App (FlowBT, GlobalState(..), TRIP_DETAILS_SCREEN_OUTPUT(..))

tripDetailsScreen :: FlowBT String TRIP_DETAILS_SCREEN_OUTPUT
tripDetailsScreen = do
    (GlobalState state) <- getState
    act <- lift $ lift $ runScreen $ TripDetailsScreen.screen state.tripDetailsScreen
    case act of
        GoBack -> App.BackT $ pure App.GoBack 
        OnSubmit -> App.BackT $ App.BackPoint <$> (pure $ ON_SUBMIT)
        GoHome  -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_SCREEN)
        