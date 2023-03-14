module Screens.TripDetailsScreen.Handler where

import Prelude ( bind , discard, ($), pure, (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.TripDetailsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.TripDetailsScreen.View as TripDetailsScreen
import Types.App (FlowBT, GlobalState(..), TRIP_DETAILS_SCREEN_OUTPUT(..),ScreenType(..))
import ModifyScreenState (modifyScreenState)

tripDetailsScreen :: FlowBT String TRIP_DETAILS_SCREEN_OUTPUT
tripDetailsScreen = do
    (GlobalState state) <- getState
    act <- lift $ lift $ runScreen $ TripDetailsScreen.screen state.tripDetailsScreen
    case act of
        GoBack fromMyRides -> do 
          if fromMyRides then App.BackT $ App.NoBack <$> (pure $ GO_TO_RIDES)
            else App.BackT $ App.NoBack <$> (pure $ GO_TO_HELPSCREEN)
        OnSubmit state -> App.BackT $ App.NoBack <$> (pure $ ON_SUBMIT state)
        GoToInvoice updatedState -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_INVOICE updatedState )
        GoHome  -> do
            modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {props{issueReported = false}})
            App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME)
        ConnectWithDriver updatedState -> App.BackT $ App.NoBack <$> (pure $ CONNECT_WITH_DRIVER updatedState)