module Screens.CustomerUtils.FavouriteDriverTrips.Handler where

import Prelude

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import ModifyScreenState (modifyScreenState)
import Prelude (bind, discard, ($), pure, (<$>))
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.CustomerUtils.FavouriteDriverTrips.Controller (ScreenOutput(..))
import Screens.CustomerUtils.FavouriteDriverTrips.View as FavouriteDriverTrips
import Screens.Types (TripDetailsGoBackType(..))
import Types.App (FlowBT, GlobalState(..), FAVOURITE_DRIVERLIST_SCREEN_OUTPUT(..), ScreenType(..))

favouriteDriverTrips :: FlowBT String FAVOURITE_DRIVERLIST_SCREEN_OUTPUT
favouriteDriverTrips = do
    (GlobalState state) <- getState
    act <- lift $ lift $ runScreen $ FavouriteDriverTrips.screen state.favouriteDriverListScreen (GlobalState state)

    case act of
      GoBack -> App.BackT $ pure App.GoBack
      GoToSavedLocation state -> App.BackT $ App.NoBack <$> (pure $ GO_BACK_TO_SAVED_LOCATION state)
      GoToFavDriverProfile state -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_FAVOURITE_DRIVER_PROFILE state)