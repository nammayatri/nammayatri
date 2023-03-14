module Screens.DriverRideRatingScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import Screens.DriverRideRatingScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.DriverRideRatingScreen.View as DriverRideRatingScreen
import Types.App (FlowBT, GlobalState(..), DRIVER_RIDE_RATING_SCREEN_OUTPUT(..))


driverRideRatingScreen :: FlowBT String DRIVER_RIDE_RATING_SCREEN_OUTPUT
driverRideRatingScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ DriverRideRatingScreen.screen state.driverRideRatingScreen
  case action of
    SendFeedBack updatedState -> App.BackT $ App.NoBack <$> pure (SendCustomerFeedBack updatedState)
    Close                     -> App.BackT $ App.NoBack <$> pure CloseScreen