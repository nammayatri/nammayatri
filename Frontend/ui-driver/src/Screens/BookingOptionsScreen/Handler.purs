module Screens.BookingOptionsScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.BookingOptionsScreen.Controller (ScreenOutput(..))
import Screens.BookingOptionsScreen.View as BookingOptionsScreen
import Types.App (FlowBT, GlobalState(..), BOOKING_OPTIONS_SCREEN_OUTPUT(..))

bookingOptions :: FlowBT String BOOKING_OPTIONS_SCREEN_OUTPUT
bookingOptions = do
    (GlobalState state) <- getState
    action <- lift $ lift $ runScreen $ BookingOptionsScreen.screen state.bookingOptionsScreen
    case action of
        SelectCab state -> App.BackT $ App.BackPoint <$> (pure $ SELECT_CAB state)
        GoBack -> App.BackT $ pure App.GoBack