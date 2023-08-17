module Screens.BookingOptionsScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.BookingOptionsScreen.Controller (ScreenOutput(..))
import Screens.BookingOptionsScreen.View as BookingOptionsScreen
import Types.App (BOOKING_OPTIONS_SCREEN_OUTPUT(..), FlowBT, GlobalState(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)

bookingOptions :: FlowBT String BOOKING_OPTIONS_SCREEN_OUTPUT
bookingOptions = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ BookingOptionsScreen.screen state.bookingOptionsScreen
  case action of
    SelectCab updatedState -> do
      _ <- modifyScreenState $ BookingOptionsScreenType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ SELECT_CAB updatedState)
    GoBack -> App.BackT $ pure App.GoBack
