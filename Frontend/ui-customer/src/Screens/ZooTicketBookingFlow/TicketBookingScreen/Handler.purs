module Screens.TicketBookingScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>))
import Screens.TicketBookingScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import ModifyScreenState (modifyScreenState)
import Screens.TicketBookingScreen.View as TicketBookingScreen
import Types.App (FlowBT, GlobalState(..), ScreenType(..))

ticketBookingScreen :: FlowBT String ScreenOutput
ticketBookingScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ TicketBookingScreen.screen state.ticketBookingScreen
  case action of
    NextScreen language -> App.BackT $ App.NoBack <$> (pure action)
