module Screens.TicketInfoScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>), unit)
import Screens.TicketInfoScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import ModifyScreenState (modifyScreenState)
import Screens.TicketInfoScreen.View as TicketInfoScreen
import Types.App (FlowBT, GlobalState(..), TICKET_INFO_SCREEN_OUTPUT(..), ScreenType(..))

ticketInfoScreen :: FlowBT String TICKET_INFO_SCREEN_OUTPUT
ticketInfoScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ TicketInfoScreen.screen state.ticketInfoScreen
  case action of
    GoToHomeScreen -> App.BackT $ App.NoBack <$> (pure GO_TO_HOME_SCREEN_FROM_TICKET_INFO)
    GoBack -> App.BackT $ pure App.GoBack
