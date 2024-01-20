module Screens.TicketBookingFlow.MetroTicketDetails.Handler where

import Engineering.Helpers.BackTrack 
import Prelude 
import Screens.TicketBookingFlow.MetroTicketDetails.Controller 
import Control.Monad.Except.Trans 
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow
import ModifyScreenState 
import Screens.TicketBookingFlow.MetroTicketDetails.View as MetroTicketDetails
import Types.App 
import Screens.TicketBookingFlow.MetroTicketDetails.ScreenData as PlaceDetailsScreenData
import Screens.Types as ST


metroTicketDetailsScreen :: FlowBT String METRO_TICKET_DETAILS_SCREEN_OUTPUT
metroTicketDetailsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ MetroTicketDetails.screen state.metroTicketDetailsScreen
  case action of
    GoBack ->  App.BackT $ pure App.GoBack 
    _  -> App.BackT $ App.NoBack <$> (pure $ METRO_TICKET_DETAILS_SCREEN_OUTPUT_NO_OUTPUT)













-- module Screens.TicketBookingFlow.MetroTicketDetails.Handler where

-- import Engineering.Helpers.BackTrack 
-- import Prelude 
-- import Screens.TicketBookingFlow.MetroTicketDetails.Controller 
-- import Control.Monad.Except.Trans 
-- import Control.Transformers.Back.Trans as App
-- import PrestoDOM.Core.Types.Language.Flow
-- import ModifyScreenState 
-- import Screens.TicketBookingFlow.MetroTicketDetails.View as MetroTicketDetails
-- import Types.App 
-- import Screens.TicketBookingFlow.MetroTicketDetails.ScreenData as PlaceDetailsScreenData
-- import Screens.Types as ST


-- metroTicketDetailsScreen :: FlowBT String METRO_TICKET_DETAILS_SCREEN_OUTPUT
-- metroTicketDetailsScreen = do
--   (GlobalState state) <- getState
--   action <- lift $ lift $ runScreen $ MetroTicketDetails.screen state.metroTicketDetailsScreen
--   case action of
--     _  -> App.BackT $ App.NoBack <$> (pure $ METRO_TICKET_DETAILS_SCREEN_OUTPUT_NO_OUTPUT)

