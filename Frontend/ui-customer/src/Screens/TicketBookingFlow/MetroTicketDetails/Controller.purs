module Screens.TicketBookingFlow.MetroTicketDetails.Controller where

import Log 
import Prelude 
import PrestoDOM (Eval, continue, continueWithCmd)
import Screens 
import Screens.Types 
import Helpers.Utils 
import Effect.Uncurried 
import Effect.Unsafe 
import Screens.Types 
import Common.Types.App as Common
import Language.Strings
import Language.Types
import PrestoDOM.Types.Core (class Loggable)
import Data.Array
import Data.Maybe
import Engineering.Helpers.Commons
import JBridge
import Screens.TicketBookingFlow.MetroTicketDetails.ComponentConfig

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    _ -> pure unit
    
data Action = NoAction
            | BackPressed 
            | ShareTicketClick
            | ViewPaymentInfoClick
            | StopsBtnClick Int
            | PrevTicketClick 
            | NextTicketClick

data ScreenOutput = NoOutput


eval :: Action -> MetroTicketDetailsScreenState -> Eval Action ScreenOutput MetroTicketDetailsScreenState

eval BackPressed state = 
  if state.props.stage == MetroMapStage || state.props.stage == MetroRouteDetailsStage then 
    continue
      state {
        props {
          stage = MetroTicketDetailsStage
        }
      }
  else
    continue state

eval ShareTicketClick state = do
  void $ pure $ shareImageMessage "" getShareTicketConfig
  continue state

eval ViewPaymentInfoClick state = 
  continue
    state {
      props {
        stage = MetroRouteDetailsStage
      }
    }

eval (StopsBtnClick index) state = do
  let routeAtIndex = state.data.metroRoute !! index
  case routeAtIndex of 
    Just routeObj -> do 
      let updatedRouteObj = routeObj {
                              listExpanded = not routeObj.listExpanded
                            }
          updatedRouteArray = updateAt index updatedRouteObj state.data.metroRoute
      case updatedRouteArray of 
        Just routeArray -> do 
          let updatedState = state {
                                data {
                                  metroRoute = routeArray
                                }
                              }
          continue updatedState
        Nothing -> continue state
    Nothing -> continue state

eval PrevTicketClick state = do 
  if state.props.currentTicketIndex - 1 >= 0 then do
    let updatedState = state {
                        props {
                          currentTicketIndex = state.props.currentTicketIndex - 1
                        }
                      }
        currentTicket = updatedState.data.ticketsInfo !! updatedState.props.currentTicketIndex 
        qrString = case currentTicket of 
                    Just ticket -> ticket.qrString
                    Nothing -> ""
    continueWithCmd updatedState [ do 
      runEffectFn4 generateQR qrString (getNewIDWithTag "metro_ticket_qr_code" ) 218 1
      pure NoAction
    ]
  else 
    continue state


eval NextTicketClick state = do 
  let size = length state.data.ticketsInfo 
  if state.props.currentTicketIndex + 1 <= size - 1 then do
    let updatedState = state {
                        props {
                          currentTicketIndex = state.props.currentTicketIndex + 1
                        }
                      }
        currentTicket = updatedState.data.ticketsInfo !! updatedState.props.currentTicketIndex 
        qrString = case currentTicket of 
                    Just ticket -> ticket.qrString
                    Nothing -> ""
    continueWithCmd updatedState [ do 
      runEffectFn4 generateQR qrString (getNewIDWithTag "metro_ticket_qr_code" ) 218 1
      pure NoAction
    ]
  else 
    continue state

eval _ state = continue state


















-- module Screens.TicketBookingFlow.MetroTicketDetails.Controller where

-- import Log 
-- import Prelude 
-- import PrestoDOM (Eval, continue)
-- import Screens 
-- import Screens.Types 
-- import Helpers.Utils 
-- import Effect.Uncurried 
-- import Effect.Unsafe 
-- import Screens.Types 
-- import Common.Types.App as Common
-- import Language.Strings
-- import Language.Types
-- import PrestoDOM.Types.Core (class Loggable)


-- instance showAction :: Show Action where
--   show _ = ""

-- instance loggableAction :: Loggable Action where
--   performLog action appId  = case action of
--     _ -> pure unit
    
-- data Action = NoAction
--             | BackPressed 
--             | ShareTicketClick

-- data ScreenOutput = NoOutput


-- eval :: Action -> MetroTicketDetailsScreenState -> Eval Action ScreenOutput MetroTicketDetailsScreenState

-- eval _ state = continue state