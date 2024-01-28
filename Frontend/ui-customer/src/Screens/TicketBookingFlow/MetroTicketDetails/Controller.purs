module Screens.TicketBookingFlow.MetroTicketDetails.Controller where

import Log 
import Prelude 
import PrestoDOM (Eval, continue, continueWithCmd, exit)
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
            | TicketQRRendered String String

data ScreenOutput = NoOutput | GoBack | BackToSearchMetroLocation


eval :: Action -> MetroTicketDetailsScreenState -> Eval Action ScreenOutput MetroTicketDetailsScreenState

eval BackPressed state = 
  if (state.props.stage == MetroMapStage || state.props.stage == MetroRouteDetailsStage) && state.props.previousScreenStage == MetroMyTicketsStage then 
    continue
      state {
        props {
          stage = MetroTicketDetailsStage
        }
      }
  else if state.props.previousScreenStage == SearchMetroLocationStage then exit BackToSearchMetroLocation
  else
  exit GoBack

eval ShareTicketClick state = do
  _ <- pure $ shareImageMessage "Here is metro ticket!" (shareImageMessageConfig "")
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
      pure $ (TicketQRRendered (getNewIDWithTag "metro_ticket_qr_code") qrString)
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
      pure $ (TicketQRRendered (getNewIDWithTag "metro_ticket_qr_code") qrString)
    ]
  else 
    continue state

eval (TicketQRRendered id text) state  = 
  continueWithCmd state [ do
    runEffectFn4 generateQR text id 200 0
    pure $ NoAction
  ]

eval _ state = continue state


shareImageMessageConfig :: String ->  Common.ShareImageConfig 
shareImageMessageConfig _ = {
    viewId : getNewIDWithTag "MetroTicketView"
  , code : ""
  , logoId : getNewIDWithTag "metro_ticket_qr_code"
  , isReferral : false
}