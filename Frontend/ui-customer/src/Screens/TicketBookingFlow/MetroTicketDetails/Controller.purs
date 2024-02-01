{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

data ScreenOutput = NoOutput | GoBack | BackToSearchMetroLocation | GoToHome


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
  else if state.props.previousScreenStage == MetroTicketStatusStage then exit GoToHome
  else
  exit GoBack

eval ShareTicketClick state = do
  _ <- pure $ shareImageMessage (getString HERE_IS_METRO_TICKET) (shareImageMessageConfig "")
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
    viewId : getNewIDWithTag "metro_ticket_details_view"
  , code : ""
  , logoId : getNewIDWithTag "metro_ticket_qr_code"
  , isReferral : false
}