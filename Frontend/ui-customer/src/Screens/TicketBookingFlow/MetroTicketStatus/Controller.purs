{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingFlow.MetroTicketStatus.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude 
import PrestoDOM (Eval, continue, exit, updateAndExit, continueWithCmd, continueWithCmd)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types 
import Helpers.Utils (getDateAfterNDaysv2, compareDate, getCurrentDatev2)
import Effect.Uncurried (runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Screens.Types
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried(runEffectFn4)
import Debug (spy)
import Helpers.Utils (generateQR)
import Data.Array (length, (:), foldl, mapWithIndex, head, (!!), filter, elem, groupBy, find)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Engineering.Helpers.Commons(convertUTCTimeToISTTimeinHHMMSS, getCurrentUTC, convertUTCtoISC, getNewIDWithTag)
import Resources.Constants
import Services.API (TicketPlaceResp(..), TicketServicesResponse(..), BusinessHoursResp(..), TicketServiceResp(..), PeopleCategoriesResp(..), BookingStatus(..), PeopleCategoriesResp(..), TicketCategoriesResp(..), PlaceType(..))
import Data.Int (ceil)
import Common.Types.App as Common
import Screens.TicketBookingFlow.MetroTicketStatus.ScreenData as MetroTicketBookingScreenData
import Data.Function.Uncurried as Uncurried
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import Services.API
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.Types as ST
import Screens.TicketBookingFlow.MetroTicketStatus.Transformer
import Storage

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen TICKET_BOOKING_SCREEN)
    _ -> pure unit

data Action = BackPressed
            | AfterRender
            | Copy String
            | NoAction
            | RefreshStatusAC PrimaryButton.Action
            | MetroPaymentStatusAction MetroTicketBookingStatus
            | ViewTicketBtnOnClick PrimaryButton.Action

data ScreenOutput = NoOutput 
                  | GoBack
                  | GoToMetroTicketDetails MetroTicketStatusScreenState MetroTicketBookingStatus
                  | RefreshPaymentStatus MetroTicketStatusScreenState
                  | GoToTryAgainPayment MetroTicketStatusScreenState

eval :: Action -> MetroTicketStatusScreenState -> Eval Action ScreenOutput MetroTicketStatusScreenState

eval (BackPressed) state = exit GoBack --continue state -- Handle Back Press

eval (MetroPaymentStatusAction (MetroTicketBookingStatus metroTicketBookingStatus)) state =
  case metroTicketBookingStatus.status of 
    "CONFIRMED" -> do
      void $ pure $ setValueToLocalStore METRO_PAYMENT_STATUS_POOLING "false"
      continue state{ props{paymentStatus = Common.Success, showShimmer = false}, data {resp = (MetroTicketBookingStatus metroTicketBookingStatus)}}
    "FAILED" -> 
      continue 
        state{
          props{
            paymentStatus = Common.Failed
          , showShimmer = false
          }
        }
    _ -> continue state

eval (RefreshStatusAC PrimaryButton.OnClick) state = exit $ RefreshPaymentStatus state

eval (Copy text) state = 
  continueWithCmd state [ do 
    void $ pure $ JB.copyToClipboard text
    void $ pure $ JB.toast (getString COPIED)
    pure NoAction
  ]

eval (ViewTicketBtnOnClick PrimaryButton.OnClick) state = 
  case state.props.paymentStatus of 
    Common.Success -> updateAndExit state $ GoToMetroTicketDetails state state.data.resp
    Common.Failed -> exit $ GoToTryAgainPayment state
    _ -> continue state

eval (AfterRender) state = continue state{
  props{
    showShimmer = false
  }
}


eval _ state = continue state

