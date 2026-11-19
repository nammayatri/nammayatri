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
import PrestoDOM (Eval, update, continue, exit, updateAndExit, continueWithCmd, continueWithCmd)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types 
import Helpers.Utils (getDateAfterNDaysv2, getCurrentDatev2)
import Engineering.Helpers.Utils(compareDate)
import Effect.Uncurried (runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Screens.Types
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried(runEffectFn4)
import Debug (spy)
import Helpers.Utils (generateQR)
import Data.Array (length, (:), foldl, mapWithIndex, head, (!!), filter, elem, groupBy, find)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Engineering.Helpers.Commons(convertUTCTimeToISTTimeinHHMMSS, getCurrentUTC, convertUTCtoISC, getNewIDWithTag, flowRunner, liftFlow)
import Resources.Constants
import Services.API (TicketPlaceResp(..), TicketServicesResponse(..), BusinessHoursResp(..), TicketServiceResp(..), PeopleCategoriesResp(..), BookingStatus(..), PeopleCategoriesResp(..), TicketCategoriesResp(..), PlaceType(..))
import Data.Int (ceil)
import Common.Types.App as Common
import Screens.TicketBookingFlow.MetroTicketStatus.ScreenData as MetroTicketBookingScreenData
import Data.Function.Uncurried as Uncurried
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import JBridge as JB
import Services.API
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.Types as ST
import Screens.TicketBookingFlow.MetroTicketStatus.Transformer
import Storage
import Timers (clearTimerWithId)
import PrestoDOM.Core (getPushFn)
import Effect.Aff (launchAff)
import Types.App
import Presto.Core.Types.Language.Flow ( delay)
import Data.Time.Duration (Milliseconds(..))
import Domain.Payments as PP

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
            | MetroPaymentStatusAction FRFSTicketBookingStatusAPIRes
            | ViewTicketBtnOnClick PrimaryButton.Action
            | CountDown Int String String

data ScreenOutput = GoToHomeScreen
                  | GoToMetroTicketDetails MetroTicketStatusScreenState FRFSTicketBookingStatusAPIRes
                  | RefreshPaymentStatus MetroTicketStatusScreenState
                  | GoToTryAgainPayment MetroTicketStatusScreenState
                  | GoToMyMetroTicketsScreen
                  | GoToBusTicketBookingScreen

eval :: Action -> MetroTicketStatusScreenState -> Eval Action ScreenOutput MetroTicketStatusScreenState

eval (BackPressed) state = 
  case state.props.entryPoint of 
    HomescreenToMetroTicketStatus -> exit GoToHomeScreen 
    MyMetroTicketsToMetroTicketStatus -> exit GoToMyMetroTicketsScreen
    BusTicketToMetroTicketStatus -> exit GoToBusTicketBookingScreen

eval (MetroPaymentStatusAction (FRFSTicketBookingStatusAPIRes metroTicketBookingStatus)) state = do
  case metroTicketBookingStatus.status of 
    "CONFIRMED" -> do
      void $ pure $ setValueToLocalStore METRO_PAYMENT_STATUS_POOLING "false"
      continueWithCmd state{ props{paymentStatus = PP.Success, showShimmer = false}, data {resp = (FRFSTicketBookingStatusAPIRes metroTicketBookingStatus)}} [do
            push <- getPushFn Nothing "MetroTicketStatusScreen"
            void $ launchAff $ flowRunner defaultGlobalState $ do
              void $ delay $ Milliseconds 4000.0 
              liftFlow $ push $ ViewTicketBtnOnClick PrimaryButton.OnClick 
            pure NoAction]
    "FAILED" -> do
      void $ pure $ setValueToLocalStore METRO_PAYMENT_STATUS_POOLING "false"
      continue state{ props{paymentStatus = PP.Failed, showShimmer = false}, data {resp = (FRFSTicketBookingStatusAPIRes metroTicketBookingStatus)}}
    "EXPIRED" -> do
      void $ pure $ setValueToLocalStore METRO_PAYMENT_STATUS_POOLING "false"
      continue state{ props{paymentStatus = PP.Failed, showShimmer = false}, data {resp = (FRFSTicketBookingStatusAPIRes metroTicketBookingStatus)}}
    "PAYMENT_PENDING" -> 
      continue $ metroTicketStatusTransformer ( FRFSTicketBookingStatusAPIRes metroTicketBookingStatus) state
    _ -> continue state

eval (RefreshStatusAC PrimaryButton.OnClick) state = exit $ RefreshPaymentStatus state

eval (Copy text) state = 
  continueWithCmd state [ do 
    void $ pure $ JB.copyToClipboard text
    void $ pure $ EHU.showToast (getString COPIED)
    pure NoAction
  ]

eval (ViewTicketBtnOnClick PrimaryButton.OnClick) state = 
  case state.props.paymentStatus of 
    PP.Success -> updateAndExit state $ GoToMetroTicketDetails state state.data.resp
    PP.Failed -> exit $ GoToTryAgainPayment state
    _ -> continue state

eval (AfterRender) state = continue state{
  props{
    showShimmer = false
  }
}

eval (CountDown seconds status timerID) state = do
  if status == "EXPIRED" then do
    void $ pure $ clearTimerWithId state.data.timerId
    continueWithCmd state { data { timerId = ""} } [pure (ViewTicketBtnOnClick PrimaryButton.OnClick)]
    else do
      continue $ state { data { timerId = timerID } }

eval _ state = update state

