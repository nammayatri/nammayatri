{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PaymentHistoryScreen.Controller where

import Components.GenericHeader as GenericHeader
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, pure, unit, not, ($), (==))
import PrestoDOM (Eval, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (PaymentHistoryScreenState, PaymentHistorySubview(..))

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "PaymentHistoryScreen"
    _ -> pure unit


data Action = BackPressed
            | AfterRender
            | NoAction
            | ItemClick
            | GenericHeaderAC GenericHeader.Action
            | ChangeTab
            | ViewRideDetails
            | ListItemClick


data ScreenOutput = ViewPaymentDetails PaymentHistoryScreenState | GoBack


eval :: Action -> PaymentHistoryScreenState -> Eval Action ScreenOutput PaymentHistoryScreenState

eval BackPressed state = if state.props.subView == TransactionDetails then continue state { props{ subView = PaymentHistory}}
                         else if state.props.subView == RideDetails then continue state { props{ subView = TransactionDetails}}
                         else exit $ GoBack

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick )) state = if state.props.subView == TransactionDetails then continue state { props{ subView = PaymentHistory}}
                         else if state.props.subView == RideDetails then continue state { props{ subView = TransactionDetails}}
                         else exit $ GoBack

eval ItemClick state = exit $ ViewPaymentDetails state

eval ChangeTab state = continue state { props{ autoPayHistory = not state.props.autoPayHistory}}

eval ListItemClick state = continue state { props{ subView = TransactionDetails}}

eval ViewRideDetails state = continue state { props{ subView = RideDetails}}

eval _ state = continue state