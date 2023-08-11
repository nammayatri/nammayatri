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