module Screens.PaymentHistoryScreen.Controller where

import Components.GenericHeader as GenericHeader
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, pure, unit, not, ($))
import PrestoDOM (Eval, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (PaymentHistoryScreenState)

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


data ScreenOutput = ViewPaymentDetails PaymentHistoryScreenState


eval :: Action -> PaymentHistoryScreenState -> Eval Action ScreenOutput PaymentHistoryScreenState

eval ItemClick state = exit $ ViewPaymentDetails state

eval ChangeTab state = continue state { props{ autoPayHistory = not state.props.autoPayHistory}}


eval _ state = continue state