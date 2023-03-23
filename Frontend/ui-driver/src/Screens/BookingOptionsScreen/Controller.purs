module Screens.BookingOptionsScreen.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, pure, unit, ($), discard)
import PrestoDOM (Eval, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (BookingOptionsScreenState)

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "BookingOptionsScreen"
    BackPressed -> pure unit

data Action = BackPressed
            | AfterRender

data ScreenOutput = GoBack | SelectCab BookingOptionsScreenState

eval :: Action -> BookingOptionsScreenState -> Eval Action ScreenOutput BookingOptionsScreenState
eval BackPressed state = exit GoBack
eval _ state = continue state