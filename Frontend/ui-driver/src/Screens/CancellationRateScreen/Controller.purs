module Screens.CancellationRateScreen.Controller where

import Prelude (class Show, pure, unit, discard, bind, (>=), (==), (||), ($), (&&), (+), (<>), (-), show)
import PrestoDOM (Eval, update, exit, continue, updateAndExit)
import Screens.Types (CancellationRateScreenState)
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Components.PopUpModal.Controller as PopUpModal
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Data.String (length)
import JBridge (toast)
import Storage (KeyStore(..), setValueToLocalStore, getValueToLocalStore)
import Data.Array (elem)

data ScreenOutput = GoBack

instance showAction :: Show Action where
  show (BackPressed) = "BackPressed"
  show (NoAction) = "NoAction"
  show (AfterRender) = "AfterRender"


data Action = BackPressed | NoAction | AfterRender

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

eval :: Action -> CancellationRateScreenState -> Eval Action ScreenOutput CancellationRateScreenState
eval AfterRender state = continue state
eval BackPressed _state = exit GoBack
eval NoAction state = continue state
