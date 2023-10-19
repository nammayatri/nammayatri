module Calendar.Handler where

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Prelude (Unit, bind, discard, pure, unit, ($), (<>), show)
import Presto.Core.Flow (Flow)
import Presto.Core.Types.Language.Flow (doAff, getState)
import PrestoDOM.Core.Types.Language.Flow (initUIWithNameSpace, showScreenWithNameSpace, initUIWithScreen)
import Calendar.View as CalendarScreen
import Calendar.Controller(ScreenOutput(..))
import Types.App (GlobalState(..))
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)
import PrestoDOM.Core (terminateUI)
import Debug (spy)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App


calendarScreen :: Flow GlobalState ScreenOutput
calendarScreen  = do
  (GlobalState state) <- getState
  doAff $ liftEffect $ initUIWithNameSpace "Calendar" Nothing
  showScreenWithNameSpace ( CalendarScreen.screen state.calendarScreen)