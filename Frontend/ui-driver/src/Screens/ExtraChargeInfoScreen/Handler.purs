module Screens.ExtraChargeInfoScreen.Handler where

import Prelude
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (liftFlow)
import PrestoDOM.Core.Types.Language.Flow (showScreen)
import Presto.Core.Types.Language.Flow (Flow, doAff , getLogFields)
import Presto.Core.Types.Language.Flow (getState) as Flow
import Screens.ExtraChargeInfoScreen.Controller (ScreenOutput(..))
import Types.App (FlowBT, GlobalState(..), EXTRA_CHARGE_INFO_SCREEN(..))
import PrestoDOM.Core.Types.Language.Flow (initUIWithNameSpace, showScreenWithNameSpace, runScreenWithNameSpace)
import Data.Maybe (Maybe(..))
import PrestoDOM.Core (terminateUI)
import Effect.Class (liftEffect)
import Screens.ExtraChargeInfoScreen.View as ExtraChargeInfoScreen
import Debug
import JBridge
import PrestoDOM.Core.Types.Language.Flow (runScreen)


extraChargeInfoScreen :: FlowBT String Unit
extraChargeInfoScreen = do
    (GlobalState state) <- getState
    act <- lift $ lift $ runScreen $ ExtraChargeInfoScreen.screen state.extraChargeInfoScreen
    let _ = releaseYoutubeView unit
    pure unit
