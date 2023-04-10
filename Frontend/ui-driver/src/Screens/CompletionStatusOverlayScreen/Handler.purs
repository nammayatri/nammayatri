module Screens.CompletionStatusOverlayScreen.Handler where

import Prelude (bind, pure, ($), (<$>), unit, Unit)
import Engineering.Helpers.BackTrack (getState)
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow(runScreenWithNameSpace, initUIWithNameSpace)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Control.Monad.Except.Trans (lift)
import Effect.Class (liftEffect)
import Presto.Core.Types.Language.Flow (doAff)
import Types.App (FlowBT, GlobalState(..))
import Screens.CompletionStatusOverlayScreen.View as CompletionStatusOverlayScreen
import Screens.CompletionStatusOverlayScreen.Controller (ScreenOutput(..))
import Data.Maybe (Maybe(..))
import PrestoDOM.Core2 (terminateUI)

completionStatus:: FlowBT String Unit
completionStatus = do
  (GlobalState state) <- getState
  _ <- lift $ lift $ doAff $ liftEffect $ initUIWithNameSpace "CompletionStatus" Nothing 
  act <- lift $ lift $ runScreenWithNameSpace (CompletionStatusOverlayScreen.screen state.completionStatusOverlayScreen)
  _ <- lift $ lift $ doAff $ liftEffect $ terminateUI $ Just "CompletionStatus"
  pure unit
