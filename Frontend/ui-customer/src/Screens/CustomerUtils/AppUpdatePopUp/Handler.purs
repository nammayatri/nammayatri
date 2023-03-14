module Screens.AppUpdatePopUp.Handler where

import Prelude (bind, pure, ($), (<$>))
import Presto.Core.Types.Language.Flow (doAff)
import Screens.AppUpdatePopUp.Controller as CD
import Screens.AppUpdatePopUp.View as AppUpdatePopUpScreen
import PrestoDOM.Core.Types.Language.Flow(runScreenWithNameSpace, initUIWithNameSpace)
import Types.App (FlowBT, GlobalState(..))
import Control.Monad.Except.Trans (lift)
import Effect.Class (liftEffect)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Data.Maybe (Maybe(..))
import PrestoDOM.Core2 (terminateUI)

handleAppUpdatePopUp :: FlowBT String String 
handleAppUpdatePopUp  = do
  (GlobalState state) ← getState
  _ <- lift $ lift $ doAff $ liftEffect $ initUIWithNameSpace "AppUpdatePopUpScreen" Nothing 
  act <- lift $ lift $ runScreenWithNameSpace ( AppUpdatePopUpScreen.screen state.appUpdatePopUpScreen)
  _ <- lift $ lift $ doAff $ liftEffect $ terminateUI $ Just "AppUpdatePopUpScreen"
  case act of
    CD.Decline -> App.BackT $ App.NoBack <$> pure "Decline"
    CD.Accept  -> App.BackT $ App.NoBack <$> pure "Accept"
