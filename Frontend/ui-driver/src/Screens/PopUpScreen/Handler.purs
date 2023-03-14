module Screens.PopUpScreen.Handler where

import Prelude
import Engineering.Helpers.BackTrack (getState)
import Screens.PopUpScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen, runScreenWithNameSpace,initUIWithNameSpace)
import Presto.Core.Types.Language.Flow (doAff)
import Screens.PopUpScreen.View as PopUpScreen
import Types.App (GlobalState(..), FlowBT, POPUP_SCREEN_OUTPUT(..), ScreenType(..))
import Data.Maybe
import Screens.Types (PopUpScreenState)
import Types.ModifyScreenState (modifyScreenState)
import PrestoDOM.Core2(terminateUI)
import Effect.Class (liftEffect)
import JBridge (deletePopUpCallBack)
import Engineering.Helpers.Commons (liftFlow)

popUpScreen :: FlowBT String POPUP_SCREEN_OUTPUT
popUpScreen = do
    (GlobalState state) <- getState
    _ <- lift $ lift $ doAff $ liftEffect $ initUIWithNameSpace "PopUpScreen" Nothing
    action <- lift $ lift $ runScreenWithNameSpace $ PopUpScreen.screen state.popUpScreen
    _ <- lift $ lift $ doAff $ liftEffect $ terminateUI $ Just "PopUpScreen"
    case action of 
        GoBack -> App.BackT $ pure App.GoBack
        RequestRide id extraFare -> App.BackT $ App.NoBack <$> ( pure (POPUP_REQUEST_RIDE id extraFare) )
