module Screens.PermissionScreen.Handler where

import Prelude (bind, ($), pure, (<$>))
import Engineering.Helpers.BackTrack (getState)
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App 
import PrestoDOM.Core.Types.Language.Flow (runScreenWithNameSpace, initUIWithNameSpace)
import Screens.PermissionScreen.View as PermissionScreen
import Presto.Core.Types.Language.Flow (doAff)
import Data.Maybe
import PrestoDOM.Core2(terminateUI)
import Effect.Class (liftEffect)
import Screens.PermissionScreen.Controller (ScreenOutput(..))
import Types.App (FlowBT, GlobalState(..), PERMISSION_SCREEN_OUTPUT(..))

permissionScreen :: String -> FlowBT String PERMISSION_SCREEN_OUTPUT 
permissionScreen triggertype= do 
  (GlobalState state) <- getState
  _ <- lift $ lift $ doAff $ liftEffect $ initUIWithNameSpace "PermissionScreen" Nothing
  act <- lift $ lift $ runScreenWithNameSpace $ PermissionScreen.screen state.permissionScreen triggertype
  _ <- lift $ lift $ doAff $ liftEffect $ terminateUI $ Just "PermissionScreen"
  case act of
    GoBack -> App.BackT $ pure App.GoBack 
    Refresh -> App.BackT $ App.BackPoint <$> (pure REFRESH_INTERNET)
    LocationCallBack updatedState -> App.BackT $ App.NoBack <$> (pure TURN_ON_GPS)
    InternetCallBack updatedState -> App.BackT $ App.NoBack <$> (pure TURN_ON_INTERNET)
