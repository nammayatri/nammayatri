module Engineering.Helpers.BackTrack where

import Prelude

import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import Effect.Class (liftEffect)
-- import JBridge as JBridge
import Presto.Core.Flow (doAff)
import Presto.Core.Types.Language.Flow as Flow
import Types.App  (FlowBT, GlobalState)


modifyState :: (GlobalState → GlobalState) -> FlowBT String Unit
modifyState a = void $ lift $ lift $ Flow.modifyState a

getState :: FlowBT String GlobalState
getState = lift $ lift $ Flow.getState

-- toggleLoader :: Boolean -> FlowBT String Unit
-- toggleLoader flag = void $ lift $ lift $ JBridge.toggleLoader flag

-- loaderText :: String -> String -> FlowBT String Unit
-- loaderText mainTxt subTxt = void $ lift $ lift $ JBridge.loaderText mainTxt subTxt

-- setFCMToken ::  String -> FlowBT String Unit
-- setFCMToken str = lift $ lift $ pure $ JBridge.setFCMToken str

-- getTime :: FlowBT String String
-- getTime = lift $ lift $ Utils.getTime

liftFlowBT :: forall val . (Effect val)  -> FlowBT String val
liftFlowBT effVal = lift $ lift $ doAff do liftEffect (effVal)

-- getCurrentLatLong :: FlowBT String JBridge.LocationLatLong
-- getCurrentLatLong = lift $ lift $ JBridge.getCurrentLatLong

-- pure :: forall a. a -> FlowBT String a
-- pure a = BackT (ExceptT "" (Free (FlowWrapper GlobalState))) a

-- -- runScreen :: forall action state s st. (Screen action state s) -> Flow st s
-- -- type FlowBT e a = BackT (ExceptT e (Free (FlowWrapper GlobalState))) a
-- -- type Flow st a = Free (FlowWrapper st) a

-- runScreen :: forall action state s st. (Screen action state s) -> FlowBT String s
-- runScreen screen = lift $ lift $ Flow.runScreen screen

-- initUIWithScreen
-- doAff
-- callApi
-- delay
-- T.trackAPICalls



