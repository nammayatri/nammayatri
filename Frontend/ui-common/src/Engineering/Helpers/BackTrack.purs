{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Engineering.Helpers.BackTrack where

import Prelude

import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import Effect.Class (liftEffect)
import Presto.Core.Flow (doAff)
import Presto.Core.Types.Language.Flow as Flow
import Common.Types.App  (FlowBT)


modifyState :: forall st. (st → st ) -> FlowBT String st Unit
modifyState a = void $ lift $ lift $ Flow.modifyState a

getState :: forall st. FlowBT String st st
getState = lift $ lift $ Flow.getState

-- toggleLoader :: Boolean -> FlowBT String Unit
-- toggleLoader flag = void $ lift $ lift $ JBridge.toggleLoader flag

-- loaderText :: String -> String -> FlowBT String Unit
-- loaderText mainTxt subTxt = void $ lift $ lift $ JBridge.loaderText mainTxt subTxt

-- setFCMToken ::  String -> FlowBT String Unit
-- setFCMToken str = lift $ lift $ pure $ JBridge.setFCMToken str

-- getTime :: FlowBT String String
-- getTime = lift $ lift $ Utils.getTime

liftFlowBT :: forall e val st. Effect val -> FlowBT e st val
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



