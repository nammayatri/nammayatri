{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SplashScreen.Handler where

import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Prelude (Unit, bind, discard, void, ($))
import PrestoDOM.Core.Types.Language.Flow (initUIWithNameSpace, showScreenWithNameSpace, runScreen, runScreenWithNameSpace)
import Screens.SplashScreen.View as SplashScreen
import Types.App (GlobalState(..), FlowBT)
import Presto.Core.Types.Language.Flow
import Engineering.Helpers.Commons
import JBridge as JB
import Engineering.Helpers.Events as EHE
import Helpers.PrestoUtils

splashScreen :: FlowBT String Unit
splashScreen = do
  (GlobalState globalState) <- getState
  void $ liftFlowBT $ initUIWithNameSpace "SplashScreen" (getFragmentView "")
  let _ = EHE.addEvent (EHE.defaultEventObject "splash_usp_screen_loaded") { module = "onboarding"}
  void $ lift $ lift $ runScreen $ SplashScreen.screen globalState.splashScreen
  liftFlowBT $ JB.hideLoader ""