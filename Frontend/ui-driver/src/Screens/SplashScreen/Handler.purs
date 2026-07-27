{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SplashScreen.Handler where

import Prelude
import Control.Monad.Except.Trans (lift)
import PrestoDOM.Core.Types.Language.Flow (initUIWithScreen, initUIWithNameSpace, runScreenWithNameSpace, runLoggableScreen)
import Screens.SplashScreen.View as SplashScreen
import Engineering.Helpers.Commons (liftFlow)
import Engineering.Helpers.BackTrack as EHB
import Types.App as TA
import Presto.Core.Flow as PCF
import Data.Maybe
import Helpers.Utils as HU
import DecodeUtil as DU

splashScreen :: TA.FlowBT String Unit
splashScreen = do
  (TA.GlobalState globalState) <- EHB.getState
  void $ lift $ lift $ runLoggableScreen $ SplashScreen.screen globalState.splashScreen
  EHB.liftFlowBT HU.hideSplash
