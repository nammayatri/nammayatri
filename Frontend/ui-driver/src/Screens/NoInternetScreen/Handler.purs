{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NoInternetScreen.Handler where

import Prelude
import Engineering.Helpers.BackTrack (getState)
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (showScreen, initUIWithNameSpace, showScreenWithNameSpace)
import Screens.NoInternetScreen.View as NoInternetScreen
import Data.Maybe
import Screens.NoInternetScreen.Controller (ScreenOutput(..))
import Types.App (FlowBT, GlobalState(..), NO_INTERNET_SCREEN_OUTPUT(..))
import Presto.Core.Types.Language.Flow (Flow)
import Engineering.Helpers.Commons as EHC
import PrestoDOM.Core (terminateUI)
import Presto.Core.Types.Language.Flow (getState) as Flow
import Engineering.Helpers.Utils as EHU
import DecodeUtil as DU
import JBridge as JB

noInternetScreen :: String -> FlowBT String NO_INTERNET_SCREEN_OUTPUT
noInternetScreen triggertype= do
  act <- lift $ lift $ showScreen $ NoInternetScreen.screen {} triggertype
  case act of
    GoBack -> App.BackT $ pure App.GoBack
    Refresh -> App.BackT $ App.BackPoint <$> (pure REFRESH_INTERNET)
    LocationCallBack updatedState -> App.BackT $ App.NoBack <$> (pure TURN_ON_GPS)
    InternetCallBack updatedState -> App.BackT $ App.NoBack <$> (pure CHECK_INTERNET)


noInternetScreen' :: forall st. Flow st Unit
noInternetScreen' = do 
  void $ EHC.liftFlow $ EHU.terminateLoader ""
  void $ EHC.liftFlow $ initUIWithNameSpace "NoInternetScreen" Nothing
  let 
    screen = NoInternetScreen.screen {} "INTERNET_ACTION"
    scopedScreen = { 
      initialState : screen.initialState
      , view : screen.view
      , name : screen.name  
      , globalEvents : screen.globalEvents
      , eval : screen.eval
      , parent : Just "NoInternetScreen"}
  void $ showScreenWithNameSpace scopedScreen 
  let _ = DU.setKeyInWindow "noInternetCount" 0
  EHC.liftFlow $ terminateUI $ Just "NoInternetScreen"
  EHC.liftFlow $ JB.triggerReloadApp "lazy"
