{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AppUpdatePopUpScreen.Handler where

import Prelude (bind, pure, ($), (<$>), unit, Unit, discard, void, (==))
import Presto.Core.Types.Language.Flow (doAff)
import Screens.AppUpdatePopUpScreen.Controller as CD
import Screens.AppUpdatePopUpScreen.View as AppUpdatePopUpScreen
import PrestoDOM.Core.Types.Language.Flow(showScreenWithNameSpace, initUIWithNameSpace)
import Types.App (FlowBT, GlobalState(..), APP_UPDATE_POPUP(..))
import Control.Monad.Except.Trans (lift)
import Effect.Class (liftEffect)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Data.Maybe (Maybe(..))
import PrestoDOM.Core (terminateUI)
import Common.Types.App (LazyCheck(..))
import Engineering.Helpers.BackTrack (liftFlowBT)
import ConfigProvider (getAppConfig, appConfig)
import JBridge as JB
import Screens.Types as ST
import Helpers.PrestoUtils


handleAppUpdatePopUp :: FlowBT String APP_UPDATE_POPUP
handleAppUpdatePopUp = do
  (GlobalState state) ← getState
  _ <- lift $ lift $ doAff $ liftEffect $ initUIWithNameSpace "AppUpdatePopUpScreen" (getFragmentView "")
  act <- lift $ lift $ showScreenWithNameSpace ( AppUpdatePopUpScreen.screen state.appUpdatePopUpScreen)
  _ <- lift $ lift $ doAff $ liftEffect $ terminateUI $ Just "AppUpdatePopUpScreen"
  case act of
    CD.Accept -> do
      if state.appUpdatePopUpScreen.appUpdatedView.popupFlowType == ST.REG_PROF_PAN_AADHAAR then void $ liftFlowBT $ JB.openUrlInApp (getAppConfig appConfig).appUpdatePopupUrl else pure unit
      App.BackT $ App.NoBack <$> pure UpdateNow
    CD.Decline -> App.BackT $ App.BackPoint <$> pure Later
    CD.Exit -> pure Later
  
