{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.PopUpScreen.Handler where

import Prelude
import Engineering.Helpers.BackTrack (getState)
import Screens.PopUpScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen, runScreenWithNameSpace, initUIWithNameSpace)
import Presto.Core.Types.Language.Flow (doAff)
import Screens.PopUpScreen.View as PopUpScreen
import Types.App (GlobalState(..), FlowBT, POPUP_SCREEN_OUTPUT(..), ScreenType(..))
import Data.Maybe
import Screens.Types (PopUpScreenState)
import Types.ModifyScreenState (modifyScreenState)
import PrestoDOM.Core (terminateUI)
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
    RequestRide id extraFare -> App.BackT $ App.NoBack <$> (pure (POPUP_REQUEST_RIDE id extraFare))
