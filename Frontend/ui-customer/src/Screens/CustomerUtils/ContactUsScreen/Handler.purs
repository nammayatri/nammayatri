{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ContactUsScreen.Handler where

import Prelude (bind, ($), pure , (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.ContactUsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.ContactUsScreen.View as ContactUsScreen
import Domain.Cache (getAppConfigFromCache)
import Types.App (FlowBT, GlobalState(..),CONTACT_US_SCREEN_OUTPUT(..))


contactUsScreen ::FlowBT String CONTACT_US_SCREEN_OUTPUT
contactUsScreen = do
  (GlobalState state) <- getState
  config <- getAppConfigFromCache
  act <- lift $ lift $ runScreen $ ContactUsScreen.screen state.contactUsScreen{data{config= config}}
  case act of
    GoBack ->  App.BackT $ pure App.GoBack 
    GoHome updatedState -> App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME_FROM_CONTACT updatedState)