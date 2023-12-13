{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.EnterMobileNumberScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import ModifyScreenState (modifyScreenState)
import Prelude (bind, discard, ($), pure, (<$>))
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.EnterMobileNumberScreen.Controller (ScreenOutput(..))
import Screens.EnterMobileNumberScreen.View as EnterMobileNumberScreen
import Types.App (GlobalState(..), FlowBT, ScreenType(..), defaultGlobalState)
import Presto.Core.Types.Language.Flow (getLogFields)

enterMobileNumberScreen :: FlowBT String ScreenOutput
enterMobileNumberScreen = do
  (GlobalState state') <- getState
  let
    (GlobalState defaultGlobalState') = defaultGlobalState
  logField_ <- lift $ lift $ getLogFields
  act <- lift $ lift $ runScreen $ EnterMobileNumberScreen.screen state'.enterMobileNumberScreen { data { logField = logField_ } }
  case act of
    GoToAccountSetUp state -> do
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> state)
      App.BackT $ App.NoBack <$> pure act
    GoToOTP state -> do
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> state)
      App.BackT $ App.BackPoint <$> pure act
    ResendOTP state -> do
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> state)
      App.BackT $ App.BackPoint <$> pure act
    GoBack state -> do
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> state { data { otp = "" }, props { wrongOTP = false } })
      App.BackT $ App.BackPoint <$> pure act
    GoToWelcomeScreen state -> do
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> defaultGlobalState'.enterMobileNumberScreen)
      App.BackT $ App.NoBack <$> pure act

-- REFERENCE TO UPDATE STATE GLOBALLY
-- modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen ->  state)
