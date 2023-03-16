{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RegistrationScreen.Handler where

import Prelude (bind, pure, ($), (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.RegistrationScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.RegistrationScreen.View as RegistrationScreen
import Types.App (FlowBT, GlobalState(..), REGISTRATION_SCREENOUTPUT(..))


registration :: FlowBT String REGISTRATION_SCREENOUTPUT
registration = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ RegistrationScreen.screen state.registrationScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    GoToUploadDriverLicense -> App.BackT $ App.BackPoint <$> (pure $ UPLOAD_DRIVER_LICENSE)