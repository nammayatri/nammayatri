{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.UploadAdhaarScreen.Handler where

import Prelude (bind, pure, ($), (<$>), discard)
import Engineering.Helpers.BackTrack (getState)
import Screens.UploadAdhaarScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.UploadAdhaarScreen.View as UploadAdhaarScreen
import Types.App (FlowBT, GlobalState(..), UPLOAD_ADHAAR_CARD_SCREENOUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)

uploadAdhaar :: FlowBT String UPLOAD_ADHAAR_CARD_SCREENOUTPUT
uploadAdhaar = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ UploadAdhaarScreen.screen state.uploadAdhaarScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    GoToBankDetails updatedState -> do
      modifyScreenState $ UploadAdhaarScreenStateType (\uploadAdhaarScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_ADD_BANK_DETAILS)
