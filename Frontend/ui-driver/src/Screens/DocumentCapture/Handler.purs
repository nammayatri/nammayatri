{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DocumentCaptureScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>))
import Screens.DocumentCaptureScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.DocumentCaptureScreen.View as DocumentCaptureScreen
import Types.App (FlowBT, GlobalState(..), DOCUMENT_CAPTURE_SCREEN_OUTPUT(..),ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import Screens.Types as ST

documentCaptureScreen :: FlowBT String DOCUMENT_CAPTURE_SCREEN_OUTPUT
documentCaptureScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ DocumentCaptureScreen.screen state.documentCaptureScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack 
    UploadAPI updatedState -> do
      modifyScreenState $ DocumentCaptureScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ UPLOAD_DOC_API updatedState $ transformImageType updatedState.data.docType)
    LogoutAccount -> App.BackT $ App.NoBack <$> pure LOGOUT_FROM_DOC_CAPTURE

transformImageType :: ST.RegisterationStep -> String
transformImageType docType = 
  case docType of
    ST.DRIVING_LICENSE_OPTION -> "DriverLicense"
    ST.VEHICLE_DETAILS_OPTION -> "VehicleRegistrationCertificate"
    ST.VEHICLE_PERMIT -> "RCPermit"
    ST.FITNESS_CERTIFICATE -> "RCFitnessCertficate"
    ST.VEHICLE_INSURANCE -> "RCInsurance"
    ST.VEHICLE_PUC -> "RCPUC"
    _ -> ""