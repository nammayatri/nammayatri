module Screens.UploadParcelImageScreen.Handler where

import Prelude
import Engineering.Helpers.BackTrack
import Screens.UploadParcelImageScreen.Controller
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runLoggableScreen)
import Screens.UploadParcelImageScreen.View as UploadParcelImageScreen
import Types.App (FlowBT, GlobalState(..), UPLOAD_PARCEL_IMAGE_SCREEN_OUTPUT(..),ScreenType(..))
import Types.ModifyScreenState

uploadParcelImageScreen :: FlowBT String UPLOAD_PARCEL_IMAGE_SCREEN_OUTPUT
uploadParcelImageScreen = do
    (GlobalState state) <- getState
    action <- lift $ lift $ runLoggableScreen $ UploadParcelImageScreen.screen state.uploadParcelImageScreen
    case action of
        GoBack updatedState -> do
            modifyScreenState $ UploadParcelImageScreenStateType $ \uploadParcelImageScreen -> updatedState
            App.BackT $ App.NoBack <$> (pure $ GOTO_HOME_SCREEN)
        UploadImage updatedState -> do
            modifyScreenState $ UploadParcelImageScreenStateType (\uploadParcelImageScreen -> updatedState)
            App.BackT $ App.NoBack <$> (pure $ UPLOAD_IMAGE updatedState)