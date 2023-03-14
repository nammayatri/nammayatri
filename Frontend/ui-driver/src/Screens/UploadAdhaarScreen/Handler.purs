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