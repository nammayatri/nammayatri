module Screens.DriverCompleteProfileScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.DriverCompleteProfileScreen.Controller (ScreenOutput(..))
import Screens.DriverCompleteProfileScreen.View as DriverCompleteProfileScreen
import Types.App (DRIVER_COMPLETE_PROFILE_SCREEN_OUTPUT(..), FlowBT, GlobalState(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import Services.Backend as Remote 
import Debug (spy)

driverCompleteProfileScreen :: FlowBT String DRIVER_COMPLETE_PROFILE_SCREEN_OUTPUT
driverCompleteProfileScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ DriverCompleteProfileScreen.screen state.driverCompleteProfileScreen (GlobalState state)
  case action of
    SubmitRes updatedState -> do
      _ <- Remote.submitDriverProfile $ Remote.makeDriverProfileReq updatedState.data.pledge updatedState.data.vehicalOffer updatedState.data.whyNy updatedState.data.drivingSince updatedState.data.addImagesState.imageMediaIds
      App.BackT $ pure App.GoBack

    GoToProfile _ -> App.BackT $ pure App.GoBack
    
    _ -> App.BackT $ pure App.GoBack