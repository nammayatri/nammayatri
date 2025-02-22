module Screens.DriverCompleteProfileScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard, (==), (+), not)
import PrestoDOM.Core.Types.Language.Flow (runLoggableScreen)
import Screens.DriverCompleteProfileScreen.Controller (ScreenOutput(..))
import Screens.DriverCompleteProfileScreen.View as DriverCompleteProfileScreen
import Types.App (DRIVER_COMPLETE_PROFILE_SCREEN_OUTPUT(..), FlowBT, GlobalState(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import Helpers.Utils (getValueBtwRange)
import Mobility.Prelude (boolToInt)
import Services.Backend as Remote 
import Debug (spy)
import Data.Maybe
import Data.Array (null)

driverCompleteProfileScreen :: FlowBT String DRIVER_COMPLETE_PROFILE_SCREEN_OUTPUT
driverCompleteProfileScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runLoggableScreen $ DriverCompleteProfileScreen.screen state.driverCompleteProfileScreen (GlobalState state)
  case action of
    SubmitRes updatedState -> do
      let completedModules = getValueBtwRange (boolToInt (not $ null updatedState.data.pledge) + boolToInt (not $ null updatedState.data.aspirations) + boolToInt (not $ isNothing updatedState.data.drivingSince) + boolToInt (not $ isNothing updatedState.data.homeTown) + boolToInt (not $ null updatedState.data.vehicalOffer) + boolToInt (not $ null updatedState.data.uploadedImagesIds) ) 0 6 0 4
      modifyScreenState $ DriverProfileScreenStateType (\driverDetails -> driverDetails { data { profileCompletedModules = completedModules}})
      _ <- Remote.uploadProfileReq $ Remote.makeDriverProfileReq (if updatedState.data.homeTown == Just "" then Nothing else updatedState.data.homeTown) updatedState.data.pledge updatedState.data.vehicalOffer updatedState.data.aspirations updatedState.data.drivingSince updatedState.data.addImagesState.imageMediaIds
      App.BackT $ pure App.GoBack

    GoToProfile _ -> App.BackT $ pure App.GoBack
    
    _ -> App.BackT $ pure App.GoBack