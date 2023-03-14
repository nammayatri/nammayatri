module Screens.SplashScreen.Handler where
import Prelude
import Control.Monad.Except.Trans (lift)
import PrestoDOM.Core.Types.Language.Flow (initUIWithScreen)
import Screens.Types (SplashScreenState)
import Screens.SplashScreen.View as SplashScreen
import Types.App (FlowBT)
import Engineering.Helpers.Commons (liftFlow)
import JBridge (initiateLocationServiceClient)

splashScreen :: SplashScreenState → FlowBT String Unit
splashScreen screenState = do
    _ <- lift $ lift $ liftFlow $ initiateLocationServiceClient
    lift $ lift $ initUIWithScreen $ SplashScreen.screen screenState