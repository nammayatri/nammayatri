module Screens.OnBoardingSubscriptionScreen.Handler where

import Prelude
import Engineering.Helpers.BackTrack (getState)
import Control.Monad.Except.Trans (lift)
import PrestoDOM.Core.Types.Language.Flow (runLoggableScreen)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Screens.OnBoardingSubscriptionScreen.View as OnBoardingSubscriptionScreen
import Screens.OnBoardingSubscriptionScreen.Controller (ScreenOutput(..))
import Types.App (FlowBT, GlobalState(..), ONBOARDING_SUBSCRIPTION_SCREENOUTPUT(..),  ScreenType(..))
import Presto.Core.Types.Language.Flow (getLogFields)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)
import Types.ModifyScreenState (modifyScreenState)
import Helpers.SplashUtils as HSP
import Engineering.Helpers.Commons as EHC
import Effect.Aff
import Data.Either (Either(..))
import Presto.Core.Types.Language.Flow (fork)


onBoardingSubscriptionScreen :: FlowBT String ONBOARDING_SUBSCRIPTION_SCREENOUTPUT
onBoardingSubscriptionScreen = do
    (GlobalState state) <- getState
    logField_ <- lift $ lift $ getLogFields
    HSP.hideLoaderFlow
    action <- lift $ lift $ runLoggableScreen $ OnBoardingSubscriptionScreen.screen state.onBoardingSubscriptionScreen
    case action of 
        GoBack -> App.BackT $ pure App.GoBack
        StartFreeTrialExit updatedState -> do
            modifyScreenState $ OnBoardingSubscriptionScreenStateType (\_ -> updatedState)
            App.BackT $ App.NoBack <$> (pure $ MAKE_PAYMENT_FROM_ONBOARDING updatedState)
        GoToRegisterationScreen updatedState -> do
            modifyScreenState $ OnBoardingSubscriptionScreenStateType (\_ -> updatedState)
            App.BackT $ App.NoBack <$> (pure $ REGISTERATION_ONBOARDING updatedState)