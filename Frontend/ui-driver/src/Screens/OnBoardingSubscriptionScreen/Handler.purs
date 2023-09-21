module Screens.OnBoardingSubscriptionScreen.Handler where

import Prelude
import Engineering.Helpers.BackTrack (getState)
import Control.Monad.Except.Trans (lift)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Screens.OnBoardingSubscriptionScreen.View as OnBoardingSubscriptionScreen
import Screens.OnBoardingSubscriptionScreen.Controller (ScreenOutput(..))
import Types.App (FlowBT, GlobalState(..), ONBOARDING_SUBSCRIPTION_SCREENOUTPUT(..),  ScreenType(..))
import Presto.Core.Types.Language.Flow (getLogFields)
import Helpers.Utils (hideSplash)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)
import Types.ModifyScreenState (modifyScreenState)


onBoardingSubscriptionScreen :: FlowBT String ONBOARDING_SUBSCRIPTION_SCREENOUTPUT
onBoardingSubscriptionScreen = do
    (GlobalState state) <- getState
    logField_ <- lift $ lift $ getLogFields
    lift $ lift $ doAff do liftEffect hideSplash 
    action <- lift $ lift $ runScreen $ OnBoardingSubscriptionScreen.screen state.onBoardingSubscriptionScreen
    case action of 
        GoBack -> App.BackT $ pure App.GoBack
        GoToHome -> App.BackT $ App.NoBack <$> pure GOTO_HOME_SCREEN_FROM_ONBOARDING_SUBSCRIPTION_SCREEN
        StartFreeTrialExit updatedState -> do
            modifyScreenState $ OnBoardingSubscriptionScreenStateType (\_ -> updatedState)
            App.BackT $ App.NoBack <$> (pure $ MAKE_PAYMENT_FROM_ONBOARDING updatedState)