module Screens.Benefits.BenefitsScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, ($), pure, (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.Benefits.BenefitsScreen.Controller (ScreenOutput(..))
import Screens.Benefits.BenefitsScreen.View as BenefitsScreen
import Types.App (FlowBT, GlobalState(..), BENEFITS_SCREEN_OUTPUT(..), ScreenType(..), NAVIGATION_ACTIONS(..))
import Types.ModifyScreenState (modifyScreenState)
import Screens.Benefits.BenefitsScreen.ScreenData as BenefitsScreenData

benefitsScreen :: FlowBT String BENEFITS_SCREEN_OUTPUT
benefitsScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ BenefitsScreen.screen state.benefitsScreen
  case act of
    GoBack -> do
      modifyScreenState $ BenefitsScreenStateType (\benefitsScreen -> BenefitsScreenData.initData)
      App.BackT $ pure App.GoBack
    GoToHomeScreen updatedState -> do
      modifyScreenState $ BenefitsScreenStateType (\benefitsScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ DRIVER_REFERRAL_SCREEN_NAV HomeScreenNav)
    GoToNotifications updatedState -> do
      modifyScreenState $ BenefitsScreenStateType (\benefitsScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ DRIVER_REFERRAL_SCREEN_NAV GoToAlerts)
    SubscriptionScreen updatedState -> do
      modifyScreenState $ BenefitsScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ DRIVER_REFERRAL_SCREEN_NAV GoToSubscription)
    EarningsScreen updatedState -> do
      modifyScreenState $ BenefitsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ DRIVER_REFERRAL_SCREEN_NAV $ GoToEarningsScreen false)
    GoToDriverContestScreen updatedState -> do
      modifyScreenState $ BenefitsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ DRIVER_CONTEST_SCREEN)
    EarningsScreen updatedState -> do
      modifyScreenState $ BenefitsScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ DRIVER_REFERRAL_SCREEN_NAV $ GoToEarningsScreen false )
    GoToLmsVideoScreen updatedState -> do
      modifyScreenState $ BenefitsScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure  $ GO_TO_LMS_VIDEO_SCREEN updatedState)
    GoToCustomerReferralTrackerScreen openPP updatedState -> do  
      modifyScreenState $ BenefitsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ CUSTOMER_REFERRAL_TRACKER_NAV openPP )
    GoToRegistrationScreen updatedState -> do
      modifyScreenState $ BenefitsScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_REGISTRATION_SCREEN updatedState)
    SelectLang updatedState -> do
      modifyScreenState $ BenefitsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ SELECT_LANG_FROM_BENEFITS_SCREEN updatedState)
    LogoutAccount -> App.BackT $ App.NoBack <$> pure LOGOUT_FROM_BENEFITS_SCREEN
    GoToFaqsScreen updatedState -> do
      modifyScreenState $ BenefitsScreenStateType (\_ -> updatedState)  
      App.BackT $ App.NoBack <$> pure (GO_TO_FAQS_SCREEN_FROM_BENEFITS_SCREEN)