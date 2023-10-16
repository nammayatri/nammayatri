module Screens.OnBoardingSubscriptionScreen.Controller where

import Prelude

import Components.PrimaryButton as PrimaryButton
import Data.Array ((!!))
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Debug (spy)
import Engineering.Helpers.Commons (convertUTCtoISC)
import JBridge as JB
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import MerchantConfig.Utils (getValueFromConfig)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.SubscriptionScreen.ScreenData (dummyPlanConfig)
import Screens.SubscriptionScreen.Transformer (alternatePlansTransformer, getAutoPayDetailsList, getSelectedId, getSelectedPlan, myPlanListTransformer, planListTransformer)
import Screens.Types (OnBoardingSubscriptionScreenState)
import Screens.Types (PlanCardConfig)
import Services.API (UiPlansResp(..))
import Storage (KeyStore(..), setValueToLocalStore)


instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
        BackPressed -> do
                trackAppBackPress appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN)
                trackAppEndScreen appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN)
        NoAction -> trackAppScreenEvent appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN) "in_screen" "no_action"
        GoToHomeScreen -> do
                trackAppEndScreen appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN)
        CallSupport -> trackAppScreenEvent appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN) "in_screen" "call_support"
        SelectPlan config -> trackAppScreenEvent appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN) "in_screen" "select_plan"
        JoinPlanAC act -> trackAppScreenEvent appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN) "in_screen" "join_plan"
        _ -> pure unit

data ScreenOutput =  GoBack | GoToHome | StartFreeTrialExit OnBoardingSubscriptionScreenState

data Action = BackPressed
            | NoAction
            | GoToHomeScreen
            | LoadPlans UiPlansResp
            | SelectPlan PlanCardConfig
            | JoinPlanAC PrimaryButton.Action
            | CallSupport

eval :: Action -> OnBoardingSubscriptionScreenState -> Eval Action ScreenOutput OnBoardingSubscriptionScreenState
eval BackPressed state = exit GoBack
eval NoAction state = continue state
eval GoToHomeScreen state = exit GoToHome
eval (LoadPlans plans) state = do
    let (UiPlansResp planResp) = plans
        config = getValueFromConfig "subscriptionConfig"
    _ <- pure $ setValueToLocalStore DRIVER_SUBSCRIBED "false"
    let planList = planListTransformer plans false config.gradientConfig 
    continue state { data{ plansList = planList , selectedPlanItem = (planList !! 0)}}
eval (SelectPlan config ) state = continue state {data { selectedPlanItem = Just config }}
eval (JoinPlanAC PrimaryButton.OnClick) state = updateAndExit state $ StartFreeTrialExit state
eval CallSupport state = do
  _ <- pure $ JB.showDialer "08069490091" false
  continue state
eval _ state = continue state
