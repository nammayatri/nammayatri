module Screens.OnBoardingSubscriptionScreen.Controller where

import Prelude

import Components.PrimaryButton as PrimaryButton
import Data.Array ((!!))
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Debug (spy)
import Engineering.Helpers.Commons (convertUTCtoISC)
import JBridge as JB
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.SubscriptionScreen.ScreenData (dummyPlanConfig)
import Screens.SubscriptionScreen.Transformer (alternatePlansTransformer, getAutoPayDetailsList, getSelectedId, getSelectedPlan, myPlanListTransformer, planListTransformer)
import Screens.Types (OnBoardingSubscriptionScreenState)
import Screens.Types (PlanCardConfig)
import Services.API (ReelsResp(..), UiPlansResp(..))
import Storage (KeyStore(..), setValueToLocalStore, getValueToLocalStore)
import Components.PopUpModal as PopUpModal
import PrestoDOM.Core (getPushFn)
import Effect.Uncurried (runEffectFn5, runEffectFn1)
import Screens.OnBoardingSubscriptionScreen.Transformer (transformReelsPurescriptDataToNativeData)
import Engineering.Helpers.Commons(getNewIDWithTag)
import RemoteConfig (ReelItem(..))
import Foreign (Foreign)
import Foreign.Generic (encodeJSON)
import Presto.Core.Types.Language.Flow (delay)
import Data.Time.Duration (Milliseconds(..))
import Control.Monad.Trans.Class(lift)
import ConfigProvider (getAppConfig)
import Helpers.Utils (getCityConfig)
import Constants as Const

instance showAction :: Show Action where
  show (BackPressed ) = "BackPressed"
  show (NoAction ) = "NoAction"
  show (GoToRegisteration ) = "GoToRegisteration"
  show (LoadPlans _) = "LoadPlans"
  show (SelectPlan _) = "SelectPlan"
  show (JoinPlanAC var1) = "JoinPlanAC_" <> show var1
  show (CallSupport ) = "CallSupport"
  show (PopUpModalAC var1) = "PopUpModalAC_" <> show var1
  show (OpenReelsView _) = "OpenReelsView"
  show (GetCurrentPosition _ _ _ _) = "GetCurrentPosition"

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
        BackPressed -> do
                trackAppBackPress appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN)
                trackAppEndScreen appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN)
        NoAction -> trackAppScreenEvent appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN) "in_screen" "no_action"
        GoToRegisteration -> do
                trackAppEndScreen appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN)
        CallSupport -> trackAppScreenEvent appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN) "in_screen" "call_support"
        SelectPlan config -> trackAppScreenEvent appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN) "in_screen" "select_plan"
        JoinPlanAC act -> trackAppScreenEvent appId (getScreen ONBOARDING_SUBSCRIPTION_SCREEN) "in_screen" "join_plan"
        _ -> pure unit

data ScreenOutput =  GoBack | GoToRegisterationScreen OnBoardingSubscriptionScreenState | StartFreeTrialExit OnBoardingSubscriptionScreenState

data Action = BackPressed
            | NoAction
            | GoToRegisteration
            | LoadPlans UiPlansResp
            | SelectPlan PlanCardConfig
            | JoinPlanAC PrimaryButton.Action
            | CallSupport
            | PopUpModalAC PopUpModal.Action
            | OpenReelsView Int
            | GetCurrentPosition String String Foreign Foreign


eval :: Action -> OnBoardingSubscriptionScreenState -> Eval Action ScreenOutput OnBoardingSubscriptionScreenState
eval BackPressed state = 
    if state.props.supportPopup then exit GoBack
    else continue state {props {supportPopup = not state.props.supportPopup}}
    
eval NoAction state = continue state
eval GoToRegisteration state = exit $ GoToRegisterationScreen state
eval (LoadPlans plans) state = do
    let (UiPlansResp planResp) = plans
        config = state.data.subscriptionConfig
        cityConfig = getCityConfig state.data.config.cityConfig $ getValueToLocalStore DRIVER_LOCATION
    _ <- pure $ setValueToLocalStore DRIVER_SUBSCRIBED "false"
    let planList = planListTransformer plans false config cityConfig
    continue state { data{ plansList = planList , selectedPlanItem = (planList !! 0)}}
eval (SelectPlan config ) state = continue state {data { selectedPlanItem = Just config }}
eval (JoinPlanAC PrimaryButton.OnClick) state = updateAndExit state $ StartFreeTrialExit state
eval CallSupport state = do
  let cityConfig = getCityConfig state.data.config.cityConfig $ getValueToLocalStore DRIVER_LOCATION
  void $ pure $ JB.showDialer cityConfig.supportNumber false
  continue state
eval (PopUpModalAC PopUpModal.OnButton1Click) state = do
    let cityConfig = getCityConfig state.data.config.cityConfig $ getValueToLocalStore DRIVER_LOCATION
    void $ pure $ JB.showDialer cityConfig.supportNumber false
    continue state { props { supportPopup = false }}
eval (PopUpModalAC (PopUpModal.OnButton2Click)) _ = exit GoBack

eval (OpenReelsView index) state = do
  void $ pure $ setValueToLocalStore ANOTHER_ACTIVITY_LAUNCHED "true"
  continueWithCmd state [ do
    push <-  getPushFn Nothing "OnBoardingSubscriptionScreen"
    void $ runEffectFn5 JB.addReels (encodeJSON (transformReelsPurescriptDataToNativeData state.data.reelsData)) index (getNewIDWithTag "ReelsViewOnBoarding") push $ GetCurrentPosition
    pure NoAction
  ]

eval (GetCurrentPosition label stringData reelData buttonData) state = do
  case label of
    "ACTION" -> 
      case stringData of
        "CHOOSE_A_PLAN" -> do
                          void $ pure $ setValueToLocalStore ANOTHER_ACTIVITY_LAUNCHED "false"
                          continueWithCmd state [ do
                            _ <- pure $ delay $ Milliseconds 2000.0
                            _ <- JB.scrollToEnd (getNewIDWithTag "OnBoardingSubscriptionScreenScrollView") true
                            pure NoAction
                          ]
        "DESTROY_REEL" -> do
                          void $ pure $ setValueToLocalStore ANOTHER_ACTIVITY_LAUNCHED "false"
                          continue state
        _ -> continue state
    "CURRENT_POSITION" -> let hello = spy "Current position" stringData
                          in continue state
    _ -> continue state

eval _ state = update state
