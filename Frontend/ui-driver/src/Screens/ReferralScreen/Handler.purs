module Screens.ReferralScreen.Handler where


import Prelude (bind, pure, ($), (<$>), discard)
import Engineering.Helpers.BackTrack (getState)
import Screens.ReferralScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.ReferralScreen.View as ReferralScreen
import Types.App (FlowBT, GlobalState(..), REFERRAL_SCREEN_OUTPUT(..), ScreenType(..))
import Screens.ReferralScreen.ScreenData as ReferralScreenData
import Types.ModifyScreenState (modifyScreenState)
import Data.Maybe(isJust)
import Screens.Types (ReferralType(..))

referralScreen:: FlowBT String REFERRAL_SCREEN_OUTPUT
referralScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ ReferralScreen.screen state.referralScreen{ props {stage = if state.referralScreen.props.firstTime then SuccessScreen else if isJust state.referralScreen.data.driverInfo.referralCode then QRScreen else ComingSoonScreen}}
  case action of
    GoBack -> App.BackT $ App.BackPoint <$> (pure $ Go_BACK)
    GoToHomeScreen -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen{props = ReferralScreenData.initData.props })
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_SCREEN_FROM_REFERRAL_SCREEN)
    GoToRidesScreen -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen{props = ReferralScreenData.initData.props })
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_RIDES_SCREEN_FROM_REFERRAL_SCREEN)
    GoToProfileScreen -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen{props = ReferralScreenData.initData.props })
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_PROFILE_SCREEN_FROM_REFERRAL_SCREEN)
    GoToNotifications -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen{props = ReferralScreenData.initData.props })
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_NOTIFICATION_SCREEN_FROM_REFERRAL_SCREEN)
    LinkReferralApi updatedState -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_FLOW_AND_COME_BACK updatedState)