module Screens.SubscriptionScreen.Controller where

import Components.BottomNavBar as BottomNavBar
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Data.Maybe as Mb
import JBridge (firebaseLogEvent, minimizeApp)
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, pure, unit, not, ($), (==))
import PrestoDOM (Eval, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.SubscriptionScreen.Transformer (myPlanListTransformer, planListTransformer)
import Screens.Types (SubscribePopupType(..), SubscriptionScreenState, SubscriptionSubview(..))
import Services.API (GetCurrentPlanResp(..), UiPlansResp(..))
import Storage (KeyStore(..), setValueToLocalNativeStore)

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "SubscriptionScreen"
    _ -> pure unit


data Action = BackPressed
            | AfterRender
            | BottomNavBarAction BottomNavBar.Action
            | ClearDue PrimaryButton.Action
            | SwitchPlan PrimaryButton.Action
            | JoinPlanAC PrimaryButton.Action
            | GotoManagePlan
            | SelectPlan String
            | ChoosePlan String
            | ToggleDueDetails
            | NoAction
            | ViewPaymentHistory
            | PopUpModalAC PopUpModal.Action
            | HeaderRightClick
            | CancelAutoPayAC
            | ViewAutopayDetails
            | ResumeAutoPay PrimaryButton.Action
            | LoadPlans UiPlansResp
            | LoadMyPlans GetCurrentPlanResp
            | ShowError


data ScreenOutput = HomeScreen SubscriptionScreenState
                    | RideHistory SubscriptionScreenState
                    | Contest SubscriptionScreenState
                    | Alerts SubscriptionScreenState
                    | JoinPlanExit SubscriptionScreenState
                    | PaymentHistory SubscriptionScreenState
                    | PauseAutoPay SubscriptionScreenState
                    | CancelAutoPayPlan SubscriptionScreenState
                    | SwitchCurrentPlan SubscriptionScreenState
                    | ResumeAutoPayPlan SubscriptionScreenState

eval :: Action -> SubscriptionScreenState -> Eval Action ScreenOutput SubscriptionScreenState
eval BackPressed state = 
  if (not Mb.isNothing state.props.popUpState) then continue state{props { popUpState = Mb.Nothing}}
  else if state.props.subView == ManagePlan then continue state{props { subView = MyPlan}}
  else if state.props.subView == PlanDetails then continue state{props { subView = ManagePlan}}
  else do
    _ <- pure $ minimizeApp ""
    continue state

eval ToggleDueDetails state = continue state {props {myPlanProps { isDuesExpanded = not state.props.myPlanProps.isDuesExpanded}}}

eval (ClearDue PrimaryButton.OnClick) state = continue state

eval (SwitchPlan PrimaryButton.OnClick) state = exit $ SwitchCurrentPlan state

eval GotoManagePlan state = continue state {props {subView = ManagePlan }}

eval (SelectPlan planID ) state = continue state {props {managePlanProps {selectedPlan = planID}}}

eval (ChoosePlan planID ) state = continue state {props {joinPlanProps {selectedPlan = planID}}}

eval (JoinPlanAC PrimaryButton.OnClick) state = exit $ JoinPlanExit state { props {joinPlanProps {paymentMode = "AUTOPAY" } }} -- MANUAL | AUTOPAY

eval HeaderRightClick state = case state.props.subView of
    MyPlan -> exit $ PaymentHistory state 
    _ -> continue state

eval (PopUpModalAC (PopUpModal.OnButton1Click)) state = case state.props.popUpState of
                  Mb.Just SuccessPopup -> continue state {props {popUpState = Mb.Nothing}}
                  Mb.Just FailedPopup -> continue state {props {popUpState = Mb.Nothing}} -- Retry Payment
                  Mb.Just DuesClearedPopup -> continue state {props {popUpState = Mb.Nothing}}
                  Mb.Just CancelAutoPay -> exit $ CancelAutoPayPlan state -- CancelAutoPay API
                  Mb.Nothing -> continue state
              
eval (PopUpModalAC (PopUpModal.DismisTextClick)) state = exit $ ResumeAutoPayPlan state -- ResumeAutoPay

eval (ResumeAutoPay PrimaryButton.OnClick) state = exit $ PauseAutoPay state -- PauseAutoPay API

eval CancelAutoPayAC state = continue state {props {popUpState = Mb.Just CancelAutoPay}}

eval ViewAutopayDetails state = continue state{props {subView = PlanDetails }}

eval (BottomNavBarAction (BottomNavBar.OnNavigate screen)) state = do
  case screen of
    "Home" -> exit $ HomeScreen state
    "Rides" -> exit $ RideHistory state
    "Alert" -> do
      _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      _ <- pure $ firebaseLogEvent "ny_driver_alert_click"
      exit $ Alerts state
    "Rankings" -> do
      _ <- pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
      exit $ Contest state
    _ -> continue state

eval ViewPaymentHistory state = exit $ PaymentHistory state

eval (LoadPlans plans) state =   -- fill Data
  continue state {data {joinPlanData {allPlans = planListTransformer plans}} }

eval (LoadMyPlans plans) state = -- fill Data
  continue state{data{myPlanData{planEntity = myPlanListTransformer plans}}}

eval ShowError state = continue state{props{showError = true}}

eval _ state = continue state