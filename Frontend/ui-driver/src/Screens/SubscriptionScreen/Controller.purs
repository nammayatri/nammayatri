module Screens.SubscriptionScreen.Controller where

import Common.Types.App (APIPaymentStatus)
import Components.BottomNavBar as BottomNavBar
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Data.Int as DI
import Data.Maybe as Mb
import Data.String (toLower)
import Engineering.Helpers.Commons (convertUTCtoISC)
import JBridge (cleverTapCustomEvent, firebaseLogEvent, minimizeApp)
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, pure, unit, not, negate, show, ($), (==), (-), (*), (&&))
import Presto.Core.Types.API (ErrorResponse)
import PrestoDOM (Eval, continue, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.SubscriptionScreen.Transformer (alternatePlansTransformer, getAutoPayDetailsList, getPspIcon, getSelectedId, myPlanListTransformer, planListTransformer)
import Screens.Types (AutoPayStatus(..), SubscribePopupType(..), SubscriptionScreenState, SubscriptionSubview(..))
import Services.API (GetCurrentPlanResp(..), MandateData(..), PaymentBreakUp(..), PlanEntity(..), UiPlansResp(..))
import Services.Backend (getCorrespondingErrorMessage)
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
            | LoadMyPlans GetCurrentPlanResp (Mb.Maybe String)
            | ShowError ErrorResponse
            | PaymentStatusAction APIPaymentStatus
            | CheckPaymentStatus 
            | LoadAlternatePlans UiPlansResp
            | ConfirmCancelPopup PopUpModal.Action
            | TryAgainButtonAC PrimaryButton.Action


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
                    | CheckOrderStatus SubscriptionScreenState
                    | ScreenExit SubscriptionScreenState
                    | Refresh

eval :: Action -> SubscriptionScreenState -> Eval Action ScreenOutput SubscriptionScreenState
eval BackPressed state = 
  if  ( not Mb.isNothing state.props.popUpState && not (state.props.popUpState == Mb.Just SuccessPopup)) then continue state{props { popUpState = Mb.Nothing}}
  else if state.props.confirmCancel then continue state{props { confirmCancel = false}}
  else if state.props.subView == ManagePlan then continue state{props { subView = MyPlan}}
  else if state.props.subView == PlanDetails then continue state{props { subView = ManagePlan}}
  else do
    _ <- pure $ minimizeApp ""
    continue state

eval ToggleDueDetails state = continue state {props {myPlanProps { isDuesExpanded = not state.props.myPlanProps.isDuesExpanded}}}

eval (ClearDue PrimaryButton.OnClick) state = continue state

eval (SwitchPlan PrimaryButton.OnClick) state = do
  _ <- pure $ cleverTapCustomEvent "ny_driver_switch_plan_clicked"
  updateAndExit state { props{showShimmer = true}} $ SwitchCurrentPlan state { props{showShimmer = true}}

eval GotoManagePlan state = do
  _ <- pure $ cleverTapCustomEvent "ny_driver_manage_plan_clicked"
  updateAndExit state { props{showShimmer = true}} $ ScreenExit state {props {showShimmer = true, subView = ManagePlan, managePlanProps { selectedPlan = state.data.myPlanData.planEntity.id } }, data { managePlanData {currentPlan = state.data.myPlanData.planEntity }}}

eval (SelectPlan planID ) state = continue state {props {managePlanProps {selectedPlan = planID}}}

eval (ChoosePlan planID ) state = do
  _ <- pure $ cleverTapCustomEvent "ny_driver_plan_selected"
  continue state {props {joinPlanProps {selectedPlan = Mb.Just planID}}}

eval (JoinPlanAC PrimaryButton.OnClick) state = exit $ JoinPlanExit state

eval HeaderRightClick state = case state.props.subView of
    MyPlan -> exit $ PaymentHistory state 
    _ -> continue state

eval (PopUpModalAC (PopUpModal.OnButton1Click)) state = case state.props.popUpState of
                  Mb.Just SuccessPopup -> updateAndExit state { props{showShimmer = true}} $ Refresh
                  Mb.Just FailedPopup -> continue state {props {popUpState = Mb.Nothing}} -- Retry Payment
                  Mb.Just DuesClearedPopup -> exit $ Refresh
                  Mb.Just CancelAutoPay -> exit $ CancelAutoPayPlan state -- CancelAutoPay API
                  Mb.Nothing -> continue state


eval (ConfirmCancelPopup (PopUpModal.OnButton1Click)) state = continue state { props { confirmCancel = false}}

eval (ConfirmCancelPopup (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ cleverTapCustomEvent "ny_driver_cancel_autopay"
  updateAndExit state { props{showShimmer = true}} $ CancelAutoPayPlan state { props{showShimmer = true}}

eval (ResumeAutoPay PrimaryButton.OnClick) state = updateAndExit state { props{showShimmer = true}} $ ResumeAutoPayPlan state { props{showShimmer = true}}

eval CancelAutoPayAC state = continue state { props { confirmCancel = true}}

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

eval (LoadPlans plans) state = do
  let (UiPlansResp planResp) = plans
  continue state {
      data{ joinPlanData {allPlans = planListTransformer plans,
                            subscriptionStartDate = (convertUTCtoISC planResp.subscriptionStartTime "Do MMM")}},
      props{showShimmer = false, subView = JoinPlan,  
            joinPlanProps { selectedPlan = if (state.props.joinPlanProps.selectedPlan == Mb.Nothing) then getSelectedId plans else state.props.joinPlanProps.selectedPlan}} }

eval (LoadMyPlans plans autoPayStatus ) state = do
  let (GetCurrentPlanResp currentPlanResp) = plans
  let (PlanEntity planEntity) = currentPlanResp.currentPlanDetails
  let newState = state{ props{ showShimmer = false, subView = MyPlan }, data{myPlanData{planEntity = myPlanListTransformer plans, autoPayStatus = getAutopayStatus autoPayStatus}}}
  case currentPlanResp.mandateDetails of 
    Mb.Nothing -> continue newState
    Mb.Just (MandateData mandateDetails) -> continue newState 
                                          {data {myPlanData {
                                          maxDueAmount = planEntity.totalPlanCreditLimit,
                                          mandateStatus = toLower mandateDetails.status,
                                          currentDueAmount = planEntity.currentDues
                                          }
                                          , autoPayDetails {registeredPG = mandateDetails.payerVpa
                                          , detailsList = getAutoPayDetailsList (MandateData mandateDetails)
                                          , pspLogo = getPspIcon mandateDetails.payerVpa
                                          , payerUpiId = mandateDetails.payerVpa
                                            }
                                          }}

eval CheckPaymentStatus state = updateAndExit state { props{refreshPaymentStatus = true}} $ CheckOrderStatus state

eval (ShowError errorPayload )state = continue state{props{showError = true, showShimmer = false}, data { errorMessage = getCorrespondingErrorMessage errorPayload}}

eval (LoadAlternatePlans plansArray) state = continue state { data { managePlanData { alternatePlans = alternatePlansTransformer plansArray state}}, props {subView = ManagePlan, showShimmer = false}}

eval (TryAgainButtonAC PrimaryButton.OnClick) state = updateAndExit state { props{showShimmer = true}} $ Refresh

eval _ state = continue state

getPlanPrice :: Array PaymentBreakUp -> String -> String
getPlanPrice fares priceType = do
  let price = (DA.filter(\(PaymentBreakUp item) -> item.component == priceType) fares)
  case price DA.!! 0 of
    Mb.Just (PaymentBreakUp element) -> case (DI.fromNumber element.amount) of
                        Mb.Just value -> show value
                        Mb.Nothing -> show element.amount
    Mb.Nothing -> ""

getAllFareFromArray :: Array PaymentBreakUp -> Array String -> Number
getAllFareFromArray fares titles = do
  let matchingFares = (DA.filter (\(PaymentBreakUp fare) -> DA.elem fare.component titles) fares)
  let price = (DA.foldl (\acc (PaymentBreakUp fare) -> fare.amount - acc) 0.0 matchingFares)
  price * -1.0

getAutopayStatus :: Mb.Maybe String -> AutoPayStatus
getAutopayStatus autoPayStatus = 
  case autoPayStatus of 
    Mb.Nothing -> NO_AUTOPAY --  call subscribe
    Mb.Just status -> case status of 
      "ACTIVE" -> ACTIVE_AUTOPAY --  call current plan
      "SUSPENDED" -> SUSPENDED --  call resume
      "PAUSED_PSP" -> PAUSED_PSP --  ask them to resume from PSP app
      "CANCELLED_PSP" ->  CANCELLED_PSP -- call subscribe
      _ -> NO_AUTOPAY