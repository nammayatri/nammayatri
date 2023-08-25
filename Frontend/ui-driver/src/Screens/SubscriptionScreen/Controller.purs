module Screens.SubscriptionScreen.Controller where

import Common.Types.App (APIPaymentStatus, LazyCheck(..))
import Components.BottomNavBar as BottomNavBar
import Components.OptionsMenu as OptionsMenu
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Debug
import Data.Int as DI
import Data.Maybe (fromMaybe, isNothing)
import Data.Maybe as Mb
import Data.String (toLower)
import Engineering.Helpers.Commons (convertUTCtoISC)
import JBridge (cleverTapCustomEvent, firebaseLogEvent, minimizeApp, setCleverTapUserProp, openUrlInApp, showDialer, openWhatsAppSupport)
import Services.Config (getSupportNumber, getWhatsAppSupportNo)
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, Unit, bind, map, negate, not, pure, show, unit, ($), (&&), (*), (-), (/=), (==), discard)
import Presto.Core.Types.API (ErrorResponse)
import PrestoDOM (Eval, continue, exit, updateAndExit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.SubscriptionScreen.Transformer (alternatePlansTransformer, getAutoPayDetailsList, getPspIcon, getSelectedId, getSelectedPlan, myPlanListTransformer, planListTransformer)
import Screens.Types (AutoPayStatus(..), SubscribePopupType(..), SubscriptionScreenState, SubscriptionSubview(..), PlanCardConfig)
import Services.API (GetCurrentPlanResp(..), MandateData(..), OfferEntity(..), PaymentBreakUp(..), PlanEntity(..), UiPlansResp(..))
import Services.Backend (getCorrespondingErrorMessage)
import Storage (KeyStore(..), setValueToLocalNativeStore, setValueToLocalStore)
import MerchantConfig.Utils (Merchant(..), getMerchant)

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
            | ManagePlanAC
            | SelectPlan PlanCardConfig
            | ChoosePlan PlanCardConfig
            | ToggleDueDetails
            | ToggleDueDetailsView
            | NoAction
            | ViewPaymentHistory
            | PopUpModalAC PopUpModal.Action
            | HeaderRightClick
            | CancelAutoPayAC
            | ViewAutopayDetails
            | ResumeAutoPay PrimaryButton.Action
            | LoadPlans UiPlansResp
            | LoadMyPlans GetCurrentPlanResp
            | ShowError ErrorResponse
            | PaymentStatusAction APIPaymentStatus
            | CheckPaymentStatus 
            | LoadAlternatePlans UiPlansResp
            | ConfirmCancelPopup PopUpModal.Action
            | TryAgainButtonAC PrimaryButton.Action
            | RetryPaymentAC
            | RefreshPage
            | OptionsMenuAction OptionsMenu.Action


data ScreenOutput = HomeScreen SubscriptionScreenState
                    | RideHistory SubscriptionScreenState
                    | Contest SubscriptionScreenState
                    | Alerts SubscriptionScreenState
                    | JoinPlanExit SubscriptionScreenState
                    | PaymentHistory SubscriptionScreenState
                    | CancelAutoPayPlan SubscriptionScreenState
                    | SwitchCurrentPlan SubscriptionScreenState String
                    | ResumeAutoPayPlan SubscriptionScreenState
                    | CheckOrderStatus SubscriptionScreenState String
                    | GotoManagePlan SubscriptionScreenState
                    | Refresh
                    | RetryPayment SubscriptionScreenState String

eval :: Action -> SubscriptionScreenState -> Eval Action ScreenOutput SubscriptionScreenState
eval BackPressed state = 
  if  ( not Mb.isNothing state.props.popUpState && not (state.props.popUpState == Mb.Just SuccessPopup)) then continue state{props { popUpState = Mb.Nothing}}
  else if state.props.optionsMenuExpanded then continue state{props{optionsMenuExpanded = false}}
  else if state.props.confirmCancel then continue state{props { confirmCancel = false}}
  else if state.props.subView == ManagePlan then continue state{props { subView = MyPlan}}
  else if state.props.subView == PlanDetails then continue state{props { subView = ManagePlan}}
  else do
    _ <- pure $ minimizeApp ""
    continue state

eval ToggleDueDetails state = continue state {props {myPlanProps { isDuesExpanded = not state.props.myPlanProps.isDuesExpanded}}}

eval ToggleDueDetailsView state = continue state {props { isDueViewExpanded = not state.props.isDueViewExpanded}}

eval (ClearDue PrimaryButton.OnClick) state = continue state

eval (OptionsMenuAction (OptionsMenu.ItemClick item)) state = do
  let merchant = getMerchant FunctionCall
  continueWithCmd state{props{optionsMenuExpanded = false}} [do
    case item of 
      "manage_plan" -> pure ManagePlanAC 
      "payment_history" -> pure ViewPaymentHistory 
      "call_support" -> do
        _ <- pure $ showDialer "8069490091" false
        pure NoAction
      "chat_for_help" -> do
          _ <- openUrlInApp "https://wa.me/917483117936?text=Hello%2C%20I%20need%20help%20with%20setting%20up%20Autopay%20Subscription%0A%E0%B2%B8%E0%B3%8D%E0%B2%B5%E0%B2%AF%E0%B2%82%20%E0%B2%AA%E0%B2%BE%E0%B2%B5%E0%B2%A4%E0%B2%BF%20%E0%B2%9A%E0%B2%82%E0%B2%A6%E0%B2%BE%E0%B2%A6%E0%B2%BE%E0%B2%B0%E0%B2%BF%E0%B2%95%E0%B3%86%E0%B2%AF%E0%B2%A8%E0%B3%8D%E0%B2%A8%E0%B3%81%20%E0%B2%B9%E0%B3%8A%E0%B2%82%E0%B2%A6%E0%B2%BF%E0%B2%B8%E0%B2%B2%E0%B3%81%20%E0%B2%A8%E0%B2%A8%E0%B2%97%E0%B3%86%20%E0%B2%B8%E0%B2%B9%E0%B2%BE%E0%B2%AF%E0%B2%A6%20%E0%B2%85%E0%B2%97%E0%B2%A4%E0%B3%8D%E0%B2%AF%E0%B2%B5%E0%B2%BF%E0%B2%A6%E0%B3%86"
          pure NoAction
      "view_faq" -> do
          _ <- openUrlInApp "https://nammayatri.in/plans/"
          pure NoAction
      _ -> pure NoAction
  ]

eval (OptionsMenuAction (OptionsMenu.BackgroundClick)) state = do
  _ <- pure $ spy "menu background clicked" ""
  continue state{props{optionsMenuExpanded = false}}

eval (SwitchPlan PrimaryButton.OnClick) state = do
  let planId = state.props.managePlanProps.selectedPlanItem.id
  if planId /= "" then do
    _ <- pure $ cleverTapCustomEvent "ny_driver_switch_plan_clicked"
    updateAndExit state { props{showShimmer = true}} $ SwitchCurrentPlan state { props{showShimmer = true}} planId
  else continue state

eval ManagePlanAC state = do
  _ <- pure $ cleverTapCustomEvent "ny_driver_manage_plan_clicked"
  updateAndExit state { props{showShimmer = true, subView = ManagePlan, optionsMenuExpanded = false}} $ GotoManagePlan state {props {showShimmer = true, subView = ManagePlan, managePlanProps { selectedPlanItem = state.data.myPlanData.planEntity }, optionsMenuExpanded = false }, data { managePlanData {currentPlan = state.data.myPlanData.planEntity }}}

eval (SelectPlan config ) state = continue state {props {managePlanProps { selectedPlanItem = config}}}

eval (ChoosePlan config ) state = continue state {props {joinPlanProps { selectedPlanItem = Mb.Just config}}}

eval (JoinPlanAC PrimaryButton.OnClick) state = exit $ JoinPlanExit state

eval HeaderRightClick state =  continue state {props{ optionsMenuExpanded = not state.props.optionsMenuExpanded}}

eval (PopUpModalAC (PopUpModal.OnButton1Click)) state = case state.props.popUpState of
                  Mb.Just SuccessPopup -> updateAndExit state { props{showShimmer = true, popUpState = Mb.Nothing}} $ Refresh
                  Mb.Just FailedPopup -> updateAndExit state { props{showShimmer = true, popUpState = Mb.Nothing}} $ Refresh
                  Mb.Just DuesClearedPopup -> exit $ Refresh
                  Mb.Just SwitchedPlan -> exit $ Refresh
                  Mb.Just CancelAutoPay -> exit $ CancelAutoPayPlan state -- CancelAutoPay API
                  Mb.Nothing -> continue state


eval (ConfirmCancelPopup (PopUpModal.OnButton1Click)) state = continue state { props { confirmCancel = false}}

eval (ConfirmCancelPopup (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ cleverTapCustomEvent "ny_driver_cancel_autopay"
  updateAndExit state { props{showShimmer = true, confirmCancel = false}} $ CancelAutoPayPlan state { props{showShimmer = true, confirmCancel = false}}

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

eval ViewPaymentHistory state = exit $ PaymentHistory state{props{optionsMenuExpanded = false}}

eval RefreshPage state = exit $ Refresh

eval (LoadPlans plans) state = do
  let (UiPlansResp planResp) = plans
  _ <- pure $ setValueToLocalStore DRIVER_SUBSCRIBED "false"
  continue state {
      data{ joinPlanData {allPlans = planListTransformer plans,
                            subscriptionStartDate = (convertUTCtoISC planResp.subscriptionStartTime "Do MMM")}},
      props{showShimmer = false, subView = JoinPlan,  
            joinPlanProps { selectedPlanItem = if (isNothing state.props.joinPlanProps.selectedPlanItem) then getSelectedPlan plans else state.props.joinPlanProps.selectedPlanItem}} }

eval (LoadMyPlans plans) state = do
  let (GetCurrentPlanResp currentPlanResp) = plans
  case currentPlanResp.currentPlanDetails of
    Mb.Just (planEntity') -> do
      _ <- pure $ setValueToLocalStore DRIVER_SUBSCRIBED "true"
      let (PlanEntity planEntity) = planEntity'
      let newState = state{ props{ showShimmer = false, subView = MyPlan }, data{ orderId = currentPlanResp.orderId, planId = planEntity.id, myPlanData{planEntity = myPlanListTransformer planEntity' currentPlanResp.isLocalized, maxDueAmount = planEntity.totalPlanCreditLimit, currentDueAmount = planEntity.currentDues, autoPayStatus = getAutopayStatus currentPlanResp.autoPayStatus}}}
      _ <- pure $ setCleverTapUserProp "Plan" planEntity.name
      _ <- pure $ setCleverTapUserProp "Subscription Offer" $ show $ map (\(OfferEntity offer) -> offer.title) planEntity.offers
      _ <- pure $ setCleverTapUserProp "Due Amount" $ show planEntity.currentDues
      _ <- pure $ setCleverTapUserProp "Autopay status" $ fromMaybe "Nothing" currentPlanResp.autoPayStatus


      case currentPlanResp.mandateDetails of 
        Mb.Nothing -> continue newState
        Mb.Just (MandateData mandateDetails) -> do
          _ <- pure $ setCleverTapUserProp "Mandate status" mandateDetails.status
          _ <- pure $ setCleverTapUserProp "Autopay Max Amount Registered" $ show mandateDetails.maxAmount
          _ <- pure $ setCleverTapUserProp "Payment method" if mandateDetails.status == "ACTIVE" then "Autopay" else "Manual"
          _ <- pure $ setCleverTapUserProp "UPI ID availability" if isNothing mandateDetails.payerVpa then "FALSE" else "TRUE"
          continue newState 
            {data {
                myPlanData {
                  mandateStatus = toLower mandateDetails.status
                }
              , autoPayDetails {
                  detailsList = getAutoPayDetailsList (MandateData mandateDetails)
                , pspLogo = getPspIcon (Mb.fromMaybe "" mandateDetails.payerVpa)
                , payerUpiId = mandateDetails.payerVpa
                }
              }
            }
    Mb.Nothing -> do
      _ <- pure $ setValueToLocalStore DRIVER_SUBSCRIBED "false"
      continue state

eval CheckPaymentStatus state = case state.data.orderId of
  Mb.Just id -> updateAndExit state { props{refreshPaymentStatus = true}} $ CheckOrderStatus state id
  Mb.Nothing -> continue state

eval (ShowError errorPayload )state = continue state{props{showError = true, showShimmer = false}, data { errorMessage = getCorrespondingErrorMessage errorPayload}}

eval (LoadAlternatePlans plansArray) state = continue state { data { managePlanData { alternatePlans = alternatePlansTransformer plansArray state}}, props {subView = ManagePlan, showShimmer = false}}

eval (TryAgainButtonAC PrimaryButton.OnClick) state = updateAndExit state { props{showShimmer = true}} $ Refresh

eval RetryPaymentAC state = if state.data.myPlanData.planEntity.id == "" then continue state else
  exit $ RetryPayment state state.data.myPlanData.planEntity.id

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
      "PENDING" -> PENDING
      _ -> NO_AUTOPAY