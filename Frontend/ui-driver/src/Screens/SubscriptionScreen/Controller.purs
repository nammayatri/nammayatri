module Screens.SubscriptionScreen.Controller where

import Debug
import Common.Types.App (APIPaymentStatus, LazyCheck(..))
import Components.BottomNavBar as BottomNavBar
import Components.OptionsMenu as OptionsMenu
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Control.Monad.Except (runExcept)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Int as DI
import Data.Lens ((^.))
import Data.Maybe (fromMaybe, isNothing)
import Data.Maybe as Mb
import Data.Number (fromString) as Number
import Data.String (toLower)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (convertUTCtoISC)
import Foreign.Generic (decodeJSON)
import Helpers.Utils (getDistanceBwCordinates)
import JBridge (cleverTapCustomEvent, firebaseLogEvent, minimizeApp, setCleverTapUserProp, openUrlInApp, showDialer, openWhatsAppSupport, metaLogEvent)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (class Show, Unit, bind, map, negate, not, pure, show, unit, ($), (&&), (*), (-), (/=), (==), discard, Ordering, compare)
import Presto.Core.Types.API (ErrorResponse)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.SubscriptionScreen.Transformer (alternatePlansTransformer, getAutoPayDetailsList, getPspIcon, getSelectedId, getSelectedPlan, myPlanListTransformer, planListTransformer)
import Screens.Types (AutoPayStatus(..), KioskLocation(..), OptionsMenuState(..), PlanCardConfig, SubscribePopupType(..), SubscriptionScreenState, SubscriptionSubview(..))
import Services.API (GetCurrentPlanResp(..), MandateData(..), OfferEntity(..), PaymentBreakUp(..), PlanEntity(..), UiPlansResp(..), KioskLocationRes(..))
import Services.Backend (getCorrespondingErrorMessage)
import Services.Config (getSupportNumber, getWhatsAppSupportNo)
import Storage (KeyStore(..), setValueToLocalNativeStore, setValueToLocalStore, getValueToLocalStore)

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
            | ViewHelpCentre
            | PopUpModalAC PopUpModal.Action
            | HeaderRightClick OptionsMenuState
            | CancelAutoPayAC
            | ViewAutopayDetails
            | ResumeAutoPay PrimaryButton.Action
            | LoadPlans UiPlansResp
            | LoadHelpCentre Number Number (Array KioskLocationRes)
            | LoadMyPlans GetCurrentPlanResp
            | ShowError ErrorResponse
            | PaymentStatusAction APIPaymentStatus
            | CheckPaymentStatus 
            | LoadAlternatePlans UiPlansResp
            | ConfirmCancelPopup PopUpModal.Action
            | TryAgainButtonAC PrimaryButton.Action
            | RetryPaymentAC PrimaryButton.Action
            | RefreshPage
            | OptionsMenuAction OptionsMenu.Action
            | CallSupport
            | CallHelpCenter String
            | OpenGoogleMap Number Number
            | CheckPaymentStatusButton PrimaryButton.Action


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
                    | GotoFindHelpCentre SubscriptionScreenState
                    | GoToOpenGoogleMaps SubscriptionScreenState
                    | Refresh
                    | RefreshHelpCentre SubscriptionScreenState
                    | RetryPayment SubscriptionScreenState String

eval :: Action -> SubscriptionScreenState -> Eval Action ScreenOutput SubscriptionScreenState
eval BackPressed state = 
  if state.props.popUpState == Mb.Just SupportPopup then updateAndExit state{props{popUpState = Mb.Nothing}} $ HomeScreen state{props{popUpState = Mb.Nothing}}
  else if  ( not Mb.isNothing state.props.popUpState && not (state.props.popUpState == Mb.Just SuccessPopup)) then continue state{props { popUpState = Mb.Nothing}}
  else if state.props.optionsMenuState /= ALL_COLLAPSED then continue state{props{optionsMenuState = ALL_COLLAPSED}}
  else if state.props.confirmCancel then continue state{props { confirmCancel = false}}
  else if state.props.subView == ManagePlan then continue state{props { subView = MyPlan}}
  else if state.props.subView == PlanDetails then continue state{props { subView = ManagePlan}}
  else if state.props.subView == FindHelpCentre then continue state {props { subView = state.props.prevSubView, kioskLocation = [], noKioskLocation = false, showError = false}}
  else if state.data.myPlanData.autoPayStatus /= ACTIVE_AUTOPAY then continue state{props { popUpState = Mb.Just SupportPopup}}
  else exit $ HomeScreen state

eval ToggleDueDetails state = continue state {props {myPlanProps { isDuesExpanded = not state.props.myPlanProps.isDuesExpanded}}}

eval ToggleDueDetailsView state = continue state {props { isDueViewExpanded = not state.props.isDueViewExpanded}}

eval (ClearDue PrimaryButton.OnClick) state = continue state

eval (OptionsMenuAction (OptionsMenu.ItemClick item)) state = do
  let merchant = getMerchant FunctionCall
  continueWithCmd state{props{optionsMenuState = ALL_COLLAPSED}} [do
    case item of 
      "manage_plan" -> pure ManagePlanAC 
      "payment_history" -> pure ViewPaymentHistory 
      "call_support" -> pure CallSupport
      "chat_for_help" -> do
          _ <- openUrlInApp "https://wa.me/917483117936?text=Hello%2C%20I%20need%20help%20with%20setting%20up%20Autopay%20Subscription%0A%E0%B2%B8%E0%B3%8D%E0%B2%B5%E0%B2%AF%E0%B2%82%20%E0%B2%AA%E0%B2%BE%E0%B2%B5%E0%B2%A4%E0%B2%BF%20%E0%B2%9A%E0%B2%82%E0%B2%A6%E0%B2%BE%E0%B2%A6%E0%B2%BE%E0%B2%B0%E0%B2%BF%E0%B2%95%E0%B3%86%E0%B2%AF%E0%B2%A8%E0%B3%8D%E0%B2%A8%E0%B3%81%20%E0%B2%B9%E0%B3%8A%E0%B2%82%E0%B2%A6%E0%B2%BF%E0%B2%B8%E0%B2%B2%E0%B3%81%20%E0%B2%A8%E0%B2%A8%E0%B2%97%E0%B3%86%20%E0%B2%B8%E0%B2%B9%E0%B2%BE%E0%B2%AF%E0%B2%A6%20%E0%B2%85%E0%B2%97%E0%B2%A4%E0%B3%8D%E0%B2%AF%E0%B2%B5%E0%B2%BF%E0%B2%A6%E0%B3%86"
          pure NoAction
      "view_faq" -> do
          _ <- openUrlInApp "https://nammayatri.in/plans/"
          pure NoAction
      "find_help_centre" -> pure ViewHelpCentre
      _ -> pure NoAction
  ]

eval (OptionsMenuAction (OptionsMenu.BackgroundClick)) state = do
  _ <- pure $ spy "menu background clicked" ""
  continue state{props{optionsMenuState = ALL_COLLAPSED}}

eval (SwitchPlan PrimaryButton.OnClick) state = do
  let planId = state.props.managePlanProps.selectedPlanItem.id
  if planId /= "" then do
    _ <- pure $ cleverTapCustomEvent "ny_driver_switch_plan_clicked"
    _ <- pure $ metaLogEvent "ny_driver_switch_plan_clicked"
    let _ = unsafePerformEffect $ firebaseLogEvent "ny_driver_switch_plan_clicked"
    updateAndExit state { props{showShimmer = true}} $ SwitchCurrentPlan state { props{showShimmer = true}} planId
  else continue state

eval ManagePlanAC state = do
  _ <- pure $ cleverTapCustomEvent "ny_driver_manage_plan_clicked"
  _ <- pure $ metaLogEvent "ny_driver_manage_plan_clicked"
  let _ = unsafePerformEffect $ firebaseLogEvent "ny_driver_manage_plan_clicked"
  updateAndExit state { props{showShimmer = true, subView = ManagePlan, optionsMenuState = ALL_COLLAPSED}} $ GotoManagePlan state {props {showShimmer = true, subView = ManagePlan, managePlanProps { selectedPlanItem = state.data.myPlanData.planEntity }, optionsMenuState = ALL_COLLAPSED }, data { managePlanData {currentPlan = state.data.myPlanData.planEntity }}}

eval (SelectPlan config ) state = continue state {props {managePlanProps { selectedPlanItem = config}}}

eval (ChoosePlan config ) state = continue state {props {joinPlanProps { selectedPlanItem = Mb.Just config}}}

eval (JoinPlanAC PrimaryButton.OnClick) state = updateAndExit state $ JoinPlanExit state

eval (HeaderRightClick menuType) state =  if state.props.optionsMenuState == menuType 
                                             then continue state {props{ optionsMenuState = ALL_COLLAPSED}}
                                          else continue state {props{ optionsMenuState = menuType}}

eval (PopUpModalAC (PopUpModal.OnButton1Click)) state = case state.props.popUpState of
                  Mb.Just SuccessPopup -> updateAndExit state { props{showShimmer = true, popUpState = Mb.Nothing}} $ Refresh
                  Mb.Just FailedPopup -> updateAndExit state { props{showShimmer = true, popUpState = Mb.Nothing}} $ RetryPayment state state.data.myPlanData.planEntity.id
                  Mb.Just DuesClearedPopup -> exit $ Refresh
                  Mb.Just SwitchedPlan -> exit $ Refresh
                  Mb.Just SupportPopup -> continueWithCmd state [pure CallSupport]
                  Mb.Just CancelAutoPay -> exit $ CancelAutoPayPlan state -- CancelAutoPay API
                  Mb.Nothing -> continue state

eval (PopUpModalAC (PopUpModal.OnButton2Click)) state = case state.props.redirectToNav of
            "Home" -> exit $ HomeScreen state{props { popUpState = Mb.Nothing, redirectToNav = ""}}
            "Rides" -> exit $ RideHistory state{props { popUpState = Mb.Nothing, redirectToNav = ""}}
            "Alert" -> do
              _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
              _ <- pure $ firebaseLogEvent "ny_driver_alert_click"
              exit $ Alerts state{props { popUpState = Mb.Nothing, redirectToNav = ""}}
            "Rankings" -> do
              _ <- pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
              exit $ Contest state{props { popUpState = Mb.Nothing, redirectToNav = ""}}
            _ -> exit $ HomeScreen state{props { popUpState = Mb.Nothing, redirectToNav = ""}}

eval (PopUpModalAC (PopUpModal.OptionWithHtmlClick)) state = continueWithCmd state [pure CallSupport]

eval (PopUpModalAC (PopUpModal.DismissPopup)) state = continue state{props { popUpState = Mb.Nothing}}

eval (ConfirmCancelPopup (PopUpModal.OnButton1Click)) state = continue state { props { confirmCancel = false}}

eval (ConfirmCancelPopup (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ cleverTapCustomEvent "ny_driver_cancel_autopay"
  _ <- pure $ metaLogEvent "ny_driver_cancel_autopay"
  let _ = unsafePerformEffect $ firebaseLogEvent "ny_driver_cancel_autopay"
  updateAndExit state { props{showShimmer = true, confirmCancel = false}} $ CancelAutoPayPlan state { props{showShimmer = true, confirmCancel = false}}

eval (ResumeAutoPay PrimaryButton.OnClick) state = updateAndExit state $ ResumeAutoPayPlan state

eval CancelAutoPayAC state = continue state { props { confirmCancel = true}}

eval ViewAutopayDetails state = continue state{props {subView = PlanDetails }}

eval (BottomNavBarAction (BottomNavBar.OnNavigate screen)) state = do
  let newState = state{props{optionsMenuState = ALL_COLLAPSED}}
  _ <- pure $ spy "screeen" screen
  if screen == "Join" then continue state
  else if state.data.myPlanData.autoPayStatus /= ACTIVE_AUTOPAY then do 
    continue state{props {popUpState = Mb.Just SupportPopup, redirectToNav = screen, optionsMenuState = ALL_COLLAPSED}}
  else do case screen of
            "Home" -> exit $ HomeScreen newState
            "Rides" -> exit $ RideHistory newState
            "Alert" -> do
              _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
              _ <- pure $ firebaseLogEvent "ny_driver_alert_click"
              exit $ Alerts newState
            "Rankings" -> do
              _ <- pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
              exit $ Contest newState
            _ -> continue state

eval ViewPaymentHistory state = exit $ PaymentHistory state{props{optionsMenuState = ALL_COLLAPSED}}

eval ViewHelpCentre state = do
  let prevSubViewState = state.props.subView
  updateAndExit state { props{showShimmer = true, subView = FindHelpCentre, optionsMenuState = ALL_COLLAPSED, prevSubView = prevSubViewState}} $ GotoFindHelpCentre state {props {showShimmer = true, subView = FindHelpCentre, optionsMenuState = ALL_COLLAPSED, prevSubView = prevSubViewState}}

eval (OpenGoogleMap dstLt dstLn) state = updateAndExit state { props{showShimmer = true, subView = FindHelpCentre, optionsMenuState = ALL_COLLAPSED}} $ GoToOpenGoogleMaps state {props {showShimmer = true, subView = FindHelpCentre, optionsMenuState = ALL_COLLAPSED, destLat = dstLt, destLon = dstLn}}

eval RefreshPage state = exit $ Refresh

eval (LoadPlans plans) state = do
  let (UiPlansResp planResp) = plans
  _ <- pure $ setValueToLocalStore DRIVER_SUBSCRIBED "false"
  continue state {
      data{ joinPlanData {allPlans = planListTransformer plans,
                            subscriptionStartDate = (convertUTCtoISC planResp.subscriptionStartTime "Do MMM")}},
      props{showShimmer = false, subView = JoinPlan,  
            joinPlanProps { selectedPlanItem = if (isNothing state.props.joinPlanProps.selectedPlanItem) then getSelectedPlan plans else state.props.joinPlanProps.selectedPlanItem}} }

eval (LoadHelpCentre lat lon kioskLocationList) state = do
  let transformedKioskList = transformKioskLocations kioskLocationList lat lon
  continue state {props {showShimmer = false, subView = FindHelpCentre, kioskLocation = transformedKioskList, noKioskLocation = transformedKioskList == []}}

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

eval (TryAgainButtonAC PrimaryButton.OnClick) state = 
  let updateState = state { props{showShimmer = true}}
    in if state.props.subView == FindHelpCentre then updateAndExit updateState $ RefreshHelpCentre updateState
      else updateAndExit updateState $ Refresh

eval (RetryPaymentAC PrimaryButton.OnClick) state = if state.data.myPlanData.planEntity.id == "" then continue state else
  updateAndExit state $ RetryPayment state state.data.myPlanData.planEntity.id

eval CallSupport state = do
  _ <- pure $ showDialer "08069490091" false
  continue state

eval (CallHelpCenter phone) state = do
  _ <- pure $ showDialer phone false
  continue state

eval (CheckPaymentStatusButton PrimaryButton.OnClick) state = continueWithCmd state [pure CheckPaymentStatus] 

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

parseKioskLocation :: String -> Array KioskLocationRes
parseKioskLocation input = do
  case runExcept (decodeJSON input :: _ (Array KioskLocationRes)) of
    Right res -> res
    Left err -> []

transformKioskLocations :: Array KioskLocationRes -> Number -> Number -> Array KioskLocation
transformKioskLocations arr lat lon = do
  DA.sortBy compareByDistance $ map (\ec -> getItem ec lat lon) arr

compareByDistance :: KioskLocation -> KioskLocation -> Ordering
compareByDistance a b = compare a.distance b.distance

getDistanceBetweenPoints :: Number -> Number -> Number -> Number -> Number
getDistanceBetweenPoints lat1 lon1 lat2 lon2 = do
  let distance = getDistanceBwCordinates lat1 lon1 lat2 lon2
  distance

getItem :: KioskLocationRes -> Number -> Number -> KioskLocation 
getItem ec lat lon = {  longitude : ec.longitude,
          address : ec.address,
          contact : ec.contact,
          latitude : ec.latitude,
          landmark : ec.landmark,
          distance : getDistanceBetweenPoints ec.latitude ec.longitude lat lon }