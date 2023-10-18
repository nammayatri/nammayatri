module Screens.SubscriptionScreen.Controller where

import Debug

import Common.Types.App (APIPaymentStatus, LazyCheck(..))
import Components.Banner as Banner
import Components.BottomNavBar as BottomNavBar
import Components.DueDetailsList.Controller (Action(..)) as DueDetailsListController
import Components.OptionsMenu as OptionsMenu
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Control.Monad.Except (runExcept)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Int as DI
import Data.Lens ((^.))
import Data.Maybe as Mb
import Data.Number (fromString) as Number
import Data.Number.Format (fixed, toStringWith)
import Data.String (toLower)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (convertUTCtoISC, getCurrentUTC)
import Engineering.Helpers.Utils (saveObject)
import Foreign (unsafeToForeign)
import Foreign.Generic (decodeJSON)
import Helpers.Utils (fetchImage, FetchImageFrom(..), getDistanceBwCordinates, getFixedTwoDecimals)
import JBridge (cleverTapCustomEvent, firebaseLogEvent, minimizeApp, setCleverTapUserProp, openUrlInApp, showDialer, openWhatsAppSupport, metaLogEvent)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (class Show, Unit, bind, map, negate, not, pure, show, unit, ($), (&&), (*), (-), (/=), (==), discard, Ordering, compare, (<=), (>), (>=), (||), (<>))
import Presto.Core.Types.API (ErrorResponse)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Resource.Constants as Const
import Screens (getScreen, ScreenName(..))
import Screens.SubscriptionScreen.Transformer (alternatePlansTransformer, constructDues, getAutoPayDetailsList, getSelectedId, getSelectedPlan, myPlanListTransformer, planListTransformer)
import Screens.Types (AutoPayStatus(..), KioskLocation(..), OptionsMenuState(..), PlanCardConfig, SubscribePopupType(..), SubscriptionScreenState, SubscriptionSubview(..))
import Services.API (BankError(..), FeeType, GetCurrentPlanResp(..), KioskLocationRes(..), MandateData(..), OfferEntity(..), PaymentBreakUp(..), PlanEntity(..), UiPlansResp(..))
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
            | OneTimeSettlement PrimaryButton.Action
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
            | ViewDuesOverView
            | ViewDueDetails FeeType
            | ClearManualDues PrimaryButton.Action
            | DueDetailsListAction DueDetailsListController.Action
            | OfferCardBanner Banner.Action
            | TogglePlanDescription PlanCardConfig

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
                    | ClearDues SubscriptionScreenState
                    | SubscribeAPI SubscriptionScreenState

eval :: Action -> SubscriptionScreenState -> Eval Action ScreenOutput SubscriptionScreenState
eval BackPressed state = 
  if state.props.popUpState == Mb.Just SupportPopup then updateAndExit state{props{popUpState = Mb.Nothing}} $ HomeScreen state{props{popUpState = Mb.Nothing}}
  else if ( not Mb.isNothing state.props.popUpState && not (state.props.popUpState == Mb.Just SuccessPopup)) then continue state{props { popUpState = Mb.Nothing}}
  else if state.props.optionsMenuState /= ALL_COLLAPSED then continue state{props{optionsMenuState = ALL_COLLAPSED}}
  else if state.props.confirmCancel then continue state{props { confirmCancel = false}}
  else if state.props.subView == ManagePlan then continue state{props { subView = MyPlan}}
  else if state.props.subView == DuesView then continue state{props { subView = MyPlan}}
  else if state.props.subView == DueDetails then do
    let subView' = if state.props.myPlanProps.multiTypeDues then DuesView else MyPlan
    continue state{props { subView = subView'}, data{myPlanData{selectedDue = ""}}}
  else if state.props.subView == PlanDetails then continue state{props { subView = if state.data.config.optionsMenuItems.managePlan then ManagePlan else MyPlan}}
  else if state.props.subView == FindHelpCentre then continue state {props { subView = state.props.prevSubView, kioskLocation = [], noKioskLocation = false, showError = false}}
  else if state.props.myPlanProps.isDueViewExpanded then continue state{props { myPlanProps{isDueViewExpanded = false}}}
  else if state.data.myPlanData.autoPayStatus /= ACTIVE_AUTOPAY && not state.props.isEndRideModal && not state.data.config.enableIntroductoryView && state.data.config.enableSubscriptionPopups then continue state{props { popUpState = Mb.Just SupportPopup}}
  else exit $ HomeScreen state

eval ToggleDueDetails state = continue state {props {myPlanProps { isDuesExpanded = not state.props.myPlanProps.isDuesExpanded}}}

eval ToggleDueDetailsView state = continue state {props {myPlanProps { isDueViewExpanded = not state.props.myPlanProps.isDueViewExpanded}}}

eval (OptionsMenuAction (OptionsMenu.ItemClick item)) state = 
  continueWithCmd state{props{optionsMenuState = ALL_COLLAPSED}} [
    case item of 
      "manage_plan" -> pure ManagePlanAC 
      "payment_history" -> pure ViewPaymentHistory 
      "call_support" -> pure CallSupport
      "chat_for_help" -> do
          _ <- openUrlInApp state.data.config.whatsappSupportLink
          pure NoAction
      "view_faq" -> do
          _ <- openUrlInApp state.data.config.faqLink
          pure NoAction
      "find_help_centre" -> pure ViewHelpCentre
      "view_autopay_details" -> pure ViewAutopayDetails
      _ -> pure NoAction
  ]

eval (OptionsMenuAction (OptionsMenu.BackgroundClick)) state = 
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
                  Mb.Just FailedPopup ->  continue state{props { popUpState = Mb.Nothing}}
                  Mb.Just DuesClearedPopup -> exit $ Refresh
                  Mb.Just SwitchedPlan -> exit $ Refresh
                  Mb.Just SupportPopup -> continueWithCmd state [pure CallSupport]
                  Mb.Just CancelAutoPay -> exit $ CancelAutoPayPlan state -- CancelAutoPay API
                  Mb.Just PaymentSuccessPopup -> updateAndExit state { props{showShimmer = true, popUpState = Mb.Nothing}} $ Refresh
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

eval (ResumeAutoPay PrimaryButton.OnClick) state = 
  case state.data.myPlanData.manualDueAmount > 0.0, state.data.myPlanData.autoPayStatus of
    true, ACTIVE_AUTOPAY -> updateAndExit state $ ClearDues state
    false, SUSPENDED -> updateAndExit state $ ResumeAutoPayPlan state
    _,_ -> updateAndExit state $ SubscribeAPI state

eval (RetryPaymentAC PrimaryButton.OnClick) state =
  case state.props.lastPaymentType of
    Mb.Just "CLEAR_DUE" -> updateAndExit state $ ClearDues state
    Mb.Just "AUTOPAY_REGISTRATION" -> updateAndExit state $ SubscribeAPI state
    _ -> continueWithCmd state [ pure $ ResumeAutoPay PrimaryButton.OnClick]

eval (OneTimeSettlement PrimaryButton.OnClick) state = updateAndExit state $ ClearDues state

eval CancelAutoPayAC state = continue state { props { confirmCancel = true}}

eval ViewAutopayDetails state = continue state{props {subView = PlanDetails }}

eval (BottomNavBarAction (BottomNavBar.OnNavigate screen)) state = do
  let newState = state{props{optionsMenuState = ALL_COLLAPSED, myPlanProps{ isDueViewExpanded = false }}}
  if screen == "Join" then continue state
  else if state.data.myPlanData.autoPayStatus /= ACTIVE_AUTOPAY && state.data.config.enableSubscriptionPopups then do 
    continue state{props {popUpState = Mb.Just SupportPopup, redirectToNav = screen, optionsMenuState = ALL_COLLAPSED, myPlanProps{ isDueViewExpanded = false }}}
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
      data{ joinPlanData {allPlans = planListTransformer plans state.data.config.enableIntroductoryView state.data.config.gradientConfig,
                            subscriptionStartDate = (convertUTCtoISC planResp.subscriptionStartTime "Do MMM")}},
      props{showShimmer = false, subView = JoinPlan,  
            joinPlanProps { selectedPlanItem = if (Mb.isNothing state.props.joinPlanProps.selectedPlanItem) then getSelectedPlan plans state.data.config.gradientConfig else state.props.joinPlanProps.selectedPlanItem}} }

eval (LoadHelpCentre lat lon kioskLocationList) state = do
  let transformedKioskList = transformKioskLocations kioskLocationList lat lon
  continue state {props {showShimmer = false, subView = FindHelpCentre, kioskLocation = transformedKioskList, noKioskLocation = transformedKioskList == []}}

eval (LoadMyPlans plans) state = do
  let (GetCurrentPlanResp currentPlanResp) = plans
  case currentPlanResp.currentPlanDetails of
    Mb.Just (planEntity') -> do
      _ <- pure $ setValueToLocalStore DRIVER_SUBSCRIBED "true"
      let (PlanEntity planEntity) = planEntity'
          requiredBalance = case (planEntity.bankErrors DA.!! 0) of
                              Mb.Just (BankError error) -> if error.code == "Z9" then Mb.Just error.amount else Mb.Nothing -- Checking for code of low account balance
                              Mb.Nothing -> Mb.Nothing
          isOverdue = planEntity.currentDues >= planEntity.totalPlanCreditLimit
          multiTypeDues = (planEntity.autopayDues > 0.0) && (planEntity.currentDues - planEntity.autopayDues > 0.0)
          newState = if state.data.config.enableSubscriptionPopups then 
                        state{ 
                            props{ showShimmer = false, subView = MyPlan, lastPaymentType = currentPlanResp.lastPaymentType, myPlanProps{ multiTypeDues = multiTypeDues, overDue = isOverdue } }, 
                            data { orderId = currentPlanResp.orderId, 
                                  planId = planEntity.id, 
                                  myPlanData {
                                      planEntity = myPlanListTransformer planEntity' currentPlanResp.isLocalized state.data.config.gradientConfig,
                                      maxDueAmount = planEntity.totalPlanCreditLimit,
                                      totalDueAmount = planEntity.currentDues,
                                      manualDueAmount = planEntity.currentDues - planEntity.autopayDues,
                                      autoPayDueAmount = planEntity.autopayDues ,
                                      autoPayStatus = getAutopayStatus currentPlanResp.autoPayStatus, 
                                      lowAccountBalance = requiredBalance,
                                      dueItems = constructDues planEntity.dues
                                  }}
                         }
                     else state{ 
                            props{ showShimmer = false, subView = MyPlan , lastPaymentType = currentPlanResp.lastPaymentType}, 
                            data { orderId = currentPlanResp.orderId, 
                                  planId = planEntity.id, 
                                  myPlanData {
                                      planEntity = myPlanListTransformer planEntity' currentPlanResp.isLocalized state.data.config.gradientConfig,
                                      maxDueAmount = planEntity.totalPlanCreditLimit,
                                      autoPayStatus = getAutopayStatus currentPlanResp.autoPayStatus
                                  }}
                          }
      _ <- pure $ setCleverTapUserProp [{key : "Plan", value : unsafeToForeign planEntity.name},
                                        {key : "Subscription Offer", value : unsafeToForeign (map (\(OfferEntity offer) -> offer.title) planEntity.offers)},
                                        {key : "Driver Due Amount" , value : unsafeToForeign (planEntity.currentDues)},
                                        {key : "Autopay status", value : unsafeToForeign (Mb.fromMaybe "Nothing" currentPlanResp.autoPayStatus)},
                                        {key : "Search Request Eligibility", value : unsafeToForeign $ if isOverdue then "FALSE" else "TRUE"},
                                        {key : "Driver AutoPay Dues", value : unsafeToForeign planEntity.autopayDues},
                                        {key : "Driver Manual Dues", value : unsafeToForeign $ planEntity.currentDues - planEntity.autopayDues}]


      case currentPlanResp.mandateDetails of 
        Mb.Nothing -> continue newState
        Mb.Just (MandateData mandateDetails) -> do
          let pspLogo = Const.getPspIcon $ (Mb.fromMaybe "" mandateDetails.payerVpa)
          _ <- pure $ setCleverTapUserProp [{key : "Mandate status", value : unsafeToForeign mandateDetails.status},
                                            {key : "Autopay Max Amount Register", value : unsafeToForeign mandateDetails.maxAmount},
                                            {key : "Payment method" , value : unsafeToForeign $ if mandateDetails.status == "ACTIVE" then "Autopay" else "Manual"},
                                            {key : "UPI ID availability", value : unsafeToForeign $ if Mb.isNothing mandateDetails.payerVpa then "FALSE" else "TRUE"}]
          continue newState 
            {data {
                myPlanData {
                  mandateStatus = toLower mandateDetails.status
                }
              , autoPayDetails {
                  detailsList = getAutoPayDetailsList (MandateData mandateDetails)
                , pspLogo = fetchImage FF_ASSET pspLogo
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

eval CallSupport state = do
  _ <- pure $ showDialer state.data.config.supportNumber false
  continue state

eval (CallHelpCenter phone) state = do
  _ <- pure $ showDialer phone false
  continue state

eval (CheckPaymentStatusButton PrimaryButton.OnClick) state = continueWithCmd state [pure CheckPaymentStatus] 

eval ViewDuesOverView state = continue state{props{subView = DuesView}}

eval (ViewDueDetails dueType) state = continue state{props{subView = DueDetails, myPlanProps {dueType = dueType}}}

eval (ClearManualDues PrimaryButton.OnClick) state = updateAndExit state $ ClearDues state

eval (DueDetailsListAction (DueDetailsListController.SelectDue dueItem)) state = continue state {data {myPlanData{selectedDue = if state.data.myPlanData.selectedDue == dueItem.id then "" else dueItem.id}}}

eval (TogglePlanDescription _) state = continue state{data{myPlanData{planEntity{isSelected = not state.data.myPlanData.planEntity.isSelected}}}}

eval _ state = continue state

getPlanPrice :: Array PaymentBreakUp -> String -> String
getPlanPrice fares priceType = do
  let price = (DA.filter(\(PaymentBreakUp item) -> item.component == priceType) fares)
  case price DA.!! 0 of
    Mb.Just (PaymentBreakUp element) -> getFixedTwoDecimals element.amount
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
      "MANDATE_FAILED" -> MANDATE_FAILED
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