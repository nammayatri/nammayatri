{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverEarningsScreen.Controller where

import Effect.Unsafe
import Engineering.Helpers.LogEvent
import Prelude
import Components.BottomNavBar.Controller (Action(..)) as BottomNavBar
import Components.Calendar.Controller as CalendarController
import Components.ErrorModal as ErrorModalController
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Common.Types.App (LazyCheck(..))
import Components.PrimaryButton as PrimaryButtonController
import Components.RequestInfoCard as RequestInfoCard
import Control.Monad.Except (runExcept)
import Data.Array as DA
import Data.Array (union, (!!), filter, length, (:), foldl, drop, take, replicate, updateAt, elemIndex, (..), last, find, catMaybes, sortBy, reverse)
import Data.Either
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number (fromString) as NUM
import Data.Show (show)
import Data.String as DS
import Data.String (Pattern(..), split, take, drop)
import Engineering.Helpers.Commons (getCurrentUTC, getFutureDate, getDayName, convertUTCtoISC)
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.Utils (initializeCalendar, saveObject, getCurrentDay)
import Foreign.Generic (decodeJSON)
import Helpers.Utils (checkSpecialPickupZone, getcurrentdate, getDayOfWeek, incrementValueOfLocalStoreKey, getRideLabelData, parseFloat, getRequiredTag, transformBapName ,dummyLocationInfo )
import JBridge (pauseYoutubeVideo)
import Language.Strings (getString)
import RemoteConfig.Utils
import Language.Types
import Log
import PrestoDOM (Eval, update, continue, exit, ScrollState(..), updateAndExit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Resource.Constants (decodeAddress, rideTypeConstructor)
import Screens (ScreenName(..), getScreen)
import Screens.DriverEarningsScreen.Transformer (getEventName)
import Screens.Types (DriverEarningsScreenState, DriverEarningsSubView(..), AnimationState(..), IndividualRideCardState(..), IndividualRideCardState(..), DisabilityType(..))
import Screens.Types as ST
import Services.API (Status(..), CoinTransactionRes(..), CoinTransactionHistoryItem(..), CoinsUsageRes(..), CoinUsageHistoryItem(..), RidesInfo(..), LocationInfo(..), DriverProfileSummaryRes(..), RidesSummary(..))
import Storage (KeyStore(..), getValueToLocalNativeStore, setValueToLocalNativeStore, getValueToLocalStore)
import Timers (clearTimerWithId)
import Debug
import Foreign (unsafeToForeign)
import Resource.Constants as Const
import Helpers.Utils (fetchImage, FetchImageFrom(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    BackPressed -> trackAppBackPress appId (getScreen DRIVER_EARNINGS_SCREEN)
    PrimaryButtonActionController act -> case act of
      PrimaryButtonController.OnClick -> do
        trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "primary_button_action" "next_on_click"
        trackAppEndScreen appId (getScreen DRIVER_EARNINGS_SCREEN)
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "primary_button_action" "no_action"
    NoAction -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "no_action" "no_action"
    (ChangeTab subView) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "change_tab" (show subView)
    (BottomNavBarAction act) -> case act of
      BottomNavBar.OnNavigate screen -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "bottom_nav_bar_action" screen
    (RideHistoryAPIResponseAction _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "ride_history_api_response_action" "ride_history_api_response_action"
    (RideSummaryAPIResponseAction _ _ _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "ride_summary_api_response_action" "ride_summary_api_response_action"
    (UpdateRidesEver _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "update_rides_ever" "update_rides_ever"
    (DriverSummary _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "driver_summary" "driver_summary"
    (CoinTransactionResponseAction _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "coin_transaction_response_action" "coin_transaction_response_action"
    (CoinUsageResponseAction _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "coin_transaction_response_action" "coin_transaction_response_action"
    (CountDown _ _ _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "count_down" "count_down"
    AfterRender -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "after_render" "after_render"
    (SelectPlan _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "select_plan" "select_plan"
    (GenericHeaderAC _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "generic_header_action" "generic_header_action"
    (PopUpModalAC _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "pop_up_modal_action" "pop_up_modal_action"
    (CalendarAC _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "calendar_action" "calendar_action"
    ShowCalendarPopup -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "show_calendar_popup" "show_calendar_popup"
    RenderSlider -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "render_slider" "render_slider"
    (SliderCallback _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "slider_callback" "slider_callback"
    (RequestInfoCardAction _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "request_info_card_action" "request_info_card_action"
    ShowCoinsUsagePopup -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "show_coins_usage_popup" "show_coins_usage_popup"
    (ErrorModalActionController _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "error_modal_action" "error_modal_action"
    (BarViewSelected _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "bar_view_selected" "bar_view_selected"
    (LeftChevronClicked _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "left_chevron_clicked" "left_chevron_clicked"
    (RightChevronClicked _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "right_chevron_clicked" "right_chevron_clicked"
    (OpenFaqQuestion _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "open_faq_question" "open_faq_question"
    ShowPaymentHistory -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "show_payment_history" "show_payment_history"
    FaqViewAction -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "faq_view_action" "faq_view_action"
    RemoveLottie -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "remove_lottie" "remove_lottie"
    (OpenTripDetails _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "open_trip_details" "open_trip_details"
    LoadMore -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "load_more" "load_more"
    YoutubeVideoStatus _ -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "youtube_video_status" "youtube_video_status"
    ShowMyPlanPage -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "my_plan_page" "my_plan_page"

data ScreenOutput
  = GoBack
  | HomeScreen DriverEarningsScreenState
  | Alerts DriverEarningsScreenState
  | Contest DriverEarningsScreenState
  | SubscriptionScreen DriverEarningsScreenState
  | ChangeDriverEarningsTab DriverEarningsSubView DriverEarningsScreenState
  | PurchasePlan DriverEarningsScreenState
  | RefreshScreen DriverEarningsScreenState
  | PaymentHistory
  | TripDetails DriverEarningsScreenState
  | LoaderOutput DriverEarningsScreenState
  | MyPlanPage
  | CoinsEarningInfo DriverEarningsScreenState

data Action
  = NoAction
  | ChangeTab DriverEarningsSubView
  | BackPressed
  | PrimaryButtonActionController PrimaryButtonController.Action
  | BottomNavBarAction BottomNavBar.Action
  | RideHistoryAPIResponseAction (Array RidesInfo)
  | RideSummaryAPIResponseAction (Array RidesSummary) String (Array String)
  | UpdateRidesEver Boolean
  | DriverSummary DriverProfileSummaryRes
  | CoinTransactionResponseAction CoinTransactionRes
  | CoinUsageResponseAction CoinsUsageRes
  | CountDown Int String String
  | AfterRender
  | SelectPlan Int
  | GenericHeaderAC GenericHeader.Action
  | PopUpModalAC PopUpModal.Action
  | CalendarAC CalendarController.Action
  | ShowCalendarPopup
  | RenderSlider
  | SliderCallback Int
  | RequestInfoCardAction RequestInfoCard.Action
  | ShowCoinsUsagePopup
  | ErrorModalActionController ErrorModalController.Action
  | BarViewSelected Int
  | LeftChevronClicked Int
  | RightChevronClicked Int
  | OpenFaqQuestion ST.FaqQuestions
  | ShowPaymentHistory
  | FaqViewAction
  | RemoveLottie
  | OpenTripDetails Int
  | LoadMore
  | YoutubeVideoStatus String
  | ShowMyPlanPage

eval :: Action -> DriverEarningsScreenState -> Eval Action ScreenOutput DriverEarningsScreenState
eval BackPressed state = 
  if state.props.showCoinsUsagePopup then
    continue state { props { showCoinsUsagePopup = false } }
  else if state.props.popupType /= ST.NO_POPUP then
    continue state { props { popupType = ST.NO_POPUP } }
  else 
    case state.props.subView of
      USE_COINS_VIEW -> exit $ ChangeDriverEarningsTab YATRI_COINS_VIEW state
      FAQ_VIEW -> exit $ ChangeDriverEarningsTab USE_COINS_VIEW state
      FAQ_QUESTON_VIEW -> do
        void $ pure $ pauseYoutubeVideo unit
        exit $ ChangeDriverEarningsTab FAQ_VIEW state
      _ -> exit $ HomeScreen (updateToState state)

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = do
  let _ = unsafePerformEffect $ logEventWithMultipleParams state.data.logField  "ny_driver_convert_coins_click" $ [{key : "Number of Coins", value : unsafeToForeign state.data.coinsToUse}]
  exit $ PurchasePlan state

eval (BottomNavBarAction (BottomNavBar.OnNavigate screen)) state = do
  case screen of
    "Home" -> exit $ HomeScreen (updateToState state)
    "Alert" -> do
      void $ pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      exit $ Alerts (updateToState state)
    "Rankings" -> do
      void $ pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
      exit $ Contest (updateToState state)
    "Join" -> do
      exit $ SubscriptionScreen (updateToState state)
    _ -> continue (updateToState state)

eval (ChangeTab subView') state = do
  let _ = unsafePerformEffect $ logEventWithMultipleParams state.data.logField  "ny_driver_earnings_scn_change_tab" $ [{key : "Tab", value : unsafeToForeign (show subView')}]
  if subView' == state.props.subView then continue state else exit $ ChangeDriverEarningsTab subView' state

eval RemoveLottie state = do
  continue state { props { showCoinsEarnedAnim = Nothing } }

eval LoadMore state = do
  exit $ LoaderOutput state{props{loadMoreButtonVisibility = true}}

eval (CoinTransactionResponseAction (CoinTransactionRes resp)) state = do
  void $ pure $ incrementValueOfLocalStoreKey VISITED_DRIVER_COINS_PAGE
  let
    events =
      map
        ( \(CoinTransactionHistoryItem item) ->
            { event: getEventName state item.eventFunction item.bulkUploadTitle
            , timestamp: item.createdAt
            , coins: item.coins
            , cash: 0.0
            , earnings: Nothing
            , destination: Nothing
            , status: Nothing
            , tagImages: []
            , vehicleVariant : ""
            , isValueAddNP : false
            , bapName : ""
            }
        )
        resp.coinTransactionHistory
  let oldCoinValue = fromMaybe 0 $ fromString $ getValueToLocalStore OLD_COIN_BALANCE
      coinDifference = if oldCoinValue < resp.coinBalance then Just (resp.coinBalance - oldCoinValue) else Nothing
      coinsConfig = getCoinsConfigData $ DS.toLower $ getValueToLocalStore DRIVER_LOCATION
  void $ pure $ setValueToLocalNativeStore OLD_COIN_BALANCE $ show resp.coinBalance
  continue
    state
      { data
        { expiringDays = resp.expiringDays
        , expiringCoins = resp.expiringCoins
        , coinsEarned = resp.coinEarned
        , coinsUsed = resp.coinUsed
        , coinBalance = resp.coinBalance
        , coinsEarnedPreviousDay = resp.coinsEarnedPreviousDay
        , coinsEarnedToday = resp.todayCoinSummary
        , coinHistoryItems = events
        , coinsToUse = (resp.coinBalance / coinsConfig.stepFunctionForCoinConversion) * coinsConfig.stepFunctionForCoinConversion
        }
      , props { showShimmer = false, showCoinsEarnedAnim = coinDifference}
      }

eval (CoinUsageResponseAction (CoinsUsageRes resp)) state = do
  let
    usageHistory =
      map
        ( \(CoinUsageHistoryItem item) ->
            { event: item.title
            , timestamp: item.createdAt
            , coins: item.numCoins
            , cash: item.cash
            , earnings: Nothing
            , destination: Nothing
            , status: Nothing
            , tagImages: []
            , vehicleVariant : ""
            , isValueAddNP : false
            , bapName : ""
            }
        )
        resp.coinUsageHistory
  continue
    state
      { data
        { coinBalance = resp.coinBalance
        , usageHistoryItems = union state.data.usageHistoryItems usageHistory
        , totalCoinConvertedToCash = resp.totalCoinConvertedToCash
        , coinConvertedToCashUsedForLatestDues = resp.coinConvertedToCashUsedForLatestDues
        , coinConvertedTocashLeft = resp.coinConvertedTocashLeft
        , coinConversionRate = resp.coinConversionRate
        }
      , props
        { popupType = 
          if resp.coinBalance == 0 && state.props.popupType == ST.NO_POPUP && DS.null state.props.showCoinsRedeemedAnim then
            ST.NO_COINS_POPUP
          else
            state.props.popupType
        , showShimmer = false
        , loadMoreButtonVisibility = length resp.coinUsageHistory >= 10
        }
      }

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [ pure BackPressed ]

eval (PopUpModalAC PopUpModal.OnButton1Click) state = do
  case state.props.popupType of
    ST.COIN_TO_CASH_POPUP -> handlePopupClick state ST.YATRI_COINS_VIEW
    ST.COIN_TO_CASH_FAIL_POPUP -> handlePopupClick state ST.USE_COINS_VIEW
    ST.NO_COINS_POPUP -> continue state { props { popupType = ST.NO_POPUP } }
    ST.COINS_EXPIRING_POPUP -> continue state
    _ -> continue state
  where
  handlePopupClick st newSubView = do
    void $ pure $ clearTimerWithId st.data.timerID
    let
      newState = st { props { subView = newSubView, popupType = ST.NO_POPUP, showCoinsRedeemedAnim = "", showShimmer = true } }
    updateAndExit newState $ RefreshScreen newState

eval (PopUpModalAC PopUpModal.OnButton2Click) state = continueWithCmd state [ pure $ PopUpModalAC PopUpModal.DismissPopup ]

eval (PopUpModalAC PopUpModal.DismissPopup) state = do
  void $ pure $ clearTimerWithId state.data.timerID
  let
    newState = state { props { popupType = ST.NO_POPUP, showCoinsRedeemedAnim = "" } }
  case state.props.popupType of
    ST.NO_COINS_POPUP -> continue newState
    _ -> updateAndExit newState $ RefreshScreen newState

eval (CountDown seconds status timerID) state = do
  if status == "EXPIRED" then do
    void $ pure $ clearTimerWithId state.data.timerID
    if state.props.subView == ST.YATRI_COINS_VIEW then
      continueWithCmd state { data { timerID = "", timer = 4} } [pure RemoveLottie]
    else
      continue state { data { timerID = "", timer = 4 }, props { showCoinsRedeemedAnim = "", popupType = if state.props.coinConvertedSuccess then ST.COIN_TO_CASH_POPUP else ST.COIN_TO_CASH_FAIL_POPUP, coinConvertedSuccess = false } }
  else
    continue $ state { data { timer = seconds, timerID = timerID } }

eval ShowCalendarPopup state = do
  let
    res = initializeCalendar true
  continue state { props { calendarState { weeks = res.weeks, calendarPopup = true, selectedTimeSpan = res.selectedTimeSpan, startDate = res.startDate, endDate = Nothing } } }

eval (CalendarAC CalendarController.HideCalendarPopup) state = continue state { props { calendarState { calendarPopup = false, startDate = Nothing, endDate = Nothing } } }

eval (CalendarAC (CalendarController.DecrementMonth res)) state = continue state { props { calendarState { weeks = res.weeks, selectedTimeSpan = res.selectedTimeSpan } } }

eval (CalendarAC (CalendarController.IncrementMonth res)) state = continue state { props { calendarState { weeks = res.weeks, selectedTimeSpan = res.selectedTimeSpan } } }

eval (CalendarAC (CalendarController.SelectDate res)) state = continue state { props { calendarState { startDate = res.startDate, weeks = res.weeks } } }

eval (CalendarAC (CalendarController.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = do
  let
    newState =
      state
        { props
          { calendarState { calendarPopup = false }
          , date =
            case state.props.calendarState.startDate of
              Nothing -> (getCurrentUTC "")
              Just val -> val.utcDate
          , callRideSummaryApi = false
          }
        }
  updateAndExit newState $ RefreshScreen newState

eval (CalendarAC (CalendarController.PrimaryButtonCancelActionController (PrimaryButtonController.OnClick))) state = do
  continue state { props { calendarState { calendarPopup = false } } }

eval (SliderCallback value) state = continue state { data { coinsToUse = value } }

eval ShowCoinsUsagePopup state = continue state { props { showCoinsUsagePopup = true } }

eval (RequestInfoCardAction RequestInfoCard.Close) state = continue state { props { showCoinsUsagePopup = false } }

eval (RequestInfoCardAction RequestInfoCard.BackPressed) state = continue state { props { showCoinsUsagePopup = false } }

eval (BarViewSelected index) state = do
  let
    mbSelectedBarData = state.props.currWeekData !! index

    selectedBarData =
      { fromDate: maybe "" (\record -> record.rideDate) mbSelectedBarData
      , toDate: ""
      , totalEarnings: maybe 0 (\record -> record.earnings) mbSelectedBarData
      , totalRides: maybe 0 (\record -> record.noOfRides) mbSelectedBarData
      , totalDistanceTravelled: maybe 0 (\record -> record.rideDistance) mbSelectedBarData
      }
  continue state { props { selectedBarIndex = if state.props.selectedBarIndex == index then -1 else index, totalEarningsData = if state.props.selectedBarIndex == index then getTotalCurrentWeekData state.props.currWeekData else selectedBarData } }

eval (LeftChevronClicked currentIndex) state = do
  let
    updatedIndex = if currentIndex == 0 then currentIndex else currentIndex - 1

    currentWeekData = getWeeklyEarningsPercentage (getEarningDataOfWeek updatedIndex state.data.weeklyEarningData)

    currWeekMaxEarning = foldl getMax 0 currentWeekData
  continue state { props { weekIndex = updatedIndex, selectedBarIndex = -1, currWeekData = currentWeekData, totalEarningsData = getTotalCurrentWeekData currentWeekData, currentWeekMaxEarning = currWeekMaxEarning, showShimmer = false } }

eval (RightChevronClicked currentIndex) state = do
  let
    updatedIndex = if currentIndex == 3 then currentIndex else currentIndex + 1

    currentWeekData = getWeeklyEarningsPercentage (getEarningDataOfWeek updatedIndex state.data.weeklyEarningData)

    currWeekMaxEarning = foldl getMax 0 currentWeekData
  continue state { props { weekIndex = updatedIndex, selectedBarIndex = -1, currWeekData = currentWeekData, totalEarningsData = getTotalCurrentWeekData currentWeekData, currentWeekMaxEarning = currWeekMaxEarning } }

eval (RideSummaryAPIResponseAction ridesSummaryList todaysDate datesList) state = do
  let
    earningList = mapSummaryListWithWeeklyEarnings ridesSummaryList

    earningListWithMissingDates = getEarningListWithMissingDates earningList datesList

    dataToFetchAndUpdateFromCache = getEarningsToCache earningListWithMissingDates

    dayOfWeek = getDayOfWeek (getDayName todaysDate)

    earningsToCache = DA.reverse (DA.take (21 + dayOfWeek) (DA.reverse dataToFetchAndUpdateFromCache))

    earningstoCacheWithDriverId = ST.CachedEarningsForDriver { id: getValueToLocalStore DRIVER_ID, earningsData: earningsToCache }
  _ <- pure $ saveObject "RIDE_SUMMARY_DATA" earningstoCacheWithDriverId
  let
    _ = fetchWeekyEarningData RIDE_SUMMARY_DATA

    noOfDaysToNearestSunday = 6 - dayOfWeek

    datesUptoMearestSunday = if noOfDaysToNearestSunday > 0 then map (\x -> getFutureDate todaysDate x) (1 .. noOfDaysToNearestSunday) else []

    allWeeksData = getAllWeeksData earningsToCache earningListWithMissingDates datesUptoMearestSunday

    currentWeekData = getWeeklyEarningsPercentage (getEarningDataOfWeek state.props.weekIndex allWeeksData)

    currWeekMaxEarning = foldl getMax 0 currentWeekData
  continue state { data { weeklyEarningData = allWeeksData }, props { currWeekData = currentWeekData, totalEarningsData = getTotalCurrentWeekData currentWeekData, currentWeekMaxEarning = currWeekMaxEarning } }

eval (RideHistoryAPIResponseAction rideList) state = do
  let earningHistoryItemList = earningHistoryItemsListTransformer rideList
  continue $ state { data { earningHistoryItems = earningHistoryItemList, rideHistoryItems = rideList }, props { showShimmer = false, callRideSummaryApi = true } }

eval (UpdateRidesEver anyRidesAssignedEver) state = continue state { data { anyRidesAssignedEver = anyRidesAssignedEver }, props { weekIndex = 3 } }

eval ShowPaymentHistory state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_view_details_click_coins"
  exit $ PaymentHistory

eval ShowMyPlanPage state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_view_my_plan_click_coins"
  exit $ MyPlanPage

eval FaqViewAction state = continue $ state { props { showShimmer = false } }

eval (OpenFaqQuestion faqQuestion) state = do
  let updatedState = state { props { individualQuestion = faqQuestion, subView = ST.FAQ_QUESTON_VIEW } }
  if faqQuestion.tag == ST.HowEarnLosePoints then exit $ CoinsEarningInfo updatedState else continue updatedState

eval (OpenTripDetails index) state = do
  let 
      tripDetails = rideHistoryItemTransformer $ fromMaybe dummyRideHistoryItem (state.data.rideHistoryItems !! index)
      update = state { data{selectedRideHistoryItem = tripDetails } }
  updateAndExit update $ TripDetails update

eval _ state = update state

mapSummaryListWithWeeklyEarnings :: Array RidesSummary -> Array ST.WeeklyEarning
mapSummaryListWithWeeklyEarnings ridesSummaryList =
  map
    ( \(RidesSummary rideSummary) ->
        { earnings: rideSummary.earnings
        , rideDistance: rideSummary.rideDistance
        , rideDate: rideSummary.rideDate
        , noOfRides: rideSummary.noOfRides
        , percentLength: 0.0
        }
    )
    ridesSummaryList

getEarningListWithMissingDates :: Array ST.WeeklyEarning -> Array String -> Array ST.WeeklyEarning
getEarningListWithMissingDates earningLst dates = do
  let
    x = DA.catMaybes $ map (\d -> (getEarningForDate earningLst d)) dates
  sortBy (comparing _.rideDate) (earningLst <> x)

getEarningForDate :: Array ST.WeeklyEarning -> String -> Maybe ST.WeeklyEarning
getEarningForDate earningLst date =
  let
    foundDate = DA.find (\e -> e.rideDate == date) earningLst
  in
    maybe (Just { earnings: 0, rideDistance: 0, rideDate: date, noOfRides: 0, percentLength: 0.0 }) (\_ -> Nothing) foundDate

getEarningsToCache :: Array ST.WeeklyEarning -> Array ST.WeeklyEarning
getEarningsToCache earningList = do
  let
    cachedData = fetchWeekyEarningData RIDE_SUMMARY_DATA
  fromMaybe [] cachedData <> DA.take (DA.length earningList - 1) earningList

getAllWeeksData :: Array ST.WeeklyEarning -> Array ST.WeeklyEarning -> Array String -> Array ST.WeeklyEarning
getAllWeeksData cachedEarnings earningList dates = do
  let
    objList = map (\x -> { earnings: 0, rideDistance: 0, rideDate: x, noOfRides: 0, percentLength: 0.0 }) dates

    todaysData = maybe [] (\lastEle -> [ lastEle ]) $ DA.last earningList
  cachedEarnings <> todaysData <> objList

rideHistoryItemTransformer :: RidesInfo -> IndividualRideCardState
rideHistoryItemTransformer (RidesInfo ride) =
  let specialLocationConfig = getRideLabelData ride.specialLocationTag
  in
  { date : (convertUTCtoISC (ride.createdAt) "D MMM"),
    time : (convertUTCtoISC (ride.createdAt )"h:mm A"),
    total_amount : (case (ride.status) of
                    "CANCELLED" -> 0
                    _ -> fromMaybe ride.estimatedBaseFare ride.computedFare),
    card_visibility : (case (ride.status) of
                        "CANCELLED" -> "gone"
                        _ -> "visible"),
    shimmer_visibility : "gone",
    rideDistance :  parseFloat (toNumber (fromMaybe 0 ride.chargeableDistance) / 1000.0) 2,
    status :  (ride.status),
    vehicleModel : ride.vehicleModel ,
    shortRideId : ride.shortRideId  ,
    vehicleNumber :  ride.vehicleNumber  ,
    driverName : ride.driverName  ,
    driverSelectedFare : ride.driverSelectedFare  ,
    vehicleColor : ride.vehicleColor  ,
    id : ride.shortRideId,
    updatedAt : ride.updatedAt,
    source : (Const.decodeShortAddress ride.fromLocation),
    destination : maybe "" (\toLocation -> Const.decodeShortAddress toLocation) ride.toLocation,
    vehicleType : ride.vehicleVariant,
    tripType : rideTypeConstructor ride.tripCategory,
    riderName : fromMaybe "" ride.riderName,
    customerExtraFee : ride.customerExtraFee,
    purpleTagVisibility : isJust ride.disabilityTag,
    gotoTagVisibility : isJust ride.driverGoHomeRequestId,
    spLocTagVisibility : ride.specialLocationTag /= Nothing && (getRequiredTag ride.specialLocationTag) /= Nothing,
    specialZoneLayoutBackground : specialLocationConfig.backgroundColor,
    specialZoneImage : specialLocationConfig.imageUrl,
    specialZoneText : specialLocationConfig.text,
    specialZonePickup : checkSpecialPickupZone ride.specialLocationTag,
    tollCharge : fromMaybe 0.0 ride.tollCharges,
    rideType : ride.vehicleServiceTierName,
    tripStartTime : ride.tripStartTime,
    tripEndTime : ride.tripEndTime,
    acRide : ride.isVehicleAirConditioned,
    vehicleServiceTier : ride.vehicleServiceTier,
    parkingCharge : fromMaybe 0.0 ride.parkingCharge, 
    stops : fromMaybe [] ride.stops
  }

getDisabilityType :: Maybe String -> Maybe DisabilityType
getDisabilityType disabilityString = case disabilityString of 
                                      Just "BLIND_LOW_VISION" -> Just BLIND_AND_LOW_VISION
                                      Just "HEAR_IMPAIRMENT" -> Just HEAR_IMPAIRMENT
                                      Just "LOCOMOTOR_DISABILITY" -> Just LOCOMOTOR_DISABILITY
                                      Just "OTHER" -> Just OTHER_DISABILITY
                                      _ -> Nothing

earningHistoryItemsListTransformer :: Array RidesInfo -> Array ST.CoinHistoryItem
earningHistoryItemsListTransformer list =
  ( map
      ( \(RidesInfo ride) ->
          { destination:  (\toLocation -> decodeAddress toLocation false) <$> ride.toLocation
          , timestamp: ride.createdAt
          , earnings:
              case ride.status of
                "CANCELLED" -> Just 0
                _ -> ride.computedFare
          , status: Just ride.status
          , coins: 0
          , event: ""
          , tagImages: getTagImages (RidesInfo ride)
          , cash: 0.0
          , vehicleVariant : ride.vehicleServiceTier
          , isValueAddNP : ride.isValueAddNP
          , bapName : transformBapName $ fromMaybe "" ride.bapName
          }
      )
      list
  )

getTagImages :: RidesInfo -> Array String
getTagImages (RidesInfo ride) =
  let
    tag = getRequiredTag ride.specialLocationTag
    conditionsAndTags = 
      [ {condition: isJust ride.customerExtraFee, tag: fetchImage FF_ASSET "ny_ic_tip_ride_tag"}
      , {condition: isJust ride.disabilityTag, tag: fetchImage FF_ASSET "ny_ic_disability_tag"}
      , {condition: isJust ride.specialLocationTag && isJust tag, tag: fetchImage FF_ASSET "ny_ic_star"}
      , {condition: isJust ride.driverGoHomeRequestId, tag: fetchImage FF_ASSET "ny_ic_goto_home_tag"}
      , {condition: checkSpecialPickupZone ride.specialLocationTag, tag: fetchImage COMMON_ASSET "ny_ic_sp_zone_green"}
      ]
  in
    DA.concatMap (\{condition, tag} -> if condition then [tag] else []) conditionsAndTags

getTotalCurrentWeekData :: Array ST.WeeklyEarning -> ST.TotalEarningsData
getTotalCurrentWeekData barGraphData = do
  let
    firstElement = barGraphData !! 0

    lastElement = barGraphData !! 6

    calculateTotals =
      foldl
        ( \acc record ->
            { totalEarnings: acc.totalEarnings + record.earnings
            , totalDistance: acc.totalDistance + record.rideDistance
            , totalRides: acc.totalRides + record.noOfRides
            }
        )
        { totalEarnings: 0, totalDistance: 0, totalRides: 0 }
        barGraphData
  { fromDate: maybe "" (\x -> x.rideDate) firstElement
  , toDate: maybe "" (\x -> x.rideDate) lastElement
  , totalEarnings: calculateTotals.totalEarnings
  , totalRides: calculateTotals.totalRides
  , totalDistanceTravelled: calculateTotals.totalDistance
  }

fetchWeekyEarningData :: KeyStore -> Maybe (Array ST.WeeklyEarning)
fetchWeekyEarningData name = do
  let
    result = decodeJSON $ getValueToLocalNativeStore name
  case (runExcept $ result) of
    Left err -> Nothing
    Right (ST.CachedEarningsForDriver weeklyEarnings) -> if weeklyEarnings.id == getValueToLocalStore DRIVER_ID then Just weeklyEarnings.earningsData else Nothing

getEarningDataOfWeek :: Int -> Array ST.WeeklyEarning -> Array ST.WeeklyEarning
getEarningDataOfWeek index weeklyEarningData = case index of
  3 -> DA.drop 21 weeklyEarningData
  2 -> DA.take 7 (DA.drop 14 weeklyEarningData)
  1 -> DA.take 7 (DA.drop 7 weeklyEarningData)
  0 -> DA.take 7 weeklyEarningData
  _ -> []

getWeeklyEarningsPercentage :: Array ST.WeeklyEarning -> Array ST.WeeklyEarning
getWeeklyEarningsPercentage weeklyEarningData = map (\x -> x { percentLength = ((toNumber x.earnings) * 100.0) / (toNumber maxValue) }) weeklyEarningData
  where
  maxValue = foldl getMax 0 weeklyEarningData

getMax :: Int -> ST.WeeklyEarning -> Int
getMax num1 obj1 = max obj1.earnings num1

updateToState :: DriverEarningsScreenState -> DriverEarningsScreenState
updateToState state = do
  let
    currentWeekData = getWeeklyEarningsPercentage (getEarningDataOfWeek 3 state.data.weeklyEarningData)

    currWeekMaxEarning = foldl getMax 0 currentWeekData

    date = getcurrentdate ""

    dayName = getDayName date

    noOfDaysToNearestSunday =
      6
        - case DA.elemIndex (DS.take 3 dayName) state.props.weekDay of
            Just index -> index
            Nothing -> 0
  state { props { weekIndex = 3, selectedBarIndex = -1, currWeekData = currentWeekData, totalEarningsData = getTotalCurrentWeekData currentWeekData, currentWeekMaxEarning = currWeekMaxEarning }, data { coinBalance = state.data.coinBalance } }

dummyQuestions :: LazyCheck -> Array ST.FaqQuestions
dummyQuestions lazy =
  let coinsConfig = getCoinsConfigData $ DS.toLower $ getValueToLocalStore DRIVER_LOCATION
      answer4 = getAnswerConfig (getString $ YATRI_POINTS_FAQS_QUES1_ANS4 "YATRI_POINTS_FAQS_QUES1_ANS4" )
  in
    [ { question: getString $ YATRI_POINTS_FAQS_QUES1 "YATRI_POINTS_FAQS_QUES1"
      , videoLink: videoLinkValue coinsConfig.whatAreYatriCoinFAQ
      , answer:
          [ getAnswerConfig (getString $ YATRI_POINTS_FAQS_QUES1_ANS1 "YATRI_POINTS_FAQS_QUES1_ANS1")
          , getAnswerConfig (getString $ YATRI_POINTS_FAQS_QUES1_ANS2 "YATRI_POINTS_FAQS_QUES1_ANS2")
          , getAnswerConfig (getString $ YATRI_POINTS_FAQS_QUES1_ANS3 "YATRI_POINTS_FAQS_QUES1_ANS3")
          , answer4{ hyperLinkText = Just $ getString $ YATRI_POINTS_TNC, hyperLinkUrl = videoLinkValue coinsConfig.coinTermsAndConditions, hyperLinkColor = Nothing}
          ]
      , showTable: false
      , tag : ST.DiscountPoints
      }
    , { question: getString $ YATRI_POINTS_FAQS_QUES2 "YATRI_POINTS_FAQS_QUES2"
      , videoLink: Nothing
      , answer:
          [ getAnswerConfig $ getString $ YATRI_POINTS_FAQS_QUES2_ANS1 $ show coinsConfig.coinsValidTill
          , getAnswerConfig $ getString $ YATRI_POINTS_FAQS_QUES2_ANS2 $ show coinsConfig.coinsValidTill
          , getAnswerConfig $ getString $ YATRI_POINTS_FAQS_QUES2_ANS3
          ]
      , showTable: false
      , tag : ST.PointsValidity
      }
    , { question: getString $ YATRI_POINTS_FAQS_QUES3 "YATRI_POINTS_FAQS_QUES3"
      , videoLink: videoLinkValue coinsConfig.howToEarnYatriCoinFAQ
      , answer:
          [ getAnswerConfig $ getString $ YATRI_POINTS_FAQS_QUES3_ANS1 "YATRI_POINTS_FAQS_QUES3_ANS1"
          , getAnswerConfig $ getString $ YATRI_POINTS_FAQS_QUES3_ANS2 "YATRI_POINTS_FAQS_QUES3_ANS2"
          ]
      , showTable: false
      , tag : ST.HowEarnPoints
      }
    , { question: getString $ YATRI_POINTS_FAQS_QUES4 "YATRI_POINTS_FAQS_QUES4"
      , videoLink: videoLinkValue coinsConfig.howToRedeemYatriCoinFAQ
      , answer:
          [ getAnswerConfig $ getString $ YATRI_POINTS_FAQS_QUES4_ANS1 "YATRI_POINTS_FAQS_QUES4_ANS1"
          , getAnswerConfig $ getString $ YATRI_POINTS_FAQS_QUES4_ANS2 "YATRI_POINTS_FAQS_QUES4_ANS2"
          , getAnswerConfig $ getString $ YATRI_POINTS_FAQS_QUES4_ANS3 "YATRI_POINTS_FAQS_QUES4_ANS3"
          ]
      , showTable: false
      , tag : ST.HowUsePoints
      }
    , { question: getString $ YATRI_POINTS_FAQS_QUES5 "YATRI_POINTS_FAQS_QUES5"
      , videoLink: Nothing
      , answer:
          [ getAnswerConfig $ getString $ YATRI_POINTS_FAQS_QUES5_ANS1 "YATRI_POINTS_FAQS_QUES5_ANS1"
          , getAnswerConfig $ getString $ YATRI_POINTS_FAQS_QUES5_ANS2 "YATRI_POINTS_FAQS_QUES5_ANS2"
          ]
      , showTable: false
      , tag : ST.PointsEarnEligibility
      }
    , { question: getString $ YATRI_POINTS_FAQS_QUES6 "YATRI_POINTS_FAQS_QUES6"
      , videoLink: Nothing
      , answer:
          [ getAnswerConfig $ getString $ YATRI_POINTS_FAQS_QUES6_ANS1 "YATRI_POINTS_FAQS_QUES6_ANS1"
          ]
      , showTable: true
      , tag : ST.HowEarnLosePoints
      }
    ]

getAnswerConfig :: String -> ST.AnswerConfig
getAnswerConfig value = { 
  answer : value, 
  hyperLinkText : Nothing,
  hyperLinkUrl : Nothing,
  hyperLinkColor : Nothing
}

videoLinkValue :: String -> Maybe String
videoLinkValue value = if value == "" then Nothing else Just value

dummyRideHistoryItem :: RidesInfo
dummyRideHistoryItem = RidesInfo {
      status : "",
      computedFare : Nothing,
      vehicleModel : "",
      createdAt : "",
      driverNumber : Nothing,
      shortRideId : "",
      vehicleNumber : "",
      driverName : "",
      driverSelectedFare : 0,
      chargeableDistance : Nothing,
      actualRideDistance : Nothing,
      vehicleVariant : "",
      estimatedBaseFare : 0,
      vehicleColor : "",
      id : "",
      updatedAt : "",
      riderName : Nothing,
      rideRating : Nothing,
      fromLocation : dummyLocationInfo,
      toLocation : Just dummyLocationInfo,
      estimatedDistance : 0,
      exoPhone : "",
      specialLocationTag : Nothing,
      requestedVehicleVariant : Nothing,
      customerExtraFee : Nothing,
      disabilityTag : Nothing,
      payerVpa : Nothing,
      autoPayStatus : Nothing,
      driverGoHomeRequestId : Nothing,
      isFreeRide : Nothing,
      enableFrequentLocationUpdates : Nothing,
      tripCategory : Nothing,
      tripScheduledAt : Nothing,
      tripStartTime : Nothing,
      tripEndTime : Nothing,
      nextStopLocation : Nothing,
      lastStopLocation : Nothing,
      stopLocationId : Nothing,
      estimatedDuration : Nothing,
      actualDuration : Nothing,
      startOdometerReading : Nothing,
      endOdometerReading : Nothing,
      tollCharges : Nothing,
      estimatedTollCharges : Nothing,
      isOdometerReadingsRequired : Nothing,
      vehicleServiceTierName : "",
      vehicleServiceTier : "",
      isVehicleAirConditioned : Nothing,
      vehicleCapacity : Nothing,
      tollConfidence : Nothing,
      bookingType : Nothing,
      bapName : Nothing,
      isValueAddNP : false,
      roundTrip : false,
      returnTime : Nothing,
      parkingCharge : Nothing,
      senderDetails : Nothing,
      receiverDetails : Nothing,
      coinsEarned : Nothing,
      stops : Nothing
}
