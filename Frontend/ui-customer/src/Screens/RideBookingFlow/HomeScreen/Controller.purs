{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.Controller where

import Common.Types.App (EventPayload(..), GlobalPayload(..), LazyCheck(..), OptionButtonList, Payload(..), RateCardType(..), FeedbackAnswer(..), ProviderType(..))
import Accessor (_estimatedFare, _estimateId, _vehicleVariant, _status, _estimateFareBreakup, _title, _totalFareRange, _maxFare, _minFare, _nightShiftRate, _nightShiftEnd, _nightShiftMultiplier, _nightShiftStart, _selectedQuotes, _specialLocationTag, _contents, _toLocation, _lat, _lon, _otpCode, _list, _fareProductType, _stopLocation, _toLocation)
import Components.Banner as Banner
import Components.MessagingView as MessagingView
import Components.MessagingView.Controller as MessagingView
import Components.ChooseVehicle as ChooseVehicleController
import Components.ChooseYourRide as ChooseYourRide
import Components.ChooseYourRide.Controller as ChooseYourRideController
import Components.DriverInfoCard.Controller as DriverInfoCardController
import Components.EmergencyHelp as EmergencyHelpController
import Components.ErrorModal.Controller as ErrorModalController
import Components.FavouriteLocationModel as FavouriteLocationModelController
import Components.GenericHeader.Controller as GenericHeaderController
import Components.LocationListItem.Controller as LocationListItemController
import Components.LocationTagBar as LocationTagBarController
import Components.LocationTagBarV2 as LocationTagBarV2Controller
import Components.MenuButton as MenuButton
import Components.MenuButton as MenuButton
import Components.RideCompletedCard.Controller as RideCompletedCard
import Components.MenuButton.Controller (Action(..)) as MenuButtonController
import Components.PopUpModal.Controller as PopUpModal
import Components.PricingTutorialModel.Controller as PricingTutorialModelController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.TipsView as TipsView
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.QuoteListItem.Controller as QuoteListItemController
import Components.QuoteListModel.Controller as QuoteListModelController
import Components.QuoteListModel.View (dummyQuoteList)
import Components.RateCard as RateCard
import Components.RatingCard as RatingCard
import Components.RequestInfoCard as RequestInfoCard
import Components.SaveFavouriteCard as SaveFavouriteCardController
import Components.SavedLocationCard.Controller as SavedLocationCardController
import Components.SearchLocationModel.Controller as SearchLocationModelController
import Components.SelectListModal.Controller as CancelRidePopUp
import Components.SettingSideBar.Controller as SettingSideBarController
import Components.SourceToDestination.Controller as SourceToDestinationController
import Components.Referral as ReferralComponent
import Constants (defaultDensity, languageKey)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Constants.Configs (getPolylineAnimationConfig)
import Helpers.Ride
import Control.Transformers.Back.Trans (runBackT)
import Data.Array ((!!), filter, null, any, snoc, length, head, last, sortBy, union, elem, findIndex, reverse, sortWith, foldl, index, mapWithIndex, find, updateAt, insert, delete, tail, singleton, take, drop)
import Data.Function.Uncurried (runFn3, runFn2)
import Data.Int (toNumber, round, fromString, fromNumber, ceil)
import Data.Lens ((^.), view)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number (fromString, round) as NUM
import Data.String as STR
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (runEffectFn1, runEffectFn9, runEffectFn2)
import Engineering.Helpers.Commons
import Engineering.Helpers.Events as Events
import Engineering.Helpers.LogEvent (logEvent, logEventWithTwoParams, logEventWithMultipleParams)
import Engineering.Helpers.Suggestions (getMessageFromKey, getSuggestionsfromKey, emChatSuggestion, chatSuggestion)
import Foreign (unsafeToForeign)
import Foreign.Class (encode)
import JBridge (showMarker, animateCamera, currentPosition, exitLocateOnMap, firebaseLogEvent, firebaseLogEventWithParams, firebaseLogEventWithTwoParams, getCurrentPosition, hideKeyboardOnNavigation, isLocationEnabled, isLocationPermissionEnabled, locateOnMap, minimizeApp, openNavigation, openUrlInApp,openUrlInMailApp, removeAllPolylines, removeMarker, requestKeyboardShow, requestLocation, shareTextMessage, showDialer, toast, toggleBtnLoader, goBackPrevWebPage, stopChatListenerService, sendMessage, getCurrentLatLong, isInternetAvailable, emitJOSEvent, startLottieProcess, getSuggestionfromKey, scrollToEnd, lottieAnimationConfig, methodArgumentCount, getChatMessages, scrollViewFocus, getLayoutBounds, updateInputString, checkAndAskNotificationPermission, locateOnMapConfig, addCarouselWithVideoExists, pauseYoutubeVideo, cleverTapCustomEvent, getKeyInSharedPrefKeys, generateSessionId, enableMyLocation, setMapPadding, defaultMarkerConfig, drawRoute, showDateTimePicker, removeAllMarkers)
import Helpers.Utils (addToRecentSearches, getCurrentLocationMarker, getDistanceBwCordinates, getLocationName, getScreenFromStage, getSearchType, parseNewContacts, performHapticFeedback, setText, terminateApp, withinTimeRange, toStringJSON, secondsToHms, updateLocListWithDistance, getPixels, getDeviceDefaultDensity, getDefaultPixels, getAssetsBaseUrl, getCityConfig, compareDate, getCurrentDatev2, getDateAfterNDaysv2, decodeBookingTimeList, encodeBookingTimeList, invalidBookingTime, shuffle)
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, printLog, trackAppTextInput, trackAppScreenEvent, logInfo, logStatus)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (class Applicative, class Show, Unit, Ordering, bind, compare, discard, map, negate, pure, show, unit, not, ($), (&&), (-), (/=), (<>), (==), (>), (||), (>=), void, (<), (*), (<=), (/), (+), when, (<<<), (*>))
import Control.Monad (unless)
import Presto.Core.Types.API (ErrorResponse)
import PrestoDOM (BottomSheetState(..), Eval, update, ScrollState(..), Visibility(..), continue, continueWithCmd, defaultPerformLog, exit, payload, updateAndExit, updateWithCmdAndExit)

import Resources.Constants (encodeAddress, getAddressFromBooking, decodeAddress, cancelReasons, dummyCancelReason,  emergencyContactInitialChatSuggestionId, DecodeAddress(..), mailToLink)
import Constants (defaultDensity)
import Screens (ScreenName(..), getScreen)
import Screens.AddNewAddressScreen.Controller (validTag, getSavedTagsFromHome)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.HomeScreen.Transformer (dummyRideAPIEntity, getDriverInfo, getEstimateList, getQuoteList, getQuotesTransformer, transformContactList, getNearByDrivers, dummyEstimateEntity, filterSpecialZoneAndInterCityQuotes, getFareProductType, extractFareProductType, getEstimateIdFromSelectedServices, getSpecialZoneQuotes)
import Screens.RideBookingFlow.HomeScreen.Config
import Screens.SuccessScreen.Handler as UI
import Screens.Types (CallType(..), CardType(..), CurrentLocationDetails, CurrentLocationDetailsWithDistance(..), HomeScreenState, LocationItemType(..), LocationListItemState, PopupType(..), RatingCard, SearchLocationModelType(..), SearchResultType(..), SheetState(..), SpecialTags, Stage(..), TipViewStage(..), ZoneType(..), Trip, BottomNavBarIcon(..), City(..), ReferralStatus(..), NewContacts(..), City(..), CancelSearchType(..))
import Services.API (BookingLocationAPIEntity(..), EstimateAPIEntity(..), FareRange, GetDriverLocationResp, GetQuotesRes(..), GetRouteResp, LatLong(..), OfferRes, PlaceName(..), QuoteAPIEntity(..), RideBookingRes(..), SelectListRes(..), GetEditLocResultResp(..), BookingUpdateRequestDetails(..),  SelectedQuotes(..), RideBookingAPIDetails(..), GetPlaceNameResp(..), RideBookingListRes(..), FollowRideRes(..), Followers(..), Route(..), RideAPIEntity(..), RideBookingDetails(..))
import Services.Backend as Remote
import Services.Config (getDriverNumber, getSupportNumber)
import Storage (KeyStore(..), isLocalStageOn, updateLocalStage, getValueToLocalStore, setValueToLocalStore, getValueToLocalNativeStore, setValueToLocalNativeStore, deleteValueFromLocalStore)
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Types.App (defaultGlobalState)
import Screens.RideBookingFlow.HomeScreen.Config (reportIssueOptions, metersToKm, safetyIssueOptions)
import Screens.Types (TipViewData(..) , TipViewProps(..), RateCardDetails, PermissionScreenStage(..), SuggestionsMap(..), SosBannerType(..), ReferralType(..), ReferralStage(..))
import Screens.Types as ST
import Engineering.Helpers.Suggestions (getMessageFromKey, getSuggestionsfromKey)
import PrestoDOM.Properties (sheetState) as PP
import Screens.RideBookingFlow.HomeScreen.Config(reportIssueOptions)
import Data.Function (const)
import Data.List ((:))
import Common.Resources.Constants (zoomLevel, pickupZoomLevel)
import Screens.RideBookingFlow.HomeScreen.Config
import Data.Function.Uncurried
import Data.Function.Uncurried (Fn3, runFn3, Fn1, runFn1)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Timers (clearTimerWithId)
import Mobility.Prelude (boolToInt, toBool)
import SuggestionUtils
import Data.Tuple (Tuple(..))
import PrestoDOM.Core (getPushFn)
import Components.BannerCarousel as BannerCarousel
import PrestoDOM.List
import PrestoDOM.Core
import Locale.Utils (getLanguageLocale)
import RemoteConfig as RC
import Screens.RideBookingFlow.HomeScreen.BannerConfig
import Components.PopupWithCheckbox.Controller as PopupWithCheckboxController
import LocalStorage.Cache (getValueFromCache, setValueToCache, getFromCache, setInCache, removeValueFromCache)
import DecodeUtil (getAnyFromWindow, stringifyJSON, decodeForeignAny, parseJSON, decodeForeignAnyImpl)
import JBridge as JB
import Helpers.SpecialZoneAndHotSpots (zoneLabelIcon, getSpecialTag)
import Engineering.Helpers.Utils as EHU
import Engineering.Helpers.Commons as EHC
import Components.ServiceTierCard.View as ServiceTierCard
import Components.ProviderModel as PM
import Common.Types.App as CTP
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import Screens.Types (FareProductType(..)) as FPT
import Helpers.TipConfig
import Helpers.Utils as HU
import ConfigProvider
import Screens.HomeScreen.Controllers.Types
import Data.Show.Generic 
import Components.CommonComponentConfig as CommonComponentConfig
import Data.String as DS

-- Controllers 
import Screens.HomeScreen.Controllers.CarouselBannerController as CarouselBannerController



eval2 :: Action -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
eval2 action  = 
  case action of 
    ------------------------ Carousel Banner Controller ------------------------
    BannerCarousel a -> CarouselBannerController.bannerCarouselAC a 
    MetroTicketBannerClickAC a -> CarouselBannerController.metroTicketBannerClickAC a
    DisabilityBannerAC a -> CarouselBannerController.disabilityBannerAC a
    TicketBookingFlowBannerAC a -> CarouselBannerController.ticketBookingFlowBannerAC a
    MetroTicketBookingBannerAC a -> CarouselBannerController.metroTicketBannerClickAC a
    GenderBannerModal a -> CarouselBannerController.genderBannerModal a
    SafetyBannerAction a -> CarouselBannerController.safetyBannerAction a
    UpdateBanner -> CarouselBannerController.updateBanner

    ------------------------ Other Home Screen Controller ------------------------
    _ -> eval action 



eval :: Action -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState

eval (UpdatePickupLocation key lat lon) state = do
  let latitude = fromMaybe 0.0 (NUM.fromString lat)
      longitude = fromMaybe 0.0 (NUM.fromString lon)
  if os == "IOS" && not state.props.locateOnMapProps.cameraAnimatedToSource && (getDistanceBwCordinates latitude longitude state.props.sourceLat state.props.sourceLong) > 5.0 then do
    continueWithCmd state{ props{ locateOnMapProps{ cameraAnimatedToSource = true } } } [do
      void $ animateCamera state.props.sourceLat state.props.sourceLong 25.0 "NO_ZOOM"
      pure NoAction
    ]
  else do
    let updatedState = state{ props{ locateOnMapProps{ cameraAnimatedToSource = true } } }
        sourceManuallyMoved = true
    case key of
      "LatLon" -> do
        let selectedSpot = head (filter (\spots -> (getDistanceBwCordinates (fromMaybe 0.0 (NUM.fromString lat)) (fromMaybe 0.0 (NUM.fromString lon)) spots.lat spots.lng) * 1000.0 < (toNumber JB.locateOnMapConfig.thresholdDistToSpot) ) updatedState.data.nearByPickUpPoints)
        exit $ UpdatePickupName updatedState{props{defaultPickUpPoint = "", rideSearchProps{ sourceManuallyMoved = sourceManuallyMoved}, hotSpot{ selectedSpot = selectedSpot }, locateOnMapProps{ isSpecialPickUpGate = false }}} latitude longitude
      _ -> do
        let focusedIndex = findIndex (\item -> item.place == key) updatedState.data.nearByPickUpPoints
            spot = (filter(\item -> item.place == key) updatedState.data.nearByPickUpPoints) !! 0
        case focusedIndex, spot of
          Just index, Just spot' -> do
            _ <- pure $ scrollViewFocus (getNewIDWithTag "scrollViewParent") index
            exit $ UpdatePickupName updatedState{props{defaultPickUpPoint = key, rideSearchProps{ sourceManuallyMoved = sourceManuallyMoved}, locateOnMapProps{ isSpecialPickUpGate = fromMaybe false spot'.isSpecialPickUp }, hotSpot{ centroidPoint = Nothing }}} spot'.lat spot'.lng
          _, _ -> continue updatedState

eval (UpdateLocation key lat lon) state = do
  let latitude = fromMaybe 0.0 (NUM.fromString lat)
      longitude = fromMaybe 0.0 (NUM.fromString lon)
  if os == "IOS" && not state.props.locateOnMapProps.cameraAnimatedToSource && (getDistanceBwCordinates latitude longitude state.props.sourceLat state.props.sourceLong) > 5.0 then do
    continueWithCmd state{ props{ locateOnMapProps{ cameraAnimatedToSource = true } } } [do
      void $ animateCamera state.props.sourceLat state.props.sourceLong 25.0 "NO_ZOOM"
      pure NoAction
    ]
  else do
    let updatedState = state{ props{ locateOnMapProps{ cameraAnimatedToSource = true } } }
        sourceManuallyMoved = if updatedState.props.isSource == Just true then true else updatedState.props.rideSearchProps.sourceManuallyMoved
        destManuallyMoved = if updatedState.props.isSource == Just false then true else updatedState.props.rideSearchProps.destManuallyMoved
    case key of
      "LatLon" -> do
        let selectedSpot = head (filter (\spots -> (getDistanceBwCordinates latitude longitude spots.lat spots.lng) * 1000.0 < (toNumber JB.locateOnMapConfig.thresholdDistToSpot)  ) updatedState.data.nearByPickUpPoints)
        exit $ UpdateLocationName updatedState{props{defaultPickUpPoint = "", rideSearchProps{ sourceManuallyMoved = sourceManuallyMoved, destManuallyMoved = destManuallyMoved }, hotSpot{ selectedSpot = selectedSpot }, locateOnMapProps{ isSpecialPickUpGate = false }}} latitude longitude
      _ ->  case (filter(\item -> item.place == key) updatedState.data.nearByPickUpPoints) !! 0 of
              Just spot -> exit $ UpdateLocationName updatedState{props{defaultPickUpPoint = key, rideSearchProps{ sourceManuallyMoved = sourceManuallyMoved, destManuallyMoved = destManuallyMoved}, locateOnMapProps{ isSpecialPickUpGate = fromMaybe false spot.isSpecialPickUp }, hotSpot{ centroidPoint = Nothing }}} spot.lat spot.lng
              Nothing -> continue updatedState

eval GoToConfirmingLocationStage state = 
  exit $ ExitToConfirmingLocationStage state
eval UpdateNoInternet state = continue state { props { isOffline = true } }
eval (InternetCallBackCustomer internetAvailable) state =
  if (internetAvailable == "true") then do
    updateAndExit state { props { isOffline = false } } $ ReloadFlowStatus state { props { isOffline = false } }
  else
    continue state

eval (IntercitySpecialZone PopUpModal.DismissPopup) state = continue state

eval StartScheduledRidePolling state = continue state{props{startScheduledRidePolling = true}}

eval (IntercitySpecialZone PopUpModal.OnButton1Click) state = updateAndExit state { props { showIntercityUnserviceablePopUp = false, showNormalRideNotSchedulablePopUp = false}} $ StayInHomeScreenSO state { props { showIntercityUnserviceablePopUp = false, showNormalRideNotSchedulablePopUp = false}}

eval (IntercitySpecialZone PopUpModal.OnButton2Click) state = updateAndExit state { props { showIntercityUnserviceablePopUp = false, showNormalRideNotSchedulablePopUp = false}} $ StayInHomeScreenSO state { props { showIntercityUnserviceablePopUp = false, showNormalRideNotSchedulablePopUp = false}}

eval (ChooseSingleVehicleAction (ChooseVehicleController.ShowRateCard config)) state = do
  continue state 
    { props 
      { showRateCard = true }
    , data 
      { rateCard 
        { onFirstPage = false
        , currentRateCardType = DefaultRateCard
        , extraFare = config.extraFare
        , driverAdditions = config.driverAdditions
        , fareInfoDescription = config.fareInfoDescription
        , isNightShift = config.isNightShift
        , nightChargeTill = config.nightChargeTill
        , nightChargeFrom = config.nightChargeFrom
        , serviceTierName = config.serviceTierName
        }
      }
    }

eval (ChooseSingleVehicleAction (ChooseVehicleController.OnEditClick)) state =  do
  let 
    topProvider = filter (\element -> element.providerType == CTP.ONUS) state.data.specialZoneQuoteList
    firstTopProvider = fromMaybe ChooseVehicleController.config $ head topProvider 
    firstAllProvider = fromMaybe ChooseVehicleController.config $ head state.data.specialZoneQuoteList
    showMultiProvider' = if state.data.currentCityConfig.iopConfig.enable then null topProvider else false -- if there is no top provider then show all providers
    selectedEstimate' = if showMultiProvider' then firstAllProvider else firstTopProvider
    specialZoneQuoteList' = mapWithIndex (\index element -> element{activeIndex = selectedEstimate'.index}) state.data.specialZoneQuoteList

  exit $ ChangeVehicleVarient state{
    data{
      specialZoneQuoteList = specialZoneQuoteList',
      iopState { 
        showMultiProvider = showMultiProvider'
      , showPrefButton = state.data.currentCityConfig.iopConfig.enable && (not (null topProvider))
      } 
      , selectedEstimatesObject = selectedEstimate' {activeIndex = selectedEstimate'.index}
    }
  , props{
      isRepeatRide = false 
    -- , estimateId = estimateId
    }
  }

eval ShowMoreSuggestions state = do
  void $ pure $ map (\item -> startLottieProcess lottieAnimationConfig{ rawJson =  (getAssetsBaseUrl FunctionCall) <> "lottie/right_arrow.json" , speed = 1.0,lottieId = (getNewIDWithTag $ "movingArrowView" <> show item), minProgress = 0.0 }) [0,1]
  continueWithCmd state { props {suggestionsListExpanded = not state.props.suggestionsListExpanded} } [pure NoAction]

eval RemoveShimmer state = continue state{props{showShimmer = false}}

eval ShowPref state = continue state { data{ iopState { providerPrefInfo = false, providerPrefVisible = not state.data.iopState.providerPrefVisible}}}

-- Provider Switch Action in Estimates Screen
eval (ShowMultipleProvider showMultiProvider) state = do
  let 
    customerTip = if showMultiProvider then HomeScreenData.initData.props.customerTip else state.props.customerTip
    topProvider = filter (\element -> element.providerType == CTP.ONUS) state.data.specialZoneQuoteList
    firstTopProvider = fromMaybe ChooseVehicleController.config $ head topProvider 
    firstAllProvider = fromMaybe ChooseVehicleController.config $ head state.data.specialZoneQuoteList
    selectedEstimate' = if showMultiProvider then firstAllProvider else firstTopProvider
    specialZoneQuoteList' = mapWithIndex (\index element -> element{activeIndex = selectedEstimate'.index}) state.data.specialZoneQuoteList
    (Tuple estimateId otherSelectedEstimates) = getEstimateId specialZoneQuoteList' selectedEstimate'

  continueWithCmd state {
    data { 
      specialZoneQuoteList = specialZoneQuoteList', 
      triggerPatchCounter = state.data.triggerPatchCounter + 1, 
      otherSelectedEstimates = otherSelectedEstimates,
      iopState { 
        showMultiProvider = showMultiProvider, 
        providerPrefVisible = false
      }, 
      selectedEstimatesObject = selectedEstimate'{
        activeIndex = selectedEstimate'.index
      }
    }, 
    props { 
      customerTip = customerTip, 
      estimateId = estimateId
    }
  } [pure NoAction]

eval (ShowProviderInfo showProviderInfo) state = continue state { 
  data { 
    iopState { 
      providerPrefInfo = showProviderInfo
    , providerPrefVisible = false
    }
  }
}

eval (UpdateFollowers (FollowRideRes resp)) state = do
  let followers = map (\(Followers follower) -> follower) resp
  continue state{
    data{
      followers = Just followers
    }
  }

eval GoToFollowRide state = exit $ ExitToFollowRide state

eval (UpdateRepeatTrips rideList) state = do
  void $ pure $ setValueToLocalStore UPDATE_REPEAT_TRIPS "false"
  let listResp = rideList ^._list
      filteredList = filter (\(RideBookingRes item) -> getFareProductType (item.bookingDetails ^._fareProductType) /= FPT.RENTAL) listResp
      list = rideListToTripsTransformer filteredList
  if not (null list) then do
    let updatedMap = updateMapWithPastTrips list state
    void $ pure $ setSuggestionsMap updatedMap
    if state.props.sourceLat /= 0.0 && state.props.sourceLong /= 0.0 then
      updateCurrentLocation state (show state.props.sourceLat) (show state.props.sourceLong)
    else 
      continue state
  else do
    continue state

        
eval UpdatePeekHeight state = continue state{data{peekHeight = getPeekHeight state}}

eval (MarkerLabelOnClick markerName) state = 
  if state.props.currentStage == SettingPrice then do
    void $ pure $ updateLocalStage SearchLocationModel
    let isSource = Just (markerName == "ny_ic_src_marker")
    let updatedState = state{props{isSource = isSource, hasEstimateBackpoint = true, currentStage = SearchLocationModel}}
    continue updatedState
  else continue state

eval (Scroll item) state = do
  let sheetState = if item == state.props.currSlideIndex then state.props.isHomescreenExpanded
                   else item > state.props.currSlideIndex
      updatedState = state { props { isHomescreenExpanded = sheetState, currSlideIndex = item } }
  continue updatedState

eval ReAllocate state =
  if isLocalStageOn ReAllocated then do
    let updatedState = state{ props{ currentStage = FindingQuotes } }
    void $ pure $ setValueToLocalStore LOCAL_STAGE ( show FindingQuotes)
    updateAndExit updatedState $ ReAllocateRide updatedState
  else continue state
  
eval (SetBannerItem bannerItem) state = continue state{data{bannerData{bannerItem = Just bannerItem}}, props{isBannerDataComputed = true}}

eval (SetIssueReportBannerItems bannerItem) state = continue state {
  data {
    rideCompletedData {
      issueReportData {
        bannerItem = Just bannerItem
      }
    }
  }
}


eval (BannerChanged item) state = do
  let currentBanner = fromString item
  case currentBanner of
    Just idx -> do 
        let newState = state{data {bannerData{currentBanner = idx}}}
        if state.data.bannerData.currentPage /= idx then void $ pure $ unsafePerformEffect $ processEvent "RestartAutoScroll" unit -- To stop and stop the new autosroll
          else pure unit
        continue newState
    Nothing  -> continue state

eval (BannerStateChanged item) state = do
  let newState = state{data {bannerData{bannerScrollState = item}}}
  continue newState


eval GoToSafetyEducationScreen state = exit $ GoToSafetyEducation state

eval SearchForSelectedLocation state = do
  let currentStage = if state.props.searchAfterEstimate then TryAgain else FindingEstimate
  updateAndExit state{props{isPopUp = NoPopUp}} $ LocationSelected (fromMaybe dummyListItem state.data.selectedLocationListItem) false state{props{currentStage = currentStage, rideSearchProps{ sourceSelectType = ST.RETRY_SEARCH }, isPopUp = NoPopUp}}

eval CheckFlowStatusAction state = exit $ CheckFlowStatus state

eval TerminateApp state = do
  pure $ terminateApp state.props.currentStage true
  continue state

eval (KeyboardCallback keyBoardState) state = do 
  let isOpen = case keyBoardState of
                    "onKeyboardOpen" -> true
                    "onKeyboardClose" -> false
                    _ -> false 
  if isLocalStageOn ChatWithDriver && isOpen then
    void $ pure $ scrollToEnd (getNewIDWithTag "ChatScrollView") true 
  else pure unit
  continue state{props{isKeyBoardOpen = isOpen}}

eval (NotifyDriverStatusCountDown seconds status timerID) state = do 
  if status == "EXPIRED" then do
    _ <- pure $ clearTimerWithId timerID
    _ <- pure $ setValueToLocalStore NOTIFIED_CUSTOMER "true"
    let eta = fromMaybe 0 state.data.driverInfoCardState.eta
    if isLocalStageOn RideAccepted && isJust state.data.driverInfoCardState.eta && (secondsToHms eta) /= "--" then 
      continue state{data{lastMessage = state.data.lastMessage{message = state.data.config.notifyRideConfirmationConfig.autoGeneratedText <> (secondsToHms eta), sentBy = "Driver"}},props{unReadMessages = true, showChatNotification = true}}
    else continue state
  else continue state

eval (RepeatRideCountDown seconds status timerID) state = do
  if state.props.currentStage /= SettingPrice then do
    void $ pure $ clearTimerWithId timerID
    continue state{props{repeatRideTimer = "", repeatRideTimerId = "", repeateRideTimerStoped = true}}
  else if status == "EXPIRED" then do
    void $ pure $ clearTimerWithId timerID
    void $ pure $ performHapticFeedback unit
    -- void $ pure $ updateLocalStage FindingQuotes
    -- void $ pure $ setValueToLocalStore SELECTED_VARIANT state.data.selectedEstimatesObject.vehicleVariant
    void $ pure $ cacheRateCard state
    let updatedState = state{data{rideHistoryTrip = Nothing}, props{repeatRideTimerId = "",repeateRideTimerStoped = true, searchExpire = (getSearchExpiryTime true)}}
    exit $ SelectEstimateAndQuotes updatedState
  else continue state{props{repeatRideTimer = (show seconds), repeatRideTimerId = timerID, repeateRideTimerStoped = false}}

eval StopRepeatRideTimer state =  do
  void $ pure $ clearTimerWithId state.props.repeatRideTimerId
  continue state{props{repeatRideTimer = "", repeatRideTimerId = "", repeateRideTimerStoped = true}}

eval (AutoScrollCountDown seconds status timerID) state = do
  if status == "EXPIRED" then do
    void $ pure $ clearTimerWithId timerID
    void $ pure $ performHapticFeedback unit
    let updatedState = state{props{autoScroll = false, autoScrollTimerId = "", homeScreenSheetState = EXPANDED, isHomescreenExpanded=true, currSlideIndex=1.0, autoScrollTimer = ""}}
    continue updatedState
  else continue state{props{autoScrollTimer = (show seconds), autoScrollTimerId = timerID}}

eval StopAutoScrollTimer state =  do
  void $ pure $ clearTimerWithId state.props.autoScrollTimerId
  continue state{props{autoScrollTimer = "", autoScrollTimerId = ""}}

eval (IsMockLocation isMock) state = do
  let val = isMock == "true"
      _ = unsafePerformEffect $ if val then  logEvent (state.data.logField) "ny_fakeGPS_enabled" else pure unit -- we are using unsafePerformEffect becasue without it we are not getting logs in firebase, since we are passing a parameter from state i.e. logField then the output will be inline and it will not be able to precompute so it's safe to use it here.
  continue state{props{isMockLocation = false}}

eval (UpdateCurrentStage stage (RideBookingRes resp)) state = do
  _ <- pure $ spy "updateCurrentStage" stage
  let fareProductType = getFareProductType $ resp.bookingDetails ^._fareProductType
      stopLocation = if fareProductType == FPT.RENTAL then _stopLocation else _toLocation
      stopLocationDetails = fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^.stopLocation)
      (BookingLocationAPIEntity toLocation) = stopLocationDetails
      otpCode = ((resp.bookingDetails) ^. _contents ^. _otpCode)
      (RideAPIEntity rideList) = (fromMaybe dummyRideAPIEntity (head resp.rideList))
      -- searchResultType = if (fareProductType == FPT.ONE_WAY_SPECIAL_ZONE || otpCode /= Nothing) then QUOTES 
      --                           else if fareProductType == FPT.INTER_CITY then INTERCITY
      --                           else if fareProductType == FPT.RENTAL then RENTALS 
      --                           else ESTIMATES
      otp = if (( any (_ == fareProductType) [ FPT.RENTAL , FPT.INTER_CITY] ) && state.props.currentStage == RideStarted) then fromMaybe "" rideList.endOtp else if (fareProductType == FPT.ONE_WAY_SPECIAL_ZONE || otpCode /= Nothing) then fromMaybe "" ((resp.bookingDetails)^._contents ^._otpCode) else rideList.rideOtp
      destAddress = getAddressFromBooking stopLocationDetails
      dest = decodeAddress (Booking stopLocationDetails)
      newState = state{data{driverInfoCardState 
                              { otp = otp, 
                                rentalData {
                                  startTimeUTC = fromMaybe "" resp.rideStartTime, 
                                  startOdometer = show $ fromMaybe 0.0 rideList.startOdometerReading,
                                  endOdometer = show $ fromMaybe 0.0 rideList.endOdometerReading,
                                  baseDuration = (fromMaybe 0 resp.estimatedDuration) / 3600, 
                                  baseDistance = (fromMaybe 0 resp.estimatedDistance) / 1000 
                                },
                                destination = dest,
                                price = resp.estimatedTotalFare,
                                destinationLat = toLocation.lat , destinationLng = toLocation.lon , destinationAddress = destAddress,
                                driversPreviousRideDropLocLat = resp.driversPreviousRideDropLocLat,
                                driversPreviousRideDropLocLon = resp.driversPreviousRideDropLocLon
                              }
                        , fareProductType = fareProductType
                        , destinationAddress = destAddress
                        , destination = dest
                        }
                      , props{ stopLoc = Just {lat : stopLocationDetails^._lat,
                                                lng : stopLocationDetails^._lon,
                                                stopLocAddress : decodeAddress (Booking stopLocationDetails) }
                              , destinationLat = toLocation.lat
                              , destinationLong = toLocation.lon}}
      isDestChanged = not (state.data.driverInfoCardState.destinationLat == toLocation.lat && state.data.driverInfoCardState.destinationLng == toLocation.lon)
  if isDestChanged then do
    void $ pure $ setValueToLocalStore TRACKING_DRIVER "False"
  else pure unit
  if stage == "REALLOCATED" then
    exit $ NotificationHandler "REALLOCATE_PRODUCT" newState
  else if (stage == "INPROGRESS") && (not $ (isLocalStageOn RideStarted || (isLocalStageOn ChatWithDriver && state.props.stageBeforeChatScreen == RideStarted))) then
    updateAndExit newState $ NotificationHandler "TRIP_STARTED" newState
  else if stage == "INPROGRESS" && isDestChanged then
    updateAndExit newState $ NotificationHandler "TRIP_STARTED" newState
  else if stage == "NEW" && isDestChanged then
    updateAndExit newState $ NotificationHandler "DRIVER_ASSIGNMENT" newState
  else if (stage == "COMPLETED") && (not $ isLocalStageOn HomeScreen) then
    exit $ NotificationHandler "TRIP_FINISHED" newState
  else if (stage == "CANCELLED") && (not $ isLocalStageOn HomeScreen) then
    exit $ NotificationHandler "CANCELLED_PRODUCT" newState
  else
    continue newState

eval OnResumeCallback state =
  if(state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted) && state.props.emergencyHelpModelState.waitingDialerCallback then 
    continue state {props {emergencyHelpModelState {showCallSuccessfulPopUp = true}, rideDurationTimer = show $ compareUTCDate (getCurrentUTC "") state.data.driverInfoCardState.rentalData.startTimeUTC}}
  else 
    case getValueToLocalNativeStore LOCAL_STAGE of
      "FindingQuotes" -> do
        let secondsLeft = findingQuotesSearchExpired false true
        case (methodArgumentCount "startLottieProcess") == 1 of
          true  -> do
            let findingQuotesProgress = 1.0 - (toNumber secondsLeft)/(toNumber (getSearchExpiryTime true))
            if secondsLeft > 0 then
              void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = (getAssetsBaseUrl FunctionCall) <> "lottie/progress_loader_line.json", lottieId = (getNewIDWithTag "lottieLoaderAnimProgress"), minProgress = findingQuotesProgress, scaleType="CENTER_CROP"}
            else pure unit
          false -> pure unit
        case flowWithoutOffers WithoutOffers of
          true | secondsLeft <= 0 -> do
              _ <- pure $ updateLocalStage QuoteList
              continueWithCmd state [do
                let response = SelectListRes { selectedQuotes: Nothing, bookingId : Nothing }
                pure $ GetQuotesList response
              ]
          _ -> continue state
      "QuoteList" -> do
        let findingQuotesProgress = 1.0 - 30.0/(toNumber (getSearchExpiryTime true))
        void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = (getAssetsBaseUrl FunctionCall) <> "lottie/progress_loader_line.json", lottieId = (getNewIDWithTag "lottieLoaderAnimProgress"), minProgress = findingQuotesProgress, scaleType="CENTER_CROP"}
        continue state
      "RideAccepted" | (state.data.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE) -> exit $ Retry state
      "ConfirmingQuotes" -> do
        let findingQuotesProgress = 1.0 - 30.0/(toNumber (getSearchExpiryTime false))
        void $ pure $ startLottieProcess lottieAnimationConfig {rawJson =(getAssetsBaseUrl FunctionCall) <> "lottie/progress_loader_line.json", lottieId = (getNewIDWithTag "lottieLoaderAnimProgress"), minProgress = findingQuotesProgress, scaleType="CENTER_CROP"}
        continue state
      _ -> continue state

eval (UpdateSavedLoc savedLoc) state = continue state{data{savedLocations = savedLoc}}

------------------------------- Ride Completed Screen - Start --------------------------

eval ( RideCompletedAC (RideCompletedCard.RateClick index)) state = do
  void $ pure $ setValueToLocalStore REFERRAL_STATUS "HAS_TAKEN_RIDE"
  continue
    state
      { props { currentStage = if state.data.driverInfoCardState.providerType == CTP.ONUS then RideRating else state.props.currentStage }
      , data
        { rideRatingState 
            { rating = index
            , feedbackList = state.data.rideRatingState.feedbackList
            }
          , ratingViewState { selectedRating = index }
        }
      }

eval (RideCompletedAC(RideCompletedCard.BannerChanged idxStr)) state = 
  case fromString idxStr of 
    Just idx -> if idx == state.data.rideCompletedData.issueReportData.currentBannerIndex then update state else continue state {data {rideCompletedData { issueReportData {currentBannerIndex = idx}}}} 
    Nothing -> update state

eval (RideCompletedAC (RideCompletedCard.SelectButton selectedYes pageIndex)) state = do 
  let 
    availableBanners = issueReportBannerConfigs state
    noOfAvailableBanners = length availableBanners 
    bannerAtIndex = availableBanners !! pageIndex
  
  case bannerAtIndex of 
    Just bannerObj -> do 
      let 
        issueType = bannerObj.issueType 
        issueResponse = find (\obj -> obj.issueType == issueType) state.data.rideCompletedData.issueReportData.customerResponse 
        issueResponseIndex = findIndex (\obj -> obj.issueType == issueType) state.data.rideCompletedData.issueReportData.customerResponse 
      case issueResponse , issueResponseIndex of 
        Just respObj, Just respIdx -> do
          let 
            updatedResponse = respObj {selectedYes = Just selectedYes}
            updatedIssueResponseArr = updateAt respIdx updatedResponse state.data.rideCompletedData.issueReportData.customerResponse
          case updatedIssueResponseArr of 
            Just updatedIssueResponseArrObj -> do
              let 
                updatedState = state {data {rideCompletedData { issueReportData {customerResponse = updatedIssueResponseArrObj}}}}
                userRespondedIssues = filter (\issueResp -> issueResp.selectedYes /= Nothing) updatedIssueResponseArrObj
                userRespondedIssuesCount = length userRespondedIssues

              if noOfAvailableBanners == userRespondedIssuesCount then do
                continue updatedState{data {rideCompletedData { issueReportData {respondedValidIssues = true}}}}
              else 
                continue updatedState{data {rideCompletedData { issueReportData {currentPageIndex = if  (pageIndex + 1) < noOfAvailableBanners then pageIndex + 1 else pageIndex}}}}  
            Nothing -> update state
        _ , _ -> update state
    Nothing -> update state


eval (RideCompletedAC RideCompletedCard.Support) state = continue state {props {callSupportPopUp = true}}

eval (RideCompletedAC RideCompletedCard.RideDetails) state = exit $ RideDetailsScreen state 

eval (RideCompletedAC RideCompletedCard.HelpAndSupportAC) state = 
  if state.data.config.feature.enableHelpAndSupport 
    then exit $ GoToHelpAndSupport state
    else continue state {props{isContactSupportPopUp = true}}

eval (RideCompletedAC (RideCompletedCard.ContactSupportPopUpAC (PopUpModal.DismissPopup))) state = continue state{props{isContactSupportPopUp = false}}

eval (RideCompletedAC (RideCompletedCard.ContactSupportPopUpAC (PopUpModal.OnSecondaryTextClick))) state =   
    continueWithCmd state{props{isContactSupportPopUp = false}} [do
        void $ openUrlInMailApp $ mailToLink <> (getAppConfig appConfig).appData.supportMail
        pure NoAction
    ]

eval (RideCompletedAC (RideCompletedCard.ContactSupportPopUpAC (PopUpModal.OnButton1Click))) state = do
    void $ pure $ showDialer (getSupportNumber "") false
    continue state{props{isContactSupportPopUp = false}}

eval (RideCompletedAC (RideCompletedCard.ContactSupportPopUpAC (PopUpModal.OnButton2Click))) state = continue state{props{isContactSupportPopUp = false}}

eval (RideCompletedAC RideCompletedCard.GoToSOS) state = exit $ GoToNammaSafety state true false 

eval (RideCompletedAC (RideCompletedCard.SkipButtonActionController (PrimaryButtonController.OnClick))) state = do
  void $ pure $ toggleBtnLoader "SkipButton" false
  if state.data.rideCompletedData.issueReportData.respondedValidIssues  && state.data.rideCompletedData.issueReportData.showIssueBanners then do
    let 
      negativeResp = filter (\issueResp -> issueResp.selectedYes == Just false) state.data.rideCompletedData.issueReportData.customerResponse

      hasAssistenceIssue = any (\issueResp -> issueResp.issueType == CTP.Accessibility) negativeResp 
      hasSafetyIssue = any (\issueResp -> issueResp.issueType == CTP.NightSafety) negativeResp
      hasTollIssue = any (\issueResp -> issueResp.issueType == CTP.TollCharge) negativeResp

      priorityIssue = case hasSafetyIssue, hasTollIssue of
        true, _ -> CTP.NightSafety
        false, true -> CTP.TollCharge
        _, _ -> CTP.NoIssue

      ratingUpdatedState = state {
        data{
          rideCompletedData {
            issueReportData {
              showIssueBanners = false
            }
          }
        , ratingViewState{
            wasOfferedAssistance = Just $ not hasAssistenceIssue
          }
        }
      }
    
    if priorityIssue == CTP.NoIssue then
      continue ratingUpdatedState 
    else 
      exit $ GoToIssueReportChatScreenWithIssue ratingUpdatedState priorityIssue
  else do
    void $ pure $ setValueToLocalStore REFERRAL_STATUS "HAS_TAKEN_RIDE"
    if state.data.fareProductType == FPT.RENTAL then continue state{data{fareProductType = FPT.ONE_WAY}} 
    else updateAndExit state $ SubmitRating state{ data {rideRatingState {rating = state.data.ratingViewState.selectedRating }}}

------------------------------- Ride Completed Screen - End --------------------------

eval (UpdateNextIssueBannerPage index) state = update state --{data {rideCompletedData { issueReportData {currentPageIndex = index}}}}

eval (UpdateNextIssueBanneerSwipe index) state = update state --{data {rideCompletedData { issueReportData {currentBannerIndex = index}}}}



------------------------------- ChatService - Start --------------------------

eval (UpdateMessages message sender timeStamp size) state = do
  if not state.props.chatcallbackInitiated then continue state else do
    continueWithCmd state{data{messagesSize = size}} [do
      pure $ LoadMessages
    ]

eval LoadMessages state = do
  let allMessages = getChatMessages FunctionCall
  case last allMessages of
      Just value -> if value.message == "" then continue state {data { messagesSize = show (fromMaybe 0 (fromString state.data.messagesSize) + 1)}, props {canSendSuggestion = true, isChatNotificationDismissed = false}} 
                      else do
                        let currentUser = if state.props.isChatWithEMEnabled then (getValueFromCache (show CUSTOMER_ID) getKeyInSharedPrefKeys) else "Customer" 
                        if value.sentBy == currentUser then updateMessagesWithCmd state {data {messages = allMessages, chatSuggestionsList = if state.props.isChatWithEMEnabled then getSuggestionsfromKey emChatSuggestion emergencyContactInitialChatSuggestionId else [], lastMessage = value, lastSentMessage = value}, props {canSendSuggestion = true,  isChatNotificationDismissed = false}}
                        else do
                          let readMessages = fromMaybe 0 (fromString (getValueToLocalNativeStore READ_MESSAGES))
                              unReadMessages = if readMessages == 0 && state.props.currentStage /= ChatWithDriver then true else (readMessages < (length allMessages) && state.props.currentStage /= ChatWithDriver)
                              suggestionKey = if state.props.isChatWithEMEnabled then emChatSuggestion else chatSuggestion
                              suggestions = getSuggestionsfromKey suggestionKey value.message
                              isChatNotificationDismissed = not state.props.isChatNotificationDismissed || state.data.lastMessage.message /= value.message
                              showNotification = isChatNotificationDismissed && unReadMessages
                          updateMessagesWithCmd state {data {messages = allMessages, chatSuggestionsList = suggestions, lastMessage = value, lastSentMessage = MessagingView.dummyChatComponent, lastReceivedMessage = value}, props {unReadMessages = unReadMessages, showChatNotification = showNotification, canSendSuggestion = true, isChatNotificationDismissed = false, removeNotification = not showNotification, enableChatWidget = showNotification}}
      Nothing -> 
        if state.props.isChatWithEMEnabled then
            continueWithCmd state [do 
            pure $ SendQuickMessage "c013253fcbe2fdc50b1c261501de9045"
            ] 
          else continue state {props {canSendSuggestion = true}}

eval (OpenChatScreen) state = do
  if not state.props.chatcallbackInitiated then continue state else do
    continueWithCmd state{props{openChatScreen = false}} [do
      pure $ (DriverInfoCardActionController (DriverInfoCardController.MessageDriver))
    ]

eval MessageDriver state = do
    continueWithCmd state{props{openChatScreen = false}} [do
      pure $ (DriverInfoCardActionController (DriverInfoCardController.MessageDriver))
    ]

eval (MessagingViewActionController (MessagingView.TextChanged value)) state = continue state{data{messageToBeSent = (STR.trim value)},props{sendMessageActive = (STR.length (STR.trim value)) >= 1}}

eval (DriverInfoCardActionController (DriverInfoCardController.BannerCarousel act)) state = 
  continueWithCmd state [do
      pure $ BannerCarousel act
    ]
    
eval (DriverInfoCardActionController (DriverInfoCardController.UpdateBanner)) state = 
  continueWithCmd state [do
      pure $ UpdateBanner
    ]

eval (DriverInfoCardActionController (DriverInfoCardController.BannerChanged value)) state = 
  continueWithCmd state [do
      pure $ BannerChanged value
    ]

eval (DriverInfoCardActionController (DriverInfoCardController.BannerStateChanged value)) state = 
  continueWithCmd state [do
      pure $ BannerStateChanged value
    ]

eval (DriverInfoCardActionController DriverInfoCardController.RateCardInfo) state = 
  case state.data.rateCardCache of
    Just val -> continue state {props {showRateCard = true}, data {rateCard = val}}
    Nothing -> continue state

eval (DriverInfoCardActionController DriverInfoCardController.EditingDestination) state = do
  _ <- pure $ deleteValueFromLocalStore TRACKING_ID
  continue state {props {currentStage = EditingDestinationLoc, isSource = Just false, isSearchLocation = SearchLocation}}

  
eval(MessagingViewActionController (MessagingView.Call)) state = do
  void $ pure $ performHapticFeedback unit
  void $ pure $ hideKeyboardOnNavigation true
  if state.props.isChatWithEMEnabled 
    then do
      let filterContacts = filter (\item -> item.priority == 0) $ fromMaybe [] state.data.contactList
      case head filterContacts of
        Nothing -> continue state
        Just contact -> do 
          void $ pure $ showDialer contact.number true
          continue state
    else if length state.data.config.callOptions > 1 then
      continue state { props { showCallPopUp = true } }
    else callDriver state $ fromMaybe "ANONYMOUS" $ state.data.config.callOptions !! 0

eval (MessagingViewActionController (MessagingView.SendMessage)) state = do
  if state.data.messageToBeSent /= ""
  then do
    pure $ sendMessage state.data.messageToBeSent
    pure $ setText (getNewIDWithTag "ChatInputEditText") ""
    continue state{data{messageToBeSent = ""},props {sendMessageActive = false}}
  else
    continue state

eval (MessagingViewActionController (MessagingView.BackPressed)) state = do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ hideKeyboardOnNavigation true
  continueWithCmd state [do
      pure $ BackPressed
    ]

eval ScrollToBottom state = do
  _ <- pure $ scrollToEnd (getNewIDWithTag "ChatScrollView") true
  continue state

eval InitializeChat state = do
  continue state {props { chatcallbackInitiated = true } }


eval RemoveChat state = do
  continueWithCmd state {props{chatcallbackInitiated = false}} [ do
    _ <- stopChatListenerService
    _ <- pure $ setValueToLocalNativeStore READ_MESSAGES "0"
    pure $ NoAction
  ]

eval (DriverInfoCardActionController (DriverInfoCardController.WaitingInfo)) state =
  if state.props.currentStage == RideAccepted then
    continue state { data { waitTimeInfo = true } }
  else
    continue state

eval (SendQuickMessage chatSuggestion) state = do
  if state.props.canSendSuggestion then do
    _ <- pure $ sendMessage chatSuggestion
    continue state {props {unReadMessages = false}}
  else continue state

eval (DriverInfoCardActionController (DriverInfoCardController.MessageDriver)) state = do
  if state.data.config.feature.enableChat && state.data.driverInfoCardState.providerType == CTP.ONUS then do
    if not state.props.chatcallbackInitiated || state.data.waitTimeInfo then continue state else do
      void $ pure $ performHapticFeedback unit
      _ <- pure $ updateLocalStage ChatWithDriver
      _ <- pure $ setValueToLocalNativeStore READ_MESSAGES (show (length state.data.messages))
      let allMessages = getChatMessages FunctionCall
      continueWithCmd state {data{messages = allMessages}, props {currentStage = ChatWithDriver, stageBeforeChatScreen = if state.props.currentStage /= ChatWithDriver then state.props.currentStage else state.props.stageBeforeChatScreen, sendMessageActive = false, unReadMessages = false, showChatNotification = false, isChatNotificationDismissed = false,sheetState = Just COLLAPSED}}  [ do pure $ UpdateSheetState COLLAPSED]
  else continueWithCmd state[ do
        pure $ DriverInfoCardActionController (DriverInfoCardController.CallDriver) 
      ]

eval (UpdateSheetState sheetState) state = continue state {props {sheetState = Nothing, currentSheetState = sheetState}}

eval (DriverInfoCardActionController (DriverInfoCardController.CollapseBottomSheet)) state = continue state {props {sheetState = Just COLLAPSED, currentSheetState = COLLAPSED}}

eval (DriverInfoCardActionController (DriverInfoCardController.AddStop)) state = exit $ Add_Stop state

eval (DriverInfoCardActionController (DriverInfoCardController.RentalInfo)) state = continue state {props {showRentalInfo = true}}

eval RemoveNotification state = do
  continue state {props { showChatNotification = false, isChatNotificationDismissed = true}}

eval NotificationAnimationEnd state = do
  let isExpanded = state.props.showChatNotification && state.props.chatcallbackInitiated
      areMessagesEmpty =  (length $ getChatMessages FunctionCall) == 0 && (not state.props.isChatWithEMEnabled)
      showNotification = (areMessagesEmpty || state.props.showChatNotification) && ((state.props.currentStage == RideAccepted) || (state.props.currentStage == RideStarted && state.data.fareProductType == FPT.RENTAL) ||state.props.isChatWithEMEnabled) && not state.props.isChatNotificationDismissed
  continue state {props { isNotificationExpanded = isExpanded, showChatNotification = showNotification, removeNotification = not showNotification, enableChatWidget = (isExpanded || areMessagesEmpty) && not state.props.isChatNotificationDismissed}}

eval MessageViewAnimationEnd state = do
  continue state {props { removeNotification = not state.props.showChatNotification}}

eval (MessagingViewActionController (MessagingView.SendSuggestion chatSuggestion)) state = do
  if state.props.canSendSuggestion then do
    _ <- pure $ sendMessage chatSuggestion
    continue state {data {chatSuggestionsList = []}, props {canSendSuggestion = false}}
  else continue state

eval AllChatsLoaded state = do
  if state.props.isChatWithEMEnabled then do
    void $ pure $ sendMessage "c013253fcbe2fdc50b1c261501de9045"
    continue state
  else
    continue state

------------------------------- ChatService - End --------------------------

eval (MessageExpiryTimer seconds status timerID) state = do
  let newState = state{data{triggerPatchCounter = state.data.triggerPatchCounter + 1}}
  if status == "EXPIRED"
    then do
      _ <- pure $ clearTimerWithId timerID
      let currentUser = if state.props.isChatWithEMEnabled then (getValueFromCache (show CUSTOMER_ID) getKeyInSharedPrefKeys) else "Customer" 
      if state.data.lastMessage.sentBy == currentUser then
      continueWithCmd newState [ do
        pure $ RemoveNotification
      ]
      else continue newState
  else
      continue newState

eval (DriverInfoCardActionController (DriverInfoCardController.NoAction)) state = continue state {data{infoCardPeekHeight = getInfoCardPeekHeight state}}

eval (ScrollStateChanged scrollState) state = do
  let sheetState = case scrollState of 
              "1" -> STATE_DRAGGING
              "2" -> STATE_SETTLING
              "3" -> STATE_EXPANDED
              "4" -> STATE_COLLAPSED
              "5" -> STATE_HIDDEN
              "6" -> STATE_HALF_EXPANDED
              _ -> STATE_HIDDEN
  continue state {props {bottomSheetState = sheetState, currentSheetState = if sheetState == STATE_EXPANDED then EXPANDED else state.props.currentSheetState, sheetState = Nothing}}

eval (DriverInfoCardActionController (DriverInfoCardController.CallDriver)) state = do
  if length state.data.config.callOptions > 1 then
    continue state { props { showCallPopUp = true } }
  else callDriver state $ fromMaybe "ANONYMOUS" $ state.data.config.callOptions !! 0

eval (DriverInfoCardActionController (DriverInfoCardController.SpecialZoneInfoTag)) state = continue state{ props{ showSpecialZoneInfoPopup = true } }

eval DirectSearch state =continue state{props{currentStage = SearchLocationModel}}

eval BackPressed state = do
  void $ pure $ toggleBtnLoader "" false
  let _ = runFn2 updatePushInIdMap "EstimatePolling" true
  case state.props.currentStage of
    SearchLocationModel -> do
      if state.props.hasEstimateBackpoint then do
        void $ pure $ updateLocalStage SettingPrice
        continue state{props{currentStage = SettingPrice, hasEstimateBackpoint = false}}
      else if state.props.isSaveFavourite then 
        continueWithCmd state{props{isSearchCancelled = false}} [pure $ (SaveFavouriteCardAction (SaveFavouriteCardController.OnClose))]
      else do
        if state.props.isSearchLocation == LocateOnMap then do
          void $ pure $ exitLocateOnMap ""
          void $ pure $ hideKeyboardOnNavigation true
          continue state{data{nearByPickUpPoints = []},props{defaultPickUpPoint = "" , isSearchLocation = SearchLocation, locateOnMap = false, isSearchCancelled = false}}
        else do
          if (getSearchType unit) == "direct_search" then
            pure $ terminateApp state.props.currentStage false
          else 
            pure unit
          void $ pure $ removeAllPolylines ""
          void $ pure $ updateLocalStage HomeScreen
          void $ pure $ setValueToLocalStore SESSION_ID (generateSessionId unit)
          void $ pure $ enableMyLocation true
          void $ pure $ setValueToLocalStore NOTIFIED_CUSTOMER "false"
          let 
            { savedLocationsWithOtherTag
            , recentlySearchedLocations
            , suggestionsMap
            , trips
            , suggestedDestinations
            } = getHelperLists state.data.savedLocations state.data.recentSearchs state state.props.currentLocation.lat state.props.currentLocation.lng
            _ = removeMarker $ getCurrentLocationMarker ""
          continue 
            HomeScreenData.initData
              { data
                { disability = state.data.disability
                , bannerData = state.data.bannerData
                , tripSuggestions = trips
                , recentSearchs {predictionArray = recentlySearchedLocations}
                , destinationSuggestions = suggestedDestinations
                , source = state.props.currentLocation.place
                , rentalsInfo = state.data.rentalsInfo
                , settingSideBar
                  { gender = state.data.settingSideBar.gender
                  , email = state.data.settingSideBar.email
                  , hasCompletedSafetySetup = state.data.settingSideBar.hasCompletedSafetySetup
                  }
                  , followers = state.data.followers
                , currentCityConfig = state.data.currentCityConfig
                , famousDestinations = state.data.famousDestinations
                }
              , props { 
                  isBanner = state.props.isBanner
                , showShimmer = false
                , city = state.props.city
                , currentLocation = state.props.currentLocation
                , sosBannerType = state.props.sosBannerType 
                , followsRide = state.props.followsRide
                , sourceLat = state.props.currentLocation.lat 
                , sourceLong = state.props.currentLocation.lng
                , isSafetyCenterDisabled = state.props.isSafetyCenterDisabled
                , rideSearchProps { 
                    cachedPredictions = state.props.rideSearchProps.cachedPredictions
                  }
                }
              }
    SettingPrice -> do
      void $ pure $ performHapticFeedback unit
      void $ pure $ clearTimerWithId state.props.repeatRideTimerId
      let updatedState = state{props{repeatRideTimer = "", repeatRideTimerId = "", isSearchCancelled = false}}
      if updatedState.props.showRateCard then 
        if updatedState.data.rateCard.currentRateCardType /= DefaultRateCard then
          continue updatedState{data{rateCard {currentRateCardType = DefaultRateCard}}}
        else 
          continue updatedState{props{showRateCard = false}}
      else if updatedState.props.showMultipleRideInfo then 
        continue updatedState{props{showMultipleRideInfo=false}}
      else if state.props.showBookingPreference then 
        continue state {props {showBookingPreference = false}}
      else if state.data.iopState.providerPrefVisible || state.data.iopState.providerPrefInfo then 
        continue state { data { iopState { providerPrefInfo = false ,providerPrefVisible = false}}}
      else if state.data.iopState.providerSelectionStage then do
        void $ pure $ clearTimerWithId state.data.iopState.timerId
        continue state { props{isPopUp = ConfirmBack}, data { iopState { timerVal = "0"}}}
      else do
        void $ pure $ updateLocalStage SearchLocationModel
        continue state{
          data{
            rideHistoryTrip = Nothing
          , specialZoneQuoteList = []
          }
        , props{
            rideRequestFlow = false
          , currentStage = SearchLocationModel
          , searchId = ""
          , isSource = Just false
          , isSearchLocation = SearchLocation
          , isRepeatRide = false
          , customerTip = HomeScreenData.initData.props.customerTip
          , tipViewProps = HomeScreenData.initData.props.tipViewProps
          , hasEstimateBackpoint = false
          }
        }
    ConfirmingLocation -> do
                      void $ pure $ performHapticFeedback unit
                      _ <- pure $ exitLocateOnMap ""
                      _ <- pure $ removeAllPolylines ""
                      _ <- pure $ updateLocalStage SearchLocationModel
                      continue state{props{defaultPickUpPoint = "", rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation},data{polygonCoordinates = "", nearByPickUpPoints = []}}

    FindingEstimate -> do
                      void $ pure $ performHapticFeedback unit
                      _ <- pure $ updateLocalStage SearchLocationModel
                      let newState = state{props{rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation}}
                      updateAndExit newState $ GoToHome newState
    ConfirmingQuotes -> do 
                      -- let isAcCab = ServiceTierCard.showACDetails (fromMaybe "" state.data.driverInfoCardState.serviceTierName) Nothing
                      if state.props.isPopUp == NoPopUp then continue $ state { props{isPopUp = CancelConfirmingQuotes}} else continue state    
                      -- continue state { props { isCancelRide = true, cancellationReasons = cancelReasons isAcCab, cancelRideActiveIndex = Nothing, cancelReasonCode = "", cancelDescription = "" } }
    QuoteList       -> do
                      void $ pure $ performHapticFeedback unit
                      if state.props.isPopUp == NoPopUp then continue $ state { props{isPopUp = ConfirmBack}} else continue state                      
    PricingTutorial -> do
                      void $ pure $ performHapticFeedback unit
                      continue state { props { currentStage = SettingPrice}}
    DistanceOutsideLimits -> do
                      void $ pure $ performHapticFeedback unit
                      _ <- pure $ updateLocalStage SearchLocationModel
                      continue state{props{rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation }}
    ShortDistance -> do
                      void $ pure $ performHapticFeedback unit
                      _ <- pure $ updateLocalStage SearchLocationModel
                      continue state{props{isSource = Just false,isPopUp = NoPopUp, rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation}}
    FindingQuotes ->  do
                      void $ pure $ performHapticFeedback unit
                      continue $ state { props{isPopUp = ConfirmBack}}
    FavouriteLocationModel -> do
                      void $ pure $ performHapticFeedback unit
                      _ <- pure $ updateLocalStage (if state.props.isSearchLocation == NoView then HomeScreen else SearchLocationModel)
                      continue state { props { currentStage = if state.props.isSearchLocation == NoView then HomeScreen else SearchLocationModel}}
    ChatWithDriver -> do
                        if state.props.showCallPopUp then continue state {props{showCallPopUp = false}}
                         else do
                            -- let lastStage = if state.props.isChatWithEMEnabled then RideStarted else RideAccepted --todo handle this case
                            -- _ <- pure $ updateLocalStage lastStage
                            -- continue state {props {currentStage = lastStage}}
                            _ <- pure $ updateLocalStage state.props.stageBeforeChatScreen
                            updateAndExit state {props {currentStage = state.props.stageBeforeChatScreen}} $ RefreshHomeScreen state {props {currentStage = state.props.stageBeforeChatScreen}}
    RideRating ->     do
                      _ <- pure $ updateLocalStage RideCompleted
                      continue state {props {currentStage = RideCompleted}}
    EditingDestinationLoc -> do
      void $ pure $ performHapticFeedback unit
      exit $ EditDestBackPressed state
    ConfirmEditDestinationLoc -> do
      void $ pure $ performHapticFeedback unit
      exit $ EditDestBackPressed state
    ConfirmingEditDestinationLoc -> do
      void $ pure $ performHapticFeedback unit
      exit $ EditDestBackPressed state
    RevisedEstimate -> do
      void $ pure $ performHapticFeedback unit
      exit $ EditDestBackPressed state
    FavouriteLocationModelEditDest -> do
                      void $ pure $ performHapticFeedback unit
                      _ <- pure $ updateLocalStage (if state.props.isSearchLocation == NoView then HomeScreen else EditingDestinationLoc)
                      continue state { props { currentStage = if state.props.isSearchLocation == NoView then HomeScreen else EditingDestinationLoc}}
    ReAllocated ->    continue state
    _               -> do
                        if state.props.isLocationTracking then continue state{props{isLocationTracking = false}}
                          else if state.props.cancelSearchCallDriver then continue state{props{cancelSearchCallDriver = false}}
                          else if state.props.showCallPopUp then continue state{props{showCallPopUp = false}}
                          else if state.props.isCancelRide then continue state{props{isCancelRide = false}}
                          else if state.props.isSaveFavourite then continueWithCmd state [pure $ SaveFavouriteCardAction SaveFavouriteCardController.OnClose]
                          else if state.props.showShareAppPopUp then continue state{props{showShareAppPopUp=false}}
                          else if state.props.showMultipleRideInfo then continue state{props{showMultipleRideInfo=false}}
                          else if state.props.showLiveDashboard then do
                            continueWithCmd state [do
                              _ <- pure $ goBackPrevWebPage (getNewIDWithTag "webview")
                              pure NoAction
                            ]
                          else if state.props.callSupportPopUp then continue state {props {callSupportPopUp = false}}
                          else if state.data.ratingViewState.openReportIssue then continue state {data {ratingViewState {openReportIssue = false}}}
                          else if state.props.showEducationalCarousel then do 
                            _ <- pure $ pauseYoutubeVideo unit
                            continue state{props{showEducationalCarousel = false}}
                          else if state.data.waitTimeInfo then continue state { data {waitTimeInfo =false} }
                          else if state.props.showSpecialZoneInfoPopup then continue state { props{ showSpecialZoneInfoPopup = false } }
                          else if state.props.zoneOtpExpired then continue state {props {zoneOtpExpired = false}}
                          else if state.props.showScheduledRideExistsPopUp then continue state { props { showScheduledRideExistsPopUp = false }}
                          else do
                              pure $ terminateApp state.props.currentStage true
                              continue state{props{showShimmer = false}}

eval GoBackToSearchLocationModal state = do
  void $ pure $ updateLocalStage SearchLocationModel
  void $ pure $ exitLocateOnMap ""
  continue state { props { rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation, isSource = Just true, isSrcServiceable = true, isRideServiceable = true } }
  -- let newState = state { props { rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation, isSource = Just true, isSrcServiceable = true, isRideServiceable = true } }
  -- updateAndExit newState $ Go_To_Search_Location_Flow newState true
eval HandleCallback state = do
  continue state { props { callbackInitiated = true } }

eval (UpdateSource lat lng name) state = do
  _ <- pure $ printLog "Name::" name
  exit $ UpdatedState state { data { source = name, sourceAddress = encodeAddress name [] state.props.sourcePlaceId lat lng}, props { sourceLat = lat, sourceLong = lng, searchLocationModelProps{crossBtnSrcVisibility = (STR.length name) > 2}} } true

eval (HideLiveDashboard val) state = continue state {props {showLiveDashboard =false, showShimmer = false}}

eval LiveDashboardAction state = do
  _ <- pure $ firebaseLogEvent "ny_user_on_ride_live_stats"
  if os == "IOS" then do
      continueWithCmd state [do
        _ <- openUrlInApp "https://nammayatri.in/open?source=in-app"
        pure NoAction
      ]
  else continue state {props {showLiveDashboard = true}}


eval (UpdateSourceName lat lon name) state = continue state {data{source = name, sourceAddress = encodeAddress name [] state.props.sourcePlaceId lat lon}, props{searchLocationModelProps{crossBtnSrcVisibility = (STR.length name) > 2}} }

eval (MAPREADY key latitude longitude) state = 
  if any (_ == state.props.currentStage) [ConfirmEditDestinationLoc, ConfirmingEditDestinationLoc, RevisedEstimate] then do
    continueWithCmd state [ do
      void $ pure $ removeAllPolylines ""
      let srcLat = state.props.sourceLat
          srcLon = state.props.sourceLong
          dstLat = state.props.destinationLat
          dstLon = state.props.destinationLong
          primaryText = state.data.destination
          markers = HU.normalRoute ""
          srcMarkerConfig = defaultMarkerConfig{ pointerIcon = markers.srcMarker }
          destMarkerConfig = defaultMarkerConfig{ pointerIcon = markers.destMarker, primaryText = primaryText, anchorU = 0.5, anchorV = 1.0 }
          routeConfig = JB.mkRouteConfig (Remote.walkCoordinate srcLat srcLon dstLat dstLon) srcMarkerConfig destMarkerConfig Nothing "NORMAL_ROUTE" "DOT" false JB.DEFAULT (JB.mapRouteConfig{vehicleSizeTagIcon = HU.getVehicleSize unit, polylineAnimationConfig = getPolylineAnimationConfig})
      void $ drawRoute [routeConfig] (getNewIDWithTag "CustomerHomeScreenEditDest")
      pure AfterRender
    ]
  else do
    case key of
      _ -> continueWithCmd state [ do
        _ <- checkPermissionAndUpdatePersonMarker state
        pure AfterRender
      ]

eval ShowBookingPreference state = continue state {props {showBookingPreference = not state.props.showBookingPreference, showMultipleRideInfo = false}}

eval OpenSearchLocation state = do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ firebaseLogEvent "ny_user_hs_pickup_click"
  let _ = unsafePerformEffect $ Events.addEventData "External.Clicked.PickupSearch" "true"
  let srcValue = if state.data.source == "" then (getString CURRENT_LOCATION) else state.data.source
  _ <- pure $ updateLocalStage SearchLocationModel
  exit $ UpdateSavedLocation state { props { isSource = Just false, currentStage = SearchLocationModel, isSearchLocation = SearchLocation, searchLocationModelProps{crossBtnSrcVisibility = (STR.length srcValue) > 2},  rideSearchProps{ sessionId = generateSessionId unit }}, data {source=srcValue, locationList = state.data.recentSearchs.predictionArray} }

eval (SourceUnserviceableActionController (ErrorModalController.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = continueWithCmd state [ do pure $ OpenSearchLocation ]

eval (LocateOnMapCallBack key lat lon) state = do
  let latitude = fromMaybe 0.0 (NUM.fromString lat)
      longitude = fromMaybe 0.0 (NUM.fromString lon)
  if os == "IOS" && not state.props.locateOnMapProps.cameraAnimatedToSource && (getDistanceBwCordinates latitude longitude state.props.sourceLat state.props.sourceLong) > 5.0 then do
    continueWithCmd state{ props{ locateOnMapProps{ cameraAnimatedToSource = true } } } [do
      void $ animateCamera state.props.sourceLat state.props.sourceLong 25.0 "NO_ZOOM"
      pure NoAction
    ]
  else do
    let updatedState = state{ props{ locateOnMapProps{ cameraAnimatedToSource = true } } }
        sourceManuallyMoved = if updatedState.props.isSource == Just true then true else updatedState.props.rideSearchProps.sourceManuallyMoved
        destManuallyMoved = if updatedState.props.isSource == Just false then true else updatedState.props.rideSearchProps.destManuallyMoved
    case key of
      "LatLon" -> do
        let selectedSpot = head (filter (\spots -> (getDistanceBwCordinates latitude longitude spots.lat spots.lng) * 1000.0 < (toNumber JB.locateOnMapConfig.thresholdDistToSpot)  ) updatedState.data.nearByPickUpPoints)
        exit $ UpdateLocationName updatedState{props{defaultPickUpPoint = "", rideSearchProps{ sourceManuallyMoved = sourceManuallyMoved, destManuallyMoved = destManuallyMoved }, hotSpot{ selectedSpot = selectedSpot }, locateOnMapProps{ isSpecialPickUpGate = false }}} latitude longitude
      _ -> do
        if state.props.currentStage == ConfirmingLocation then do
          let focusedIndex = findIndex (\item -> item.place == key) updatedState.data.nearByPickUpPoints
          case focusedIndex of
            Just index -> void $ pure $ scrollViewFocus (getNewIDWithTag "scrollViewParent") index
            Nothing -> pure unit
        else pure unit

        case (filter(\item -> item.place == key) updatedState.data.nearByPickUpPoints) !! 0 of
          Just spot -> exit $ UpdateLocationName updatedState{props{defaultPickUpPoint = key, rideSearchProps{ sourceManuallyMoved = sourceManuallyMoved, destManuallyMoved = destManuallyMoved}, locateOnMapProps{ isSpecialPickUpGate = fromMaybe false spot.isSpecialPickUp }, hotSpot{ centroidPoint = Nothing }}} spot.lat spot.lng
          Nothing -> continue updatedState

eval (CheckBoxClick autoAssign) state = do
  void $ pure $ performHapticFeedback unit
  let event = if autoAssign then "ny_user_pref_autoassigned" else "ny_user_pref_driveroffers"
  let _ = unsafePerformEffect $ logEvent state.data.logField event
  _ <- pure $ setValueToLocalStore FLOW_WITHOUT_OFFERS (show autoAssign)
  _ <- pure $ setValueToLocalStore TEST_MINIMUM_POLLING_COUNT $ if autoAssign then "4" else "17"
  _ <- pure $ setValueToLocalStore TEST_POLLING_INTERVAL $ if autoAssign then "8000.0" else "1500.0"
  _ <- pure $ setValueToLocalStore TEST_POLLING_COUNT $ if autoAssign then "22" else "117"
  continue state{props{flowWithoutOffers = autoAssign, showBookingPreference = false}, data { iopState { providerPrefVisible = false}}}

eval (OnIconClick autoAssign) state = do
  continue state { props {showMultipleRideInfo = true}}

eval PreferencesDropDown state = do
  continue state { data { showPreferences = not state.data.showPreferences}}

eval (RatingCardAC (RatingCard.Rating index)) state = do
  let feedbackListArr = if index == state.data.rideRatingState.rating then state.data.rideRatingState.feedbackList else []
  continue state { data { rideRatingState { rating = index , feedbackList = feedbackListArr}, ratingViewState { selectedRating = index} } }

eval (RatingCardAC (RatingCard.SelectPill feedbackItem id)) state = do
  let newFeedbackList = updateFeedback id feedbackItem state.data.rideRatingState.feedbackList
      filterFeedbackList = filter (\item -> length item.answer > 0) newFeedbackList
  continue state { data { rideRatingState {  feedbackList = filterFeedbackList} } }

eval (RatingCardAC (RatingCard.PrimaryButtonAC PrimaryButtonController.OnClick)) state = updateAndExit state $ SubmitRating state

eval (RatingCardAC (RatingCard.PrimaryButtonAC PrimaryButtonController.NoAction)) state = continue state

eval (RatingCardAC (RatingCard.FeedbackChanged value)) state = continue state { data { rideRatingState { feedback = value } } }

eval (RatingCardAC (RatingCard.BackPressed)) state = do
  _ <- pure $ updateLocalStage RideCompleted
  continue state {props {currentStage = RideCompleted}}

eval (SettingSideBarActionController (SettingSideBarController.PastRides)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_myrides_click"
      updatedState = state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }
  exit $ PastRides updatedState false

eval (SettingSideBarActionController (SettingSideBarController.OnHelp)) state = do 
  if state.data.config.feature.enableHelpAndSupport
    then exit $ GoToHelp state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }
    else continue state {props{isContactSupportPopUp = true}}

eval (SettingSideBarActionController (SettingSideBarController.ChangeLanguage)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_language"
  exit $ ChangeLanguage state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SettingSideBarActionController (SettingSideBarController.GoToAbout)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_about"
  exit $ GoToAbout state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SettingSideBarActionController (SettingSideBarController.GoToNammaSafety)) state = do
  exit $ GoToNammaSafety state { data { settingSideBar { opened = SettingSideBarController.OPEN } } } false false

eval (SettingSideBarActionController (SettingSideBarController.GoToMyTickets)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_zoo_tickets"
  exit $ GoToMyTickets state { data{settingSideBar{opened = SettingSideBarController.OPEN}}}

eval (SettingSideBarActionController (SettingSideBarController.GoToMyMetroTickets)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_metro_tickets"
  exit $ GoToMyMetroTickets state { data{settingSideBar{opened = SettingSideBarController.OPEN}}}

eval (SettingSideBarActionController (SettingSideBarController.ShareAppLink)) state =
  exit $ GoToReferral GIVE_REFERRAL state

eval (SettingSideBarActionController (SettingSideBarController.EditProfile)) state = exit $ GoToMyProfile state { data { settingSideBar { opened = SettingSideBarController.OPEN } } } false

eval (SettingSideBarActionController (SettingSideBarController.OnClosed)) state = continue state{ data{settingSideBar {opened = SettingSideBarController.CLOSED}}}

eval (SettingSideBarActionController (SettingSideBarController.OnClose)) state =
  if state.props.showLiveDashboard then do
    continueWithCmd state [do
      _ <- pure $ goBackPrevWebPage (getNewIDWithTag "webview")
      pure NoAction
    ]
    else if state.props.isPopUp == Logout then
      continue state {props{isPopUp = NoPopUp}}
      else case state.data.settingSideBar.opened of
                SettingSideBarController.CLOSED -> do
                                                    if state.props.currentStage == HomeScreen then do
                                                      pure $ terminateApp state.props.currentStage true
                                                      continue state
                                                      else continueWithCmd state [pure $ BackPressed]
                _                               -> continue state {data{settingSideBar{opened = SettingSideBarController.CLOSING}}}

eval (SettingSideBarActionController (SettingSideBarController.OnLogout)) state = continue state { props { isPopUp = Logout } }

eval (SettingSideBarActionController (SettingSideBarController.GoToFavourites)) state = exit $ GoToFavourites state {data{settingSideBar{opened = SettingSideBarController.OPEN}}}

eval (SettingSideBarActionController (SettingSideBarController.GoToMyProfile)) state = exit $ GoToMyProfile state { data { settingSideBar { opened = SettingSideBarController.OPEN } } } false


eval (SettingSideBarActionController (SettingSideBarController.LiveStatsDashboard)) state = openLiveDashboard state

eval OpenLiveDashboard state = openLiveDashboard state{props{showShimmer = false}}

eval (SearchLocationModelActionController (SearchLocationModelController.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ exitLocateOnMap ""
  let newState = state{props{isSource = Just false, isSearchLocation = SearchLocation, currentStage = SearchLocationModel, locateOnMap = false, defaultPickUpPoint = ""}}
  updateAndExit newState $ LocationSelected (fromMaybe dummyListItem (if state.props.isSource == Just false then state.data.selectedLocationListItem else Nothing)) (state.props.isSource == Just false) newState

eval (SearchLocationModelActionController (SearchLocationModelController.DateTimePickerButtonClicked)) state = openDateTimePicker state 

eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) newState = do
    _ <- pure $ spy "state homeScreen" newState
    let state = newState {data {rentalsInfo = Nothing}}
    case state.props.currentStage of
      HomeScreen   -> do
        void $ pure $ performHapticFeedback unit
        let _ = unsafePerformEffect $ Events.addEventData "External.Clicked.DestinationSearch" "true"
            _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_where_to_btn"
            maybeInvalidBookingDetails = invalidBookingTime (getCurrentUTC "") Nothing
        if (isJust maybeInvalidBookingDetails) then do
          continue state {data{invalidBookingId = maybe Nothing (\invalidBookingTime -> Just invalidBookingTime.bookingId) maybeInvalidBookingDetails}, props{showScheduledRideExistsPopUp = true}}
        else do
          void $ pure $ updateLocalStage SearchLocationModel
          exit $ UpdateSavedLocation state{props{isSource = Just false, isSearchLocation = SearchLocation, currentStage = SearchLocationModel, searchLocationModelProps{crossBtnSrcVisibility = false }}, data{source= state.data.source}}
      ConfirmingLocation -> do
        void $ pure $ performHapticFeedback unit
        _ <- pure $ exitLocateOnMap ""
        _ <- pure $ updateLocalStage FindingEstimate
        let _ = unsafePerformEffect $ Events.addEventData "External.Clicked.ConfirmLocation" "true"
        let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_confirm_pickup"
        let sourceAddressWard = if state.props.isSpecialZone && not (state.props.defaultPickUpPoint == "")
                                  then state.props.defaultPickUpPoint
                                  else state.data.source
        let updatedState = state{props{currentStage = FindingEstimate, locateOnMap = false}, data { iopState { showMultiProvider = false}, sourceAddress { ward = Just sourceAddressWard}}}
        updateAndExit updatedState $  (UpdatedSource updatedState)
      SettingPrice -> do
                        void $ pure $ performHapticFeedback unit
                        void $ pure $ setValueToLocalStore SELECTED_VARIANT state.data.selectedEstimatesObject.vehicleVariant
                        void $ pure $ cacheRateCard state
                        let updatedState = state{data{rideHistoryTrip = Nothing}, props{ searchExpire = (getSearchExpiryTime true)}}
                        exit $ SelectEstimateAndQuotes updatedState
      RevisedEstimate -> do
        exit $ ConfirmFare state
      _            -> continue state

eval WhereToClick state = do
  void $ pure $ performHapticFeedback unit
  let _ = unsafePerformEffect $ Events.addEventData "External.Clicked.DestinationSearch" "true"
      _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_where_to_btn"
      maybeInvalidBookingDetails = invalidBookingTime (getCurrentUTC "") Nothing
  if (isJust maybeInvalidBookingDetails) then do
    continue state {data{invalidBookingId = maybe Nothing (\invalidBookingTime -> Just invalidBookingTime.bookingId) maybeInvalidBookingDetails}, props{showScheduledRideExistsPopUp = true}}
  else do
    void $ pure $ updateLocalStage SearchLocationModel
    exit $ UpdateSavedLocation state{props{isSource = Just false, isSearchLocation = SearchLocation, currentStage = SearchLocationModel, searchLocationModelProps{crossBtnSrcVisibility = false }}, data{source= if state.data.source == "" then (getString CURRENT_LOCATION) else state.data.source}}
  


eval OpenSettings state = do
  _ <- pure $ hideKeyboardOnNavigation true
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_burger_menu"
  if state.props.isOffline then do
    void $ pure $ toast (getString CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN)
    continue state
  else
    continue state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SearchExpireCountDown seconds status timerID) state = do
  if status == "EXPIRED" then do
    _ <- pure $ clearTimerWithId timerID
    let tipViewData = HomeScreenData.initData.props.tipViewProps
    _ <- pure $ setTipViewData (TipViewData { stage : tipViewData.stage , activeIndex : tipViewData.activeIndex , isVisible : tipViewData.isVisible })
    continue state { props { searchExpire = seconds, currentStage = QuoteList } }
  else do
    let enableTips = isTipEnabled state
    if any ( _ == state.props.currentStage) [FindingQuotes , QuoteList] then continue state { props { searchExpire = seconds ,timerId = timerID , tipViewProps {isVisible = enableTips && (seconds <= (getSearchExpiryTime true)-state.data.config.tipDisplayDuration || state.props.tipViewProps.isVisible || state.props.tipViewProps.activeIndex >= 0)}, customerTip{enableTips = enableTips}} }
      else do
        _ <- pure $ clearTimerWithId timerID
        continue state { props { searchExpire = (getSearchExpiryTime true) ,timerId = timerID , tipViewProps {isVisible = false}} }

eval CancelSearch state = case state.props.currentStage of
  FindingEstimate -> do
    void $ pure $ performHapticFeedback unit
    _ <- pure $ updateLocalStage SearchLocationModel
    let _ = unsafePerformEffect $ logEvent state.data.logField  "ny_user_estimate_cancel_search"
    continue state { props { currentStage = SearchLocationModel, rideRequestFlow = false, isSearchLocation = SearchLocation } }
    -- let newState = state { props { currentStage = HomeScreen, rideRequestFlow = false, isSearchLocation = SearchLocation } }
    -- updateAndExit newState $ Go_To_Search_Location_Flow newState true
  ConfirmingRide -> do
    void $ pure $ performHapticFeedback unit
    continue state { props { currentStage = SettingPrice, isSearchLocation = NoView } }
  _ -> continue state

eval SidebarCloseAnimationCompleted state = continue state --{props{sideBarStatus = SettingSideBarController.CLOSED}}

eval OpenPricingTutorial state = continue state { props { currentStage = PricingTutorial } }

eval (PricingTutorialModelActionController (PricingTutorialModelController.Close)) state = continue state { props { currentStage = SettingPrice } }

eval (DriverInfoCardActionController (DriverInfoCardController.PrimaryButtonAC PrimaryButtonController.OnClick)) state = do
  void $ pure $ performHapticFeedback unit
  continueWithCmd state
    [ do
        _ <- pure $ showDialer (getDriverNumber "") false -- TODO: FIX_DIALER
        _ <- (logEventWithTwoParams state.data.logField "ny_user_call_click" "trip_id" (state.props.bookingId) "user_id" (getValueToLocalStore CUSTOMER_ID))
        pure NoAction
    ]
eval (DriverArrivedAction driverArrivalTime) state =
  if any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver] then do
      _ <- pure $ setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_WAITING_ACTION"
      exit $ RefreshHomeScreen state { data { driverInfoCardState { driverArrived = true, driverArrivalTime = getExpiryTime driverArrivalTime true } } }
    else continue state

eval (WaitingTimeAction timerID timeInMinutes seconds) state = do
  _ <- pure $ if getValueToLocalStore DRIVER_ARRIVAL_ACTION == "TRIGGER_WAITING_ACTION"
                then setValueToLocalStore DRIVER_ARRIVAL_ACTION "WAITING_ACTION_TRIGGERED"
                else pure unit
  continue state { data { driverInfoCardState { waitingTime = timeInMinutes} }, props { waitingTimeTimerIds = union state.props.waitingTimeTimerIds [timerID] } }

eval (DriverInfoCardActionController (DriverInfoCardController.RideDurationTimer timerID timeInHHMM _)) state = 
  continue state{props{rideDurationTimerId = timerID, rideDurationTimer = timeInHHMM}}

eval (RideDurationTimer timerID timeInHHMM _) state = 
  continue state{props{rideDurationTimerId = timerID, rideDurationTimer = timeInHHMM}}

eval (SpecialZoneOTPExpiryAction seconds status timerID) state = do
  if status == "EXPIRED" then do
    _ <- pure $ toast $ getString $ OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN "OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN"
    _ <- pure $ clearTimerWithId timerID
    continue state{props{zoneOtpExpired = true}}
  else do
    let timeInMinutes = formatDigits $ seconds/60
        timeInSeconds = formatDigits $ seconds - (seconds/60) * 60
    continue state { data { driverInfoCardState { waitingTime = timeInMinutes <> " : " <> timeInSeconds } }, props { waitingTimeTimerIds = union state.props.waitingTimeTimerIds [timerID] } }
  where
    formatDigits :: Int -> String
    formatDigits time = (if time >= 10 then "" else "0") <> show time

eval (DriverInfoCardActionController (DriverInfoCardController.OnNavigate mode lat lon)) state = do
  void $ pure $ openNavigation lat lon (show mode)
  continue state 

eval (DriverInfoCardActionController (DriverInfoCardController.ShowDirections lat lon)) state = 
  case state.props.zoneType.sourceTag , state.data.driverInfoCardState.addressWard, state.data.driverInfoCardState.spLocationName of 
        ST.AIRPORT, Just ward, Just spLocationName -> exit $ ExitToPickupInstructions state lat lon ward spLocationName
        _ , _, _ -> continueWithCmd state [pure $ DriverInfoCardActionController (DriverInfoCardController.OnNavigate ST.WALK lat lon)]
  
eval (ZoneTimerExpired (PopUpModal.OnButton2Click)) state = continue state{props{zoneOtpExpired = false}}

eval (DriverInfoCardActionController (DriverInfoCardController.RideSupport)) state = do
  void $ pure $ performHapticFeedback unit
  continue state{props{callSupportPopUp = true}}

eval (CancelSearchAction PopUpModal.DismissPopup) state = do continue state {props { cancelSearchCallDriver = false }}

eval (CancelSearchAction PopUpModal.OnButton1Click) state = do
  if length state.data.config.callOptions > 1 then
    continue state { props { showCallPopUp = true, cancelSearchCallDriver = false } }
  else callDriver state $ fromMaybe "ANONYMOUS" $ state.data.config.callOptions !! 0

eval (CancelSearchAction PopUpModal.OnButton2Click) state = do
  let isAcCab = ServiceTierCard.showACDetails (fromMaybe "" state.data.driverInfoCardState.serviceTierName) Nothing
                && state.data.currentCityConfig.enableAcViews
      cancellationReasons = cancelReasons isAcCab
      noOfReasons = length cancellationReasons
      shuffledCancellationReason = shuffle (take ( noOfReasons - 1) cancellationReasons ) <> (take 1 (drop (noOfReasons - 1) cancellationReasons) )
  continue state { props {  isCancelRide = true
                          , cancellationReasons = if state.data.config.cancelReasonConfig.shuffleCancelReasons then shuffledCancellationReason else cancellationReasons
                          , cancelRideActiveIndex = Nothing
                          , cancelReasonCode = ""
                          , cancelDescription = ""
                          , cancelSearchCallDriver = false } }             

eval (DriverInfoCardActionController (DriverInfoCardController.CancelRide infoCard)) state =
  if (state.data.config.driverInfoConfig.showCancelPrevention && not state.props.isSpecialZone) || state.props.zoneType.sourceTag == METRO then
    continue state { props { cancelSearchCallDriver = true } }
      else continueWithCmd state [ pure $ CancelSearchAction PopUpModal.OnButton2Click]

eval (DriverInfoCardActionController (DriverInfoCardController.LocationTracking)) state = do
  void $ pure $ performHapticFeedback unit
  continue state { props { isLocationTracking = true } }

eval OpenEmergencyHelp state = do
  void $ pure $ performHapticFeedback unit
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_ic_safety_center_clicked"
  if state.props.isOffline then do
    void $ pure $ toast (getString CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN)
    continue state
  else do
    exit $ GoToNammaSafety state true false

eval OpenOffUsSOS state = do
  void $ pure $ performHapticFeedback unit
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_ic_safety_center_clicked"
  exit $ GoToSafetySettingScreen

eval (DriverInfoCardActionController (DriverInfoCardController.ToggleBottomSheet)) state = continue state{props{currentSheetState = if state.props.currentSheetState == EXPANDED then COLLAPSED else EXPANDED}}

eval (DriverInfoCardActionController (DriverInfoCardController.ShareRide)) state = 
  if state.props.isOffline then do
    void $ pure $ toast (getString CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN)
    continue state
  else do
    if state.data.config.feature.shareWithEmergencyContacts 
    then exit $ GoToShareRide state
    else continueWithCmd state [pure ShareRide]

eval ShareRide state = do
  continueWithCmd state
        [ do
            let appName = fromMaybe state.data.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
            _ <- pure $ shareTextMessage "" $ getString $ TRACK_RIDE_STRING appName state.data.driverInfoCardState.driverName (state.data.config.appData.website <> "t?i="<>state.data.driverInfoCardState.rideId) state.data.driverInfoCardState.registrationNumber
            void $ pure $ cleverTapCustomEvent "ny_user_share_ride_via_link"
            pure NoAction
         ]

eval (CancelRidePopUpAction (CancelRidePopUp.Button1 PrimaryButtonController.OnClick)) state = do
      void $ pure $ performHapticFeedback unit
      continue state { props { isCancelRide = false } }

eval (CancelRidePopUpAction (CancelRidePopUp.OnGoBack)) state = continue state { props { isCancelRide = false } }

eval (CancelRidePopUpAction (CancelRidePopUp.UpdateIndex index)) state = continue state { props { cancelRideActiveIndex = Just index, cancelReasonCode = (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode } }

eval (CancelRidePopUpAction (CancelRidePopUp.TextChanged valId newVal)) state = continue state { props { cancelDescription = newVal } }

eval (CancelRidePopUpAction (CancelRidePopUp.ClearOptions)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continue state { props { cancelDescription = "", cancelReasonCode = "", cancelRideActiveIndex = Nothing } }

eval (CancelRidePopUpAction (CancelRidePopUp.Button2 PrimaryButtonController.OnClick)) state = do
  if state.props.isOffline then do
    void $ pure $ toast (getString CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN)
    continue state
  else do
    let _ = unsafePerformEffect $ Events.addEventData ("External.Clicked.Search." <> state.props.searchId <> ".CancelRide") "true"
    void $ pure $ performHapticFeedback unit
    case state.props.cancelRideActiveIndex of
      Just index -> if ( (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode == "OTHER" || (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode == "TECHNICAL_GLITCH" ) then exit $ CancelRide state{props{cancelDescription = if (state.props.cancelDescription == "") then (fromMaybe dummyCancelReason (state.props.cancellationReasons !!index)).description else state.props.cancelDescription }} NORMAL_RIDE_CANCEL
                      else exit $ CancelRide state{props{cancelDescription = (fromMaybe dummyCancelReason (state.props.cancellationReasons !!index)).description , cancelReasonCode = (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode }} NORMAL_RIDE_CANCEL
      Nothing    -> continue state

eval (PredictionClickedAction (LocationListItemController.OnClick item)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_prediction_list_item"
  locationSelected item false state{data{source = if state.data.source == "" then (getString CURRENT_LOCATION) else state.data.source}, props{isSource = Just false}} (state.props.currentStage == EditingDestinationLoc) 

eval (SuggestedDestinationClicked item isFamousDest) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_sd_list_item"
  let _ = unsafePerformEffect $ Events.addEventData "External.Clicked.SuggestedDestination" "true"
  locationSelected item (not isFamousDest) state{props{isSource = Just false, rideSearchProps{sessionId = generateSessionId unit}, suggestedRideFlow = true}, data{source = if state.data.source == "" then (getString CURRENT_LOCATION) else state.data.source, nearByPickUpPoints = [], polygonCoordinates = ""}} false

eval (PredictionClickedAction (LocationListItemController.FavClick item)) state = do
  if (length state.data.savedLocations >= 20) then do
    void $ pure $ toast (getString SORRY_LIMIT_EXCEEDED_YOU_CANT_ADD_ANY_MORE_FAVOURITES)
    continue state
    else exit $ CheckFavDistance state{data{saveFavouriteCard{ address = item.description, selectedItem = item, tag = "", tagExists = false, isBtnActive = false }, selectedLocationListItem = Just item}}

eval (SaveFavouriteCardAction (SaveFavouriteCardController.OnClose)) state = continue state{props{isSaveFavourite = false},data{selectedLocationListItem = Nothing, saveFavouriteCard {address = "" , tag = "", isBtnActive = false}}}

eval (SaveFavouriteCardAction (SaveFavouriteCardController.SaveFavourite)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  exit $ SaveFavourite state{props{isSaveFavourite = false},data{selectedLocationListItem = Nothing}}

eval (SaveFavouriteCardAction (SaveFavouriteCardController.PrimayEditTA (PrimaryEditTextController.TextChanged id val))) state = do
  let input = STR.trim val
  let updatedState = state{data{saveFavouriteCard{isBtnActive = ((STR.length input) >=3),tagExists = not (validTag (getSavedTagsFromHome state.data.savedLocations) input ""),tag = input}}}
  continue updatedState

eval (SearchLocationModelActionController (SearchLocationModelController.LocationListItemActionController (LocationListItemController.FavClick item))) state = continueWithCmd state [pure $ (PredictionClickedAction (LocationListItemController.FavClick item))]

eval (FavouriteLocationModelAC (FavouriteLocationModelController.GenericHeaderAC (GenericHeaderController.PrefixImgOnClick))) state = continue state { props { currentStage = if state.props.isSearchLocation == NoView then HomeScreen else if state.props.currentStage == FavouriteLocationModelEditDest then EditingDestinationLoc else  SearchLocationModel} }

eval (FavouriteLocationModelAC (FavouriteLocationModelController.FavouriteLocationAC (SavedLocationCardController.CardClicked item))) state = do
  if state.props.isSource == Just true then do
    let newState = state {data{ source = item.savedLocation, sourceAddress = item.fullAddress},props{sourcePlaceId = item.placeId,sourceLat = fromMaybe 0.0 item.lat,sourceLong =fromMaybe 0.0  item.lon, rideSearchProps{ sourceSelectType = ST.FAVOURITE } }}
    pure $ setText (getNewIDWithTag "SourceEditText") item.savedLocation
    exit $ LocationSelected item  false newState
    else do
      let newState = state {data{ destination = item.savedLocation,destinationAddress = item.fullAddress},props{destinationPlaceId = item.placeId, destinationLat = fromMaybe 0.0 item.lat, destinationLong = fromMaybe 0.0 item.lon}}
      pure $ setText (getNewIDWithTag "DestinationEditText") item.savedLocation
      if state.props.currentStage == FavouriteLocationModelEditDest then
        exit $ EditDestLocationSelected item false newState
      else
        exit $ LocationSelected item false newState
      

eval (SavedAddressClicked (LocationTagBarController.TagClick savedAddressType arrItem)) state = if not state.props.isSrcServiceable then continue state else do 
  _ <- pure $ firebaseLogEvent ("ny_user_savedLoc_" <> show savedAddressType)
  let _ = unsafePerformEffect $ Events.addEventData ("External.Clicked.SavedLocation." <> show savedAddressType) "true"
  tagClickEvent savedAddressType arrItem state{data{source = if state.data.source == "" then (getString CURRENT_LOCATION) else state.data.source},props{isSource = Just false}} false

eval (SearchLocationModelActionController (SearchLocationModelController.SavedAddressClicked (LocationTagBarController.TagClick savedAddressType arrItem))) state = tagClickEvent savedAddressType arrItem state false

eval (TagClick savedAddressType arrItem) state = tagClickEvent savedAddressType arrItem state false

eval (SearchLocationModelActionController (SearchLocationModelController.LocationListItemActionController (LocationListItemController.OnClick item))) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_location_list_item"
  let condition = state.props.isSource == Just true && any (_ == item.locationItemType) [Just RECENTS, Just SUGGESTED_DESTINATIONS] 
  locationSelected item {tag = if condition then "" else item.tag, showDistance = Just false} true state{ props { rideSearchProps{ sourceSelectType = if condition then ST.SUGGESTION else state.props.rideSearchProps.sourceSelectType } }, data { nearByDrivers = Nothing } } false

eval (ExitLocationSelected item addToRecents)state = exit $ LocationSelected item  addToRecents state

eval (SearchLocationModelActionController (SearchLocationModelController.DebounceCallBack searchString isSource)) state = do
  if (STR.length searchString > 2) && (isSource == fromMaybe true state.props.isSource) then 
    validateSearchInput state searchString
  else continue state

eval (SearchLocationModelActionController (SearchLocationModelController.SourceChanged input)) state = do
  let srcValue = (state.data.source == "" || state.data.source == "Current Location")
  let sourceSelectType = if state.props.locateOnMap then ST.MAP else state.props.rideSearchProps.sourceSelectType
      newState = state {props{ rideSearchProps{ sourceSelectType = sourceSelectType } }}
  if (input /= state.data.source) then do 
    continueWithCmd newState { props { isRideServiceable = true, searchLocationModelProps{crossBtnSrcVisibility = (STR.length input) > 2, isAutoComplete = if (STR.length input) > 2 then state.props.searchLocationModelProps.isAutoComplete else false}}} 
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]
  else
    continueWithCmd newState{props {searchLocationModelProps{crossBtnSrcVisibility = (STR.length input) > 2, isAutoComplete = false}}}
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]

eval (SearchLocationModelActionController (SearchLocationModelController.DestinationChanged input)) state = do
  if (input /= state.data.destination) then do
    continueWithCmd state { props { isRideServiceable = true, searchLocationModelProps{crossBtnDestVisibility = (STR.length input) > 2, isAutoComplete = if (STR.length input)>2 then state.props.searchLocationModelProps.isAutoComplete else false}} }
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]
  else
    continueWithCmd state{props {searchLocationModelProps{crossBtnDestVisibility = (STR.length input) > 2, isAutoComplete = false}}}
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]

eval (SearchLocationModelActionController (SearchLocationModelController.EditTextFocusChanged textType)) state = do
  _ <- pure $ spy "searchLocationModal" textType
  if textType == "D" then
    continue state { props { isSource = Just false, searchLocationModelProps{crossBtnDestVisibility = (STR.length state.data.destination) > 2}}, data {source = if state.data.source == "" then state.data.searchLocationModelData.prevLocation else state.data.source, locationList = if state.props.isSource == Just false then state.data.locationList else state.data.destinationSuggestions } }
  else
    continue state { props { isSource = Just true, searchLocationModelProps{crossBtnSrcVisibility = (STR.length state.data.source) > 2}} , data{ locationList = if state.props.isSource == Just true then state.data.locationList else state.data.recentSearchs.predictionArray } }

eval (SearchLocationModelActionController (SearchLocationModelController.NoAction)) state = continue state

eval (SearchLocationModelActionController (SearchLocationModelController.SourceClear)) state = do
  void $ pure $ performHapticFeedback unit
  if (state.props.isSearchLocation /= LocateOnMap) then do
    _ <- pure $ requestKeyboardShow (getNewIDWithTag "SourceEditText")
    pure unit
  else
    pure unit
  let predicArray = (updateLocListWithDistance state.data.recentSearchs.predictionArray state.props.sourceLat state.props.sourceLong true state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist)
  continue state { data { source = "", recentSearchs {predictionArray = predicArray}, locationList = predicArray, searchLocationModelData{prevLocation = state.data.source}}, props { isSource = Just true, isSrcServiceable = true, isRideServiceable = true, searchLocationModelProps{crossBtnSrcVisibility = false} } }

eval (SearchLocationModelActionController (SearchLocationModelController.DestinationClear)) state = do
  void $ pure $ performHapticFeedback unit
  if (state.props.isSearchLocation /= LocateOnMap) then do
    _ <- pure $ requestKeyboardShow (getNewIDWithTag "DestinationEditText")
    pure unit
  else
    pure unit
  let predicArray = (updateLocListWithDistance state.data.recentSearchs.predictionArray state.props.sourceLat state.props.sourceLong true state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist)
  continue state { data { destination = "", locationList = predicArray }, props {isSource = Just false, isDestServiceable = true, isRideServiceable = true, searchLocationModelProps{crossBtnDestVisibility = false}} }

eval (SearchLocationModelActionController (SearchLocationModelController.GoBack)) state = do
  void $ pure $ performHapticFeedback unit
  continueWithCmd state
    [ do
        _ <- pure $ hideKeyboardOnNavigation true
        pure $ BackPressed
    ]

eval (SearchLocationModelActionController (SearchLocationModelController.SetCurrentLocation)) state = do
  _ <- pure $ currentPosition ""
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_currentlocation_click"
  pure $ setText (getNewIDWithTag "SourceEditText") (if (state.data.source == "") then (getString CURRENT_LOCATION) else state.data.source)
  continue state{ props{ rideSearchProps{ sourceSelectType = if state.props.isSource == Just true then ST.SEARCH else state.props.rideSearchProps.sourceSelectType }, searchLocationModelProps{isAutoComplete = false}}, data{source = if state.props.currentLocation.place /= "" then state.props.currentLocation.place else (getString CURRENT_LOCATION)}}

eval (SearchLocationModelActionController (SearchLocationModelController.SetLocationOnMap)) state = do
  void $ pure $ performHapticFeedback unit
  let isSource = case state.props.isSource of
                    Just true -> true
                    _         -> false
      isDestinationNotEmpty = (not isSource && state.props.destinationLat /= 0.0 && state.props.destinationLong /= 0.0)
      lat = if isDestinationNotEmpty then state.props.destinationLat else state.props.sourceLat
      lon = if isDestinationNotEmpty then state.props.destinationLong else state.props.sourceLong
  _ <- pure $ hideKeyboardOnNavigation true
  _ <- pure $ removeAllPolylines ""
  _ <- pure $ unsafePerformEffect $ runEffectFn1 locateOnMap locateOnMapConfig { lat = lat, lon = lon, geoJson = state.data.polygonCoordinates, points = state.data.nearByPickUpPoints, zoomLevel = pickupZoomLevel, labelId = getNewIDWithTag "LocateOnMapPin", locationName = fromMaybe "" state.props.locateOnMapProps.sourceLocationName, specialZoneMarkerConfig{ labelImage = zoneLabelIcon state.props.confirmLocationCategory }}
  pure $ unsafePerformEffect $ logEvent state.data.logField if state.props.isSource == Just true  then "ny_user_src_set_location_on_map" else "ny_user_dest_set_location_on_map"
  let srcValue = if state.data.source == "" then getString CURRENT_LOCATION else state.data.source
  when (state.data.destination == "") $ do
    pure $ setText (getNewIDWithTag "DestinationEditText") ""
  let newState = state
                  { data {source = srcValue}
                  , props { isSearchLocation = LocateOnMap
                          , currentStage = SearchLocationModel
                          , locateOnMap = true
                          , isRideServiceable = true
                          , showlocUnserviceablePopUp = false
                          , searchLocationModelProps{isAutoComplete = false}
                          , locateOnMapLocation
                             { sourceLat = state.props.sourceLat
                             , sourceLng = state.props.sourceLong
                             , source = state.data.source
                             , sourceAddress = state.data.sourceAddress
                             , destinationLat = if state.props.destinationLat /= 0.0 then state.props.destinationLat else state.props.sourceLat
                             , destinationLng = if state.props.destinationLong /= 0.0 then state.props.destinationLong else state.props.sourceLong
                             , destination = state.data.destination
                             , destinationAddress = state.data.destinationAddress 
                             }
                          , hasEstimateBackpoint = false
                          }
                    }
  (updateAndExit newState) $ UpdatedState newState false

eval (SearchLocationModelActionController (SearchLocationModelController.UpdateSource lat lng name)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  if state.props.isSource == Just true then do
    let newState = state{data{source = if state.data.source == "" then getString CURRENT_LOCATION else state.data.source, sourceAddress = encodeAddress name [] Nothing lat lng},props{ sourceLat= lat,  sourceLong = lng, sourcePlaceId = Nothing, searchLocationModelProps{isAutoComplete = false}}}
    updateAndExit newState $ LocationSelected (fromMaybe dummyListItem newState.data.selectedLocationListItem) false newState
    else do
      let newState = state{data{destination = name,destinationAddress = encodeAddress name [] Nothing lat lng},props{ destinationLat = lat,  destinationLong = lng, destinationPlaceId = Nothing}}
      updateAndExit newState $ LocationSelected (fromMaybe dummyListItem newState.data.selectedLocationListItem) false newState

eval (QuoteListModelActionController (QuoteListModelController.QuoteListItemActionController (QuoteListItemController.Click quote))) state = do
  void $ pure $ performHapticFeedback unit
  continueWithCmd (state { data { quoteListModelState = map (\x -> x { selectedQuote = (Just quote.id) }) state.data.quoteListModelState }, props { selectedQuote = Just quote.id } })
    [ do
        if (getValueToLocalStore AUTO_SELECTING) == "CANCELLED_AUTO_ASSIGN" then
          pure NoAction
        else do
          void $ pure $ setValueToLocalStore AUTO_SELECTING quote.id
          pure NoAction
    ]


eval (QuoteListModelActionController (QuoteListModelController.CancelAutoAssigning)) state = do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ setValueToLocalStore AUTO_SELECTING "CANCELLED_AUTO_ASSIGN"
  continue state


eval (QuoteListModelActionController (QuoteListModelController.TipViewPrimaryButtonClick PrimaryButtonController.OnClick)) state = do
  let _ = unsafePerformEffect $ Events.addEventData ("External.Clicked.Search." <> state.props.searchId <> ".Tip") "true"
  let tipConfig = getTipConfig state.data.selectedEstimatesObject.vehicleVariant
      customerTipArrayWithValues = tipConfig.customerTipArrayWithValues
  _ <- pure $ clearTimerWithId state.props.timerId
  let tipViewData = state.props.tipViewProps{stage = TIP_ADDED_TO_SEARCH, onlyPrimaryText = true}
  let newState = state{ props{rideSearchProps{ sourceSelectType = ST.RETRY_SEARCH }, findingRidesAgain = true ,searchExpire = (getSearchExpiryTime true), currentStage = TryAgain, isPopUp = NoPopUp ,tipViewProps = tipViewData ,customerTip {tipForDriver = (fromMaybe 0 (customerTipArrayWithValues !! state.props.tipViewProps.activeIndex)) , tipActiveIndex = state.props.tipViewProps.activeIndex, isTipSelected = true } }, data{nearByDrivers = Nothing}}
  _ <- pure $ setTipViewData (TipViewData { stage : tipViewData.stage , activeIndex : tipViewData.activeIndex , isVisible : tipViewData.isVisible })
  updateAndExit newState $ RetryFindingQuotes false newState

eval (QuoteListModelActionController (QuoteListModelController.TipsViewActionController (TipsView.TipBtnClick index value))) state = do
  let check = index == state.props.tipViewProps.activeIndex
  continue state { props {tipViewProps { stage = (if check then DEFAULT else TIP_AMOUNT_SELECTED) , isprimaryButtonVisible = not check , activeIndex = (if check then -1 else index)}}}

eval (QuoteListModelActionController (QuoteListModelController.QuoteListItemActionController QuoteListItemController.ConfirmRide)) state = do
  void $ pure $ performHapticFeedback unit
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_quote_confirm"
  exit $ ConfirmRide state

eval (QuoteListModelActionController (QuoteListModelController.QuoteListItemActionController (QuoteListItemController.CountDown seconds status id))) state = do
  if status == "EXPIRED" then do
    _ <- pure $ clearTimerWithId id
    let
      autoSelecting = (getValueToLocalStore AUTO_SELECTING) == id
    if (id == fromMaybe "" state.props.selectedQuote && autoSelecting && state.props.currentStage == QuoteList) then do
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_auto_assign"
      continueWithCmd state [ pure $ (QuoteListModelActionController (QuoteListModelController.PrimaryButtonActionController PrimaryButtonController.OnClick)) ]
    else do
      let
        newState = state { data { quoteListModelState = filter (\x -> x.id /= id) state.data.quoteListModelState } }
      continue newState { props { selectedQuote = if newState.data.quoteListModelState == [] then Nothing else newState.props.selectedQuote, expiredQuotes = (snoc state.props.expiredQuotes id) } }
  else do
    let
      newState = state { data = state.data { quoteListModelState = map (\x -> if x.id == id then x { timer = (show seconds) } else x) state.data.quoteListModelState } }
    continue newState { props { selectedQuote = if newState.data.quoteListModelState == [] then Nothing else newState.props.selectedQuote } }

eval (QuoteListModelActionController (QuoteListModelController.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = do
  void $ pure $ performHapticFeedback unit
  case state.props.selectedQuote, (null state.data.quoteListModelState) of
    Just _, false -> do
      _ <- pure $ updateLocalStage ConfirmingRide
      let
        newState = state { props { currentStage = ConfirmingRide } }
      updateAndExit newState $ ConfirmRide newState
    _, _ -> continue state

eval (QuoteListModelActionController (QuoteListModelController.GoBack)) state = do
  void $ pure $ performHapticFeedback unit
  continueWithCmd state [ do pure $ BackPressed ]

eval (QuoteListModelActionController (QuoteListModelController.ChangeTip)) state = do
  continue state {props { tipViewProps {stage = DEFAULT}}}

eval (Restart err) state = exit $ LocationSelected (fromMaybe dummyListItem state.data.selectedLocationListItem) false state

eval (ContactSupportAction (PopUpModal.DismissPopup)) state = continue state{props{isContactSupportPopUp = false}}

eval (ContactSupportAction (PopUpModal.OnSecondaryTextClick)) state =   
    continueWithCmd state{props{isContactSupportPopUp = false}} [do
        void $ openUrlInMailApp $ mailToLink <> (getAppConfig appConfig).appData.supportMail
        pure NoAction
    ]

eval (ContactSupportAction (PopUpModal.OnButton1Click)) state = do
    void $ pure $ showDialer (getSupportNumber "") false
    continue state{props{isContactSupportPopUp = false}}

eval (ContactSupportAction (PopUpModal.OnButton2Click)) state = continueWithCmd state [pure $ ContactSupportAction (PopUpModal.DismissPopup)]

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state =   case state.props.isPopUp of     
  TipsPopUp -> do
    void $ pure $ performHapticFeedback unit
    let _ = unsafePerformEffect $ logEvent state.data.logField if state.props.customerTip.isTipSelected then ("ny_added_tip_for_" <> (show state.props.currentStage)) else "ny_no_tip_added"
    _ <- pure $ clearTimerWithId state.props.timerId
    let tipViewData = state.props.tipViewProps{stage = RETRY_SEARCH_WITH_TIP , isVisible = not (state.props.customerTip.tipActiveIndex == 0) , activeIndex = state.props.customerTip.tipActiveIndex, onlyPrimaryText = true}
    let newState = state{ props{findingRidesAgain = true ,searchExpire = (getSearchExpiryTime true), currentStage = RetryFindingQuote, isPopUp = NoPopUp ,tipViewProps = tipViewData, rideSearchProps{ sourceSelectType = ST.RETRY_SEARCH } }}
    _ <- pure $ setTipViewData (TipViewData { stage : tipViewData.stage , activeIndex : tipViewData.activeIndex , isVisible : tipViewData.isVisible })
    logInfo "retry_finding_quotes" ( "TipConfirmed : Current Stage: " <> (show newState.props.currentStage) <> " LOCAL_STAGE : " <> (getValueToLocalStore LOCAL_STAGE) <> "Estimate Id:" <> state.props.estimateId)
    exit $ RetryFindingQuotes true newState
  Logout -> continue state{props{isPopUp = NoPopUp}}
  CancelConfirmingQuotes -> continue state{props{isPopUp = NoPopUp}}
  _ -> do
    void $ pure $ performHapticFeedback unit
    _ <- pure $ firebaseLogEvent "ny_tip_not_applicable" 
    if (isLocalStageOn FindingQuotes ) then do
        _ <- pure $ clearTimerWithId state.props.timerId
        let tipViewData = HomeScreenData.initData.props.tipViewProps
        _ <- pure $ setTipViewData (TipViewData { stage : tipViewData.stage , activeIndex : tipViewData.activeIndex , isVisible : tipViewData.isVisible })
        exit $ RepeatSearch state{props{customerTip = HomeScreenData.initData.props.customerTip, tipViewProps = HomeScreenData.initData.props.tipViewProps, isPopUp = NoPopUp, selectedQuote = Nothing, isRepeatRide = false}, data{quoteListModelState = []}}
      else if state.data.iopState.providerSelectionStage then do
      _ <- pure $ updateLocalStage SearchLocationModel
      void $ pure $ clearTimerWithId state.data.iopState.timerId
      continue state{data{rideHistoryTrip = Nothing, iopState{ providerSelectionStage = false}},props{ isPopUp = NoPopUp, rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation, isRepeatRide = false}}
      else do
      _ <- pure $ clearTimerWithId state.props.timerId
      let newState = state{props{findingRidesAgain = true , searchExpire = (getSearchExpiryTime true), currentStage = RetryFindingQuote, isPopUp = NoPopUp, rideSearchProps{ sourceSelectType = ST.RETRY_SEARCH }}}
      updateAndExit newState $ RetryFindingQuotes true newState

eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = case state.props.isPopUp of
    TipsPopUp -> case state.props.currentStage of
      QuoteList -> do
        void $ pure $ performHapticFeedback unit
        updateAndExit state CheckCurrentStatus
      FindingQuotes -> do
        void $ pure $ performHapticFeedback unit
        exit $ CheckCurrentStatus
      _ -> continue state
    Logout -> exit LogoutUser
    ConfirmBack -> do
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_no_retry"
      case (getValueToLocalStore LOCAL_STAGE) of
        "QuoteList" -> do
          void $ pure $ performHapticFeedback unit
          exit $ CheckCurrentStatus
        "SettingPrice" -> do
          void $ pure $ performHapticFeedback unit
          continue state{props{isPopUp = NoPopUp}}
        "FindingQuotes" -> do
          void $ pure $ performHapticFeedback unit
          continue state{props{isPopUp = NoPopUp}}
        _ -> continue state
    NoPopUp -> continue state
    CancelConfirmingQuotes -> exit $ CancelRide state RENTAL_SEARCH_CANCEL
    ActiveQuotePopUp -> do
      void $ pure $ performHapticFeedback unit
      exit $ CheckCurrentStatus

eval (PopUpModalAction (PopUpModal.TipsViewActionController (TipsView.TipBtnClick index value))) state = do
  void $ pure $ performHapticFeedback unit
  case state.props.isPopUp of
    TipsPopUp -> continue state{props{customerTip{tipActiveIndex = index, tipForDriver= value, isTipSelected = not (index == 0)}}}
    _ -> continue state

eval (PopUpModalAction (PopUpModal.DismissPopup)) state = do
  let newState = if (isLocalStageOn QuoteList) then state else state{props{isPopUp = NoPopUp, customerTip{tipActiveIndex = 1,tipForDriver = 10, isTipSelected = false} }}
  continue newState

eval (DistanceOutsideLimitsActionController (PopUpModal.OnButton2Click)) state = do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ updateLocalStage SearchLocationModel
  continue state { props { isPopUp = NoPopUp, rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation, isSource = Just false, isSrcServiceable = true, isDestServiceable = true, isRideServiceable = true } }
  -- _ <- pure $ updateLocalStage HomeScreen
  -- let newState = state { props { isPopUp = NoPopUp, rideRequestFlow = false, currentStage = HomeScreen, searchId = "", isSearchLocation = SearchLocation, isSource = Just false, isSrcServiceable = true, isDestServiceable = true, isRideServiceable = true } }
  -- updateAndExit newState $ Go_To_Search_Location_Flow newState true

eval (ShortDistanceActionController (PopUpModal.OnButton2Click)) state = do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ exitLocateOnMap ""
  exit $ UpdatedSource state

eval (ShortDistanceActionController (PopUpModal.OnButton1Click)) state = do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ updateLocalStage SearchLocationModel
  continue state{props{isSource = Just false, isPopUp = NoPopUp, rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation}}
  -- _ <- pure $ updateLocalStage HomeScreen
  -- let newState = state{props{isSource = Just false, isPopUp = NoPopUp, rideRequestFlow = false, currentStage = HomeScreen, searchId = "", isSearchLocation = SearchLocation}}
  -- updateAndExit newState $ Go_To_Search_Location_Flow newState true

eval (PickUpFarFromCurrentLocAC (PopUpModal.OnButton2Click)) state = do 
  if (state.props.isShorterTrip)  then do 
    void $ pure $ updateLocalStage ShortDistance
    continue state {props{currentStage = ShortDistance}}
    else continueWithCmd state [ do pure $ ShortDistanceActionController (PopUpModal.OnButton2Click) ]

eval (PickUpFarFromCurrentLocAC (PopUpModal.OnButton1Click)) state = do 
  continueWithCmd state [ do pure $ ShortDistanceActionController (PopUpModal.OnButton1Click) ]

eval (EstimateChangedPopUpController (PopUpModal.OnButton1Click)) state = exit $ GoToHome state

eval (EstimateChangedPopUpController (PopUpModal.OnButton2Click)) state = do
  let updatedState = state { props { isEstimateChanged = false, searchExpire = (getSearchExpiryTime true) } }
  exit $ SelectEstimateAndQuotes updatedState

eval CloseLocationTracking state = continue state { props { isLocationTracking = false } }

eval CloseShowCallDialer state = continue state { props { showCallPopUp = false } }

eval (ShowCallDialer item) state = do
  case item of
    ANONYMOUS_CALLER -> callDriver state "ANONYMOUS"
    DIRECT_CALLER -> callDriver state "DIRECT"

eval (DriverInfoCardActionController (DriverInfoCardController.StartLocationTracking item)) state = continueWithCmd state [do pure $ StartLocationTracking item]

eval (StartLocationTracking item) state = do
  void $ pure $ performHapticFeedback unit
  case item of
    "GOOGLE_MAP" -> do
      let
        newState = state { props { isLocationTracking = false } }
      updateAndExit (newState) (OpenGoogleMaps newState)
    "IN_APP" -> exit $ InAppTrackStatus state { props { isInApp = not state.props.isInApp, isLocationTracking = false, forFirst = true } }
    _ -> continue state

eval (GetEstimates (GetQuotesRes quotesRes) count ) state = do
  logStatus "finding_estimates_and_quotes" quotesRes
  let 
    alreadyGotEstimates = not $ null $ state.data.specialZoneQuoteList 
    estimates = getEstimateList quotesRes.estimates state.data.config.estimateAndQuoteConfig state.data.selectedEstimatesObject.activeIndex
    quotes = filter filterNonAcAsPerGates $ getSpecialZoneQuotes quotesRes.quotes state.data.config.estimateAndQuoteConfig (state.data.fareProductType == FPT.INTER_CITY)
    allQuoteListWithUpdatedIndex = spy "debug quotes allQuoteListWithUpdatedIndex" (mapWithIndex (\index item -> item{ index = index }) (estimates <> quotes))
    quoteList = filter (\item -> item.providerType == ONUS || (item.providerType == OFFUS && state.data.currentCityConfig.iopConfig.enable)) allQuoteListWithUpdatedIndex
    repeatRideFailCheck =  not $ checkRecentRideVariantInEstimates quoteList state.props.repeatRideServiceTierName -- check if the repeat ride variant is available in the estimates
    isRepeatRide = state.props.isRepeatRide && not repeatRideFailCheck -- if repeat ride is enabled and the variant is not available in the estimates then disable repeat ride
    nYQuotes = filter (\item -> item.providerType == ONUS) quoteList
    showMultiProvider' =  if alreadyGotEstimates then
                            state.data.iopState.showMultiProvider
                          else
                            null nYQuotes -- if we already got the estimate show current screen only else if we have NY show ny provider else show multi provider    

    defaultSelected = fromMaybe ChooseVehicleController.config $ 
      if isRepeatRide then do 
        let defaultQuote_ = find (\item -> isJust item.serviceTierName && item.serviceTierName == state.props.repeatRideServiceTierName) nYQuotes
        if isJust defaultQuote_ then 
          defaultQuote_
        else 
          quoteList !! state.data.selectedEstimatesObject.activeIndex
      else 
        if showMultiProvider' then 
          quoteList !! state.data.selectedEstimatesObject.activeIndex
        else 
          nYQuotes !! state.data.selectedEstimatesObject.activeIndex 


    zoneType = getSpecialTag defaultSelected.specialLocationTag

    hasToll = any (\item -> maybe false (\fareBreakupList -> isEstimateFareBreakupHastitle fareBreakupList "TOLL_CHARGES") (item ^. _estimateFareBreakup)) quotesRes.estimates

    -- topProviderEstimates = filter (\element -> element.providerType == ONUS) quoteList -- filter the ny provider estimates
    -- shouldShowEstimates = not $ null quoteList-- if iop is not enabled then show ny provider else show multi provider
  let _ = spy "INside quoteList" quoteList
  if not $ null quoteList then do -- if choosing multiple provider is not enabled then only show ny
    let 
      _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_quote"
      nearByDrivers = getNearByDrivers quotesRes.estimates
      nearByDriversLength = length nearByDrivers
      defaultQuoteId = map (\quote -> quote.id) quoteList !! state.data.selectedEstimatesObject.activeIndex
      -- selectedEstimateIdForBookAny = if defaultSelected.vehicleVariant == "BOOK_ANY"
      --                                   then getEstimateIdFromSelectedServices state.data.specialZoneQuoteList defaultSelected
      --                                   else [] 
      _ = runFn2 updatePushInIdMap "EstimatePolling" true
      quoteList' = map (\quote -> quote{activeIndex = defaultSelected.index}) quoteList
      isIntercity = state.data.fareProductType == FPT.INTER_CITY

    void $ pure $ updateLocalStage SettingPrice
    logStatus "drivers_available" nearByDriversLength

    void $ pure $ setValueToLocalStore HAS_TOLL_CHARGES $ show hasToll -- need to remove
    exit $ SelectEstimate state 
      { data
        { specialZoneQuoteList = if (not isIntercity) then quoteList' else state.data.specialZoneQuoteList
        , quoteList = if isIntercity then quoteList' else state.data.quoteList
        , selectedQuoteId = if isIntercity then defaultQuoteId else state.data.selectedQuoteId
        , specialZoneSelectedQuote = if (not isIntercity) then defaultQuoteId else state.data.specialZoneSelectedQuote
        , selectedEstimatesObject = defaultSelected
        , nearByDrivers = if nearByDriversLength > 0 then Just nearByDriversLength else Nothing
        , iopState { 
            showPrefButton = state.data.currentCityConfig.iopConfig.enable && (not (null nYQuotes)) && (not isRepeatRide)
          , providerPrefInfo = state.data.iopState.providerPrefInfo
          , hasTopProviderEstimate = not $ null nYQuotes
          , showMultiProvider = showMultiProvider'
          }
        }
      , props
        { currentStage = SettingPrice
        , zoneType = zoneType
        , isRepeatRide = isRepeatRide
        , hasToll = hasToll
        }
      }
  else do
    void $ pure $ updateLocalStage SearchLocationModel
    void $ pure $ toast (getString NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN)
    continue state { props {currentStage = SearchLocationModel}, data{fareProductType = FPT.ONE_WAY}}

  where 
    isEstimateFareBreakupHastitle fareBreakUpList title = any (\item -> item ^. _title == title) fareBreakUpList
    
    getSelectedEstimates :: ChooseVehicleController.Config -> Array ChooseVehicleController.Config -> Tuple String (Array String) 
    getSelectedEstimates quote quotes = 
      let filteredEstimates = foldl(\acc item -> if elem (fromMaybe "" item.serviceTierName) quote.selectedServices then acc <> [item.id] else acc) [] quotes
      in (Tuple (fromMaybe "" $ head filteredEstimates) (fromMaybe [] $ tail filteredEstimates))

    filterNonAcAsPerGates quote = not $ quote.vehicleVariant == "TAXI" && (STR.contains (STR.Pattern "(ac only)") $ spy "filterNonAcAsPerGates" DS.toLower state.props.defaultPickUpPoint)

eval (GetEditLocResult (GetEditLocResultResp resp)) state = do
  logStatus "bookingUpdateRequestDetails" resp
  _ <- pure $ setValueToLocalStore FINDING_EDIT_LOC_RESULTS "false"
  let (BookingUpdateRequestDetails bookingUpdateRequestDetails) = resp.bookingUpdateRequestDetails

  continue state { data { newEstimatedFare = roundOff bookingUpdateRequestDetails.estimatedFare, newEstimatedDistance = truncate 1 bookingUpdateRequestDetails.estimatedDistance }, props { currentStage = RevisedEstimate } }

eval (EstimatesTryAgain (GetQuotesRes quotesRes) count ) state = do
  case (getMerchant FunctionCall) of
    YATRI -> estimatesListTryAgainFlow (GetQuotesRes quotesRes) state
    YATRISATHI -> estimatesListTryAgainFlow (GetQuotesRes quotesRes) state
    _ -> do
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_estimate_try_again"
      let
        estimatedQuotes = quotesRes.estimates

        estimatedVarient = filter (\x -> x ^. _vehicleVariant == "AUTO_RICKSHAW") estimatedQuotes

        estimatedPrice = if (isJust (estimatedVarient !! 0)) then (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _estimatedFare else 0

        estimateId = if isJust (estimatedVarient !! 0) then (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _estimateId else ""
      case (null estimatedVarient) of
        true -> do
          _ <- pure $ hideKeyboardOnNavigation true
          _ <- pure $ toast (getString NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN)
          continue state { props { currentStage = SearchLocationModel, rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = true, isDestServiceable = true, isRideServiceable = true } }
          -- let newState = state { props { currentStage = HomeScreen, rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = true, isDestServiceable = true, isRideServiceable = true } }
          -- updateAndExit newState $ Go_To_Search_Location_Flow newState true
        false -> do
          if (estimatedPrice > state.data.suggestedAmount) then
            continue state { data { suggestedAmount = estimatedPrice }, props { estimateId = estimateId, isEstimateChanged = true } }
          else do
            let
              updatedState = state { data { suggestedAmount = estimatedPrice }, props { estimateId = estimateId, searchExpire = (getSearchExpiryTime true) } }
            exit $ SelectEstimateAndQuotes updatedState


eval (GetQuotesList (SelectListRes resp)) state = do
  if flowWithoutOffers WithoutOffers then
    continueWithCmd state [pure $ ContinueWithoutOffers (SelectListRes resp)]
  else do
    let allQuotes = getQuoteList ((fromMaybe dummySelectedQuotes resp.selectedQuotes)^._selectedQuotes) state.props.city
        existingQuotes = state.data.quoteListModelState
        newQuotes = filter (\quote -> quote.seconds > 0) (filter (\a -> length (filter (\b -> a.id == b.id ) existingQuotes) == 0 ) allQuotes) --filter (\quote -> quote.seconds > 0) (union allQuotes existingQuotes)
        updatedQuotes = existingQuotes <> newQuotes
        newState = state{data{quoteListModelState = updatedQuotes },props{isSearchLocation = NoView, isSource = Nothing, currentStage = FindingQuotes}}
        
    if getValueToLocalStore GOT_ONE_QUOTE == "FALSE" && length updatedQuotes > 0 then do
      void $ pure $ firebaseLogEvent "ny_user_received_quotes"
      void $ pure $ setValueToLocalStore GOT_ONE_QUOTE "TRUE"
    else pure unit
    
    if isLocalStageOn QuoteList then do
      logInfo "retry_finding_quotes" ( "QuoteList : Current Stage: " <> (show newState.props.currentStage) <> " LOCAL_STAGE : " <> (getValueToLocalStore LOCAL_STAGE) <> "Estimate Id:" <> state.props.estimateId)
      let updatedState = if isTipEnabled state then tipEnabledState newState{props{isPopUp = TipsPopUp  ,findingQuotesProgress = 0.0}} else newState{props{isPopUp = ConfirmBack}}
      exit $ GetSelectList updatedState
    else if state.props.selectedQuote == Nothing && (getValueToLocalStore AUTO_SELECTING) /= "CANCELLED_AUTO_ASSIGN" then do
      logInfo "retry_finding_quotes" ( "SelectedQuote: Current Stage: " <> (show newState.props.currentStage) <> " LOCAL_STAGE : " <> (getValueToLocalStore LOCAL_STAGE) <> "Estimate Id:" <> state.props.estimateId)
      case head updatedQuotes of
        Just quote -> do
          let selectedQuote = Just quote.id
          void $ pure $ setValueToLocalStore AUTO_SELECTING quote.id
          continue newState{ data{ quoteListModelState = map (\quote' -> quote'{ selectedQuote = selectedQuote }) updatedQuotes }, props{ selectedQuote = selectedQuote }}
        Nothing -> continue newState
    else if null updatedQuotes then do
      logInfo "retry_finding_quotes" ( "Default :Current Stage: " <> (show newState.props.currentStage) <> " LOCAL_STAGE : " <> (getValueToLocalStore LOCAL_STAGE) <> "Estimate Id:" <> state.props.estimateId)
      void $ pure $ setValueToLocalStore AUTO_SELECTING "false"
      continue newState{props{ selectedQuote = Nothing }}
    else do
      logInfo "retry_finding_quotes" ( "Current Stage: " <> (show newState.props.currentStage) <> " LOCAL_STAGE : " <> (getValueToLocalStore LOCAL_STAGE) <> "Estimate Id:" <> state.props.estimateId)
      continue newState

eval (ContinueWithoutOffers (SelectListRes resp)) state = do
  case resp.bookingId of
    Just bookingId -> do
      case STR.trim bookingId of
        "" -> continue state
        _  -> do
          _ <- pure $ updateLocalStage ConfirmingRide
          exit $ ConfirmRide state{props{currentStage = ConfirmingRide, bookingId = bookingId, isPopUp = NoPopUp, selectedQuote = Nothing}}
    Nothing -> do
      if isLocalStageOn QuoteList then do
        let onUs = state.data.selectedEstimatesObject.providerType == CTP.ONUS
            updatedState = if (isTipEnabled state && onUs) then tipEnabledState state{props{isPopUp = TipsPopUp, customerTip{enableTips = true}}} else state{props{isPopUp = ConfirmBack}} 
        continue updatedState 
        else continue state

eval (GetRideConfirmation (RideBookingRes response)) state = do
  logStatus "confirming_ride" response
  let (RideBookingAPIDetails bookingDetails) = response.bookingDetails
      (RideBookingDetails contents) = bookingDetails.contents
      otpCode = contents.otpCode
      currentStage =  
        case head response.rideList of
          Just rideList -> 
            case rideList ^. _status of
              "NEW" -> RideAccepted
              "INPROGRESS" -> RideStarted
              "COMPLETED" -> RideCompleted
              "CANCELLED" -> HomeScreen
              _ -> RideAccepted
          Nothing -> RideAccepted
      (RideBookingAPIDetails bookingDetails) = response.bookingDetails
      isSpecialZoneOtpRide = bookingDetails.fareProductType == "OneWaySpecialZoneAPIDetails" || isJust otpCode
      newState = state {  props { currentStage = currentStage
                                , isSearchLocation = NoView
                                , bookingId = response.id
                                , isInApp = true
                                , isSpecialZone = isSpecialZoneOtpRide
                                }
                        , data { driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes response) (state.data.fareProductType == ST.ONE_WAY_SPECIAL_ZONE || isJust otpCode) state.data.driverInfoCardState }
                        }
  exit $ RideConfirmed newState

eval (NotificationListener notificationType) state = do
  _ <- pure $ printLog "storeCallBackCustomer notificationType" notificationType
  case notificationType of
    "DRIVER_QUOTE_INCOMING" -> continue state
    _ -> exit $ NotificationHandler notificationType state { props { callbackInitiated = false}}

eval RecenterCurrentLocation state = do
  recenterCurrentLocation state

eval (SearchLocationModelActionController (SearchLocationModelController.RecenterCurrentLocation)) state = recenterCurrentLocation state

eval (SearchLocationModelActionController (SearchLocationModelController.UpdateCurrentLocation lat lng)) state = do
  if state.props.isSource == Just true then
    updateCurrentLocation state lat lng
  else
    continue state

eval (UpdateCurrentLocation lat lng) state = updateCurrentLocation state lat lng

eval (CurrentLocation lat lng) state = do
  void $ pure $ setValueToLocalStore LAST_KNOWN_LAT lat
  void $ pure $ setValueToLocalStore LAST_KNOWN_LON lng
  if isLocalStageOn FindingEstimate
    then continue state
    else exit $ UpdatedState state { props { currentLocation { lat =  fromMaybe 0.0 (NUM.fromString lat), lng = fromMaybe 0.0 (NUM.fromString lng) }, sourceLat = fromMaybe 0.0 (NUM.fromString lat), sourceLong = fromMaybe 0.0 (NUM.fromString lng) } } false

eval (RateCardAction RateCard.Close) state = continue state { props { showRateCard = false } , data{rateCard{onFirstPage = false,currentRateCardType = DefaultRateCard}}}

eval (RateCardAction RateCard.BackPressed) state = continue state { props { showRateCard = false } ,data{rateCard{onFirstPage = false,currentRateCardType = DefaultRateCard}}}

eval (RateCardAction RateCard.NoAction) state = continue state

eval (RateCardAction RateCard.GoToDefaultStart) state = continue state { data{rateCard{currentRateCardType = DefaultRateCard}}}

eval (RateCardAction RateCard.GoToDriverAddition) state = continue state { data{rateCard{currentRateCardType = DriverAddition,onFirstPage = true}}}

eval (RateCardAction RateCard.GoToFareUpdate) state = continue state { data{rateCard{currentRateCardType = FareUpdate,onFirstPage = true}}}

eval (RateCardAction RateCard.GoToWaitingCharges) state = continue state { data{rateCard{currentRateCardType = WaitingCharges,onFirstPage = true}}}

eval (RateCardAction RateCard.GoToTollOrParkingCharges) state = continue state { data{rateCard{currentRateCardType = TollOrParkingCharges,onFirstPage = true}}}

eval (RequestInfoCardAction RequestInfoCard.Close) state = 
  continueWithCmd state { props { showMultipleRideInfo = false }, data {waitTimeInfo = false , iopState {providerPrefInfo = false }}} [ do
    pure $ (RequestInfoCardAction RequestInfoCard.BackPressed)
  ]

eval (RequestInfoCardAction RequestInfoCard.BackPressed) state = 
  if state.props.showSpecialZoneInfoPopup then
    continue state{ props{ showSpecialZoneInfoPopup = false } }
  else
    continue state { props { showMultipleRideInfo = false }, data {waitTimeInfo = false }}

eval SpecialZoneInfoTag state = 
  continue state{ props{ showSpecialZoneInfoPopup = true } }

eval (RentalInfoAction PopUpModal.DismissPopup) state = continue state

eval (RentalInfoAction PopUpModal.OnButton1Click) state = continue state { props { showRentalInfo = false}}

eval (RentalInfoAction PopUpModal.OnButton2Click) state = continue state { props { showRentalInfo = false}}

eval (RequestInfoCardAction RequestInfoCard.NoAction) state = continue state


eval (UpdateProfileButtonAC PrimaryButtonController.OnClick) state = do 
  _ <- pure $ pauseYoutubeVideo unit
  let newState = state{props{showEducationalCarousel = false}} 
  updateAndExit newState $ GoToMyProfile newState true


eval (SkipAccessibilityUpdateAC PrimaryButtonController.OnClick) state = do 
  _ <- pure $ pauseYoutubeVideo unit
  let _ = runFn2 updatePushInIdMap "bannerCarousel" true
  continue state{props{showEducationalCarousel = false}}

eval (DisabilityPopUpAC PopUpModal.OnButton1Click) state = do 
  _ <- pure $ pauseYoutubeVideo unit
  continue state{props{showDisabilityPopUp = false}}


eval ShowRateCard state = do
  continue state { props { showRateCard = true } }

eval ShowRevisedFareDetails state = do
  continue state { props { showRevisedFareDetails = not state.props.showRevisedFareDetails } }

eval (PopUpModalShareAppAction PopUpModal.OnButton1Click) state= continue state{props{showShareAppPopUp=false}}

eval (PopUpModalShareAppAction PopUpModal.OnButton2Click) state= do
  _ <- pure $ setValueToLocalStore SHARE_APP_COUNT "-1"
  let shareAppConfig = state.data.config.shareAppConfig
  _ <- pure $ shareTextMessage shareAppConfig.title shareAppConfig.description
  continue state{props{showShareAppPopUp=false}}

eval (CallSupportAction PopUpModal.OnButton1Click) state= do
  void $ pure $ performHapticFeedback unit
  continue state{props{callSupportPopUp=false}}

eval (CallSupportAction PopUpModal.OnButton2Click) state= do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ showDialer (getSupportNumber "") false -- TODO: FIX_DIALER
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_ride_support_click"
  continue state{props{callSupportPopUp=false}}

eval (UpdateETA currentETA currentDistance) state = do
  let initDistance = state.data.driverInfoCardState.initDistance
  distance <- case initDistance of
                      Just initDistance -> pure initDistance
                      Nothing -> do
                                    let storedDistance = getValueToLocalStore PICKUP_DISTANCE
                                    if storedDistance == "0" || storedDistance == "__failed" || storedDistance == "(null)" then do
                                      _ <- pure $ setValueToLocalStore PICKUP_DISTANCE (show currentDistance)
                                      pure currentDistance
                                      else pure $ fromMaybe 0 (fromString storedDistance)
  let
    sheetState = if isLocalStageOn ChatWithDriver && state.props.currentSheetState == EXPANDED then Just COLLAPSED else state.props.sheetState
    currentSheetState =  if isLocalStageOn ChatWithDriver && state.props.currentSheetState == EXPANDED then COLLAPSED else state.props.currentSheetState
    newState = state { data { driverInfoCardState { eta = Just currentETA, distance = currentDistance, initDistance = Just distance } }, props {sheetState = sheetState, currentSheetState = currentSheetState}}
  continue newState

eval (RepeatRide index item) state = do 
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_repeat_trip"
  let _ = unsafePerformEffect $ Events.addEventData "External.Clicked.RepeatRide" "true"
  void $ pure $ setValueToLocalStore FLOW_WITHOUT_OFFERS (show true)
  void $ pure $ setValueToLocalStore TEST_MINIMUM_POLLING_COUNT $ "4" 
  void $ pure $ setValueToLocalStore TEST_POLLING_INTERVAL $ "8000.0" 
  void $ pure $ setValueToLocalStore TEST_POLLING_COUNT $ "22" 
  updateAndExit state{props{currentStage = LoadMap, suggestedRideFlow = true}, data{settingSideBar { opened = SettingSideBarController.CLOSED }}} $ RepeatTrip state{props{isRepeatRide = true, suggestedRideFlow = true, repeatRideServiceTierName = item.serviceTierNameV2}} item

eval (ReferralFlowAction) state = 
  continue state{ props{ referral{ showAddReferralPopup = true }, referralComponentProps = HomeScreenData.initData.props.referralComponentProps } }

eval RideSearchAction state = do 
  void $ pure $ setValueToLocalStore FLOW_WITHOUT_OFFERS (show true)
  void $ pure $ setValueToLocalStore TEST_MINIMUM_POLLING_COUNT $ "4" 
  void $ pure $ setValueToLocalStore TEST_POLLING_INTERVAL $ "8000.0" 
  void $ pure $ setValueToLocalStore TEST_POLLING_COUNT $ "22"
  updateAndExit state{props{currentStage = LoadMap}} $ RideSearchSO

eval ConfirmRentalRideAction state = do 
  updateAndExit state{props{currentStage = LoadMap}} $ ConfirmRentalRideSO state

eval ChangeToRideAcceptedAction state = do
  void $ pure $ updateLocalStage RideAccepted
  updateAndExit state{props{currentStage = LoadMap}} $ RefreshHomeScreen state{props{currentStage = RideAccepted}}

eval ChangeToRideStartedAction state = do
  void $ pure $ updateLocalStage RideStarted
  updateAndExit state{props{currentStage = LoadMap}} $ RefreshHomeScreen state{props{currentStage = RideStarted}}

eval NewUser state = continueWithCmd state [ do
  if (getValueToLocalNativeStore REGISTRATION_APPROVED) == "true" then do
    _ <- pure $ setValueToLocalStore REGISTRATION_APPROVED "false"
    _ <- launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
      _ <- UI.successScreen ((getString HEY) <> " " <> (getValueToLocalStore USER_NAME)) (getString $ SUCCESSFUL_ONBOARD "SUCCESSFUL_ONBOARD")
      pure unit
    pure unit
    else
      pure unit
  pure NoAction
]

eval UpdateSourceFromPastLocations state = do
  let currLocationArr = (getNearestCurrentLocation state.props.sourceLat state.props.sourceLong state.data.previousCurrentLocations.pastCurrentLocations)
      currLocation = (fromMaybe {locationDetails: {lat:0.0, lon : 0.0, placeName : ""}, distance: 0.0} ((currLocationArr)!!0))
      savedLocationArr = (getNearestSavedLocation state.props.sourceLat state.props.sourceLong state.data.savedLocations)
      arr = (sortBy compareByDistance (currLocationArr <> savedLocationArr))
      nearestLocation = (fromMaybe {locationDetails: {lat:0.0, lon : 0.0, placeName : ""}, distance: 0.0} ((arr)!!0))
  continue state{data{source = nearestLocation.locationDetails.placeName, sourceAddress = encodeAddress nearestLocation.locationDetails.placeName [] Nothing 0.0 0.0}}

eval (UpdateLocAndLatLong lat lng) state = do
  let slat = fromMaybe 0.0 (NUM.fromString lat)
      slng = fromMaybe 0.0 (NUM.fromString lng)
  continueWithCmd state{props{currentLocation { lat = slat, lng = slng } , sourceLat = slat, sourceLong = slng , locateOnMapLocation {sourceLat = slat, sourceLng = slng, source = state.data.source, sourceAddress = state.data.sourceAddress}}} [do
    if os == "IOS" && state.props.currentStage == HomeScreen then
      void $ showMarkerOnMap (getCurrentLocationMarker $ getValueToLocalStore VERSION_NAME) 9.9 9.9
      else pure unit
    pure if state.props.sourceLat == 0.0 && state.props.sourceLong == 0.0 
          then UpdateCurrentLocation lat lng
          else NoAction
  ]

eval GoToEditProfile state = do
  exit $ GoToMyProfile state true
eval (MenuButtonActionController (MenuButtonController.OnClick config)) state = do
  continueWithCmd state{props{defaultPickUpPoint = config.id}} [do
      let focusedIndex = findIndex (\item -> item.place == config.id) state.data.nearByPickUpPoints
      case focusedIndex of
        Just index -> do
          _ <- pure $ scrollViewFocus (getNewIDWithTag "scrollViewParent") index
          pure unit
        Nothing -> pure unit
      _ <- animateCamera config.lat config.lng 25.0 "NO_ZOOM"
      pure NoAction
    ]
eval (ChooseYourRideAction (ChooseYourRideController.ChooseVehicleAC (ChooseVehicleController.NoAction config))) state = do
  let height = (runFn1 getLayoutBounds $ getNewIDWithTag config.id).height
      updatedState = state{props{defaultPickUpPoint = "", currentEstimateHeight = if config.vehicleVariant == "BOOK_ANY" then height else state.props.currentEstimateHeight, selectedEstimateHeight = if config.vehicleVariant /= "BOOK_ANY" then height else state.props.selectedEstimateHeight}}
  continue updatedState

eval (ChooseYourRideAction (ChooseYourRideController.ChooseVehicleAC (ChooseVehicleController.ServicesOnClick config item))) state = do
  let updatedServices = if elem item config.selectedServices then delete item config.selectedServices else insert item config.selectedServices
  if length updatedServices < 1 then continue state
  else do
    let selectedEstimates = foldl(\acc item -> if elem (fromMaybe "" item.serviceTierName) updatedServices then acc <> [item.id] else acc) [] state.data.specialZoneQuoteList
        estimateId = if config.vehicleVariant == "BOOK_ANY" then fromMaybe "" (head selectedEstimates) else config.id
        otherSelectedEstimates = fromMaybe [] $ tail $ selectedEstimates
        updatedQuotes = map (\item -> if item.vehicleVariant == "BOOK_ANY" then item{selectedServices = updatedServices}
                                      else item
                            ) state.data.specialZoneQuoteList 
    continue state{data{specialZoneQuoteList = updatedQuotes, otherSelectedEstimates = otherSelectedEstimates, selectedEstimatesObject = config{selectedServices = updatedServices, activeIndex = config.index, id = estimateId}}, props {estimateId = estimateId}}

eval (ChooseYourRideAction (ChooseYourRideController.ChooseVehicleAC (ChooseVehicleController.OnSelect config))) state = do
  let _ = unsafePerformEffect $ Events.addEventData ("External.Clicked.Search." <> state.props.searchId <> ".ChooseVehicle") "true"
  let updatedQuotes = map (\item -> item{activeIndex = config.index}) state.data.specialZoneQuoteList
      props = if config.activeIndex == config.index then state.props else state.props{customerTip = HomeScreenData.initData.props.customerTip, tipViewProps = HomeScreenData.initData.props.tipViewProps}
      newState = state{data{specialZoneQuoteList = updatedQuotes}, props = props}
      selectedEstimates = if config.vehicleVariant == "BOOK_ANY" then foldl(\acc item -> if elem (fromMaybe "" item.serviceTierName) config.selectedServices then acc <> [item.id] else acc) [] state.data.specialZoneQuoteList else [] 
      estimateId = if config.vehicleVariant == "BOOK_ANY" then fromMaybe "" (head selectedEstimates) else config.id
      otherSelectedEstimates = fromMaybe [] $ tail $ selectedEstimates
  void $ pure $ setValueToLocalNativeStore SELECTED_VARIANT (config.vehicleVariant)
  let updatedSpecialZOneQuotes = map (\item -> item{activeIndex = config.index}) state.data.specialZoneQuoteList
      props = if config.activeIndex == config.index then state.props else state.props{customerTip = HomeScreenData.initData.props.customerTip, tipViewProps = HomeScreenData.initData.props.tipViewProps}
      updatedQuoteList = map (\item -> item{activeIndex = config.index}) state.data.quoteList
      newState = state{data{specialZoneQuoteList = updatedSpecialZOneQuotes, quoteList = updatedQuoteList}, props = props}
  
  if state.data.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE then do
    _ <- pure $ spy "ChooseYourRideAction 2" config.activeIndex 
    continue newState
      { data
          { specialZoneSelectedQuote = Just config.id
          , specialZoneSelectedVariant = Just config.vehicleVariant 
          }
      }
  else if state.data.fareProductType == FPT.INTER_CITY then do
    continue newState
      { data
          { selectedQuoteId = Just config.id
          , selectedQuoteVariant = Just config.vehicleVariant
          , selectedEstimatesObject = config
          }
      }
  else 
    continue newState{props{estimateId = estimateId }, data {selectedEstimatesObject = config{activeIndex = config.index}, otherSelectedEstimates = otherSelectedEstimates}}

eval (ChooseYourRideAction (ChooseYourRideController.ChooseVehicleAC (ChooseVehicleController.ShowRateCard config))) state = do
  let _ = unsafePerformEffect $ Events.addEventData ("External.Clicked.Search." <> state.props.searchId <> ".RateCard") "true"
  continue state{ props { showRateCard = true }
                , data {  rateCard {  onFirstPage = false
                                    , currentRateCardType = DefaultRateCard
                                    , extraFare = config.extraFare
                                    , fareInfoDescription = config.fareInfoDescription
                                    , additionalFare = config.additionalFare
                                    , isNightShift = config.isNightShift
                                    , nightChargeTill = config.nightChargeTill
                                    , nightChargeFrom = config.nightChargeFrom
                                    , driverAdditions = config.driverAdditions
                                    , serviceTierName = config.serviceTierName
                                    }}}




eval (ChooseYourRideAction (ChooseYourRideController.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = do
  let _ = unsafePerformEffect $ Events.addEventData ("External.Clicked.Search." <> state.props.searchId <> ".BookNow") "true"
      (Tuple estimateId otherSelectedEstimates) = getEstimateId state.data.specialZoneQuoteList state.data.selectedEstimatesObject 
  _ <- pure $ setValueToLocalStore FARE_ESTIMATE_DATA state.data.selectedEstimatesObject.price
  void $ pure $ setValueToLocalStore SELECTED_VARIANT (state.data.selectedEstimatesObject.vehicleVariant)
  void $ pure $ cacheRateCard state
  if any (_ == state.data.fareProductType) [FPT.ONE_WAY_SPECIAL_ZONE, FPT.INTER_CITY] then do
    _ <- pure $ updateLocalStage ConfirmingRide
    exit $ ConfirmRide state{props{currentStage = ConfirmingRide}}
  else if state.data.iopState.showMultiProvider then do 
    void $  pure $ updateLocalStage ProviderSelection 
    exit $ RefreshHomeScreen state { data { iopState { providerSelectionStage = true}, otherSelectedEstimates = otherSelectedEstimates}, props {estimateId = estimateId}}
  else do
    
    let customerTip = if state.props.tipViewProps.activeIndex == -1 then HomeScreenData.initData.props.customerTip else state.props.customerTip
        tipViewProps = if state.props.tipViewProps.activeIndex == -1 then HomeScreenData.initData.props.tipViewProps 
                          else if state.props.tipViewProps.stage == TIP_AMOUNT_SELECTED then state.props.tipViewProps{stage = TIP_ADDED_TO_SEARCH}
                          else state.props.tipViewProps
        updatedState = state{props{ searchExpire = (getSearchExpiryTime true), customerTip = customerTip, tipViewProps = tipViewProps, estimateId = estimateId}, data{otherSelectedEstimates = otherSelectedEstimates}}
    void $ pure $ setTipViewData (TipViewData { stage : tipViewProps.stage , activeIndex : tipViewProps.activeIndex , isVisible : tipViewProps.isVisible })
    exit $ SelectEstimateAndQuotes updatedState

eval (ChooseYourRideAction ChooseYourRideController.NoAction) state = do
  continue state{ props{ defaultPickUpPoint = "" } }

eval (QuoteListModelActionController (QuoteListModelController.CancelTimer)) state = do
  void $ pure $ clearTimerWithId state.data.iopState.timerId
  continue state { data { iopState { timerVal = "0"}}}

eval (QuoteListModelActionController (QuoteListModelController.ProviderModelAC (PM.ButtonClick (PrimaryButtonController.OnClick)))) state = do
  void $ pure $ clearTimerWithId state.data.iopState.timerId
  void $ pure $ spy "ButtonClick state" state
  
  let updatedState = state{props{searchExpire = (getSearchExpiryTime true)}, data { iopState { providerSelectionStage = false}}}
  void $ pure $ spy "ButtonClick updatedState" updatedState
  exit $ (SelectEstimateAndQuotes updatedState)

eval (QuoteListModelActionController (QuoteListModelController.ProviderModelAC (PM.FavClick item))) state = do
  let selectedItem = find (\quote -> quote.id == item.id) state.data.specialZoneQuoteList 
  void $ pure $ spy "quote" selectedItem
  case selectedItem of
    Just quote -> continue state { data { selectedEstimatesObject = quote}, props { estimateId = item.id}}
    _ -> continue state

eval (ProviderAutoSelected seconds status timerID) state = do
  if status == "EXPIRED" then do
    void $ pure $ clearTimerWithId timerID
    continueWithCmd state [pure $ (QuoteListModelActionController (QuoteListModelController.ProviderModelAC (PM.ButtonClick (PrimaryButtonController.OnClick))))]
  else continue state { data { iopState { timerVal = show seconds, timerId = timerID}}}-- update timer in ui

eval MapReadyAction state = do
  continueWithCmd state [ do
      permissionConditionA <- isLocationPermissionEnabled unit
      permissionConditionB <- isLocationEnabled unit
      internetCondition <- isInternetAvailable unit
      let action =  if( not internetCondition) then TriggerPermissionFlow INTERNET_ACTION
                    else if ( not (permissionConditionA && permissionConditionB)) then TriggerPermissionFlow LOCATION_DISABLED
                    else CheckAndAskNotificationPermission
      pure action
    ]

eval (ChooseYourRideAction( ChooseYourRideController.RadioButtonClick autoAssign)) state = 
  continueWithCmd state [ do
    pure (CheckBoxClick autoAssign)
  ]
  
eval (ChooseYourRideAction (ChooseYourRideController.OnIconClick autoAssign)) state = 
  continue state { props {showMultipleRideInfo = not autoAssign}}

eval (ChooseYourRideAction ChooseYourRideController.PreferencesDropDown) state = do
  continue state { data { showPreferences = not state.data.showPreferences}}

eval (ChooseYourRideAction ChooseYourRideController.SpecialZoneInfoTag) state = do
  continue state{ props{ showSpecialZoneInfoPopup = true } }

eval (ChooseYourRideAction (ChooseYourRideController.PrimaryButtonActionController (PrimaryButtonController.NoAction))) state = continueWithCmd state{data {triggerPatchCounter = state.data.triggerPatchCounter + 1}} [pure NoAction]

eval (ChooseYourRideAction (ChooseYourRideController.TipBtnClick index value)) state = do
  let tipConfig = getTipConfig state.data.selectedEstimatesObject.vehicleVariant
      customerTipArrayWithValues = tipConfig.customerTipArrayWithValues
      tip = fromMaybe 0 (customerTipArrayWithValues !! index)
      isTipSelected = tip > 0
      customerTip = if isTipSelected then 
                      state.props.customerTip {isTipSelected = isTipSelected, enableTips = isTipEnabled state, tipForDriver = tip, tipActiveIndex = index}
                      else HomeScreenData.initData.props.customerTip
      tipViewProps = if isTipSelected then 
                      state.props.tipViewProps{ stage = RETRY_SEARCH_WITH_TIP, activeIndex = index, onlyPrimaryText = true}
                      else HomeScreenData.initData.props.tipViewProps
  void $ pure $ setTipViewData (TipViewData { stage : tipViewProps.stage , activeIndex : tipViewProps.activeIndex , isVisible : tipViewProps.isVisible })
  continue state { props {customerTip = customerTip , tipViewProps = tipViewProps }}

eval (ChooseYourRideAction ChooseYourRideController.AddTip) state = do
  if state.data.selectedEstimatesObject.searchResultType == ChooseVehicleController.QUOTES ChooseVehicleController.OneWaySpecialZoneAPIDetails then continue state
  else continue state { props { tipViewProps {stage = TIP_AMOUNT_SELECTED}}}

eval (ChooseYourRideAction ChooseYourRideController.ChangeTip) state = do
  continue state { props {tipViewProps { activeIndex = state.props.customerTip.tipActiveIndex, stage = TIP_AMOUNT_SELECTED}}} 

eval CheckAndAskNotificationPermission state = do 
  _ <- pure $ checkAndAskNotificationPermission false
  continue state

eval (TriggerPermissionFlow flowType) state = exit $ ExitToPermissionFlow flowType


eval ReportIssueClick state = exit $  GoToHelp state

eval (DateTimePickerAction dateResp year month day timeResp hour minute) state = do 
  if any (_ /= "SELECTED") [dateResp, timeResp] then continue state 
  else
    let selectedDateString = (show year) <> "-" <> (if (month + 1 < 10) then "0" else "") <> (show (month+1)) <> "-" <> (if day < 10 then "0"  else "") <> (show day)
        selectedUTC = unsafePerformEffect $ convertDateTimeConfigToUTC year (month + 1) day hour minute 0
        isAfterThirtyMinutes = (compareUTCDate selectedUTC (getCurrentUTC "")) > (30 * 60)
        validDate = (unsafePerformEffect $ runEffectFn2 compareDate (getDateAfterNDaysv2 (state.props.maxDateBooking)) selectedDateString)
                        && (unsafePerformEffect $ runEffectFn2 compareDate selectedDateString (getCurrentDatev2 "" ))
        updatedDateTime = state.data.selectedDateTimeConfig { year = year, month = month, day = day, hour = hour, minute = minute }
        newState = if validDate && isAfterThirtyMinutes then state { data { selectedDateTimeConfig = updatedDateTime, startTimeUTC = selectedUTC}} else state
    in 
      if validDate && isAfterThirtyMinutes then do
        let maybeInvalidBookingDetails = invalidBookingTime selectedUTC Nothing
        if (isJust maybeInvalidBookingDetails) then do
          continue state {data{invalidBookingId = maybe Nothing (\invalidBookingTime -> Just invalidBookingTime.bookingId) maybeInvalidBookingDetails}, props{showScheduledRideExistsPopUp = true}}
        else continue newState
      else 
        if isAfterThirtyMinutes then do 
          void $ pure $ toast $ getVarString DATE_INVALID_MESSAGE $ singleton $ show state.props.maxDateBooking
          continue state
        else do
          void $ pure $ toast $ getString SCHEDULE_RIDE_AVAILABLE
          continue state 

eval (LocationTagBarAC (LocationTagBarV2Controller.TagClicked tag)) state = do 
  case tag of 
    "RENTALS" -> exit $ GoToRentalsFlow state { data {rentalsInfo = Nothing } }
    "INTER_CITY" ->
      if state.data.currentCityConfig.enableIntercity then do 
        void $ pure $ updateLocalStage SearchLocationModel 
        continue state { data { source=(getString CURRENT_LOCATION), rentalsInfo = Nothing}, props{isSource = Just false, canScheduleRide = false, isSearchLocation = SearchLocation, currentStage = SearchLocationModel, searchLocationModelProps{crossBtnSrcVisibility = false }}}
        else do
          void $ pure $ toast $ getString INTERCITY_RIDES_COMING_SOON
          continue state
    "INSTANT" -> continueWithCmd state [ pure $ WhereToClick]
    _ -> continue state
  
eval (RentalBannerClick) state = maybe (exit GoToScheduledRides) (\rentalsInfo -> if rentalsInfo.multipleScheduled then exit (PastRides state true) else exit GoToScheduledRides) state.data.rentalsInfo
eval (BottomNavBarAction id) state = do 
  let newState = state {props {focussedBottomIcon = id}}
  case id of 
    TICKETING -> updateAndExit newState $ GoToTicketBookingFlow newState
    MOBILITY -> continue newState 
    _ -> continue state 
    
eval (SafetyAlertAction PopUpModal.OnButton1Click) state = do
  void $ pure $ cleverTapCustomEvent "ny_user_night_safety_mark_i_feel_safe"
  exit $ SafetySupport state{props{safetyAlertType = Nothing}} true

eval (SafetyAlertAction PopUpModal.OnButton2Click) state = do
    void $ pure $ cleverTapCustomEvent "ny_user_night_safety_mark_need_help"
    void $ pure $ setValueToLocalNativeStore SAFETY_ALERT_TYPE "false"
    exit $ GoToNammaSafety state{props{safetyAlertType = Nothing}} true false

eval (NotifyRideShare PrimaryButtonController.OnClick) state = exit $ GoToNotifyRideShare state

eval (ToggleShare index) state = continue state {data{contactList = Just $ mapWithIndex (\i item -> if index == i then item {isSelected = not item.isSelected} else item) (fromMaybe [] state.data.contactList)}}

eval (UpdateContacts contacts) state = continue state {data{contactList = Just $ contacts}}
eval (UpdateChatWithEM flag) state = continue state {props{isChatWithEMEnabled = flag}}
eval (ShareRideAction PopupWithCheckboxController.DismissPopup) state = continue state {props{showShareRide = false}}

eval (ShareRideAction (PopupWithCheckboxController.ClickPrimaryButton PrimaryButtonController.OnClick)) state = exit $ GoToNotifyRideShare state

eval (ShareRideAction (PopupWithCheckboxController.ClickSecondaryButton)) state = continueWithCmd state [pure ShareRide]

eval (ShareRideAction (PopupWithCheckboxController.ToggleSelect index)) state = do 
  let contacts = fromMaybe [] state.data.contactList
  case contacts !! index of 
    Just contactToUpdate -> do
      let updatedContactList = updateAt index contactToUpdate{isSelected = not contactToUpdate.isSelected} contacts
      continue state {
        data{
          contactList = updatedContactList
        }
      }
    Nothing -> continue state

eval (UpdateBookingDetails (RideBookingRes response)) state = do
  let rideStatus = (fromMaybe dummyRideAPIEntity ((response.rideList) !! 0)) ^. _status
      newState = state{ props { currentStage =
                      case rideStatus of
                        "NEW" -> RideAccepted
                        "INPROGRESS" -> RideStarted
                        "COMPLETED" -> RideCompleted
                        "CANCELLED" -> HomeScreen
                        _ -> RideAccepted
                    , bookingId = response.id
                    }, data { 
                      driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes response) (state.data.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE) state.data.driverInfoCardState}}
  continue newState
  
eval (DriverInfoCardActionController (DriverInfoCardController.ShowEndOTP)) state = continue state { props { showEndOTP = true } }

eval (ScheduledRideExistsAction (PopUpModal.OnButton2Click)) state = continue state{data{ startedAt = "", invalidBookingId = Nothing, maxEstimatedDuration = 0}, props{showScheduledRideExistsPopUp = false}}

eval (ReferralComponentAction componentAction) state =
  case componentAction of
    ReferralComponent.OnClickDone referralCode ->
      if STR.length referralCode == 6 then 
        continue state{ props{ referralComponentProps{ applyButtonActive = true, referralCode = Just referralCode } } }
      else
        continue state{ props{ referralComponentProps{ applyButtonActive = false } } }

    ReferralComponent.PopUpModalAction popUpAction ->
      case popUpAction of
        PopUpModal.OnButton1Click -> do
          case state.props.referral.referralStatus of
            REFERRAL_INVALID -> do
              void $ pure $ JB.showKeyboard (getNewIDWithTag "RefferalCode")
              continue state{ props{ referral{ referralStatus = NO_REFERRAL, showAddReferralPopup = true } } }
            REFERRAL_APPLIED -> do
              void $ pure $ setValueToLocalStore REFERRAL_STATUS "REFERRED_NOT_TAKEN_RIDE"
              continue state{ props{ referral{ referralStatus = NO_REFERRAL }, isReferred = true } } 
            _ -> continue state
        PopUpModal.OnButton2Click ->
          continue state{ props{ referral{ referralStatus = NO_REFERRAL, showAddReferralPopup = false } } }
        _ -> continue state

    ReferralComponent.ApplyAction buttonAction ->
      case buttonAction of
        PrimaryButtonController.OnClick ->
          case state.props.referralComponentProps.referralCode of
            Just code -> exit $ UpdateReferralCode state{ props{ referralComponentProps{ applyButtonActive = false } } } code
            Nothing -> continue state
        _ -> continue state

    ReferralComponent.SkipAction buttonAction ->
      case buttonAction of
        PrimaryButtonController.OnClick -> do
          void $ pure $ hideKeyboardOnNavigation true
          continue state{ props{ referralComponentProps{ stage = NO_REFERRAL_STAGE }, referral{ showAddReferralPopup = false } } }
        _ -> continue state

    ReferralComponent.OpenReferralProgramInfo ->
      continue state{ props{ referralComponentProps{ showReferralProgramInfoPopup = true } } }
    
    ReferralComponent.ReferredUserInfo PopUpModal.OnButton2Click ->
      continue state{ props{ referralComponentProps{ showReferredUserInfoPopup = false } } }
    
    ReferralComponent.ReferralProgramInfo PopUpModal.OnButton2Click -> 
      continue state{ props{ referralComponentProps{ showReferralProgramInfoPopup = false } } }

    _ -> continue state

eval GoToHomeScreen state = do
  logStatus "confirming_ride" "no_active_ride"
  exit $ GoToHome state

eval (AcWorkingPopupAction (PopUpModal.OnButton1Click)) state = do
  let isAcCabRide = ServiceTierCard.showACDetails (fromMaybe "" state.data.driverInfoCardState.serviceTierName) Nothing
  if isAcCabRide then
    void $ pure $ toast $ getString GREAT_ENJOY_THE_TRIP
  else pure unit
  void $ pure $ setValueToCache (show AC_POPUP_SHOWN_FOR_RIDE) state.data.driverInfoCardState.rideId (\id -> id)
  continue state{props{showAcWorkingPopup = false}}

eval (AcWorkingPopupAction (PopUpModal.OnButton2Click)) state = do
  void $ pure $ setValueToCache (show AC_POPUP_SHOWN_FOR_RIDE) state.data.driverInfoCardState.rideId (\id -> id)
  let isAcCabRide = ServiceTierCard.showACDetails (fromMaybe "" state.data.driverInfoCardState.serviceTierName) Nothing
  if isAcCabRide then
    exit $ GoToRideRelatedIssues state
  else 
    continue state{props{showAcWorkingPopup = false}}

eval (AcWorkingPopupAction PopUpModal.DismissPopup) state = continue state{props{showAcWorkingPopup = false}}

eval NoRender state = update state

eval UpdateRateCardCache state = do
  let (rateCard :: Maybe CTP.RateCard) = handleRateCard $ runFn3 getFromCache (show RATE_CARD_INFO) Nothing Just
  continue state{data{rateCardCache = rateCard}}
  where 
    handleRateCard :: Maybe CTP.RateCard -> Maybe CTP.RateCard
    handleRateCard rateCard = do
      case rateCard of
        Nothing -> do 
            let stringifiedValue = getKeyInSharedPrefKeys (show RATE_CARD_INFO)
            if (any (_ == stringifiedValue) ["__failed", "", "(null)"]) 
              then Nothing
            else case (decodeForeignAnyImpl (parseJSON stringifiedValue)) of
                  Nothing -> Nothing
                  Just rateCard -> Just $ runFn2 setInCache (show RATE_CARD_INFO) rateCard
        Just val -> Just val

eval (TollChargeAmbigousPopUpAction PopUpModal.OnButton2Click) state = continue state { data { rideCompletedData { toll {showAmbiguousPopUp = false }}}}

eval (RequestEditAction PopUpModal.DismissPopup) state = do continue state {props { showConfirmEditDestPopUp = false }}

eval (RequestEditAction PopUpModal.OnButton1Click) state = do
  let updatedState = state {props {showConfirmEditDestPopUp = false } }
  updateAndExit updatedState $ ConfirmFare updatedState

eval (RequestEditAction PopUpModal.OnButton2Click) state = do
  continue state { props { showConfirmEditDestPopUp = false } }




eval (EditDestSearchLocationModelActionController (SearchLocationModelController.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ exitLocateOnMap ""
  let updatedState = state{props{locateOnMap = false}}
  if state.props.currentStage == EditingDestinationLoc then
    updateAndExit updatedState $ EditDestLocSelected updatedState

  else
    updateAndExit updatedState $ EditDestinationSoft updatedState
  

eval (EditDestSearchLocationModelActionController (SearchLocationModelController.LocationListItemActionController (LocationListItemController.FavClick item))) state = continueWithCmd state [pure $ (PredictionClickedAction (LocationListItemController.FavClick item))]

eval (EditDestSearchLocationModelActionController (SearchLocationModelController.SavedAddressClicked (LocationTagBarController.TagClick savedAddressType arrItem))) state = tagClickEvent savedAddressType arrItem state true

eval (EditDestSearchLocationModelActionController (SearchLocationModelController.LocationListItemActionController (LocationListItemController.OnClick item))) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_location_list_item"
  let condition = state.props.isSource == Just true && any (_ == item.locationItemType) [Just RECENTS, Just SUGGESTED_DESTINATIONS] 
  locationSelected item {tag = if condition then "" else item.tag, showDistance = Just false} true state{ props { rideSearchProps{ sourceSelectType = if condition then ST.SUGGESTION else state.props.rideSearchProps.sourceSelectType } }, data { nearByDrivers = Nothing } } true

eval (EditDestSearchLocationModelActionController (SearchLocationModelController.DebounceCallBack searchString isSource)) state = do
  if (STR.length searchString > 2) && (isSource == fromMaybe true state.props.isSource) then 
    validateSearchInput state searchString
  else continue state

eval (EditDestSearchLocationModelActionController (SearchLocationModelController.SourceChanged input)) state = do
  let srcValue = state.data.source == "" || state.data.source == "Current Location"
  let sourceSelectType = if state.props.locateOnMap then ST.MAP else state.props.rideSearchProps.sourceSelectType
      newState = state {props{ rideSearchProps{ sourceSelectType = sourceSelectType } }}
  if (input /= state.data.source) then do 
    continueWithCmd newState { props { isRideServiceable = true, searchLocationModelProps{crossBtnSrcVisibility = (STR.length input) > 2, isAutoComplete = if (STR.length input) > 2 then state.props.searchLocationModelProps.isAutoComplete else false}}} 
      [ updateInputStringFlow
      ]
  else
    continueWithCmd newState{props {searchLocationModelProps{crossBtnSrcVisibility = (STR.length input) > 2, isAutoComplete = false}}}
      [ updateInputStringFlow
      ]
  where
    updateInputStringFlow = do
      void $ pure $ updateInputString input
      pure NoAction

eval (EditDestSearchLocationModelActionController (SearchLocationModelController.DestinationChanged input)) state = do
  if (input /= state.data.destination) then do
    continueWithCmd state { props { isRideServiceable = true, searchLocationModelProps{crossBtnDestVisibility = (STR.length input) > 2, isAutoComplete = if (STR.length input)>2 then state.props.searchLocationModelProps.isAutoComplete else false}} }
      [ editDestStageFlow
      ]
  else
    continueWithCmd state{props {searchLocationModelProps{crossBtnDestVisibility = (STR.length input) > 2, isAutoComplete = false}}}
      [ editDestStageFlow
      ]
  where
  editDestStageFlow = do
    void $ pure $ updateInputString input
    pure NoAction

eval (EditDestSearchLocationModelActionController (SearchLocationModelController.EditTextFocusChanged textType)) state = do
  _ <- pure $ spy "searchLocationModal" textType
  if textType == "D" then
    continue state { props { isSource = Just false, searchLocationModelProps{crossBtnDestVisibility = (STR.length state.data.destination) > 2}}, data {source = if state.data.source == "" then state.data.searchLocationModelData.prevLocation else state.data.source, locationList = if state.props.isSource == Just false then state.data.locationList else state.data.destinationSuggestions } }
  else
    continue state { props { isSource = Just true, searchLocationModelProps{crossBtnSrcVisibility = (STR.length state.data.source) > 2}} , data{ locationList = if state.props.isSource == Just true then state.data.locationList else state.data.recentSearchs.predictionArray } }

eval (EditDestSearchLocationModelActionController (SearchLocationModelController.NoAction)) state = continue state

eval (EditDestSearchLocationModelActionController (SearchLocationModelController.DestinationClear)) state = do
  void $ pure $ performHapticFeedback unit
  if (state.props.isSearchLocation /= LocateOnMap) then do
    _ <- pure $ requestKeyboardShow (getNewIDWithTag "DestinationEditText")
    pure unit
  else
    pure unit
  let predicArray = (updateLocListWithDistance state.data.recentSearchs.predictionArray state.props.sourceLat state.props.sourceLong true state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist)
  continue state { data { destination = "", locationList = predicArray }, props {isSource = Just false, isDestServiceable = true, isRideServiceable = true, searchLocationModelProps{crossBtnDestVisibility = false}} }

eval (EditDestSearchLocationModelActionController (SearchLocationModelController.GoBack)) state = do
  void $ pure $ performHapticFeedback unit
  void $ pure $ exitLocateOnMap ""
  let updatedState = state{props{locateOnMap = false, showShimmer = true}}
  continueWithCmd updatedState
    [ do
        void $ pure $ hideKeyboardOnNavigation true
        pure $ BackPressed
    ]

eval (EditDestSearchLocationModelActionController (SearchLocationModelController.SetCurrentLocation)) state = do
  _ <- pure $ currentPosition ""
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_currentlocation_click"
  pure $ setText (getNewIDWithTag "SourceEditText") (if (state.data.source == "") then (getString CURRENT_LOCATION) else state.data.source)
  continue state{ props{ rideSearchProps{ sourceSelectType = if state.props.isSource == Just true then ST.SEARCH else state.props.rideSearchProps.sourceSelectType }, searchLocationModelProps{isAutoComplete = false}}, data{source = if state.props.currentLocation.place /= "" then state.props.currentLocation.place else (getString CURRENT_LOCATION)}}

eval (EditDestSearchLocationModelActionController (SearchLocationModelController.SetLocationOnMap)) state = do
  void $ pure $ performHapticFeedback unit
  let isSource = case state.props.isSource of
                    Just true -> true
                    _         -> false
      isDestinationNotEmpty = (not isSource && state.props.destinationLat /= 0.0 && state.props.destinationLong /= 0.0)
      lat = if isDestinationNotEmpty then state.props.destinationLat else state.props.sourceLat
      lon = if isDestinationNotEmpty then state.props.destinationLong else state.props.sourceLong
  void $ pure $ hideKeyboardOnNavigation true
  void $ pure $ removeAllPolylines ""
  void $ pure $ unsafePerformEffect $ runEffectFn1 locateOnMap locateOnMapConfig { lat = lat, lon = lon, geoJson = state.data.polygonCoordinates, points = state.data.nearByPickUpPoints, zoomLevel = pickupZoomLevel, labelId = getNewIDWithTag "LocateOnMapPin", locationName = fromMaybe "" state.props.locateOnMapProps.sourceLocationName, specialZoneMarkerConfig{ labelImage = zoneLabelIcon state.props.confirmLocationCategory }}
  pure $ unsafePerformEffect $ logEvent state.data.logField if state.props.isSource == Just true  then "ny_user_src_set_location_on_map" else "ny_user_dest_set_location_on_map"
  let srcValue = if STR.null state.data.source then getString CURRENT_LOCATION else state.data.source
  when ( STR.null state.data.destination) $ do
    pure $ setText (getNewIDWithTag "DestinationEditText") ""
  let newState = state
                  { data {source = srcValue}
                  , props { isSearchLocation = LocateOnMap
                          , locateOnMap = true,
                           isRideServiceable = true
                           , showlocUnserviceablePopUp = false
                           , searchLocationModelProps{isAutoComplete = false}
                           , locateOnMapLocation
                              { sourceLat = state.props.sourceLat
                              , sourceLng = state.props.sourceLong
                              , source = state.data.source
                              , sourceAddress = state.data.sourceAddress
                              , destinationLat = if state.props.destinationLat /= 0.0 then state.props.destinationLat else state.props.sourceLat
                              , destinationLng = if state.props.destinationLong /= 0.0 then state.props.destinationLong else state.props.sourceLong
                              , destination = state.data.destination
                              , destinationAddress = state.data.destinationAddress 
                              }
                           }
                    }
  (updateAndExit newState) $ UpdatedState newState false

eval (EditDestSearchLocationModelActionController (SearchLocationModelController.UpdateSource lat lng name)) state = do
  void $ pure $ hideKeyboardOnNavigation true
  if state.props.isSource == Just true then do
    let newState = state{data{source = if state.data.source == "" then getString CURRENT_LOCATION else state.data.source, sourceAddress = encodeAddress name [] Nothing lat lng},props{ sourceLat= lat,  sourceLong = lng, sourcePlaceId = Nothing, searchLocationModelProps{isAutoComplete = false}}}
    updateAndExit newState $ LocationSelected (fromMaybe dummyListItem newState.data.selectedLocationListItem) false newState
    else do
      let newState = state{data{destination = name,destinationAddress = encodeAddress name [] Nothing lat lng},props{ destinationLat = lat,  destinationLong = lng, destinationPlaceId = Nothing}}
      updateAndExit newState $ LocationSelected (fromMaybe dummyListItem newState.data.selectedLocationListItem) false newState

eval (EditDestSearchLocationModelActionController (SearchLocationModelController.RecenterCurrentLocation)) state = recenterCurrentLocation state

eval (EditDestSearchLocationModelActionController (SearchLocationModelController.UpdateCurrentLocation lat lng)) state = do
  if state.props.isSource == Just true then
    updateCurrentLocation state lat lng
  else
    continue state

eval (ShimmerTimer seconds status timerID) state = do
  if status == "EXPIRED" then do
    void $ pure $ clearTimerWithId timerID
    continue state{props{shimmerViewTimerId = "", showShimmer = false}}
  else update state{props{shimmerViewTimer = seconds, shimmerViewTimerId = timerID}}

eval _ state = update state

validateSearchInput :: HomeScreenState -> String -> Eval Action ScreenOutput HomeScreenState
validateSearchInput state searchString =
  if STR.length (STR.trim searchString) > 2 && searchString /= state.data.source && searchString /= (getString CURRENT_LOCATION) && (searchString /= state.data.destination || ((getSearchType unit) == "direct_search") && (state.props.isSearchLocation == SearchLocation)) then
    callSearchLocationAPI
  else
    continue state
  where
  autoCompleteType = if state.props.isSource == Just true then Just ST.PICKUP else Just ST.DROP
  sourceManuallyMoved = if state.props.isSource == Just true then false else state.props.rideSearchProps.sourceManuallyMoved
  destManuallyMoved = if state.props.isSource == Just false then false else state.props.rideSearchProps.destManuallyMoved
  callSearchLocationAPI = updateAndExit state{props{ searchLocationModelProps{showLoader = true}}} $ SearchPlace searchString state{ props{ rideSearchProps{ autoCompleteType = autoCompleteType, sourceManuallyMoved = sourceManuallyMoved, destManuallyMoved = destManuallyMoved } } }

constructLatLong :: Number -> Number -> String -> JB.Location
constructLatLong lat lng _ =
  { lat: lat
  , lng: lng
  , place: ""
  , address: Nothing
  , city : Nothing
  , isSpecialPickUp : Nothing
  }

addItemToFeedbackList :: Array String -> String -> Array String
addItemToFeedbackList feedbackList feedbackItem = if (any (_ == feedbackItem) feedbackList ) then (filter (\item -> feedbackItem /= item) feedbackList) else snoc feedbackList feedbackItem

checkPermissionAndUpdatePersonMarker :: HomeScreenState -> Effect Unit
checkPermissionAndUpdatePersonMarker state = do
  conditionA <- isLocationPermissionEnabled unit
  conditionB <- isLocationEnabled unit
  let
    conditionC = (state.props.currentStage == HomeScreen)
  if (conditionA && conditionB && conditionC) then do
    _ <- getLocationName (showPersonMarker state (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))) 9.9 9.9 "Current Location" constructLatLong
    pure unit
  else do
    if (os == "IOS" && conditionC) then do
      _ <- getLocationName (showPersonMarker state (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))) 9.9 9.9 "Current Location" constructLatLong
      pure unit
    else pure unit

updateFeedback :: String -> String -> Array FeedbackAnswer -> Array FeedbackAnswer
updateFeedback feedbackId feedbackItem feedbackList =
  if hasFeedbackId feedbackId feedbackList
    then updateFeedbackAnswer feedbackId feedbackItem feedbackList
    else addFeedbackAnswer feedbackId feedbackItem feedbackList
  where
    hasFeedbackId :: String -> Array FeedbackAnswer -> Boolean
    hasFeedbackId fid list = any (\feedback -> feedback.questionId == fid) list

    updateFeedbackAnswer :: String -> String -> Array FeedbackAnswer -> Array FeedbackAnswer
    updateFeedbackAnswer fid newItem list =
      map (\feedback ->
        if feedback.questionId == fid
                then feedback { answer = if newItem `elem` (feedback.answer) then filter (\x -> x /= newItem) feedback.answer else feedback.answer <> [newItem] }
          else feedback
        ) list

    addFeedbackAnswer :: String -> String -> Array FeedbackAnswer -> Array FeedbackAnswer
    addFeedbackAnswer fid newItem list = do
      let config = {questionId : fid, answer : [newItem]}
      list <> [config]

showPersonMarker :: HomeScreenState -> String -> JB.Location -> Effect Unit
showPersonMarker state marker location = do
  when (state.props.currentStage == HomeScreen) $ do
    void $ showMarkerOnMap (getCurrentLocationMarker $ getValueToLocalStore VERSION_NAME) location.lat location.lng
  void $ pure $ printLog "Location :: " location
  animateCamera location.lat location.lng zoomLevel "ZOOM"

showMarkerOnMap :: String -> Number -> Number -> Effect Unit
showMarkerOnMap markerName lat lng = do
  let markerConfig = defaultMarkerConfig{ markerId = markerName, pointerIcon = markerName }
  void $ showMarker markerConfig lat lng 160 0.5 0.9 (getNewIDWithTag "CustomerHomeScreenMap")

getEstimateId :: Array ChooseVehicleController.Config -> ChooseVehicleController.Config -> (Tuple String (Array String)) 
getEstimateId esimates config =
  let selectedEstimates = foldl(\acc item -> if elem (fromMaybe "" item.serviceTierName) config.selectedServices then acc <> [item.id] else acc) [] esimates
      estimateId = if config.vehicleVariant == "BOOK_ANY" then fromMaybe "" (head selectedEstimates) else config.id
      otherSelectedEstimates = fromMaybe [] $ tail $ selectedEstimates
  in (Tuple estimateId otherSelectedEstimates)


dummyRideRatingState :: RatingCard
dummyRideRatingState = {
  rating              : 0,
  driverName          : "",
  rideId              :  "",
  finalAmount         : 0,
  rideStartTime       : "",
  rideStartDate       : "",
  rideEndTime         : "",
  source              : "",
  destination         : "",
  vehicleNumber       : "",
  status              : "",
  shortRideId         : "",
  bookingId           : "",
  rideEndTimeUTC      : "",
  dateDDMMYY          : "",
  offeredFare         : 0,
  distanceDifference  : 0,
  feedback            : "",
  feedbackList        : []
}
dummyListItem :: LocationListItemState
dummyListItem = {
    prefixImageUrl : ""
  , postfixImageUrl : ""
  , postfixImageVisibility : false
  , lat : Nothing
  , lon : Nothing
  , placeId : Nothing
  , subTitle : ""
  , title : ""
  , description : ""
  , tag : ""
  , tagType : Nothing
  , cardType : Nothing
  , address : ""
  , tagName : ""
  , isEditEnabled : true
  , savedLocation : ""
  , placeName : ""
  , isClickable : true
  , alpha : 1.0
  , fullAddress : HomeScreenData.dummyAddress
  , locationItemType : Nothing
  , distance : Nothing
  , showDistance : Just false
  , actualDistance : Nothing
  , frequencyCount : Nothing
  , recencyDate : Nothing
  , locationScore : Nothing
}

tagClickEvent :: CardType -> (Maybe LocationListItemState) -> HomeScreenState -> Boolean -> Eval Action ScreenOutput HomeScreenState
tagClickEvent savedAddressType arrItem state isEditDestination = do
    let stage' = if os == "IOS" && state.props.currentStage == HomeScreen then ConfirmingLocation else LoadMap
    case savedAddressType, arrItem of
        OTHER_TAG,_  -> do
          if state.props.currentStage == EditingDestinationLoc || isEditDestination then do
            _ <- pure $ updateLocalStage FavouriteLocationModelEditDest
            continue state{props{currentStage = FavouriteLocationModelEditDest}}
          else do
            _ <- pure $ updateLocalStage FavouriteLocationModel
            continue state{props{currentStage = FavouriteLocationModel}}
        _,Nothing    -> do
          if (length state.data.savedLocations >= 20) then do
            _ <- pure $ toast (getString SORRY_LIMIT_EXCEEDED_YOU_CANT_ADD_ANY_MORE_FAVOURITES)
            continue state
            else updateAndExit state{props{tagType = Just savedAddressType}}  $ CheckFavDistance state{props{tagType = Just savedAddressType}}
        _,Just item  -> do
          if state.props.isSource == Just true then do
              let newState = state {data{ source = item.description, sourceAddress = item.fullAddress},props{sourcePlaceId = item.placeId,sourceLat = fromMaybe 0.0 item.lat,sourceLong =fromMaybe 0.0  item.lon, rideSearchProps{ sourceSelectType = ST.FAVOURITE } }}
              pure $ setText (getNewIDWithTag "SourceEditText") item.description
              pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
              updateAndExit state{props{currentStage = stage'}} $ LocationSelected item false newState
            else do
              let newState = state {data{ destination = item.description,destinationAddress = item.fullAddress},props{destinationPlaceId = item.placeId, destinationLat = fromMaybe 0.0 item.lat, destinationLong = fromMaybe 0.0 item.lon}}
              pure $ setText (getNewIDWithTag "DestinationEditText") item.description
              pure $ removeMarker $ getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)
              if isEditDestination || state.props.currentStage == EditingDestinationLoc then
                updateAndExit state{props{currentStage = stage'}} $ EditDestLocationSelected item false newState
              else
                updateAndExit state{props{currentStage = stage'}} $ LocationSelected item false newState

flowWithoutOffers :: LazyCheck -> Boolean
flowWithoutOffers dummy = not $ (getValueToLocalStore FLOW_WITHOUT_OFFERS) == "false"

recenterCurrentLocation :: HomeScreenState -> Eval Action ScreenOutput HomeScreenState
recenterCurrentLocation state = continueWithCmd state [ do
    if state.props.locateOnMap || (not state.props.locateOnMap && state.props.currentStage == ConfirmingLocation) then do
      _ <- pure $ currentPosition "NO_ZOOM"
      pure unit
    else do
      let markerName = getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)
          markerConfig = defaultMarkerConfig{ markerId = markerName, pointerIcon = markerName }
      _ <- pure $ currentPosition ""
      void $ showMarkerOnMap (getCurrentLocationMarker $ getValueToLocalStore VERSION_NAME) 9.9 9.9
      pure unit
    -- let newState = state{data{source = state.props.currentLocation.place}}
    pure NoAction
  ]

updateCurrentLocation :: HomeScreenState -> String -> String -> Eval Action  ScreenOutput HomeScreenState
updateCurrentLocation state lat lng = exit $ (CheckLocServiceability state (fromMaybe 0.0 (NUM.fromString lat )) (fromMaybe 0.0 (NUM.fromString lng)))

locationSelected :: LocationListItemState -> Boolean -> HomeScreenState -> Boolean -> Eval Action ScreenOutput HomeScreenState
locationSelected item addToRecents state isEditDestination = do
  let stage' = if os == "IOS" && state.props.currentStage == HomeScreen then ConfirmingLocation else LoadMap
  _ <- pure $ hideKeyboardOnNavigation true
  let favClick = if item.postfixImageUrl == "ny_ic_fav_red,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_fav_red.png" then "true" else "false"
  if state.props.isSource == Just true then do
    let _ = unsafePerformEffect $ logEventWithMultipleParams state.data.logField  "ny_user_pickup_select" $ [ {key : "Source", value : unsafeToForeign item.title},
                                                                                                              {key : "Favourite", value : unsafeToForeign favClick}]
        sourceSelectType = if item.tag /= "" then ST.FAVOURITE else ST.SEARCH
        newState = state {data{ source = item.title, sourceAddress = encodeAddress (item.title <> ", " <>item.subTitle) [] item.placeId (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon)},props{sourcePlaceId = item.placeId,sourceLat = fromMaybe 0.0 item.lat,sourceLong =fromMaybe 0.0  item.lon, rideSearchProps{ sourceSelectType = sourceSelectType } }}
    pure $ setText (getNewIDWithTag "SourceEditText") item.title
    updateAndExit state $ LocationSelected item addToRecents newState
    else do
      let _ = unsafePerformEffect $ logEventWithMultipleParams state.data.logField  "ny_user_destination_select" $ [{key : "Destination", value : unsafeToForeign item.title},
                                                                                                                    {key : "Favourite", value : unsafeToForeign favClick}]
      let newState = state {data{ destination = item.title,destinationAddress = encodeAddress (item.title <> ", " <>item.subTitle) [] item.placeId (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon)},props{destinationPlaceId = item.placeId, destinationLat = fromMaybe 0.0 item.lat, destinationLong = fromMaybe 0.0 item.lon}}
      pure $ setText (getNewIDWithTag "DestinationEditText") item.title
      if isEditDestination then 
        updateAndExit state{props{currentStage = stage'}} $ EditDestLocationSelected item addToRecents newState
      else
        updateAndExit state{props{currentStage = stage'}} $ LocationSelected item addToRecents newState

checkCurrentLocation :: Number -> Number -> Array CurrentLocationDetails -> Boolean
checkCurrentLocation lat lon previousCurrentLocations =  (length (filter (\ (item) -> (filterFunction lat lon item))(previousCurrentLocations)) > 0)

checkSavedLocations :: Number -> Number -> Array LocationListItemState -> Boolean
checkSavedLocations lat lon savedLocations = (length (filter(\item -> (filterSavedLocations lat lon item)) (savedLocations)) > 0 )
filterSavedLocations :: Number -> Number -> LocationListItemState -> Boolean
filterSavedLocations lat lon savedLocation = not ((getDistanceBwCordinates lat lon (fromMaybe 0.0 savedLocation.lat) (fromMaybe 0.0 savedLocation.lon)) > 0.05)

filterFunction :: Number -> Number -> CurrentLocationDetails -> Boolean
filterFunction lat lon   currLocation = not ((getDistanceBwCordinates lat lon (currLocation.lat) (currLocation.lon)) > 0.05)

getNearestCurrentLocation :: Number -> Number -> Array CurrentLocationDetails -> Array CurrentLocationDetailsWithDistance
getNearestCurrentLocation lat lon previousCurrentLocations =  (sortBy compareByDistance (map (\ (item) ->
  { distance : (getDistanceBwCordinates lat lon (item.lat) (item.lon)),
    locationDetails : item
  })
  (previousCurrentLocations)))

getNearestSavedLocation :: Number -> Number -> Array LocationListItemState -> Array CurrentLocationDetailsWithDistance
getNearestSavedLocation lat lon savedLocations = (sortBy compareByDistance (map(\item ->
  { distance : (getDistanceBwCordinates lat lon (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon)),
    locationDetails :  {lat : (fromMaybe 0.0 item.lat), lon : (fromMaybe 0.0 item.lon), placeName : (item.description)}
  }) (savedLocations)))

compareByDistance :: CurrentLocationDetailsWithDistance -> CurrentLocationDetailsWithDistance -> Ordering
compareByDistance ( a) ( b) = compare (a.distance ) (b.distance)

updateMessagesWithCmd :: HomeScreenState -> Eval Action ScreenOutput HomeScreenState
updateMessagesWithCmd state =
  continueWithCmd state [ do
    if(state.props.currentStage == ChatWithDriver) then do
      _ <- pure $ setValueToLocalNativeStore READ_MESSAGES (show (length state.data.messages))
      pure unit
    else
      pure unit
    if state.props.showChatNotification then pure $ (DriverInfoCardActionController (DriverInfoCardController.CollapseBottomSheet)) else pure NoAction
    ]

dummySelectedQuotes :: SelectedQuotes
dummySelectedQuotes = SelectedQuotes {
  selectedQuotes : []
}

getSearchExpiryTime :: Boolean -> Int
getSearchExpiryTime isNormalRide =
  let count = fromMaybe 0 (fromString (getValueToLocalStore $ if isNormalRide then TEST_POLLING_COUNT else CONFIRM_QUOTES_POLLING_COUNT))
      interval = (fromMaybe 0.0 (NUM.fromString (getValueToLocalStore $ if isNormalRide then TEST_POLLING_INTERVAL else CONFIRM_QUOTES_POLLING_COUNT)) / 1000.0)
      searchExpiryTime = round $ (toNumber count) * interval
  in searchExpiryTime

tipEnabledState :: HomeScreenState -> HomeScreenState
tipEnabledState state = do
  let tipConfig = getTipConfig state.data.selectedEstimatesObject.vehicleVariant
      customerTipArrayWithValues = tipConfig.customerTipArrayWithValues
      isTipEnabled = not $ null customerTipArrayWithValues
      tipActiveIndex = if state.props.customerTip.tipActiveIndex == -1 then 1 else state.props.customerTip.tipActiveIndex
  state { props{customerTip {isTipSelected = isTipEnabled, tipForDriver= (fromMaybe 0 (customerTipArrayWithValues !! tipActiveIndex)), tipActiveIndex = tipActiveIndex}}}

isTipEnabled :: HomeScreenState -> Boolean
isTipEnabled state =
    let tipConfig = getTipConfig state.data.selectedEstimatesObject.vehicleVariant
        customerTipArrayWithValues = tipConfig.customerTipArrayWithValues
    in not $ null customerTipArrayWithValues

estimatesListTryAgainFlow :: GetQuotesRes -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
estimatesListTryAgainFlow (GetQuotesRes quotesRes) state = do
  let
    estimates = quotesRes.estimates
    estimatedVarient = filter (\x -> x ^. _vehicleVariant == state.data.selectedEstimatesObject.vehicleVariant) estimates
    estimatedPrice = if (isJust (estimatedVarient !! 0)) then (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _estimatedFare else 0
    quoteList = getEstimateList estimatedVarient state.data.config.estimateAndQuoteConfig state.data.selectedEstimatesObject.activeIndex
    defaultQuote = fromMaybe ChooseVehicleController.config (quoteList !! 0)
  case (null estimatedVarient) of
    true -> do
      _ <- pure $ hideKeyboardOnNavigation true
      _ <- pure $ toast $ getString NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN
      continue state { props { currentStage = SearchLocationModel, rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = true, isDestServiceable = true, isRideServiceable = true } }
    false -> do
      if (estimatedPrice >  state.data.selectedEstimatesObject.basePrice) then
            continue state { data { suggestedAmount = estimatedPrice }, props { estimateId = defaultQuote.id, isEstimateChanged = true } }
      else do
        let updatedState = state { data { suggestedAmount = estimatedPrice }, props { estimateId = defaultQuote.id, searchExpire = (getSearchExpiryTime true) } }
        exit $ SelectEstimateAndQuotes updatedState


normalRideFlow :: RideBookingRes -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
normalRideFlow  (RideBookingRes response) state = do
  let rideStatus = (fromMaybe dummyRideAPIEntity ((response.rideList) !! 0)) ^. _status
      bookingStatus = response.status
      rideScheduledAt = if bookingStatus == "CONFIRMED" then fromMaybe "" response.rideScheduledTime else ""
      newState = state{ props { currentStage =
            case rideStatus, bookingStatus of
              "NEW" , _ -> RideAccepted
              "INPROGRESS" , _ -> RideStarted
              "COMPLETED", _ -> RideCompleted
              "CANCELLED",_ -> HomeScreen
              _ , "CONFIRMED" -> ConfirmingQuotes
              _ , _ -> RideAccepted
          , isSearchLocation = NoView
          , bookingId = response.id
          }
        , data
          { driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes response) (state.data.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE) state.data.driverInfoCardState
          , fareProductType = getFareProductType $ response.bookingDetails^._fareProductType
          }}
  exit $ RideConfirmed newState { props { isInApp = true } }


specialZoneRideFlow :: RideBookingRes -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
specialZoneRideFlow  (RideBookingRes response) state = do
  let
    newState =
      state
        { props
          { currentStage = RideAccepted
          , isSearchLocation = NoView
          , bookingId = response.id
          }
        , data
          { driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes response) (state.data.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE) state.data.driverInfoCardState
          }
        }
  exit $ RideConfirmed newState { props { isInApp = true } }


findingQuotesSearchExpired :: Boolean -> Boolean -> Int
findingQuotesSearchExpired gotQuotes isNormalRide =
  let secondsPassed = getExpiryTime (getValueToLocalStore $ if isNormalRide then FINDING_QUOTES_START_TIME else CONFIRM_QUOTES_START_TIME) isNormalRide
      searchExpiryTime = getSearchExpiryTime isNormalRide
      secondsLeft = case gotQuotes of
                      true  -> if (searchExpiryTime - secondsPassed) < 30 then (searchExpiryTime - secondsPassed) else 30
                      false -> (searchExpiryTime - secondsPassed)
  in secondsLeft

callDriver :: HomeScreenState -> String -> Eval Action ScreenOutput HomeScreenState
callDriver state callType = do
  let newState = state{props{ showCallPopUp = false }}
      driverNumber = case callType of
                        "DIRECT" ->(fromMaybe state.data.driverInfoCardState.merchantExoPhone state.data.driverInfoCardState.driverNumber)
                        _ -> if (STR.take 1 state.data.driverInfoCardState.merchantExoPhone) == "0" then state.data.driverInfoCardState.merchantExoPhone else "0" <> state.data.driverInfoCardState.merchantExoPhone
  updateWithCmdAndExit newState
    [ do
        _ <- pure $ showDialer driverNumber false
        let _ = unsafePerformEffect $ logEventWithTwoParams state.data.logField ("ny_user_"<> callType <>"_call_click") "trip_id" (state.props.bookingId) "user_id" (getValueToLocalStore CUSTOMER_ID)
        pure NoAction
    ] $ CallDriver newState (if callType == "DIRECT" then DIRECT_CALLER else ANONYMOUS_CALLER) driverNumber

getInfoCardPeekHeight :: HomeScreenState -> Int
getInfoCardPeekHeight state = 
  let bottomSheetLayout = (runFn1 getLayoutBounds $ getNewIDWithTag (if state.data.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE then "driverInfoViewSpecialZone" else "driverInfoView"))
      brandingBanner = runFn1 getLayoutBounds $ getNewIDWithTag "BrandingBanner"
      actionsView = runFn1 getLayoutBounds $ getNewIDWithTag "DriverInfoCardActionView"
      isDriverInfoCardBanner = isJust state.props.sosBannerType && state.data.config.feature.enableSafetyFlow && state.props.currentStage == RideStarted
      driverDetailsView = if isDriverInfoCardBanner && (state.data.fareProductType == FPT.RENTAL) then runFn1 getLayoutBounds $ getNewIDWithTag "DriverDetailsView"  else {height : 0, width : 0}
      driverDetailsViewPadding = if isDriverInfoCardBanner && (state.data.fareProductType == FPT.RENTAL) then 16 else 0
      fareEstimate = runFn1 getLayoutBounds $ getNewIDWithTag "PaymentMethodView"
      fareEstimateViewPadding = 12
      pixels = runFn1 getPixels FunctionCall
      density = (runFn1 getDeviceDefaultDensity FunctionCall)/  defaultDensity
      currentPeekHeight = if bottomSheetLayout.height == 0 || actionsView.height == 0
                          then 0
                          else bottomSheetLayout.height + if state.data.config.driverInfoConfig.footerVisibility then brandingBanner.height else 0 - (if state.data.fareProductType == FPT.RENTAL then (fareEstimate.height + fareEstimateViewPadding + driverDetailsView.height + driverDetailsViewPadding) else 0) + actionsView.height
      requiredPeekHeight = if os /= "IOS" then ceil (((toNumber currentPeekHeight) /pixels) * density) else currentPeekHeight
    in requiredPeekHeight


getPeekHeight :: HomeScreenState -> Int
getPeekHeight state = 
      let homescreenHeader =  (runFn1 getLayoutBounds (getNewIDWithTag "homescreenHeader")).height
          scrHeight = (getDeviceHeight unit)
          requiredPeekHeight = if os == "IOS"
                                then getPeekHeightForIos homescreenHeader scrHeight
                                else  getPeekHeightForAndroid homescreenHeader
      in if homescreenHeader == 0 then 500 else requiredPeekHeight
      where 
        getPeekHeightForIos :: Int -> Int -> Int
        getPeekHeightForIos homescreenHeader scrHeight =
          let iosScale = runFn1 getPixels FunctionCall
              iosNativeScale = runFn1 getDefaultPixels ""
              displayZoomFactor = iosNativeScale / iosScale
          in ceil((( (toNumber scrHeight) / displayZoomFactor)/ iosScale) - (toNumber homescreenHeader) )

        getPeekHeightForAndroid :: Int -> Int
        getPeekHeightForAndroid homescreenHeader =
          let androidPixels = runFn1 getPixels FunctionCall
              androidDensity = (runFn1 getDeviceDefaultDensity FunctionCall)/  defaultDensity
          in (screenHeight unit) - ( ceil(((toNumber homescreenHeader)/androidPixels) *androidDensity))

checkRecentRideVariant :: HomeScreenState -> Boolean
checkRecentRideVariant state = do
  let _ = spy "RepeatRideVariant" state
  any (\item -> item.providerType == ONUS && isJust item.serviceTierName && item.serviceTierName == state.props.repeatRideServiceTierName) state.data.specialZoneQuoteList 

openLiveDashboard :: HomeScreenState -> Eval Action ScreenOutput HomeScreenState
openLiveDashboard state = do 
  void $ pure $ setValueToLocalStore LIVE_DASHBOARD "LIVE_DASHBOARD_SELECTED"
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_live_stats_dashboard"
      dashboardUrl = if STR.null state.data.currentCityConfig.dashboardUrl then state.data.config.dashboard.url else state.data.currentCityConfig.dashboardUrl
  if os == "IOS" then do
    continueWithCmd state [do
      void $ openUrlInApp dashboardUrl
      pure NoAction
    ]
  else continue state {props {showLiveDashboard = true}}

cacheRateCard :: HomeScreenState -> Effect Unit 
cacheRateCard state = do
  let rateCard =  state.data.rateCard { onFirstPage = false
                  , serviceTierName = state.data.selectedEstimatesObject.serviceTierName
                  , currentRateCardType = DefaultRateCard
                  , extraFare = state.data.selectedEstimatesObject.extraFare
                  , fareInfoDescription = state.data.selectedEstimatesObject.fareInfoDescription
                  , additionalFare = state.data.selectedEstimatesObject.additionalFare
                  , isNightShift = state.data.selectedEstimatesObject.isNightShift
                  , nightChargeTill = state.data.selectedEstimatesObject.nightChargeTill
                  , nightChargeFrom = state.data.selectedEstimatesObject.nightChargeFrom
                  , driverAdditions = state.data.selectedEstimatesObject.driverAdditions
                  , waitingTimeInfo = state.data.selectedEstimatesObject.waitingTimeInfo
                  }
  if state.data.selectedEstimatesObject.vehicleVariant == "BOOK_ANY" then do
    let _ = JB.removeKeysInSharedPrefs $ show RATE_CARD_INFO
    void $ pure $ removeValueFromCache (show RATE_CARD_INFO)
  else void $ pure $ setValueToCache (show RATE_CARD_INFO) rateCard (\a -> stringifyJSON $ encode a)

logChatSuggestion :: HomeScreenState -> String -> Unit
logChatSuggestion state chatSuggestion = unsafePerformEffect $ logEvent state.data.logField $ "ny_" <> STR.toLower (STR.replaceAll (STR.Pattern "'") (STR.Replacement "") (STR.replaceAll (STR.Pattern ",") (STR.Replacement "") (STR.replaceAll (STR.Pattern " ") (STR.Replacement "_") chatSuggestion)))


openDateTimePicker :: HomeScreenState -> Eval Action ScreenOutput HomeScreenState
openDateTimePicker state =
  continueWithCmd state
    [ do 
      push <- getPushFn Nothing "HomeScreen"
      _ <- launchAff $ showDateTimePicker push DateTimePickerAction
      pure NoAction
    ]


truncate :: Int -> Maybe Number -> Maybe Number
truncate precision value = if isJust value then Just $ EHC.truncate precision ((fromMaybe 0.0 value)/1000.0) else Nothing

roundOff :: Maybe Number -> Maybe Int
roundOff value = if isJust value then Just $ round (fromMaybe 0.0 value) else Nothing
  
checkRecentRideVariantInEstimates :: Array ChooseVehicleController.Config -> Maybe String -> Boolean
checkRecentRideVariantInEstimates estimatesAndQuotes repeatRideServiceName = 
  any (\item -> item.providerType == CTP.ONUS && isJust item.serviceTierName && item.serviceTierName == repeatRideServiceName) estimatesAndQuotes 
