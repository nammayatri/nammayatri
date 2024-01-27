{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.View where

import Accessor (_lat, _lon, _selectedQuotes, _fareProductType)
import Animation (fadeInWithDelay, fadeIn, fadeOut, translateYAnimFromTop, scaleAnim, translateYAnimFromTopWithAlpha , translateInXAnim, translateOutXAnim, translateInXForwardAnim, translateOutXBackwardAnimY, translateInXSidebarAnim, screenAnimation, fadeInWithDuration, fadeOutWithDuration, scaleYAnimWithDelay, shimmerAnimation)
import Animation.Config (Direction(..), translateFullYAnimWithDurationConfig, translateYAnimHomeConfig, messageInAnimConfig, messageOutAnimConfig)
import Common.Types.App (LazyCheck(..), YoutubeData, CarouselData)
import Components.Banner.Controller as BannerConfig
import Components.ChooseVehicle as ChooseVehicle
import Components.Banner.View as Banner
import Components.MessagingView as MessagingView
import Components.ChooseYourRide as ChooseYourRide 
import Components.DriverInfoCard as DriverInfoCard
import Components.EmergencyHelp as EmergencyHelp
import Components.ErrorModal as ErrorModal
import Components.FavouriteLocationModel as FavouriteLocationModel
import Components.LocationListItem.View as LocationListItem
import Components.LocationTagBar as LocationTagBar
import Components.MenuButton as MenuButton
import Components.PopUpModal as PopUpModal
import Components.RideCompletedCard as RideCompletedCard
import Components.PricingTutorialModel as PricingTutorialModel
import Components.PrimaryButton as PrimaryButton
import Components.QuoteListModel.View as QuoteListModel
import Components.RateCard as RateCard
import Components.RatingCard as RatingCard
import Components.RequestInfoCard as RequestInfoCard
import Components.SaveFavouriteCard as SaveFavouriteCard
import Components.SearchLocationModel as SearchLocationModel
import Components.SelectListModal as CancelRidePopUp
import Components.SettingSideBar as SettingSideBar
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (any, length, mapWithIndex,take, (!!),head, filter, cons, null, tail)
import Data.Array as Arr
import Data.Either (Either(..))
import Data.Int (ceil, floor, fromNumber, fromString, toNumber)
import Data.Function.Uncurried (runFn1)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number as NUM
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1, runEffectFn2)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, liftFlow, os, safeMarginBottom, safeMarginTop, screenHeight, isPreviousVersion, screenWidth, camelCaseToSentenceCase, truncate,getExpiryTime, getDeviceHeight, getScreenPpi)
import Engineering.Helpers.Suggestions (getMessageFromKey, getSuggestionsfromKey)
import Engineering.Helpers.Utils (showAndHideLoader)
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.Utils (showAndHideLoader)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), decodeError, fetchAndUpdateCurrentLocation, getAssetsBaseUrl, getCurrentLocationMarker, getLocationName, getNewTrackingId, getSearchType, parseFloat, storeCallBackCustomer, didDriverMessage, getPixels, getDefaultPixels, getDeviceDefaultDensity)
import JBridge (addMarker, animateCamera, clearChatMessages, drawRoute, enableMyLocation, firebaseLogEvent, generateSessionId, getArray, getCurrentPosition, getExtendedPath, getHeightFromPercent, getLayoutBounds, initialWebViewSetUp, isCoordOnPath, isInternetAvailable, isMockLocation, lottieAnimationConfig, removeAllPolylines, removeMarker, requestKeyboardShow, scrollOnResume, showMap, startChatListenerService, startLottieProcess, stopChatListenerService, storeCallBackMessageUpdated, storeCallBackOpenChatScreen, storeKeyBoardCallback, toast, updateRoute, addCarousel, updateRouteConfig, addCarouselWithVideoExists, storeCallBackLocateOnMap, storeOnResumeCallback, setMapPadding)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (Unit, bind, const, discard, map, negate, not, pure, show, unit, void, when, ($), (&&), (*), (+), (-), (/), (/=), (<), (<<<), (<=), (<>), (==), (>), (||), (<$>), identity)
import Presto.Core.Types.API (ErrorResponse)
import Presto.Core.Types.Language.Flow (Flow, doAff, delay)
import PrestoDOM (BottomSheetState(..), Gradient(..), Gravity(..), Length(..), Accessiblity(..), Margin(..), Accessiblity(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Shadow(..), adjustViewWithKeyboard, afterRender, alignParentBottom, background, clickable, color, cornerRadius, disableClickFeedback, ellipsize, fontStyle, frameLayout, gradient, gravity, halfExpandedRatio, height, id, imageView, imageWithFallback, lineHeight, linearLayout, lottieAnimationView, margin, maxLines, onBackPressed, onClick, orientation, padding, peakHeight, relativeLayout, scaleType, singleLine, stroke, text, textFromHtml, textSize, textView, url, visibility, webView, weight, width, layoutGravity, accessibilityHint, accessibility, accessibilityFocusable, focusable, scrollView, onAnimationEnd, clipChildren, enableShift,horizontalScrollView, shadow,onStateChanged,scrollBarX, clipToPadding, onSlide, rotation, rippleColor, shimmerFrameLayout)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import PrestoDOM.Properties (cornerRadii, sheetState, alpha, nestedScrollView)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.AddNewAddressScreen.Controller as AddNewAddress
import Screens.HomeScreen.Controller (Action(..), ScreenOutput, checkCurrentLocation, checkSavedLocations, dummySelectedQuotes, eval, flowWithoutOffers, getPeekHeight)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.HomeScreen.Transformer (transformSavedLocations)
import Screens.RideBookingFlow.HomeScreen.Config
import Services.API (GetDriverLocationResp(..), GetQuotesRes(..), GetRouteResp(..), LatLong(..), RideAPIEntity(..), RideBookingRes(..), Route(..), SavedLocationsListRes(..), SearchReqLocationAPIEntity(..), SelectListRes(..), Snapped(..), GetPlaceNameResp(..), PlaceName(..), RideBookingListRes(..))
import Screens.Types (CallType(..), HomeScreenState, LocationListItemState, PopupType(..), SearchLocationModelType(..), SearchResultType(..), Stage(..), ZoneType(..), SheetState(..), Trip(..), SuggestionsMap(..), Suggestions(..),City(..))
import Services.Backend (getDriverLocation, getQuotes, getRoute, makeGetRouteReq, rideBooking, selectList, getRouteMarkers, walkCoordinates, walkCoordinate, getSavedLocationList)
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore, isLocalStageOn, setValueToLocalStore, updateLocalStage, getValueToLocalNativeStore)
import Styles.Colors as Color
import Types.App (GlobalState, defaultGlobalState)
import Halogen.VDom.DOM.Prop (Prop)
import Data.String as DS
import Data.Function.Uncurried (runFn1, runFn2)
import Components.CommonComponentConfig as CommonComponentConfig
import Constants.Configs 
import Common.Resources.Constants (zoomLevel)
import Constants (defaultDensity)
import Animation as Anim
import Animation.Config (AnimConfig, animConfig)
import Components.SourceToDestination as SourceToDestination
import Data.Map as Map
import SuggestionUtils
import MerchantConfig.Types (MarginConfig, ShadowConfig)
import ConfigProvider
import Mobility.Prelude
import Timers
import PrestoDOM.Core
import Locale.Utils

screen :: HomeScreenState -> Screen Action HomeScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "HomeScreen"
  , globalEvents:
      [ ( \push -> do
            _ <- pure $ printLog "storeCallBackCustomer initially" "."
            _ <- pure $ printLog "storeCallBackCustomer callbackInitiated" initialState.props.callbackInitiated
            -- push NewUser -- TODO :: Handle the functionality
            _ <- if initialState.data.config.enableMockLocation then isMockLocation push IsMockLocation else pure unit
            _ <- launchAff $ flowRunner defaultGlobalState $ checkForLatLongInSavedLocations push UpdateSavedLoc initialState
            if (not initialState.props.callbackInitiated) then do
              _ <- pure $ printLog "storeCallBackCustomer initiateCallback" "."
              _ <- storeCallBackCustomer push NotificationListener
              _ <- pure $ runFn2 storeOnResumeCallback push OnResumeCallback
              _ <- runEffectFn2 storeKeyBoardCallback push KeyboardCallback
              push HandleCallback
              pure unit
            else do
              pure unit
            case initialState.props.currentStage of
              SearchLocationModel -> case initialState.props.isSearchLocation of
                LocateOnMap -> do
                  _ <- storeCallBackLocateOnMap push UpdateLocation
                  pure unit
                _ -> do
                  case initialState.props.isSource of
                    Just index -> do
                      _ <- pure $ requestKeyboardShow (if index then (getNewIDWithTag "SourceEditText") else (getNewIDWithTag "DestinationEditText"))
                      pure unit
                    Nothing -> pure unit
                  pure unit
              FindingEstimate -> do
                _ <- pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                _ <- launchAff $ flowRunner defaultGlobalState $ getEstimate GetEstimates CheckFlowStatusAction 10 1000.0 push initialState
                pure unit
              FindingQuotes -> do
                when ((getValueToLocalStore FINDING_QUOTES_POLLING) == "false") $ do
                  void $ pure $ setValueToLocalStore FINDING_QUOTES_POLLING "true"
                  void $ pure $ setValueToLocalStore AUTO_SELECTING "false"
                  void $ startTimer initialState.props.searchExpire "findingQuotes" "1" push SearchExpireCountDown 
                  void $ pure $ setValueToLocalStore GOT_ONE_QUOTE "FALSE"
                  void $ pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
                  let pollingCount = ceil ((toNumber initialState.props.searchExpire)/((fromMaybe 0.0 (NUM.fromString (getValueToLocalStore TEST_POLLING_INTERVAL))) / 1000.0))
                  void $ launchAff $ flowRunner defaultGlobalState $ getQuotesPolling (getValueToLocalStore TRACKING_ID) GetQuotesList Restart pollingCount (fromMaybe 0.0 (NUM.fromString (getValueToLocalStore TEST_POLLING_INTERVAL))) push initialState
              ConfirmingRide -> void $ launchAff $ flowRunner defaultGlobalState $ confirmRide GetRideConfirmation 5 3000.0 push initialState
              HomeScreen -> do
                let suggestionsMap = getSuggestionsMapFromLocal FunctionCall
                if (getValueToLocalStore UPDATE_REPEAT_TRIPS == "true" && Map.isEmpty suggestionsMap) then do
                  void $ launchAff $ flowRunner defaultGlobalState $ updateRecentTrips UpdateRepeatTrips push Nothing
                else do 
                  push RemoveShimmer 
                  pure unit
                when (isJust initialState.data.rideHistoryTrip) $ do 
                  push $ RepeatRide 0 (fromMaybe HomeScreenData.dummyTrip initialState.data.rideHistoryTrip)
                when (initialState.props.autoScroll) $ 
                  startTimer initialState.data.config.suggestedTripsAndLocationConfig.autoScrollTime "autScroll" "1" push AutoScrollCountDown
                _ <- pure $ setValueToLocalStore SESSION_ID (generateSessionId unit)
                _ <- pure $ removeAllPolylines ""
                _ <- pure $ enableMyLocation true
                _ <- pure $ setValueToLocalStore NOTIFIED_CUSTOMER "false"
                fetchAndUpdateCurrentLocation push UpdateLocAndLatLong RecenterCurrentLocation
              SettingPrice -> do
                _ <- pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                when 
                  (initialState.props.isRepeatRide && initialState.data.config.estimateAndQuoteConfig.enableOnlyAuto) 
                    $ startTimer initialState.data.config.suggestedTripsAndLocationConfig.repeatRideTime "repeatRide" "1" push RepeatRideCountDown
              PickUpFarFromCurrentLocation -> 
                void $ pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
              RideAccepted -> do
                when 
                  (initialState.data.config.notifyRideConfirmationConfig.notify && any (_ == getValueToLocalStore NOTIFIED_CUSTOMER) ["false" , "__failed" , "(null)"])
                    $ startTimer 5 "notifyCustomer" "1" push NotifyDriverStatusCountDown
                _ <- pure $ enableMyLocation true
                if ((getValueToLocalStore DRIVER_ARRIVAL_ACTION) == "TRIGGER_WAITING_ACTION") then do
                  void $ waitingCountdownTimerV2 initialState.data.driverInfoCardState.driverArrivalTime "1" "countUpTimerId" push WaitingTimeAction
                else 
                  when 
                    (initialState.data.currentSearchResultType == QUOTES) $ do
                      let secondsLeft = initialState.data.config.driverInfoConfig.specialZoneQuoteExpirySeconds - (getExpiryTime initialState.data.driverInfoCardState.createdAt true)
                      void $ startTimer secondsLeft "SpecialZoneOTPExpiry" "1" push SpecialZoneOTPExpiryAction
                if ((getValueToLocalStore TRACKING_DRIVER) == "False") then do
                  _ <- pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                  _ <- pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
                  void $ launchAff $ flowRunner defaultGlobalState $ driverLocationTracking push UpdateCurrentStage DriverArrivedAction UpdateETA 3000.0 (getValueToLocalStore TRACKING_ID) initialState "pickup"
                else pure unit
                push LoadMessages
                if(not initialState.props.chatcallbackInitiated && not initialState.props.isSpecialZone) then do
                  _ <- clearChatMessages
                  _ <- storeCallBackMessageUpdated push initialState.data.driverInfoCardState.bppRideId "Customer" UpdateMessages
                  _ <- storeCallBackOpenChatScreen push OpenChatScreen
                  _ <- startChatListenerService
                  _ <- pure $ scrollOnResume push ScrollToBottom
                  push InitializeChat
                  pure unit
                else
                  pure unit
                void $ push $ DriverInfoCardActionController DriverInfoCard.NoAction
              RideStarted -> do
                _ <- pure $ enableMyLocation false
                if ((getValueToLocalStore TRACKING_DRIVER) == "False") then do
                  _ <- pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                  _ <- pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
                  _ <- launchAff $ flowRunner defaultGlobalState $ driverLocationTracking push UpdateCurrentStage DriverArrivedAction UpdateETA 20000.0 (getValueToLocalStore TRACKING_ID) initialState "trip"
                  pure unit
                else
                  pure unit
                _ <- push RemoveChat
                pure unit
                void $ push $ DriverInfoCardActionController DriverInfoCard.NoAction
              ChatWithDriver -> if ((getValueToLocalStore DRIVER_ARRIVAL_ACTION) == "TRIGGER_WAITING_ACTION") then waitingCountdownTimerV2 initialState.data.driverInfoCardState.driverArrivalTime "1" "countUpTimerId" push WaitingTimeAction else pure unit
              ConfirmingLocation -> do
                _ <- pure $ enableMyLocation true
                _ <- pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                _ <- storeCallBackLocateOnMap push UpdatePickupLocation
                pure unit
              TryAgain -> do
                _ <- launchAff $ flowRunner defaultGlobalState $ getEstimate EstimatesTryAgain CheckFlowStatusAction 10 1000.0 push initialState
                pure unit
              FindEstimateAndSearch -> do
                push $ SearchForSelectedLocation
                pure unit
              ReAllocated ->
                void $ launchAff $ flowRunner defaultGlobalState $ reAllocateConfirmation push initialState ReAllocate 3000.0
              _ -> pure unit
            if ((initialState.props.sourceLat /= (-0.1)) && (initialState.props.sourceLong /= (-0.1))) then do
              case initialState.props.sourceLat, initialState.props.sourceLong of
                0.0, 0.0 -> do
                  _ <- getCurrentPosition push CurrentLocation
                  pure (pure unit)
                _, _ -> pure (pure unit)
            else
              pure (pure unit)
        )
      ]
  , eval:
      \action state -> do
        let _ = spy "HomeScreen action " action
        let _ = spy "HomeScreen state " state
        eval action state
  }

getDelayForLocateOnMap :: Int
getDelayForLocateOnMap = 1000

enableCurrentLocation :: Boolean
enableCurrentLocation = true

disableCurrentLocation :: Boolean
disableCurrentLocation = false

isCurrentLocationEnabled :: Boolean
isCurrentLocationEnabled = if (isLocalStageOn HomeScreen) then enableCurrentLocation else disableCurrentLocation

view :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
view push state =
  let showLabel = if state.props.defaultPickUpPoint == "" then false else true
  in
  frameLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , onBackPressed push (const BackPressed)
    , clickable true
    , afterRender push (const AfterRender)
    , accessibility DISABLE
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , accessibility DISABLE
        , clickable true
        ]
       [ relativeLayout
            [ width MATCH_PARENT
            , weight 1.0
            , orientation VERTICAL
            , background Color.transparent
            , accessibility DISABLE
            , height MATCH_PARENT
            ]
            ([ frameLayout
                [ width MATCH_PARENT
                , height MATCH_PARENT
                , accessibility DISABLE
                , clickable true
                ]
                [ linearLayout
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , background Color.transparent
                  , accessibility if any (_ == state.props.currentStage) [RideAccepted, RideStarted, HomeScreen] && not isAnyOverlayEnabled state then ENABLE else DISABLE
                  , accessibilityHint $ camelCaseToSentenceCase (show state.props.currentStage)
                  ][ 
                    if isHomeScreenView state then homeScreenViewV2 push state else emptyTextView state
                  , if isHomeScreenView state then emptyTextView state else mapView push state "CustomerHomeScreen"
                    ]
                , linearLayout
                    [ width MATCH_PARENT
                    , height MATCH_PARENT
                    , background Color.transparent
                    , padding (PaddingBottom if os == "IOS" then 53 else 70)
                    , gravity CENTER
                    , accessibility DISABLE
                    , orientation VERTICAL
                    ]
                    [ textView
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , background Color.black800
                        , color Color.white900
                        , accessibility DISABLE_DESCENDANT
                        , text if DS.length state.props.defaultPickUpPoint > state.data.config.mapConfig.labelTextSize then
                                  (DS.take (state.data.config.mapConfig.labelTextSize - 3) state.props.defaultPickUpPoint) <> "..."
                               else
                                  state.props.defaultPickUpPoint
                        , padding (Padding 5 5 5 5)
                        , margin (MarginBottom 5)
                        , cornerRadius 5.0
                        , visibility if (showLabel && ((state.props.currentStage == ConfirmingLocation) || state.props.locateOnMap)) then VISIBLE else INVISIBLE
                        , id (getNewIDWithTag "LocateOnMapPin")
                        ]
                    , imageView
                        [ width $ V 35
                        , height $ V 35
                        , accessibility DISABLE
                        , imageWithFallback $ fetchImage FF_COMMON_ASSET $ case (state.props.currentStage == ConfirmingLocation) || state.props.isSource == (Just true) of
                            true  -> "ny_ic_src_marker"
                            false -> "ny_ic_dest_marker"
                        , visibility if ((state.props.currentStage == ConfirmingLocation) || state.props.locateOnMap) then VISIBLE else GONE
                        ]
                    ]
                ]
            -- , homeScreenView push state
            -- , buttonLayoutParentView push state
            , if (not state.props.rideRequestFlow) || any (_ == state.props.currentStage) [ FindingEstimate, ConfirmingRide, HomeScreen] then emptyTextView state else topLeftIconView state push
            , rideRequestFlowView push state
            , if state.props.currentStage == PricingTutorial then (pricingTutorialView push state) else emptyTextView state
            , if (any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithDriver]) then rideInfoView push state else emptyTextView state
            , if (any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithDriver]) then
                relativeLayout 
                [ width MATCH_PARENT
                , height MATCH_PARENT
                ][ rideTrackingView push state 
                , DriverInfoCard.brandingBannerView state.data.config.driverInfoConfig VISIBLE (Just "BrandingBanner")
                ]
              else emptyTextView state
            , if state.props.currentStage == ChatWithDriver then messagingView push state else emptyTextView state
            , if state.props.currentStage /= RideRating && state.props.isMockLocation && (getMerchant FunctionCall == NAMMAYATRI) && state.props.currentStage == HomeScreen then (sourceUnserviceableView push state) else emptyTextView state
            , if state.data.settingSideBar.opened /= SettingSideBar.CLOSED then settingSideBarView push state else emptyTextView state
            , if (state.props.currentStage == SearchLocationModel || state.props.currentStage == FavouriteLocationModel) then searchLocationView push state else emptyTextView state
            , if (any (_ == state.props.currentStage) [ FindingQuotes, QuoteList, TryAgain ]) then (quoteListModelView push state) else emptyTextView state
            , if (state.props.isCancelRide) then (cancelRidePopUpView push state) else emptyTextView state
            , if (state.props.isPopUp /= NoPopUp) then (logOutPopUpView push state) else emptyTextView state
            , if (state.props.isLocationTracking) then (locationTrackingPopUp push state) else emptyTextView state
            , if (state.props.isEstimateChanged) then (estimateChangedPopUp push state) else emptyTextView state
            , if state.props.currentStage == PickUpFarFromCurrentLocation then (pickUpFarFromCurrLocView push state) else emptyTextView state
            , if state.props.currentStage == DistanceOutsideLimits then (distanceOutsideLimitsView push state) else emptyTextView state
            , if state.props.currentStage == ShortDistance then (shortDistanceView push state) else emptyTextView state
            , if state.props.isSaveFavourite then saveFavouriteCardView push state else emptyTextView state
            , if state.props.emergencyHelpModal then (emergencyHelpModal push state) else emptyTextView state
            , if state.props.showShareAppPopUp && state.data.config.feature.enableShareApp then shareAppPopUp push state else emptyTextView state
            , if state.props.showMultipleRideInfo then (requestInfoCardView push state) else emptyTextView state
            , if state.props.showLiveDashboard then showLiveStatsDashboard push state else emptyTextView state
            , if state.props.showCallPopUp then (driverCallPopUp push state) else emptyTextView state
            , if state.props.cancelSearchCallDriver then cancelSearchPopUp push state else emptyTextView state
            , if state.props.currentStage == RideCompleted || state.props.currentStage == RideRating then rideCompletedCardView push state else emptyTextView state
            , if state.props.currentStage == RideRating then rideRatingCardView state push else emptyTextView state
            , if state.props.showRateCard then (rateCardView push state) else emptyTextView state
            -- , if state.props.zoneTimerExpired then zoneTimerExpiredView state push else emptyTextView state
            , if state.props.callSupportPopUp then callSupportPopUpView push state else emptyTextView state
            , if state.props.showDisabilityPopUp &&  (getValueToLocalStore DISABILITY_UPDATED == "true") then disabilityPopUpView push state else emptyTextView state
            , if state.data.waitTimeInfo && state.props.currentStage == RideAccepted then waitTimeInfoPopUp push state else emptyTextView state
            , if state.props.repeatRideTimer /= "0" 
              then linearLayout
                    [ width MATCH_PARENT
                    , height MATCH_PARENT
                    , onClick push $ const StopRepeatRideTimer
                    , clickable $ not DS.null state.props.repeatRideTimerId 
                    ][]
              else emptyTextView state
            ]  <> if state.props.showEducationalCarousel then 
                    [ linearLayout
                      [ height MATCH_PARENT
                      , width MATCH_PARENT
                      , gravity CENTER
                      , onClick push $ const NoAction
                      , background Color.black9000
                      ][ PrestoAnim.animationSet [ fadeIn state.props.showEducationalCarousel] $ carouselView state push ]] 
                    else [])
        ]
  ]

getMapHeight :: HomeScreenState -> Length
getMapHeight state = V (if state.data.currentSearchResultType == QUOTES then (((screenHeight unit)/ 4)*3) 
                            else if (state.props.currentStage == RideAccepted || state.props.currentStage == ChatWithDriver) then ((screenHeight unit) - (getInfoCardPeekHeight state)) + 50
                            else (((screenHeight unit)/ 15)*10))

rideCompletedCardView ::  forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rideCompletedCardView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , accessibility if state.props.currentStage == RideRating then DISABLE_DESCENDANT else DISABLE
  ][  RideCompletedCard.view (rideCompletedCardConfig state) (push <<< RideCompletedAC)]

disabilityPopUpView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
disabilityPopUpView push state = 
  PopUpModal.view (push <<< DisabilityPopUpAC) (CommonComponentConfig.accessibilityPopUpConfig state.data.disability state.data.config.purpleRideConfig)

callSupportPopUpView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
callSupportPopUpView push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][PopUpModal.view (push <<< CallSupportAction) (callSupportConfig state)]

cancelSearchPopUp :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
cancelSearchPopUp push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , accessibility DISABLE
  ][PopUpModal.view (push <<< CancelSearchAction) (cancelAppConfig state)]

rideInfoView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rideInfoView push state = 
  let isClickable = os == "IOS"
      isWidgetVisible = (((any (_ == state.props.currentStage)) [ RideAccepted, ChatWithDriver]) && state.data.currentSearchResultType /= QUOTES && state.data.config.feature.enableChat && state.data.config.feature.enableSuggestions && (os == "ANDROID" || state.props.enableChatWidget)) || (state.props.currentStage == RideStarted && os == "IOS")
      disableChatWidget = (not (os == "IOS" || state.props.enableChatWidget)) && (((any (_ == state.props.currentStage)) [ RideAccepted, ChatWithDriver]) && state.data.currentSearchResultType /= QUOTES && state.data.config.feature.enableChat) && state.data.config.feature.enableSuggestions
  in 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , accessibility if state.data.settingSideBar.opened /= SettingSideBar.CLOSED || state.props.currentStage == ChatWithDriver || state.props.isCancelRide || state.props.isLocationTracking || state.props.callSupportPopUp || state.props.cancelSearchCallDriver || state.props.showCallPopUp || state.props.emergencyHelpModal || state.props.showRateCard || state.props.bottomSheetState == STATE_EXPANDED || state.data.waitTimeInfo then DISABLE_DESCENDANT else DISABLE
  , clickable isClickable
  , orientation VERTICAL
  ][ (if disableSuggestions state then 
        PrestoAnim.animationSet[] 
      else (if state.props.showChatNotification then 
        PrestoAnim.animationSet [translateYAnimFromTop $ messageInAnimConfig true] 
      else if state.props.isNotificationExpanded then 
        PrestoAnim.animationSet [translateYAnimFromTop $ messageOutAnimConfig true] 
      else PrestoAnim.animationSet[scaleYAnimWithDelay 5000])) $ 
     linearLayout
     [ height $ MATCH_PARENT
     , width MATCH_PARENT
     , padding $ PaddingHorizontal 8 8
     , alignParentBottom "true,-1"
     , gravity BOTTOM
     , accessibility DISABLE
     , onAnimationEnd push $ const $ NotificationAnimationEnd
     , orientation VERTICAL
     , clickable isClickable
     ][ linearLayout
       [ height $ MATCH_PARENT
       , width $ MATCH_PARENT
       , clickable isClickable
       , clipChildren false
       , accessibility DISABLE
       , margin $ MarginBottom 8
       , gravity BOTTOM
       ][ linearLayout
          [ height $ MATCH_PARENT
          , accessibility DISABLE
          , gravity BOTTOM
          , clickable isClickable
          , clipChildren false
          ]$[ otpAndWaitView push state 
          ] <> if state.props.currentStage == RideStarted then [trackRideView push state] else []
        , linearLayout[weight 1.0][]
        , linearLayout
          [ height $ MATCH_PARENT
          , clipChildren false
          , gravity BOTTOM
          , clickable isClickable
          , accessibility DISABLE
          ][ rideInfoActionView push state 
           ]
         ]
       , if disableChatWidget then -- Temp fix for chat widget
            linearLayout[height $ V 138, width MATCH_PARENT][]
         else messageNotificationView push state
       , linearLayout
        [ height $ V $ ((getInfoCardPeekHeight state) - if isWidgetVisible then 140 else 0)
        , width $ MATCH_PARENT
        , accessibility DISABLE
        ][]
     ]
  ]
  where disableSuggestions :: HomeScreenState -> Boolean
        disableSuggestions state = state.data.currentSearchResultType == QUOTES || not state.data.config.feature.enableChat || not state.data.config.feature.enableSuggestions

messagingView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
messagingView push state = 
  relativeLayout
  [ height $ MATCH_PARENT
  , width $ MATCH_PARENT
  , accessibility $ DISABLE
  ][ MessagingView.view (push <<< MessagingViewActionController) $ messagingViewConfig state ]

showLiveStatsDashboard :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
showLiveStatsDashboard push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.grey800
  , afterRender
        ( \action -> do
            initialWebViewSetUp push (getNewIDWithTag "webview") HideLiveDashboard
            pure unit
        )
        (const NoAction)
  ] [ webView
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , id (getNewIDWithTag "webview")
      , url state.data.config.dashboard.url
      ]]

driverCallPopUp :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
driverCallPopUp push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , alignParentBottom "true,-1"
    ]
    [ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT 
      , background Color.black9000
      , accessibilityHint "Call driver popup double tap to dismiss : Button"
      , accessibility ENABLE
      , disableClickFeedback true
      , onClick push (const $ CloseShowCallDialer)
      ][]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , orientation VERTICAL
        , cornerRadii $ Corners 24.0 true true false false
        , padding (Padding 20 32 20 25)
        , alignParentBottom "true,-1"
        , disableClickFeedback true
        ]
        [ textView
            $
              [ text (getString CALL_DRIVER_USING)
              , height WRAP_CONTENT
              , color Color.black700
              , textSize FontSize.a_18
              , margin (MarginBottom 4)
              ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            ( map
                ( \item ->
                    linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , orientation VERTICAL
                      ]
                      [ trackingCardCallView push state item
                      , if(item.type == ANONYMOUS_CALLER) then linearLayout
                          [ height $ V 1
                          , width MATCH_PARENT
                          , background Color.grey900
                          ]
                          []
                        else linearLayout[][]
                      ]
                )
                (driverCallPopUpData state)
            )
        ]
    ]


driverCallPopUpData :: HomeScreenState -> Array { text :: String, imageWithFallback :: String, type :: CallType, data :: String }
driverCallPopUpData state =
  [ { text: (getString ANONYMOUS_CALL)
    , imageWithFallback: fetchImage FF_ASSET "ic_anonymous_call"
    , type: ANONYMOUS_CALLER
    , data: (getString YOUR_NUMBER_WILL_NOT_BE_SHOWN_TO_THE_DRIVER_THE_CALL_WILL_BE_RECORDED_FOR_COMPLIANCE)
    }
  , { text: (getString DIRECT_CALL)
    , imageWithFallback: fetchImage FF_ASSET "ic_direct_call"
    , type: DIRECT_CALLER
    , data: (getString YOUR_NUMBER_WILL_BE_VISIBLE_TO_THE_DRIVER_USE_IF_NOT_CALLING_FROM_REGISTERED_NUMBER)
    }
  ]

trackingCardCallView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> { text :: String, imageWithFallback :: String, type :: CallType, data :: String} -> PrestoDOM (Effect Unit) w
trackingCardCallView push state item =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding (Padding 0 20 0 20)
    , accessibility ENABLE
    , accessibilityHint $ item.text <> " : " <> item.data
    , gravity CENTER_VERTICAL
    , onClick push (const (ShowCallDialer item.type))
    ]
    [
    imageView
        [ imageWithFallback item.imageWithFallback
        , height $ V 30
        , width $ V 30
        , margin (MarginRight 20)
        ]
    ,  linearLayout[
        height WRAP_CONTENT
      , weight 1.0
      , orientation VERTICAL]
    [
      linearLayout
      [
        height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER
      , orientation HORIZONTAL
      , margin (MarginBottom 2)
      ][
        textView
        $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , textSize FontSize.a_16
          , text item.text
          , gravity CENTER_VERTICAL
          , color Color.black800
          ]
        , if(item.type == ANONYMOUS_CALLER) then labelView push state else linearLayout[][]
      ]
      , textView
        $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text item.data
          , color Color.black600
          ]
    ]
    , imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right"
        , height $ V 30
        , width $ V 32
        , padding (Padding 3 3 3 3)
        ]
    ]

labelView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
labelView push state =
  linearLayout[
    height WRAP_CONTENT
  , width WRAP_CONTENT
  , cornerRadii $ Corners 8.0 true true true true
  , background Color.green900
  , margin (MarginHorizontal 10 10)
  ][
    textView $ [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , color Color.white900
    , gravity CENTER
    , padding (Padding 8 1 8 1)
    , textSize FontSize.a_13
    , text (getString RECOMMENDED)
    ]
  ]

searchLocationView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
searchLocationView push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background if state.props.currentStage == SearchLocationModel && state.props.isSearchLocation == LocateOnMap then Color.transparent else Color.grey800
  ] [ if state.props.currentStage == SearchLocationModel then (searchLocationModelView push state) else emptyTextView state
    , if state.props.currentStage == FavouriteLocationModel then (favouriteLocationModel push state) else emptyTextView state
]

shareAppPopUp :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
shareAppPopUp push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.blackLessTrans
  ][PopUpModal.view (push <<< PopUpModalShareAppAction) (shareAppConfig state )]



buttonLayoutParentView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
buttonLayoutParentView push state =
  -- PrestoAnim.animationSet (buttonLayoutAnimation state) $
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , id $ getNewIDWithTag "buttonLayout"
    , alignParentBottom "true,-1"
    , orientation VERTICAL
    ][ if (state.props.currentStage == HomeScreen && (not state.props.rideRequestFlow) && (not state.props.showlocUnserviceablePopUp)) then buttonLayout state push else emptyTextView state]

recenterButtonView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
recenterButtonView push state =
  (if os == "IOS" then PrestoAnim.animationSet [] else PrestoAnim.animationSet [ translateYAnimFromTop $ translateYAnimHomeConfig BOTTOM_TOP ])
    $ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background Color.transparent
        , visibility if state.props.rideRequestFlow && state.props.currentStage /= ConfirmingLocation then GONE else VISIBLE
        , gravity RIGHT
        , alignParentBottom "true,-1"
        , padding $ Padding 0 0 16 14
        , disableClickFeedback true
        , accessibility DISABLE
        , margin if ((state.props.showlocUnserviceablePopUp) && state.props.currentStage == HomeScreen) then (MarginBottom (360 + safeMarginBottom)) else (Margin 0 0 0 0) --else if (state.props.currentStage == ConfirmingLocation) then (Margin ((screenWidth unit) - 66) 0 0 270) else(Margin ((screenWidth unit) - 66) 0 0 120)
        ]
        [ -- linearLayout
          --   [ width WRAP_CONTENT
          --   , height WRAP_CONTENT
          --   , stroke ("1," <> Color.grey900)
          --   , cornerRadii $ Corners 24.0 true true true true
          --   ][
          imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_recenter_btn"
            , accessibility DISABLE
            , onClick
                ( \action -> do
                    _ <- push action
                    _ <- getCurrentPosition push UpdateCurrentLocation
                    _ <- pure $ logEvent state.data.logField "ny_user_recenter_btn_click"
                    pure unit
                )
                (const $ RecenterCurrentLocation)
            , height $ V 40
            , width $ V 40
            ]
        ]
-- ]

referralView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
referralView push state =
  let removeRefferal = (not state.data.config.feature.enableReferral) || (((any (_ == state.props.currentStage)) [RideAccepted, RideStarted, ChatWithDriver]) || state.props.hasTakenRide || state.props.currentSheetState == EXPANDED)
  in
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , visibility $ boolToVisibility $ not removeRefferal
    , stroke $ "1," <> if not state.props.isReferred then Color.blue900 else Color.black700
    , margin (MarginHorizontal 16 13)
    , cornerRadius 20.0
    , background Color.white900
    , accessibility DISABLE_DESCENDANT
    , gravity RIGHT
    , padding (Padding 16 12 16 12)
    , onClick push $ const $ if state.props.isReferred then ReferralFlowNoAction else ReferralFlowAction
    ][
      imageView [
         imageWithFallback $ fetchImage FF_ASSET "ny_ic_tick"
        , width $ V 20
        , height $ V 15
        , margin (Margin 0 3 5 0)
        , visibility if state.props.isReferred then VISIBLE else GONE
      ]
      , textView $ [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , color if not state.props.isReferred then Color.blue900 else Color.black700
      , accessibility DISABLE
      , text if not state.props.isReferred then (getString HAVE_REFERRAL_CODE) else (getString REFERRAL_CODE_APPLIED)
      ] <> FontStyle.tags TypoGraphy
    ]

nammaSafetyView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
nammaSafetyView push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , visibility $ boolToVisibility $ (any (_ == state.props.currentStage) ) [RideAccepted, RideStarted, ChatWithDriver]
  , stroke $ "1," <> Color.grey900
  , margin $ MarginHorizontal 16 16
  , cornerRadius 20.0
  , background Color.white900
  , accessibility ENABLE
  , accessibilityHint $ "Safety + : Button : Select to view S O S Options"
  , gravity CENTER_VERTICAL
  , clickable true
  , padding (Padding 12 8 12 8)
  ][ imageView 
    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_namma_safety"
    , width $ V 24
    , height $ V 24
    , margin $ MarginRight 4
    ]
  , textView $ 
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , color Color.blue900
    , accessibility DISABLE
    , text $ getString NAMMA_SAFETY
    ] <> FontStyle.body1 TypoGraphy
  ]

sosView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
sosView push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , visibility $ boolToVisibility $ (any (_ == state.props.currentStage) ) [RideAccepted, RideStarted, ChatWithDriver] 
  , margin (MarginHorizontal 16 16)
  , cornerRadius if os == "IOS" then 20.0 else 32.0
  , clickable true
  , onClick push $ const OpenEmergencyHelp
  , rippleColor Color.rippleShade
  ][ imageView 
    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_sos"
    , width $ V 50
    , height $ V 50
    , accessibilityHint $ "S O S : Button : Select to view S O S options"
    , accessibility ENABLE
    ]
  ]

liveStatsDashboardView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
liveStatsDashboardView push state =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , visibility GONE
    , stroke $ "1," <> Color.blue900
    , margin (MarginHorizontal 16 13)
    , accessibility DISABLE_DESCENDANT
    , cornerRadius 20.0
    , background Color.white900
    , gravity RIGHT
    , padding (Padding 16 12 16 12)
    , onClick push $ const $ LiveDashboardAction
    ][
      imageView [
        imageWithFallback $ fetchImage FF_ASSET "ic_graph_blue"
        , width $ V 20
        , height $ V 15
        , margin (Margin 0 0 5 0)
      ]
      , textView $ [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , color Color.blue900
      , accessibility DISABLE
      , text (getString CHECK_OUT_LIVE_STATS)
      ] <> FontStyle.tags TypoGraphy
    ]

sourceUnserviceableView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
sourceUnserviceableView push state =
  PrestoAnim.animationSet [ fadeIn true ]
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , cornerRadii $ Corners 24.0 true true false false
        , alignParentBottom "true,-1"
        , gravity BOTTOM
        ][ErrorModal.view (push <<< SourceUnserviceableActionController) (isMockLocationConfig state)]

rateCardView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rateCardView push state =
  PrestoAnim.animationSet [ fadeIn true ]
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        ]
        [ RateCard.view (push <<< RateCardAction) (rateCardConfig state) ]

requestInfoCardView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
requestInfoCardView push state =
  PrestoAnim.animationSet [ fadeIn true ]
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        ]
        [ RequestInfoCard.view (push <<< RequestInfoCardAction) (requestInfoCardConfig FunctionCall) ]

buttonLayout :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
buttonLayout state push =
  PrestoAnim.animationSet (buttonLayoutAnimation state)
  $ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , alignParentBottom "true,-1"
    , orientation VERTICAL
    , accessibility if state.props.currentStage == HomeScreen && (not (state.data.settingSideBar.opened /= SettingSideBar.CLOSED )) then DISABLE else DISABLE_DESCENDANT
    ]
    [ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ]
      [ referralView push state
      , recenterButtonView push state
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , background if ((null state.data.savedLocations  && null state.data.recentSearchs.predictionArray ) || state.props.isSearchLocation == LocateOnMap) then Color.transparent else Color.white900
      , gradient if os == "IOS" then (Linear 90.0 ["#FFFFFF" , "#FFFFFF" , "#FFFFFF", Color.transparent]) else (Linear 0.0 ["#FFFFFF" , "#FFFFFF" , "#FFFFFF", Color.transparent])
      , orientation VERTICAL
      , padding $ if state.data.config.feature.enableZooTicketBookingFlow then PaddingTop 0 else PaddingTop 16
      ]
      [ if state.data.config.feature.enableZooTicketBookingFlow
        then zooTicketBookingBanner state push 
        else linearLayout[visibility GONE][]
      , PrimaryButton.view (push <<< PrimaryButtonActionController) (whereToButtonConfig state)
      , if state.props.isSearchLocation == LocateOnMap
        then emptyLayout state 
        else recentSearchesAndFavourites state push (null state.data.savedLocations) (null state.data.recentSearchs.predictionArray)
      ]
    ]

recentSearchesAndFavourites :: forall w. HomeScreenState -> (Action -> Effect Unit) -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w
recentSearchesAndFavourites state push hideSavedLocsView hideRecentSearches =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $ Padding 16 0 16 (16+safeMarginBottom)
  , cornerRadii $ Corners (4.0) true true false false
  ]([ if (not hideSavedLocsView) then savedLocationsView state push else linearLayout[visibility GONE][]
    , shimmerView state
    , suggestionsView push state
    , emptySuggestionsBanner state push
    ]
    )

updateDisabilityBanner :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
updateDisabilityBanner state push = 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 16 10 16 10
    ][  Banner.view (push <<< DisabilityBannerAC) (disabilityBannerConfig state)]

zooTicketBookingBanner :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
zooTicketBookingBanner state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 16 0 16 16
    ][  Banner.view (push <<< TicketBookingFlowBannerAC) (ticketBannerConfig state)]
    
emptySuggestionsBanner :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
emptySuggestionsBanner state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 12.0
    , margin $ Margin 16 10 16 10
    , background Color.white900
    , gravity CENTER_VERTICAL
    , visibility $ boolToVisibility $ (not (suggestionViewVisibility state)) && not (state.props.showShimmer && null state.data.tripSuggestions) && state.data.config.homeScreen.bannerViewVisibility
    ][ linearLayout
        [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity CENTER_VERTICAL
          , padding $ PaddingVertical 5 5
        ][ imageView
            [ height $ V 82
            , width $ V 130
            , margin $ MarginRight 5
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_empty_suggestions"
            ]
          ]
      , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        , padding $ PaddingLeft 20
        , orientation VERTICAL
        , layoutGravity "center_vertical"
        ][ textView $
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity LEFT
            , text $ getString $ WELCOME_TEXT "WELCOME_TEXT"
            , color Color.black800
            , padding $ PaddingBottom 2
            ] <> (FontStyle.body2 LanguageStyle)
         , linearLayout
            [ height WRAP_CONTENT
            , width $ V $ (screenWidth unit)/2
            , gravity CENTER_VERTICAL
            ][
              textView $
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , gravity LEFT
              , text $ getString YOUR_SUGGESTED_DESTINATIONS_AND_RECENT_RIDES_WILL_APPEAR_HERE
              , color Color.black700
              , padding $ PaddingBottom 2
              ] <> (FontStyle.body3 LanguageStyle)
            ]
          ]
     ]
    
    

genderBannerView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
genderBannerView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginVertical 10 10
    , visibility if state.data.config.showGenderBanner then VISIBLE else GONE
    ][
        genderBanner push state
    ]


savedLocationsView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
savedLocationsView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , clickable state.props.isSrcServiceable
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , margin $ MarginTop 16
        , alpha if state.props.isSrcServiceable then 1.0 else 0.4
        , onClick push (const NoAction)
        ]
        [ LocationTagBar.view (push <<< SavedAddressClicked) { savedLocations: state.data.savedLocations } ]
    ]

recentSearchesView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
recentSearchesView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 16
    , visibility if null state.data.destinationSuggestions then GONE else VISIBLE
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , cornerRadius 8.0
        , stroke $ "1," <> Color.grey900
        , orientation VERTICAL
        ]
        ( mapWithIndex
            ( \index item ->
                linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  , visibility if (state.props.isBanner && index >0) then GONE else VISIBLE
                  ]
                  [ LocationListItem.view (push <<< PredictionClickedAction) item false
                  , linearLayout
                      [ height $ V 1
                      , width MATCH_PARENT
                      , background Color.lightGreyShade
                      , visibility if (index == (length state.data.destinationSuggestions) - 1) || (state.props.isBanner) then GONE else VISIBLE
                      ]
                      []
                  ]
            )
            (take 2 state.data.destinationSuggestions)
        )
    ]

buttonLayoutAnimation :: HomeScreenState -> Array PrestoAnim.Animation
buttonLayoutAnimation state = [fadeIn state.props.showShimmer]
------------- settingSideBarView ------------
settingSideBarView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
settingSideBarView push state =
  linearLayout
    [ weight 1.0
    , height MATCH_PARENT
    , width MATCH_PARENT
    , accessibility if state.data.settingSideBar.opened /= SettingSideBar.CLOSED && not (state.props.isPopUp /= NoPopUp) then DISABLE else DISABLE_DESCENDANT
    ]
    [ SettingSideBar.view (push <<< SettingSideBarActionController) (state.data.settingSideBar{appConfig = state.data.config}) ]

homeScreenTopIconView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
homeScreenTopIconView push state =
  homeScreenAnimation TOP_BOTTOM
    $
     -- 1000 (-100) 0 0 true $ PrestoAnim.Bezier 0.37 0.0 0.63 1.0] $
     linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , accessibility if (any (_ == state.props.currentStage) ) [RideRating, RideCompleted] then DISABLE_DESCENDANT else DISABLE
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , background Color.white900
            , orientation HORIZONTAL
            , gravity LEFT
            , visibility if state.data.config.terminateBtnConfig.visibility then VISIBLE else GONE
            ]
            [ linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , margin $ MarginLeft 16
                , padding $ Padding 6 6 6 6
                , gravity CENTER_VERTICAL
                , onClick push (const TerminateApp)
                ]
                [ imageView
                    [ imageWithFallback state.data.config.terminateBtnConfig.imageUrl
                    , height $ V 20
                    , width $ V 20
                    , margin $ MarginRight 10
                    ]
                , textView
                    $ [ width WRAP_CONTENT
                      , height WRAP_CONTENT
                      , gravity CENTER_VERTICAL
                      , text state.data.config.terminateBtnConfig.title
                      , color Color.black900
                      ]
                    <> FontStyle.tags TypoGraphy
                ]
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , cornerRadius 8.0
            , background Color.white900
            , visibility if state.props.rideRequestFlow then GONE else VISIBLE
            , stroke $ "1," <> Color.grey900
            , gravity CENTER_VERTICAL
            , margin (Margin 16 26 16 0)
            , padding (Padding 0 16 16 16)
            ]
            [ linearLayout
                [ width WRAP_CONTENT -- $ V 54
                , height MATCH_PARENT
                , gravity CENTER
                , disableClickFeedback true
                , clickable if state.props.currentStage == SearchLocationModel then false else true
                , visibility if (any (_ == state.props.currentStage) [RideCompleted]) then GONE else VISIBLE
                , onClick push $ const OpenSettings
                , rippleColor Color.rippleShade
                ]
                [ imageView
                    [ imageWithFallback $ fetchImage FF_ASSET $ if state.data.config.dashboard.enable && (checkVersion "LazyCheck") then "ic_menu_notify" else "ny_ic_hamburger"
                    , height $ V 24
                    , width $ V 24
                    , margin (Margin 16 16 16 16)
                    , accessibility if state.props.emergencyHelpModal || state.props.currentStage == ChatWithDriver || state.props.isCancelRide || state.props.isLocationTracking || state.props.callSupportPopUp || state.props.cancelSearchCallDriver then DISABLE else ENABLE
                    , accessibilityHint "Navigation : Button"
                    ]
                ]
            , linearLayout
                [ height $ V 42
                , width $ V 1
                , background Color.grey900
                ]
                []
            , imageView
                [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_source_dot"
                , height $ V 16
                , width $ V 16
                , margin (Margin 5 5 5 5)
                , accessibility DISABLE
                , onClick push $ if state.props.isSrcServiceable then (const $ OpenSearchLocation) else (const $ NoAction)
                , gravity BOTTOM
                ]
            , linearLayout
                [ orientation VERTICAL
                , width MATCH_PARENT
                , height WRAP_CONTENT
                , disableClickFeedback true
                , onClick push $ if state.props.isSrcServiceable then (const $ OpenSearchLocation) else (const $ NoAction)
                , accessibility if any (_ == state.props.currentStage) [RideRating , RideCompleted] then DISABLE else ENABLE
                , accessibilityHint "Pickup Location is Current Location"
                , accessibility ENABLE
                ]
                [ textView
                    $ [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , text (getString PICK_UP_LOCATION)
                      , color Color.black800
                      , gravity LEFT
                      , lineHeight "16"
                      ]
                    <> FontStyle.body3 LanguageStyle
                , textView
                    $ [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , text if state.props.isSrcServiceable then
                              (if state.data.source /= "" then state.data.source else (getString CURRENT_LOCATION))
                             else
                               getString APP_NOT_SERVICEABLE
                      , maxLines 1
                      , ellipsize true
                      , color if state.props.isSrcServiceable then Color.black800 else Color.greyDark
                      , gravity LEFT
                      , lineHeight "23"
                      ]
                    <> FontStyle.body7 LanguageStyle
                ]
            ]
        ]
  where
  homeScreenAnimation direction = PrestoAnim.animationSet [ translateYAnimFromTop $ translateYAnimHomeConfig direction ]

checkVersion :: String -> Boolean
checkVersion str = getValueToLocalStore LIVE_DASHBOARD /= "LIVE_DASHBOARD_SELECTED" && not (isPreviousVersion (getValueToLocalStore VERSION_NAME) (if os == "IOS" then "1.2.5" else "1.2.1"))

------------------------------- rideRequestFlowView --------------------------
rideRequestFlowView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rideRequestFlowView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadii $ Corners 24.0 true true false false
    , visibility $ boolToVisibility $ isStageInList state.props.currentStage [ SettingPrice, ConfirmingLocation, RideCompleted, FindingEstimate, ConfirmingRide, FindingQuotes, TryAgain, RideRating, ReAllocated] 
    , alignParentBottom "true,-1"
    ]
    [ -- TODO Add Animations
      -- PrestoAnim.animationSet
      -- [ translateYAnim (300) 0 state.props.rideRequestFlow
      -- , translateYAnim 0 (300) (not state.props.rideRequestFlow)
      -- ] $
      relativeLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , cornerRadii $ Corners 24.0 true true false false
        , background Color.transparent
        , accessibility DISABLE
        ]
        [ PrestoAnim.animationSet [fadeIn true] $ getViewBasedOnStage push state
        , if isStageInList state.props.currentStage [ FindingEstimate, ConfirmingRide, TryAgain, FindingQuotes, ReAllocated] then
            (loaderView push state)
          else
            emptyTextView state
        ]
    ]
    where
      getViewBasedOnStage :: (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
      getViewBasedOnStage push state = 
        if state.props.currentStage == SettingPrice then
          if isCityInList state.props.city [Bangalore] && state.props.isRepeatRide then
            estimatedFareView push state
          else
            ChooseYourRide.view (push <<< ChooseYourRideAction) (chooseYourRideConfig state)
        else if state.props.currentStage == ConfirmingLocation then
          confirmPickUpLocationView push state
        else
          emptyTextView state

      isCityInList :: City -> Array City -> Boolean
      isCityInList city = any (_ == city)

      isStageInList :: Stage -> Array Stage -> Boolean
      isStageInList stage = any (_ == stage)


-------------- rideRatingCardView -------------
rideRatingCardView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rideRatingCardView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity BOTTOM
    , background Color.transparent
    ]
    [ RatingCard.view (push <<< RatingCardAC) $ ratingCardViewState state
    ]

commonTextView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> String -> String -> (forall properties. (Array (Prop properties))) -> Int -> PrestoDOM (Effect Unit) w
commonTextView state push text' color' fontStyle marginTop =
  textView $
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , accessibilityHint text'
  , accessibility ENABLE
  , text text'
  , color color'
  , gravity CENTER
  , margin $ MarginTop marginTop
  ] <> fontStyle

----------- topLeftIconView -------------
topLeftIconView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
topLeftIconView state push =
  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , visibility if state.data.config.showHamMenu then VISIBLE else GONE
      , margin $ Margin 16 (if os == "IOS" then safeMarginTop else 20) 0 0
      , accessibility if state.data.settingSideBar.opened /= SettingSideBar.CLOSED || state.props.currentStage == ChatWithDriver || state.props.isCancelRide || state.props.isLocationTracking || state.props.callSupportPopUp || state.props.cancelSearchCallDriver || state.props.showCallPopUp || state.props.emergencyHelpModal || state.props.showRateCard || state.data.waitTimeInfo then DISABLE_DESCENDANT else DISABLE
      ][
        linearLayout
          [ height $ V 48
          , width $ V 48
          , stroke ("1," <> Color.grey900)
          , background Color.white900
          , gravity CENTER
          , cornerRadius 24.0
          , visibility if (any (_ == state.props.currentStage) [ FindingEstimate, ConfirmingRide, FindingQuotes, TryAgain , RideCompleted, RideRating, ReAllocated]) then GONE else VISIBLE
          , clickable true
          , onClick push $ if (any (_ == state.props.currentStage) [ SettingPrice, ConfirmingLocation, PricingTutorial, DistanceOutsideLimits ]) then const BackPressed else const OpenSettings
          , accessibilityHint if (any (_ == state.props.currentStage) [ SettingPrice, ConfirmingLocation, PricingTutorial, DistanceOutsideLimits ]) then "Back : Button" else "Menu : Button"
          , accessibility ENABLE
          , rippleColor Color.rippleShade
          ]
          [ imageView
              [ imageWithFallback if (any (_ == state.props.currentStage) [ SettingPrice, ConfirmingLocation, PricingTutorial, DistanceOutsideLimits ]) then fetchImage FF_COMMON_ASSET "ny_ic_chevron_left" else if state.data.config.dashboard.enable && (checkVersion "LazyCheck") then fetchImage FF_ASSET "ic_menu_notify" else fetchImage FF_ASSET "ny_ic_hamburger"
              , height $ V 25
              , accessibility DISABLE
              , clickable true
              , onClick push $ if (any (_ == state.props.currentStage) [ SettingPrice, ConfirmingLocation, PricingTutorial, DistanceOutsideLimits ]) then const BackPressed else const OpenSettings
              , width $ V 25
              ]
          ]
        , linearLayout
          [ height WRAP_CONTENT
          , weight 1.0
          ][]
        , referralView push state
        , sosView push state
        , if (not state.data.config.dashboard.enable) || (isPreviousVersion (getValueToLocalStore VERSION_NAME) (if os == "IOS" then "1.2.5" else "1.2.1")) then emptyTextView state else liveStatsDashboardView push state
      ]

----------- estimatedFareView ----------------
estimatedFareView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
estimatedFareView push state =
  linearLayout
  [ orientation VERTICAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.blue800
  , clickable true
  , visibility if (state.props.currentStage == SettingPrice) then VISIBLE else GONE
  , stroke ("1," <> Color.grey900)
  , gravity CENTER
  , cornerRadii $ Corners 24.0 true true false false
  , afterRender
        ( \action -> do
            let fareEstimate = if state.data.rateCard.additionalFare == 0 then "₹" <> (show state.data.suggestedAmount) else  "₹" <> (show state.data.suggestedAmount) <> "-" <> "₹" <> (show $ (state.data.suggestedAmount + state.data.rateCard.additionalFare))
            _ <- pure $  setValueToLocalStore FARE_ESTIMATE_DATA fareEstimate
            pure unit
        )
        (const NoAction)
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER
      , padding (PaddingVertical 4 4)
      , visibility if state.props.zoneType.priorityTag == METRO then VISIBLE else GONE
      ] [ imageView
          [ width (V 15)
          , height (V 15)
          , margin (MarginRight 6)
          , imageWithFallback $ fetchImage FF_ASSET "ny_ic_metro_white"
          ]
        , textView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , textSize FontSize.a_14
          , text (getString METRO_RIDE)
          , color Color.white900
          ]
        ]
    , linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.white900
      , clickable true
      , accessibility if state.props.showRateCard then DISABLE_DESCENDANT else DISABLE
      , visibility if (state.props.currentStage == SettingPrice) then VISIBLE else GONE
      , padding (Padding 16 7 16 24)
      , stroke ("1," <> Color.grey900)
      , gravity CENTER
      , cornerRadii $ Corners 24.0 true true false false
      ][ estimateHeaderView push state
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , gravity CENTER
          , cornerRadius 8.0
          , margin $ MarginTop 16
          ][ rideDetailsViewV2 push state]
        , sourceDestinationDetailsView push state
        , requestRideButtonView push state
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , margin $ MarginTop 24
            , visibility if state.props.isRepeatRide && not DS.null state.props.repeatRideTimerId then VISIBLE else GONE
            ][ textView $
                [ textFromHtml $ "<u>" <> (getString TAP_HERE_TO_STOP_AUTO_REQUESTING) <> "</u>" 
                , color Color.black700
                ] <> FontStyle.body1 LanguageStyle
            ]
      ]
  ]

estimateHeaderView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
estimateHeaderView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER_HORIZONTAL
    , accessibility ENABLE
    , accessibilityHint $ "PickUp Location Is : " 
                         <> state.data.source 
                         <> " . And Destination Location Is : "  
                         <> state.data.destination
    ]
    [ textView $
        [ text (getString CONFIRM_YOUR_RIDE)
        , color Color.black800
        , gravity CENTER_HORIZONTAL
        , height WRAP_CONTENT
        , width MATCH_PARENT
        ] 
        <> FontStyle.h1 TypoGraphy
    , estimatedTimeDistanceView push state
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , margin $ MarginTop 12
        , background Color.grey900
        ][]
    ]

estimatedTimeDistanceView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
estimatedTimeDistanceView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , margin $ MarginTop 4
    ]
    [ createTextView state.data.rideDistance
    , linearLayout
        [ height $ V 4
        , width $ V 4
        , cornerRadius 2.5
        , background Color.black600
        , margin (Margin 6 2 6 0)
        ]
        []
    , createTextView state.data.rideDuration
    ]
  where
    createTextView :: String -> PrestoDOM (Effect Unit) w
    createTextView textContent =
      textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text textContent
        , color Color.black650
        ]
        <> FontStyle.paragraphText TypoGraphy

rideDetailsViewV2 :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rideDetailsViewV2 push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    ][ ChooseVehicle.view (push <<< ChooseSingleVehicleAction) (chooseVehicleConfig state)]

rideDetailsView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rideDetailsView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding $ PaddingTop 12
    , margin $ MarginHorizontal 16 16
    ][ linearLayout
        [ height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity LEFT
        , weight 1.0
        ][ imageView  
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_auto_quote_list"
            , width $ V 55
            , height $ V 40
            ]
         , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , accessibility DISABLE
            , gravity CENTER
            , orientation VERTICAL
            ][ textView $
                [ text state.data.rideDistance
                , width MATCH_PARENT
                , accessibilityHint $ "Estimated Ride Distance And Ride Duration Is " <> (fromMaybe "0" (head (DS.split (DS.Pattern " ") state.data.rideDistance))) <> (if any (_ == "km") (DS.split (DS.Pattern " ") state.data.rideDistance) then "Kilo Meters" else "Meters") <> " And " <> state.data.rideDuration
                , color Color.black800
                , accessibility ENABLE
                , height WRAP_CONTENT
                ] <> FontStyle.body4 LanguageStyle
              , textView $
                [ text state.data.rideDuration
                , accessibility DISABLE
                , width MATCH_PARENT
                , color Color.black700
                , height WRAP_CONTENT
                ] <> FontStyle.body3 LanguageStyle
            ]
        ]
    , linearLayout
        [ width WRAP_CONTENT
        , height MATCH_PARENT
        , gravity CENTER_VERTICAL
        ][ let fareEstimate = if state.data.rateCard.additionalFare == 0 then state.data.config.currency <> (show state.data.suggestedAmount) else  state.data.config.currency <> (show state.data.suggestedAmount) <> "-" <> state.data.config.currency <> (show $ (state.data.suggestedAmount + state.data.rateCard.additionalFare))
           in
            textView $
            [ text $ fareEstimate
            , width WRAP_CONTENT
            , color Color.black900
            , height WRAP_CONTENT
            , fontStyle $ FontStyle.bold LanguageStyle
            , accessibilityHint $ "Estimated Fare Is " <> fareEstimate
            , onClick (\action -> if state.data.config.searchLocationConfig.enableRateCard then push action else pure unit ) $ const ShowRateCard
            ] <> FontStyle.body7 LanguageStyle
         , imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_info_blue"
            , width $ V 40
            , height $ V 40
            , accessibility DISABLE
            , visibility if state.data.config.searchLocationConfig.enableRateCard then VISIBLE else GONE
            , onClick (\action -> if state.data.config.searchLocationConfig.enableRateCard then push action else pure unit ) $ const ShowRateCard
            ]
        ]
    ]

sourceDestinationDetailsView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
sourceDestinationDetailsView push state = 
  linearLayout
    [ orientation HORIZONTAL
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , clickable true
    , visibility if (state.props.currentStage == SettingPrice) && state.props.isRepeatRide then VISIBLE else GONE
    , margin (MarginTop 16)
    , padding (Padding 12 12 12 12)
    , stroke ("1," <> Color.grey900)
    , gravity CENTER_VERTICAL
    , cornerRadii $ Corners 10.0 true true true true
    ][ 
      linearLayout
        [ weight 1.0
        , height WRAP_CONTENT
        , accessibilityHint $ "PickUp Location Is : " <> state.data.source <> " . And Destination Location Is : "  <> state.data.destination
        ][SourceToDestination.view (push <<< SourceToDestinationActionController) (sourceToDestinationConfig state)]
      , imageView
          [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_edit"
          , height $ V 40
          , width $ V 40
          , accessibilityHint "Go back to edit source or destination : Button"
          , onClick push $ const BackPressed
          ]
    ]

requestRideButtonView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
requestRideButtonView push state =
  let 
    animationDuration = state.data.config.suggestedTripsAndLocationConfig.repeatRideTime * 1000 - 100
    isRepeatRideTimerNonZero = state.props.repeatRideTimer /= "0"
  in
  relativeLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    ] 
    [ PrimaryButton.view (push <<< PrimaryButtonActionController) (confirmAndBookButtonConfig state)
    , PrestoAnim.animationSet
        [ translateOutXBackwardAnimY animConfig
            { duration = animationDuration
            , toX = (screenWidth unit)
            , fromY = 0
            , ifAnim = isRepeatRideTimerNonZero
            }
        ]  
        $ linearLayout
            [ height $ V 50
            , width MATCH_PARENT
            , alpha 0.5
            , background Color.white900
            , visibility $ boolToVisibility (not DS.null state.props.repeatRideTimerId)
            , margin $ MarginTop 16
            ][]
    ]
    
bookingPreferencesView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
bookingPreferencesView push state = 
 linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , visibility $ boolToVisibility $ state.data.config.estimateAndQuoteConfig.enableBookingPreference  && not state.props.isRepeatRide && state.props.city /= Kochi 
  ][ linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , margin $ Margin 16 12 16 14
      , background Color.grey900
      ][]
  , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER_HORIZONTAL
          , onClick push $ const PreferencesDropDown
          , accessibility DISABLE
          , margin (MarginBottom 8)
          ][ textView $
              [ height $ V 24
              , width WRAP_CONTENT
              , color Color.darkCharcoal
              , text $ getString BOOKING_PREFERENCE
              , accessibility DISABLE
              ] <> FontStyle.body1 LanguageStyle
           , imageView
              [ width $ V 10
              , height $ V 10
              , margin (Margin 9 5 0 0)
              , accessibility DISABLE
              , imageWithFallback $ if state.data.showPreferences
                                      then fetchImage FF_COMMON_ASSET "ny_ic_chevron_up"
                                      else fetchImage FF_ASSET "ny_ic_chevron_down"
              ]
          ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , margin $ MarginLeft 20
            , orientation VERTICAL
            ][ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , visibility if state.data.showPreferences then VISIBLE else GONE
              ][ showMenuButtonView push (getString AUTO_ASSIGN_DRIVER) (fetchImage FF_ASSET "ny_ic_faster_lightning") true state,
                showMenuButtonView push (getString CHOOSE_BETWEEN_MULTIPLE_DRIVERS) (fetchImage FF_ASSET "ny_ic_info") false state]
            ]
      ]
  ]

showMenuButtonView :: forall w. (Action -> Effect Unit) -> String -> String -> Boolean -> HomeScreenState -> PrestoDOM (Effect Unit) w
showMenuButtonView push menuText menuImage autoAssign state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , gravity CENTER
  , margin $ MarginVertical 10 5
  ][ linearLayout
     [ height $ V 30
     , width $ V 30
     , gravity CENTER
     , onClick push $ const $ CheckBoxClick autoAssign
     ][ linearLayout
        [ height $ V 20
        , width $ V 20
        , stroke if (state.props.flowWithoutOffers && autoAssign || not state.props.flowWithoutOffers && not autoAssign) then ("2," <> state.data.config.primaryBackground) else ("2," <> Color.black600)
        , cornerRadius 10.0
        , gravity CENTER
        ][  linearLayout
            [ width $ V 10
            , height $ V 10
            , cornerRadius 5.0
            , background $ state.data.config.primaryBackground
            , visibility if (state.props.flowWithoutOffers && autoAssign || not state.props.flowWithoutOffers && not autoAssign) then VISIBLE else GONE
            ][]
          ]
     ]
    , textView $
      [ text menuText
      , width MATCH_PARENT
      , gravity CENTER
      , color state.data.config.estimateAndQuoteConfig.textColor
      , height WRAP_CONTENT
      , margin $ MarginHorizontal 5 10
      , onClick push (const $ CheckBoxClick autoAssign)
      ] <> FontStyle.paragraphText LanguageStyle
    , if autoAssign then
        linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , background state.data.config.autoSelectBackground
        , cornerRadius 14.0
        , gravity CENTER
        , padding $ Padding 10 6 10 6
        ][  imageView
            [ height $ V 12
            , width $ V 8
            , margin $ MarginRight 4
            , imageWithFallback menuImage
            ]
          , textView $
            [ text $ getString FASTER
            , width WRAP_CONTENT
            , gravity CENTER
            , color Color.white900
            , height WRAP_CONTENT
            ] <> FontStyle.body15 LanguageStyle
          ]
        else
          imageView
          [ height $ V 16
          , width $ V 16
          , imageWithFallback menuImage
          , margin $ (MarginHorizontal 5 5)
          , onClick push (const $ OnIconClick autoAssign)
          ]
  ]

estimatedTimeAndDistanceView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
estimatedTimeAndDistanceView push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , accessibility DISABLE
  , gravity CENTER
  , margin $ MarginTop 4
  ][ textView $
      [ text state.data.rideDistance
      , width MATCH_PARENT
      , gravity CENTER
      , accessibilityHint $ "Estimated Ride Distance And Ride Duration Is " <> (fromMaybe "0" (head (DS.split (DS.Pattern " ") state.data.rideDistance))) <> (if any (_ == "km") (DS.split (DS.Pattern " ") state.data.rideDistance) then "Kilo Meters" else "Meters") <> " And " <> state.data.rideDuration
      , color Color.black650
      , accessibility ENABLE
      , height WRAP_CONTENT
      ] <> FontStyle.paragraphText LanguageStyle
    , linearLayout
      [height $ V 4
      , width $ V 4
      , cornerRadius 2.5
      , background Color.black600
      , margin (Margin 6 2 6 0)
      ][]
    , textView $
      [ text state.data.rideDuration
      , accessibility DISABLE
      , width MATCH_PARENT
      , gravity CENTER
      , color Color.black650
      , height WRAP_CONTENT
      ] <> FontStyle.paragraphText LanguageStyle
  ]

emergencyHelpModal :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
emergencyHelpModal push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ EmergencyHelp.view (push <<< EmergencyHelpModalAC) $ emergencyHelpModelViewState state ]

locationTrackingPopUp :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
locationTrackingPopUp push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.black9000
    , alignParentBottom "true,-1"
    , onClick push (const $ CloseLocationTracking)
    , disableClickFeedback true
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , orientation VERTICAL
        , cornerRadii $ Corners 24.0 true true false false
        , padding (Padding 20 32 20 25)
        , onClick push (const $ TrackLiveLocationAction)
        , alignParentBottom "true,-1"
        , disableClickFeedback true
        ]
        [ textView
            $
              [ text (getString TRACK_LIVE_LOCATION_USING)
              , height WRAP_CONTENT
              , color Color.black700
              ]
            <> FontStyle.subHeading2 TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , padding (PaddingTop 32)
            ]
            ( mapWithIndex
                ( \idx item ->
                    linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , orientation VERTICAL
                      ]
                      [ trackingCardView push state item
                      , linearLayout
                          [ height $ V 1
                          , width MATCH_PARENT
                          , background Color.grey900
                          , visibility if (state.props.currentStage == RideAccepted && item.type == "GOOGLE_MAP") || (idx == (length (locationTrackingData "lazyCheck")) - 1) then GONE else VISIBLE
                          ]
                          []
                      ]
                )
                (locationTrackingData "LazyCheck")
            )
        ]
    ]

estimateChangedPopUp :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
estimateChangedPopUp push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , accessibility if state.props.isEstimateChanged then DISABLE else DISABLE_DESCENDANT
    , gravity BOTTOM
    ]
    [ PopUpModal.view (push <<< EstimateChangedPopUpController) (estimateChangedPopupConfig state) ]

trackingCardView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> { text :: String, imageWithFallback :: String, type :: String } -> PrestoDOM (Effect Unit) w
trackingCardView push state item =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding (Padding 0 20 0 20)
    , onClick push (const (StartLocationTracking item.type))
    , visibility if (state.props.currentStage == RideAccepted && item.type == "GOOGLE_MAP") then GONE else VISIBLE
    ]
    [ imageView
        [ imageWithFallback item.imageWithFallback
        , height $ V 25
        , width $ V 25
        , margin (MarginRight 20)
        ]
    , textView
        $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text item.text
          , gravity CENTER_VERTICAL
          , color Color.black800
          ]
        <> if state.props.isInApp && item.type == "IN_APP" then FontStyle.subHeading1 TypoGraphy else FontStyle.subHeading2 TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        ]
        []
    , imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right"
        , height $ V 20
        , width $ V 22
        , padding (Padding 3 3 3 3)
        ]
    ]

locationTrackingData :: String -> Array { text :: String, imageWithFallback :: String, type :: String }
locationTrackingData lazyCheck =
  [ { text: (getString GOOGLE_MAP_)
    , imageWithFallback: fetchImage FF_ASSET "ny_ic_track_google_map"
    , type: "GOOGLE_MAP"
    }
  , { text: (getString IN_APP_TRACKING)
    , imageWithFallback: fetchImage FF_ASSET "ny_ic_track_in_app"
    , type: "IN_APP"
    }
  ]

----------- confirmPickUpLocationView -------------
confirmPickUpLocationView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
confirmPickUpLocationView push state =
  let zonePadding = if os == "IOS" then 0 else (ceil (toNumber (screenWidth unit))/8)
  in
  linearLayout
    [ orientation VERTICAL
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , disableClickFeedback true
    , background Color.transparent
    , accessibility DISABLE
    , visibility if state.props.currentStage == ConfirmingLocation then VISIBLE else GONE
    , padding $ PaddingTop 16
    , cornerRadii $ Corners 24.0 true true false false
    , gravity CENTER
    ]
    [ recenterButtonView push state
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , stroke $ "1," <> Color.grey900
        , cornerRadii $ Corners 24.0 true true false false
        , background Color.blue800
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , gravity CENTER
            , padding (Padding zonePadding 4 zonePadding 4)
            , cornerRadii $ Corners 24.0 true true false false
            , visibility if state.props.confirmLocationCategory /= "" then VISIBLE else GONE
            ] [ imageView
                [ width (V 20)
                , height (V 20)
                , margin (MarginRight 6)
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_zone_walk"
                ]
              , textView
                [ width if os == "IOS" && state.props.confirmLocationCategory == "SureBlockedAreaForAutos" then (V 230) else WRAP_CONTENT
                , height WRAP_CONTENT
                , gravity CENTER
                , textSize FontSize.a_14
                , text if state.props.confirmLocationCategory == "SureBlockedAreaForAutos" then
                        (getString GO_TO_SELECTED_PICKUP_SPOT_AS_AUTOS_ARE_RESTRICTED)
                       else
                        (getString GO_TO_SELECTED_PICKUP_SPOT)
                , color Color.white900
                ]
              ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , padding $ Padding 16 16 16 24
            , cornerRadii $ Corners 24.0 true true false false
            , background Color.white900
            , accessibility DISABLE
            ] [ textView $
                [ text (getString CONFIRM_PICKUP_LOCATION)
                , color Color.black800
                , accessibility DISABLE
                , gravity CENTER_HORIZONTAL
                , height WRAP_CONTENT
                , width MATCH_PARENT
                ] <> FontStyle.h1 TypoGraphy
              , currentLocationView push state
              , nearByPickUpPointsView state push
              , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfirmPickupConfig state)
             ]
        ]
    ]

----------- loaderView -------------
loaderView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
loaderView push state =
  let loaderViewTitle = case state.props.currentStage of
                          ConfirmingRide -> getString CONFIRMING_THE_RIDE_FOR_YOU
                          FindingEstimate -> getString GETTING_ESTIMATES_FOR_YOU
                          TryAgain -> getString LET_TRY_THAT_AGAIN
                          ReAllocated -> getString LOOKING_FOR_ANOTHER_RIDE
                          _ -> getString GETTING_ESTIMATES_FOR_YOU
  in  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , padding (Padding 0 40 0 24)
      , background Color.white900
      , cornerRadii $ Corners 24.0 true true false false
      , stroke ("1," <> Color.grey900)
      , clickable true
      , gravity CENTER_HORIZONTAL
      , visibility if (any (_ == state.props.currentStage) [ FindingEstimate, ConfirmingRide, TryAgain, ReAllocated ]) then VISIBLE else GONE
      ]
      [ PrestoAnim.animationSet [ scaleAnim $ autoAnimConfig ]
          $ lottieLoaderView state push
      , PrestoAnim.animationSet [ fadeIn true ]
          $ textView $
              [ accessibilityHint $ DS.replaceAll (DS.Pattern ".") (DS.Replacement "") loaderViewTitle
              , text loaderViewTitle
              , accessibility ENABLE
              , color Color.black800
              , height WRAP_CONTENT
              , width MATCH_PARENT
              , lineHeight "20"
              , gravity CENTER
              , margin if state.props.currentStage == ReAllocated then (MarginVertical 24 0) else (MarginVertical 24 36)
              ] <> FontStyle.subHeading1 TypoGraphy
          , textView $
              [ text (getString THE_RIDE_HAD_BEEN_CANCELLED_WE_ARE_FINDING_YOU_ANOTHER)
              , color Color.black650
              , height WRAP_CONTENT
              , width MATCH_PARENT
              , lineHeight "20"
              , gravity CENTER
              , margin $ Margin 16 4 16 36
              , visibility if state.props.currentStage == ReAllocated then VISIBLE else GONE
              ] <> FontStyle.body2 TypoGraphy
      , PrestoAnim.animationSet [ translateYAnimFromTopWithAlpha $ translateFullYAnimWithDurationConfig 300 ]
          $ separator (V 1) Color.grey900 state.props.currentStage
      , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , visibility if (any (_ == state.props.currentStage) [ FindingEstimate, TryAgain]) then VISIBLE else GONE
          , orientation VERTICAL
          , gravity CENTER
          ]
          [ PrestoAnim.animationSet [ translateYAnimFromTopWithAlpha $ translateFullYAnimWithDurationConfig 300 ]
              $ linearLayout 
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , orientation HORIZONTAL
                  , gravity CENTER_VERTICAL
                  ][ imageView
                      [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_wallet_filled"
                      , height $ V 20
                      , width $ V 20
                      , margin $ MarginTop 3
                      ]
                    , textView $
                      [ text (getString PAY_DRIVER_USING_CASH_OR_UPI)
                      , accessibilityHint "Pay Driver using Cash/UPI : Text"
                      , accessibility ENABLE
                      , lineHeight "18"
                      , width MATCH_PARENT
                      , height WRAP_CONTENT
                      , padding (Padding 5 20 0 16)
                      , color Color.black800
                      , gravity CENTER
                      ] <> FontStyle.body1 TypoGraphy
                  ]
          ]
      ]
------------------------------- pricingTutorialView --------------------------
pricingTutorialView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
pricingTutorialView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , weight 1.0
    , padding (Padding 0 safeMarginTop 0 safeMarginBottom)
    , background Color.white900
    ]
    [ -- TODO Add Animations
      -- PrestoAnim.animationSet
      --   [ translateYAnim 900 0 (state.props.currentStage == PricingTutorial)
      --   , translateYAnim 0 900 (not (state.props.currentStage == PricingTutorial))
      --   ] $
      PricingTutorialModel.view (push <<< PricingTutorialModelActionController)
    ]

------------------------ searchLocationModelView ---------------------------
searchLocationModelView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
searchLocationModelView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background if state.props.isRideServiceable then Color.transparent else Color.white900
    ]
    [ SearchLocationModel.view (push <<< SearchLocationModelActionController) $ searchLocationModelViewState state]

------------------------ quoteListModelView ---------------------------
quoteListModelView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
quoteListModelView push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , accessibility if (state.props.isPopUp /= NoPopUp) then DISABLE_DESCENDANT else DISABLE
  ][
  QuoteListModel.view (push <<< QuoteListModelActionController) $ quoteListModelViewState state]


------------------------ emptyTextView ---------------------------
emptyTextView :: forall w. HomeScreenState ->  PrestoDOM (Effect Unit) w
emptyTextView state = textView [text "", width $ if os == "IOS" then V 1 else V 0]

emptyLayout :: forall w. HomeScreenState -> PrestoDOM (Effect Unit) w
emptyLayout state =
  textView
    [ width MATCH_PARENT
    , height $ V 30
    , background Color.transparent
    ]

------------------------ rideTrackingView ---------------------------
rideTrackingView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rideTrackingView push state =
  let lowVisionDisability = maybe false (\dis -> if dis.tag == "BLIND_LOW_VISION" then true else false) state.data.disability
  in
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (Padding 0 0 0 0)
    , background Color.transparent
    , accessibility if (state.data.settingSideBar.opened /= SettingSideBar.CLOSED) || state.props.currentStage == ChatWithDriver || state.props.cancelSearchCallDriver || state.props.showCallPopUp || state.props.isCancelRide || state.props.emergencyHelpModal || state.props.isLocationTracking || state.props.callSupportPopUp || (state.props.showShareAppPopUp && state.data.config.feature.enableShareApp) || state.data.waitTimeInfo then DISABLE_DESCENDANT else DISABLE
    , alignParentBottom "true,-1" -- Check it in Android.
    , onBackPressed push (const $ BackPressed)
    ]
    [ -- TODO Add Animations
      -- PrestoAnim.animationSet
      --   [ translateInXAnim (-30) ( state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted)
      --   , translateOutXAnim (-100) $ not ( state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted)
      --   ] $
      linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.transparent
        , orientation VERTICAL
        -- , gravity BOTTOM -- Check it in Android.
        ]
        [ -- TODO Add Animations
          -- PrestoAnim.animationSet
          --   [ translateYAnim 900 0 ( state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted)
          --   , translateYAnim 0 900 $ not ( state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted)
          --   ] $
                   coordinatorLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    ][ bottomSheetLayout
                        ([ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , background Color.transparent 
                        , accessibility DISABLE
                        , enableShift false
                        , peakHeight $ getInfoCardPeekHeight state
                        , halfExpandedRatio $ halfExpanded
                        , orientation VERTICAL
                        ] <> (if lowVisionDisability then 
                            [onStateChanged push $ ScrollStateChanged] else [])
                          <> case state.props.sheetState of
                              Nothing -> []
                              Just state -> [sheetState state])
                        [ linearLayout
                            [ height WRAP_CONTENT
                            , width MATCH_PARENT
                            ]
                            [ if (any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithDriver]) then
                                DriverInfoCard.view (push <<< DriverInfoCardActionController) $ driverInfoCardViewState state
                              else
                                emptyTextView state
                            ]
                        ]
              ]
        ]
    ]
  where halfExpanded = (toNumber (getInfoCardPeekHeight state)) / (toNumber if (runFn1 getLayoutBounds $ getNewIDWithTag "BottomSheetLayout").height == 0 then 536 else (runFn1 getLayoutBounds $ getNewIDWithTag "BottomSheetLayout").height)

messageNotificationView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM ( Effect Unit) w
messageNotificationView push state =
  let enableChatWidget = not (os == "ANDROID" || state.props.enableChatWidget)
  in
  (if state.props.showChatNotification then 
    if os == "IOS" then PrestoAnim.animationSet[] else PrestoAnim.animationSet [fadeInWithDuration 1000 true]
  else if state.props.isNotificationExpanded then 
    if os == "IOS" then PrestoAnim.animationSet[] else PrestoAnim.animationSet [fadeOutWithDuration 1000 true]
  else  PrestoAnim.animationSet []) $ 
  linearLayout
  [ height $ if enableChatWidget then V 1 else V 130
  , width $ if enableChatWidget then V 1 else MATCH_PARENT
  , margin $ MarginBottom if enableChatWidget then 0 else 8
  , padding $ Padding 12 12 12 12
  , background Color.black900
  , orientation VERTICAL
  , clickable true
  , accessibility $ if state.props.isNotificationExpanded && os /= "IOS" then ENABLE else if not state.props.isNotificationExpanded then DISABLE_DESCENDANT else DISABLE
  , accessibilityHint $ "Quick Chat : Widget"
  , onAnimationEnd push $ const $ MessageViewAnimationEnd
  , visibility $ if (((any (_ == state.props.currentStage)) [ RideAccepted, ChatWithDriver]) && state.data.currentSearchResultType /= QUOTES && state.data.config.feature.enableChat) && state.data.config.feature.enableSuggestions 
                  then VISIBLE 
                  else if state.props.currentStage == RideStarted && os == "IOS" 
                    then INVISIBLE 
                    else GONE
  , cornerRadius 20.0
  ][linearLayout 
    [ height $ WRAP_CONTENT
    , width $ MATCH_PARENT
    , clickable true
    , accessibility DISABLE
    ][ messagePromtView push state
     , chatNotificationMessageView push state
     , linearLayout
       [ height $ WRAP_CONTENT
       , width $ MATCH_PARENT
       , gravity RIGHT
       , clickable true
       , accessibility DISABLE
       ][ linearLayout
         [ height $ WRAP_CONTENT
         , width $ WRAP_CONTENT
         , cornerRadius 20.0
         , clickable true
         , id $ getNewIDWithTag "CrossView"
         , accessibility ENABLE
         , accessibilityHint $ "Close : Button : Select to close chat widget"
         , background Color.manatee200
         , padding $ Padding 10 10 10 10
         , onClick push $ const $ RemoveNotification
         ][imageView
           [ height $ V 16
           , width $ V 16
           , accessibility DISABLE
           , imageWithFallback $ fetchImage FF_ASSET "ny_ic_cross_white"
           ]
         ]
       ]  
    ]
    , separatorView push state
    , if (state.data.lastMessage.sentBy == "Driver" || not (didDriverMessage FunctionCall)) then quickRepliesView push state else dummyView state
    , if ((not $ DS.null state.data.lastSentMessage.sentBy) && (not $ DS.null state.data.lastReceivedMessage.sentBy)) then messageView push state.data.lastSentMessage else dummyView state
  ]

chatNotificationMessageView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM ( Effect Unit) w
chatNotificationMessageView push state = if ((not $ didDriverMessage FunctionCall) && (not $ DS.null state.data.lastSentMessage.sentBy)) then messageView push state.data.lastSentMessage 
                                            else if (not $ DS.null state.data.lastReceivedMessage.sentBy) then messageView push state.data.lastReceivedMessage 
                                            else dummyView state

separatorView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM ( Effect Unit) w
separatorView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width  MATCH_PARENT
  , margin $ MarginVertical 8 8 
  ](map (\_ -> linearLayout
  [ height $ V 1
  , width $ V 8
  , margin $ MarginRight 4
  , background Color.manatee200
  ][]) (getArray 100))

dummyView :: forall w. HomeScreenState -> PrestoDOM ( Effect Unit) w
dummyView state = 
  linearLayout
  [height $ V 0
  , width $ V 0
  ][]

messageView :: forall w. (Action -> Effect Unit) -> MessagingView.ChatComponent -> PrestoDOM ( Effect Unit) w
messageView push message =
  let value = getMessageFromKey message.message $ getLanguageLocale languageKey
  in
  linearLayout
  [ width $ V (screenWidth unit - 140)
  , height $ WRAP_CONTENT
  , clickable true
  , onClick push $ const $ MessageDriver
  , accessibility ENABLE
  , accessibilityHint $ (if message.sentBy == "Customer" then "You Sent : " else "Message From Driver : ") <> getMessageFromKey message.message "EN_US"
  ][ imageView
     [ height $ V 32
     , width $ V 32
     , imageWithFallback $ if message.sentBy == "Driver" then fetchImage FF_ASSET "ny_ic_driver_message" else fetchImage FF_ASSET "ny_ic_customer_message"
     , margin $ Margin 0 8 8 0
     , accessibility DISABLE
     ]
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      , accessibility DISABLE_DESCENDANT
      , gravity LEFT
      ][ textView $ 
        [ width $ WRAP_CONTENT
        , height $ WRAP_CONTENT
        , text $ (if message.sentBy == "Driver" then getString MESSAGE_FROM_DRIVER else getString YOU_SENT) <> ":"
        , color Color.black700
        , maxLines 1
        , ellipsize true
        , margin $ if os == "IOS" then MarginBottom 2 else MarginBottom 0
        ] <> FontStyle.captions TypoGraphy
      , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , cornerRadii $ Corners 12.0 true true true false
        , background if message.sentBy == "Driver" then Color.manatee200 else Color.blue200
        , margin $ MarginTop 4
        , padding $ Padding 8 4 8 4
        , visibility $ boolToVisibility $ not $ DS.null value
        , gravity CENTER
        ][ textView $ 
          [ height WRAP_CONTENT
          , text $ value
          , color Color.white900
          , maxLines 1
          , ellipsize true
          ] <> FontStyle.body9 TypoGraphy
        ]
      ]
    ]

quickRepliesView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM ( Effect Unit) w
quickRepliesView push state = 
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ MATCH_PARENT
  , orientation VERTICAL
  , clickable true
  , accessibility DISABLE
  ][ textView $
     [ width WRAP_CONTENT
     , height WRAP_CONTENT
     , text $ getString QUICK <> " " <> getString CHATS <> ":"
     , color Color.black700
     , accessibility ENABLE
     , accessibilityHint $ "Quick Chats"
     , margin $ MarginBottom 4
     ] <> FontStyle.captions TypoGraphy
   , relativeLayout
     [ height $ WRAP_CONTENT
     , width $ MATCH_PARENT
     , clickable true
     , accessibility DISABLE
     ][horizontalScrollView
      [ height $ WRAP_CONTENT
      , width $ MATCH_PARENT
      , scrollBarX false
      , accessibility DISABLE
      , clickable true
      ][linearLayout
        [ height $ WRAP_CONTENT
        , width $ MATCH_PARENT
        , clickable true
        , accessibility DISABLE
        ][linearLayout
          [ height $ WRAP_CONTENT
          , width $ WRAP_CONTENT
          , cornerRadius 13.0
          , background Color.white900
          , padding $ Padding 16 6 16 6
          , margin $ MarginRight 12
          , clickable true
          , accessibility ENABLE
          , accessibilityHint $ "Custom Message : Button : Select to input custom message"
          , onClick push $ const $ MessageDriver
          ][ imageView
            [ height $ V 16
            , width $ V 16
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_message_black"
            ]
          ]
        , linearLayout
          [ height $ WRAP_CONTENT
          , width $ MATCH_PARENT
          , clickable true
          , id $ getNewIDWithTag "QuickReplyItems"
          , accessibility DISABLE
          ](mapWithIndex (\index item -> 
            quickReplyItem push state item index
          ) (getChatSuggestions state))
        ]
      ]
    , linearLayout
      [ height $ WRAP_CONTENT
      , width $ MATCH_PARENT
      , gravity RIGHT
      , accessibility DISABLE
      ][linearLayout
        [ height $ V $ replyItemHeight FunctionCall
        , width $ V 40
        , accessibility DISABLE
        , gradient $ if os == "IOS" then (Linear 180.0 ["#2C2F3A","#282C2F3A",Color.transparent]) else (Linear 90.0 [Color.transparent, "#282C2F3A", "#2C2F3A"])
        ][]  
      ]
    ]
  ]
  where replyItemHeight :: LazyCheck -> Int
        replyItemHeight dummy = do
          let layout = runFn1 getLayoutBounds $ getNewIDWithTag "QuickReplyItems"
          if layout.height == 0 then 32 else layout.height + 2

quickReplyItem :: forall w. (Action -> Effect Unit) -> HomeScreenState -> String -> Int -> PrestoDOM ( Effect Unit) w
quickReplyItem push state item idx = 
  let message = getMessageFromKey item $ getLanguageLocale languageKey
      isLastItem = (idx == (length $ getChatSuggestions state) - 1)
  in
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ WRAP_CONTENT
  , cornerRadius 13.0
  , clickable true
  , rippleColor Color.rippleShade
  , accessibility ENABLE
  , margin $ MarginRight if isLastItem then 18 else 12
  , visibility $ boolToVisibility $ not $ DS.null message
  , accessibilityHint $ (getMessageFromKey item $ "EN_US") <> ": Button : Select to send message to driver"
  , onClick (\action -> do
                when (not $ DS.null state.data.lastReceivedMessage.sentBy) $ do void $ startTimer 3 ("ChatNotificationRemoval" <> (show $ state.data.triggerPatchCounter)) "1" push MessageExpiryTimer
                push action)
            (const $ SendQuickMessage item)
  , background Color.white900
  , padding $ Padding 16 6 16 6
  ][ textView $
      [ text $ message
      , color Color.black900
      ] <> FontStyle.tags TypoGraphy
  ]

messagePromtView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM ( Effect Unit) w
messagePromtView push state = 
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ V (screenWidth unit - 100)
  , orientation VERTICAL
  , clickable true
  , accessibility ENABLE
  , accessibilityHint $ "Want to message your driver?"
  , onClick push $ const $ MessageDriver
  , visibility $ boolToVisibility $ null state.data.messages 
  ][ textView $ 
      [ text $ getString MESSAGE_YOUR_DRIVER
      , accessibility ENABLE
      , color Color.white900
      , accessibility DISABLE
      , ellipsize true
      , singleLine true
      ] <> FontStyle.body6 TypoGraphy
   , textView $ 
      [ text $ getString CHECK_IN_WITH_YOUR_DRIVER 
      , color Color.white900
      , accessibility DISABLE
      , ellipsize true
      , singleLine true
      ] <> FontStyle.captions TypoGraphy
  ]

trackRideView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM ( Effect Unit) w
trackRideView push state =
  PrestoAnim.animationSet[fadeIn true] $
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , clipChildren false
  , clickable true
  , onAnimationEnd push (const NotificationAnimationEnd)
  ][ linearLayout
    [ height $ V 40
    , width $ WRAP_CONTENT
    , background Color.white900
    , cornerRadius if os == "IOS" then 20.0 else 32.0
    , padding $ PaddingHorizontal 12 12
    , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
    , rippleColor Color.rippleShade
    , visibility $ boolToVisibility $ state.props.currentStage == RideStarted
    , clickable true
    , onClick push $ const $ StartLocationTracking "GOOGLE_MAP"
    , gravity CENTER
    , accessibility ENABLE
    , accessibilityHint $ "Real Time Tracking on Google Maps : Button"
    , accessibility DISABLE_DESCENDANT
    ][ linearLayout
      [ height $ WRAP_CONTENT
      , width $ WRAP_CONTENT
      ][ imageView
          [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_nav_grey"
          , height $ V 16
          , width $ V 16
          , accessibilityHint "Track on Gooole Maps : Button"
          , accessibility ENABLE
          , margin $ MarginRight 4
          ]
        , textView $ 
          [ text $ getString TRACK_ON_GOOGLE_MAPS
          , color Color.black800
          ] <> FontStyle.body9 TypoGraphy
      ]
    ]
  ]

otpAndWaitView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM ( Effect Unit) w
otpAndWaitView push state =
  linearLayout
  [ height $ V 40
  , orientation HORIZONTAL
  , visibility $ boolToVisibility $ any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver]
  , clipChildren false
  , clickable true
  , gravity BOTTOM
  , accessibility DISABLE
  ]([linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , cornerRadius if os == "IOS" then 18.0 else 32.0
    , background Color.white900
    , gravity CENTER
    , clickable true
    , accessibility DISABLE
    , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
    ][ textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , accessibilityHint $ "O T P : " <> (DS.replaceAll (DS.Pattern "") (DS.Replacement " ")  state.data.driverInfoCardState.otp)
      , accessibility ENABLE
      , text $ getString OTP
      , padding $ Padding 12 0 4 if os == "IOS" then 0 else 3
      , color Color.black700
      ] <> FontStyle.body22 TypoGraphy
    , otpView push state
    ]
  ] <> if (state.data.currentSearchResultType == QUOTES || state.data.driverInfoCardState.driverArrived) then 
        [(PrestoAnim.animationSet [ fadeIn true ] $ 
        let isQuotes = state.data.currentSearchResultType == QUOTES
        in
        linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , cornerRadius if os == "IOS" then 18.0 else 32.0
        , background Color.white900
        , clickable true
        , onClick push $ const WaitingInfo
        , gravity CENTER
        , accessibility DISABLE
        , margin $ MarginLeft 12
        , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
        ][ textView $ 
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , accessibility ENABLE
          , accessibilityHint $ (if isQuotes then "O T P Expiry Info" else "Wait time info : ") <> "Button : Select to learn more about " <> if isQuotes then "O T P Expiry Time" else "wait time"   
          , text $ if isQuotes then getString EXPIRES_IN else getString WAIT_TIME
          , color Color.black700
          , padding $ Padding 12 0 4 if os == "IOS" then 0 else 3
          ] <> FontStyle.body22 TypoGraphy
        , imageView
            [ height $ V 12
            , width  $ V 12
            , gravity CENTER_VERTICAL
            , accessibility DISABLE
            , margin $ MarginRight 4
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info"
            ]
        , waitTimeView push state
        ])]
      else [])

shineAnimation :: forall w . Int -> Int -> PrestoDOM (Effect Unit) w
shineAnimation height' width' =
  linearLayout
  [ height $ MATCH_PARENT
  , width $ WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , cornerRadius 16.0
  , clipChildren true
  , clipToPadding true
  ][ PrestoAnim.animationSet
    [ shimmerAnimation (-100) width' 1500
    ] $ linearLayout
        [ width $ V $ width'
        , height $ MATCH_PARENT
        , gravity CENTER_VERTICAL
        ][ linearLayout
          [ width $ V 10
          , height $ V $ height'
          , background Color.white200
          , rotation 20.0
          , cornerRadius 2.0
          , margin $ MarginHorizontal 10 8
          ][]
         , linearLayout
           [ width $ V 5
           , height $ V $ height'
           , background Color.white200
           , rotation 20.0
           , cornerRadius 2.0
           , margin $ MarginRight 20
           ][]
        ]
  ]

waitTimeView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM ( Effect Unit) w
waitTimeView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius if os == "IOS" then 12.0 else 16.0
  , margin $ Margin 0 4 4 4
  , background $ colorForWaitTime state
  , gravity CENTER_VERTICAL
  , accessibility DISABLE
  ][ textView $ 
     [ height $ WRAP_CONTENT 
     , width $ V $ 60
     , padding $ Padding 0 4 0 6
     , accessibilityHint $ waitTimeHint state
     , accessibility ENABLE 
     , text $ state.data.driverInfoCardState.waitingTime
     , color Color.black900
     , gravity CENTER
     , singleLine true
     ] <> FontStyle.body22 TypoGraphy
  ]

waitTimeHint :: HomeScreenState -> String
waitTimeHint state = (if state.data.currentSearchResultType == QUOTES then "O T P Expires in : " else "Wait Time : ") <> case DS.split (DS.Pattern ":") state.data.driverInfoCardState.waitingTime of
                        [minutes, seconds] -> do 
                          let min = DS.trim $ minutes
                          let sec = DS.trim $ seconds
                          if min /= "00" then min <> " Minutes and " <> sec <> " Seconds" else sec <> " Seconds" 
                        _ -> ""

colorForWaitTime:: HomeScreenState -> String
colorForWaitTime state =
  let waitTime = DS.split (DS.Pattern ":") state.data.driverInfoCardState.waitingTime
  in
  case waitTime of
    [minutes, _] -> 
      let mins = fromMaybe 0 (fromString (DS.trim minutes))
          threshold = if state.data.currentSearchResultType == QUOTES then mins < 5 else mins > 2
      in
      if threshold then Color.carnation100 else Color.grey700 
    _ -> Color.grey700


otpView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM ( Effect Unit) w
otpView push state =
  let otpDimensions = runFn1 getLayoutBounds $ getNewIDWithTag "OTPView"
      shimmerHeight = (otpDimensions.height) + 3
      shimmerWidth = (otpDimensions.width) - (if os == "IOS" then 3 else 10)
  in
  relativeLayout
  [ height $ WRAP_CONTENT
  , width $ WRAP_CONTENT
  , cornerRadius 16.0
  , accessibility DISABLE_DESCENDANT
  ][linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , cornerRadius if os == "IOS" then 12.0 else 16.0
      , id $ getNewIDWithTag "OTPView"
      , margin $ Margin 0 4 4 4
      , background Color.grey700
      ][ textView $ 
        [ text $ state.data.driverInfoCardState.otp
        , color Color.black900
        , padding $ Padding 8 4 8 6
        ] <> FontStyle.body22 TypoGraphy
      ]
  , if state.data.currentSearchResultType == QUOTES then shineAnimation shimmerHeight shimmerWidth else emptyTextView state
  ]

rideInfoActionView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM ( Effect Unit) w
rideInfoActionView push state =
  let enableSupport = state.data.config.feature.enableSupport
      enableShareRide = state.data.config.feature.enableShareRide
  in 
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ WRAP_CONTENT
  , orientation VERTICAL
  , clickable true
  , accessibility DISABLE
  , clipChildren false
  ][linearLayout
    [ width $ V 40
    , height $ V 40
    , gravity CENTER
    , clickable true
    , visibility $ boolToVisibility $ enableSupport
    , background Color.white900
    , stroke $ "1,"<> Color.grey900
    , cornerRadius if os == "IOS" then 20.0 else 32.0
    , onClick push $ const RideSupport
    , accessibilityHint "Contact Support : Button"
    , accessibility ENABLE
    , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
    ][ imageView
      [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_contact_support"
      , height $ V 16
      , width $ V 16
      , accessibility DISABLE
      ]
    ] 
    , linearLayout
      [ width $ V 40
      , height $ V 40
      , gravity CENTER
      , visibility $ boolToVisibility $ enableShareRide
      , background Color.white900
      , stroke $ "1,"<> Color.grey900
      , cornerRadius if os == "IOS" then 20.0 else 32.0
      , margin $ MarginTop 8
      , clickable true
      , accessibilityHint "Share Ride : Button : Select to share ride details"
      , accessibility ENABLE
      , onClick push $ const ShareRide
      , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
      , rippleColor Color.rippleShade
      ][ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_share_icon"
        , height $ V 16
        , width $ V 16
        , accessibility DISABLE
        ]
      ]
  ]

distanceOutsideLimitsView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
distanceOutsideLimitsView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity BOTTOM
    , accessibility if state.props.currentStage == DistanceOutsideLimits then DISABLE else DISABLE_DESCENDANT
    ]
    [ PopUpModal.view (push <<< DistanceOutsideLimitsActionController) (distanceOusideLimitsConfig state) ]

pickUpFarFromCurrLocView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
pickUpFarFromCurrLocView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity BOTTOM
    , accessibility if state.props.currentStage == PickUpFarFromCurrentLocation then DISABLE else DISABLE_DESCENDANT
    ]
    [ PopUpModal.view (push <<< PickUpFarFromCurrentLocAC) (pickUpFarFromCurrentLocationConfig state) ]

shortDistanceView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
shortDistanceView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity BOTTOM
    , accessibility if state.props.currentStage == ShortDistance then DISABLE else DISABLE_DESCENDANT
    ]
    [ PopUpModal.view (push <<< ShortDistanceActionController) (shortDistanceConfig state) ]

saveFavouriteCardView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
saveFavouriteCardView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , accessibility if state.props.isSaveFavourite then DISABLE else DISABLE_DESCENDANT
    ]
    [ SaveFavouriteCard.view (push <<< SaveFavouriteCardAction) (state.data.saveFavouriteCard) ]

logOutPopUpView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
logOutPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , accessibility DISABLE
    ]
    [ PopUpModal.view (push <<< PopUpModalAction) (logOutPopUpModelConfig state) ]

favouriteLocationModel :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
favouriteLocationModel push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    ]
    [ FavouriteLocationModel.view (push <<< FavouriteLocationModelAC) (state.data.savedLocations) ]

------------------------------- separator --------------------------
separator :: Length -> String -> Stage -> forall w. PrestoDOM (Effect Unit) w
separator lineHeight lineColor currentStage =
  linearLayout
    [ height $ lineHeight
    , width MATCH_PARENT
    , background lineColor
    , visibility if any (_ == currentStage) [FindingQuotes, ReAllocated] then GONE else VISIBLE
    ]
    []

waitTimeInfoPopUp :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
waitTimeInfoPopUp push state =
  PrestoAnim.animationSet [ fadeIn true ]
  $ linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , accessibility DISABLE
  ][ RequestInfoCard.view (push <<< RequestInfoCardAction) (waitTimeInfoCardConfig state) ]

lottieLoaderView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
lottieLoaderView state push =
  lottieAnimationView
    [ id (getNewIDWithTag "lottieLoader")
    , afterRender
        ( \action -> do
            void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = (getAssetsBaseUrl FunctionCall) <> "lottie/ic_vehicle_processing.json", lottieId = (getNewIDWithTag "lottieLoader") }
            pure unit
        )
        (const LottieLoaderAction)
    , height $ V state.data.config.searchLocationConfig.lottieHeight
    , width $ V state.data.config.searchLocationConfig.lottieWidth
    ]

getEstimate :: forall action. (GetQuotesRes -> action) -> action -> Int -> Number -> (action -> Effect Unit) -> HomeScreenState -> Flow GlobalState Unit
getEstimate action flowStatusAction count duration push state = do
  if (isLocalStageOn FindingEstimate) || (isLocalStageOn TryAgain) then
    if (count > 0) then do
      resp <- getQuotes (state.props.searchId)
      _ <- pure $ printLog "caseId" (state.props.searchId)
      case resp of
        Right response -> do
          _ <- pure $ printLog "api Results " response
          let (GetQuotesRes resp) = response
          if not (null resp.quotes) || not (null resp.estimates) then do
            doAff do liftEffect $ push $ action response
            pure unit
          else do
            if (count == 1) then do
              _ <- pure $ updateLocalStage SearchLocationModel
              doAff do liftEffect $ push $ action response
            else do
              void $ delay $ Milliseconds duration
              getEstimate action flowStatusAction (count - 1) duration push state
        Left err -> do
          let errResp = err.response
              codeMessage = decodeError errResp.errorMessage "errorMessage"
          if ( err.code == 400 && codeMessage == "ACTIVE_BOOKING_ALREADY_PRESENT" ) then do
            -- _ <- pure $ logEvent state.data.logField "ny_fs_active_booking_found_on_search"
            void $ pure $ toast "ACTIVE BOOKING ALREADY PRESENT"
            doAff do liftEffect $ push $ flowStatusAction
          else do
            void $ delay $ Milliseconds duration
            if (count == 1) then do
              let response = GetQuotesRes { quotes: [], estimates: [], fromLocation: SearchReqLocationAPIEntity { lat: 0.0, lon: 0.0 }, toLocation: Nothing }
              _ <- pure $ updateLocalStage SearchLocationModel
              doAff do liftEffect $ push $ action response
            else do
              getEstimate action flowStatusAction (count - 1) duration push state
    else
      pure unit
  else
    pure unit

getQuotesPolling :: forall action. String -> (SelectListRes -> action) -> (ErrorResponse -> action) -> Int -> Number -> (action -> Effect Unit) -> HomeScreenState -> Flow GlobalState Unit
getQuotesPolling pollingId action retryAction count duration push state = do
  when (pollingId == (getValueToLocalStore TRACKING_ID) && (isLocalStageOn FindingQuotes)) $ do
    internetCondition <- liftFlow $ isInternetAvailable unit
    when internetCondition $ do
      let gotQuote = (getValueToLocalStore GOT_ONE_QUOTE)
      let minimumPollingCount = fromMaybe 0 (fromString (getValueToLocalStore TEST_MINIMUM_POLLING_COUNT))
      let usableCount = if gotQuote == "TRUE" && count > minimumPollingCount then minimumPollingCount else count
      if (spy "USABLECOUNT :- " usableCount > 0) then do
        resp <- selectList (state.props.estimateId)
        _ <- pure $ printLog "caseId" (state.props.estimateId)
        case resp of
          Right response -> do
            _ <- pure $ printLog "Quote api Results " response
            let (SelectListRes resp) = response
            if (resp.bookingId /= Nothing && resp.bookingId /= Just "") || (not (null ((fromMaybe dummySelectedQuotes resp.selectedQuotes)^._selectedQuotes))) then do
               doAff do liftEffect $ push $ action response
            else
              pure unit
            void $ delay $ Milliseconds duration
            getQuotesPolling pollingId action retryAction (usableCount - 1) duration push state
          Left err -> do
            _ <- pure $ printLog "api error " err
            doAff do liftEffect $ push $ retryAction err
            void $ delay $ Milliseconds duration
            pure unit
            getQuotesPolling pollingId action retryAction (usableCount - 1) duration push state
      else do
        let response = SelectListRes { selectedQuotes: Nothing, bookingId : Nothing }
        _ <- pure $ updateLocalStage QuoteList
        doAff do liftEffect $ push $ action response

updateRecentTrips :: forall action. (RideBookingListRes -> action) -> (action -> Effect Unit) -> Maybe RideBookingListRes -> Flow GlobalState Unit
updateRecentTrips action push response = do
  case response of 
    Just resp -> handleResponse resp
    Nothing -> fetchAndHandleResponse
  where
    handleResponse resp = do
      screenActive <- liftFlow $ isScreenActive "default" "HomeScreen"
      if screenActive 
        then liftFlow $ push $ action resp
        else retryAfterDelay resp

    retryAfterDelay resp = do
      void $ delay $ Milliseconds 1000.0
      updateRecentTrips action push (Just resp)

    fetchAndHandleResponse = do
      rideBookingListResponse <- Remote.rideBookingListWithStatus "30" "0" "COMPLETED"
      void $ pure $ setValueToLocalStore UPDATE_REPEAT_TRIPS "false"
      case rideBookingListResponse of
        Right listResp -> do
          handleResponse listResp
        Left _ -> liftFlow $ push $ action (RideBookingListRes {list : []} )

driverLocationTracking :: forall action. (action -> Effect Unit) -> (String -> action) -> (String -> action) -> (Int -> Int -> action) -> Number -> String -> HomeScreenState -> String -> Flow GlobalState Unit
driverLocationTracking push action driverArrivedAction updateState duration trackingId state routeState = do
  _ <- pure $ printLog "trackDriverLocation2_function" trackingId
  if (any (\stage -> isLocalStageOn stage) [ RideAccepted, RideStarted, ChatWithDriver]) && ((getValueToLocalStore TRACKING_ID) == trackingId) then do
    when (state.props.bookingId /= "") $ do
      respBooking <- rideBooking (state.props.bookingId)
      case respBooking of
        Right (RideBookingRes respBooking) -> do
          let bookingStatus = respBooking.status
          case bookingStatus of
            "REALLOCATED" -> do
                doAff do liftEffect $ push $ action bookingStatus
            _             -> do
                case (respBooking.rideList !! 0) of
                  Just (RideAPIEntity res) -> do
                    let rideStatus = res.status
                    doAff do liftEffect $ push $ action rideStatus
                    if (res.driverArrivalTime /= Nothing  && (getValueToLocalStore DRIVER_ARRIVAL_ACTION) == "TRIGGER_DRIVER_ARRIVAL" ) then 
                      doAff do liftEffect $ push $ driverArrivedAction (fromMaybe "" res.driverArrivalTime)
                    else pure unit
                  Nothing -> pure unit
        Left err -> pure unit
    if (state.props.isSpecialZone && state.data.currentSearchResultType == QUOTES) && (isLocalStageOn RideAccepted) then do
      _ <- pure $ enableMyLocation true
      _ <- pure $ removeAllPolylines ""
      _ <- doAff $ liftEffect $ animateCamera state.data.driverInfoCardState.sourceLat state.data.driverInfoCardState.sourceLng zoomLevel "ZOOM"
      _ <- doAff $ liftEffect $ addMarker "ny_ic_src_marker" state.data.driverInfoCardState.sourceLat state.data.driverInfoCardState.sourceLng 110 0.5 0.9
      void $ delay $ Milliseconds duration
      driverLocationTracking push action driverArrivedAction updateState duration trackingId state routeState
      else do
        response <- getDriverLocation state.data.driverInfoCardState.rideId
        case response of
          Right (GetDriverLocationResp resp) -> do
            let
              rideID = state.data.driverInfoCardState.rideId
              srcLat = (resp ^. _lat)
              srcLon = (resp ^. _lon)
              dstLat = if (any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver]) then state.data.driverInfoCardState.sourceLat else state.data.driverInfoCardState.destinationLat
              dstLon = if (any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver]) then state.data.driverInfoCardState.sourceLng else state.data.driverInfoCardState.destinationLng
              trackingType = if (any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver]) then Remote.DRIVER_TRACKING else Remote.RIDE_TRACKING
              markers = getRouteMarkers state.data.driverInfoCardState.vehicleVariant state.props.city trackingType 
              sourceSpecialTagIcon = specialLocationIcons state.props.zoneType.sourceTag
              destSpecialTagIcon = specialLocationIcons state.props.zoneType.destinationTag
              specialLocationTag =  if (any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver]) then
                                      specialLocationConfig destSpecialTagIcon sourceSpecialTagIcon true getPolylineAnimationConfig
                                    else
                                      specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon false getPolylineAnimationConfig
            if ((getValueToLocalStore TRACKING_ID) == trackingId) then do
              if (getValueToLocalStore TRACKING_ENABLED) == "False" then do
                _ <- pure $ setValueToLocalStore TRACKING_DRIVER "True"
                _ <- pure $ removeAllPolylines ""
                _ <- liftFlow $ drawRoute (walkCoordinate srcLat srcLon dstLat dstLon) "DOT" "#323643" false markers.srcMarker markers.destMarker 8 "DRIVER_LOCATION_UPDATE" "" "" specialLocationTag
                void $ delay $ Milliseconds duration
                driverLocationTracking push action driverArrivedAction updateState duration trackingId state routeState
                pure unit
              else if ((getValueToLocalStore TRACKING_DRIVER) == "False" || not (isJust state.data.route)) then do
                _ <- pure $ setValueToLocalStore TRACKING_DRIVER "True"
                routeResponse <- getRoute routeState $ makeGetRouteReq srcLat srcLon dstLat dstLon
                case routeResponse of
                  Right (GetRouteResp routeResp) -> do
                    case ((routeResp) !! 0) of
                      Just (Route routes) -> do
                        _ <- pure $ removeAllPolylines ""
                        let (Snapped routePoints) = routes.points
                            newPoints = if length routePoints > 1 then
                                          getExtendedPath (walkCoordinates routes.points)
                                        else
                                          walkCoordinate srcLat srcLon dstLat dstLon
                            newRoute = routes { points = Snapped (map (\item -> LatLong { lat: item.lat, lon: item.lng }) newPoints.points) }
                        liftFlow $ drawRoute newPoints "LineString" "#323643" true markers.srcMarker markers.destMarker 8 "DRIVER_LOCATION_UPDATE" "" (metersToKm routes.distance state) specialLocationTag
                        _ <- doAff do liftEffect $ push $ updateState routes.duration routes.distance
                        void $ delay $ Milliseconds duration
                        driverLocationTracking push action driverArrivedAction updateState duration trackingId state { data { route = Just (Route newRoute), speed = routes.distance / routes.duration } } routeState
                      Nothing -> do
                        _ <- pure $ spy "Nothing" "1"
                        pure unit
                  Left err -> do
                    _ <- pure $ spy "Nothing" "2"
                    pure unit
              else do
                case state.data.route of
                  Just (Route route) -> do
                        locationResp <- liftFlow $ isCoordOnPath (walkCoordinates route.points) (resp ^. _lat) (resp ^. _lon) (state.data.speed)
                        if locationResp.isInPath then do
                          let newPoints = { points : locationResp.points}
                          let specialLocationTag =  if (any (\stage -> isLocalStageOn stage) [ RideAccepted, ChatWithDriver]) then
                                                      specialLocationConfig "" sourceSpecialTagIcon true getPolylineAnimationConfig
                                                    else
                                                      specialLocationConfig "" destSpecialTagIcon false getPolylineAnimationConfig
                          liftFlow $ runEffectFn1 updateRoute updateRouteConfig { json = newPoints, destMarker =  markers.destMarker, eta =  (metersToKm locationResp.distance state), srcMarker =  markers.srcMarker, specialLocation = specialLocationTag, zoomLevel = zoomLevel}
                          _ <- doAff do liftEffect $ push $ updateState locationResp.eta locationResp.distance
                          void $ delay $ Milliseconds duration
                          driverLocationTracking push action driverArrivedAction updateState duration trackingId state routeState
                        else do
                          driverLocationTracking push action driverArrivedAction updateState duration trackingId state { data { route = Nothing } } routeState
                  Nothing -> driverLocationTracking push action driverArrivedAction updateState duration trackingId state { data { route = Nothing } } routeState
            else pure unit
          Left err -> do
            void $ delay $ Milliseconds duration
            driverLocationTracking push action driverArrivedAction updateState duration trackingId state { data { route = Nothing } } routeState
  else do
    pure unit


confirmRide :: forall action. (RideBookingRes -> action) -> Int -> Number -> (action -> Effect Unit) -> HomeScreenState -> Flow GlobalState Unit
confirmRide action count duration push state = do
  if (count /= 0) && (isLocalStageOn ConfirmingRide) && (state.props.bookingId /= "")then do
    resp <- rideBooking (state.props.bookingId)
    _ <- pure $ printLog "response to confirm ride:- " (state.props.searchId)
    case resp of
      Right response -> do
        _ <- pure $ printLog "api Results " response
        let (RideBookingRes resp) = response
            fareProductType = (resp.bookingDetails) ^. _fareProductType
            status = if fareProductType == "OneWaySpecialZoneAPIDetails" then "CONFIRMED" else "TRIP_ASSIGNED"
            willRideListNull = if fareProductType == "OneWaySpecialZoneAPIDetails" then true else false
        if  status == resp.status && (willRideListNull || not (null resp.rideList)) then do
            doAff do liftEffect $ push $ action response
            -- _ <- pure $ logEvent state.data.logField "ny_user_ride_assigned"
            pure unit
        else do
            void $ delay $ Milliseconds duration
            confirmRide action (count - 1) duration push state
      Left err -> do
        _ <- pure $ printLog "api error " err
        void $ delay $ Milliseconds duration
        confirmRide action (count - 1) duration push state
  else
    pure unit

cancelRidePopUpView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
cancelRidePopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , accessibility DISABLE
    ][ CancelRidePopUp.view (push <<< CancelRidePopUpAction) (cancelRidePopUpConfig state)]

checkForLatLongInSavedLocations :: forall action. (action -> Effect Unit) -> (Array LocationListItemState -> action) -> HomeScreenState -> Flow GlobalState Unit
checkForLatLongInSavedLocations push action state = do
  _ <- runExceptT $ runBackT $ setValueToLocalStore RELOAD_SAVED_LOCATION "false"
  _ <- runExceptT $ runBackT $ transformSavedLocations state.data.savedLocations
  if getValueToLocalStore RELOAD_SAVED_LOCATION == "true" then do
    (savedLocationResp )<- getSavedLocationList ""
    case savedLocationResp of
        Right (SavedLocationsListRes listResp) -> do
          doAff do liftEffect $ push $ action $ AddNewAddress.getSavedLocations listResp.list
          pure unit
        Left err -> pure unit
    pure unit
    else pure unit
  _ <- runExceptT $ runBackT $ setValueToLocalStore RELOAD_SAVED_LOCATION "false"
  pure unit

notinPickUpZoneView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
notinPickUpZoneView push state =
  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , stroke $ "1," <> Color.grey900
      , gravity CENTER
      , cornerRadius 8.0
      , margin $ MarginTop 16
      , padding $ PaddingVertical 2 10
      ][linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation HORIZONTAL
        , margin (MarginLeft 15)]
        [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER
        , margin $ MarginTop if os == "IOS" then 10 else 0
        ][  textView $
            [ text $ if state.data.rateCard.additionalFare == 0 then (getCurrency appConfig) <> (show state.data.suggestedAmount) else  (getCurrency appConfig) <> (show state.data.suggestedAmount) <> "-" <> (getCurrency appConfig) <> (show $ (state.data.suggestedAmount + state.data.rateCard.additionalFare))
            , color Color.black800
            , margin $ MarginTop 8
            , gravity CENTER_HORIZONTAL
            , width WRAP_CONTENT
            , height WRAP_CONTENT
            , onClick push $ const ShowRateCard
            ] <> FontStyle.priceFont LanguageStyle
            , estimatedTimeAndDistanceView push state
          ]
          , imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_info_blue"
            , width $ V 40
            , height $ V 40
            , gravity BOTTOM
            , margin (MarginTop 13)
            , onClick push $ const ShowRateCard
            ]
        ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          ]
          [ linearLayout
              [ width MATCH_PARENT
              , height $ V 1
              , margin $ Margin 16 12 16 14
              , background Color.grey900
              ][]
          , linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              ]
              [ linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , gravity CENTER_HORIZONTAL
                  , onClick push $ const PreferencesDropDown
                  , margin $ MarginBottom 8
                  ][ textView $
                      [ height $ V 24
                      , width WRAP_CONTENT
                      , color Color.darkCharcoal
                      , text $ getString BOOKING_PREFERENCE
                      ] <> FontStyle.body5 TypoGraphy,
                      imageView
                      [ width $ V 10
                      , height $ V 10
                      , margin (Margin 9 8 0 0)
                      , imageWithFallback if state.data.showPreferences then fetchImage FF_COMMON_ASSET "ny_ic_chevron_up" else fetchImage FF_ASSET "ny_ic_chevron_down"
                      ]
                  ],
                  linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , margin $ MarginLeft 20
                    , orientation VERTICAL
                    ][ linearLayout
                       [ width MATCH_PARENT
                       , height WRAP_CONTENT
                       , orientation VERTICAL
                       , visibility if state.data.showPreferences then VISIBLE else GONE
                       ][showMenuButtonView push (getString AUTO_ASSIGN_DRIVER) ( fetchImage FF_ASSET "ny_ic_faster") true state,
                         showMenuButtonView push (getString CHOOSE_BETWEEN_MULTIPLE_DRIVERS) ( fetchImage FF_ASSET "ny_ic_info") false state ]
                  ]

              ]
          ]
      ]

zoneTimerExpiredView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
zoneTimerExpiredView state push =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity CENTER
  ][ PopUpModal.view (push <<< ZoneTimerExpired) (zoneTimerExpiredConfig state)]

currentLocationView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
currentLocationView push state =
  linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , margin $ MarginVertical 20 10
            , onClick push $ const GoBackToSearchLocationModal
            , padding $ PaddingHorizontal 15 15
            , stroke $ "1," <> state.data.config.confirmPickUpLocationBorder
            , gravity CENTER_VERTICAL
            , accessibility DISABLE
            , cornerRadius 5.0
            , visibility if state.props.defaultPickUpPoint /= "" then GONE else VISIBLE
            ]
            [ imageView
                [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_source_dot"
                , height $ V 16
                , width $ V 16
                , gravity CENTER_VERTICAL
                , accessibility DISABLE
                ]
            , textView
                $
                  [ text state.data.source
                  , ellipsize true
                  , singleLine true
                  , accessibility ENABLE
                  , accessibilityHint $ "Pickup Location is " <>  (DS.replaceAll (DS.Pattern ",") (DS.Replacement " ") state.data.source)
                  , gravity LEFT
                  , width MATCH_PARENT
                  , padding (Padding 10 16 10 16)
                  , color Color.black800
                  ]
                <> FontStyle.subHeading1 TypoGraphy
            ]

nearByPickUpPointsView :: forall w . HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
nearByPickUpPointsView state push =
  scrollView
  [ height $ V 130
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ Padding 5 20 0 5
  , visibility if state.props.defaultPickUpPoint /= "" then VISIBLE else GONE
  , id $ getNewIDWithTag "scrollViewParent"
  ][linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , id $ getNewIDWithTag "scrollViewChild"
    , afterRender push (const AfterRender)
    ](mapWithIndex (\index item ->
                    linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , margin $ MarginBottom 12
                      ][MenuButton.view (push <<< MenuButtonActionController) (menuButtonConfig state item)]) state.data.nearByPickUpPoints)
  ]

confirmingLottieView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
confirmingLottieView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadii $ Corners 24.0 true true false false
    , alignParentBottom "true,-1"
    ][ relativeLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , cornerRadii $ Corners 24.0 true true false false
        , background Color.transparent
        ][ PrestoAnim.animationSet [ fadeIn true ] $
          loaderView push state
          ]
    ]

genderBanner :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
genderBanner push state =
  Banner.view (push <<< GenderBannerModal) (genderBannerConfig state)

isAnyOverlayEnabled :: HomeScreenState -> Boolean
isAnyOverlayEnabled state = state.data.settingSideBar.opened /= SettingSideBar.CLOSED || state.props.emergencyHelpModal || state.props.cancelSearchCallDriver || state.props.isCancelRide || state.props.isLocationTracking || state.props.callSupportPopUp || state.props.showCallPopUp || state.props.showRateCard || (state.props.showShareAppPopUp && state.data.config.feature.enableShareApp || state.data.waitTimeInfo)

carouselView:: HomeScreenState -> (Action -> Effect Unit)  -> forall w . PrestoDOM (Effect Unit) w
carouselView state push = 
  PrestoAnim.animationSet [ fadeIn true ] $ 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding $ Padding 16 16 16 16
  , background Color.white900
  , cornerRadius 16.0
  , gravity CENTER
  , visibility if state.props.showEducationalCarousel then VISIBLE else GONE
  , orientation VERTICAL
  , margin $ MarginHorizontal 16 16
  ][  textView $ 
      [ text $ getString INCLUSIVE_AND_ACCESSIBLE
      , margin $ MarginBottom 20
      , color Color.black800
      ] <> FontStyle.body7 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , stroke $ "1," <> Color.grey900
      , background Color.grey700
      , margin $ MarginBottom 16
      , orientation VERTICAL
      , cornerRadius 8.0
      ][  PrestoAnim.animationSet [ fadeIn true ] $  linearLayout
          [ height $ V 340
          , width MATCH_PARENT
          , orientation VERTICAL
          , id $ getNewIDWithTag "AccessibilityCarouselView"
          , accessibility DISABLE
          , gravity CENTER    
          , onAnimationEnd (\action -> do
              when (addCarouselWithVideoExists unit) $ addCarousel { gravity : "TOP", carouselData : getCarouselData state } (getNewIDWithTag "AccessibilityCarouselView")
              push action
            ) (const AfterRender)
          ][]
        ]
    , PrimaryButton.view (push <<< UpdateProfileButtonAC) (updateProfileConfig state)
    , PrimaryButton.view (push <<< SkipAccessibilityUpdateAC) (maybeLaterButtonConfig state)]

getInfoCardPeekHeight :: HomeScreenState -> Int
getInfoCardPeekHeight state = if state.data.infoCardPeekHeight == 0 then (getDefaultPeekHeight state) else state.data.infoCardPeekHeight


homeScreenView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
homeScreenView push state =
  PrestoAnim.animationSet
    [ fadeOut (state.props.currentStage == SearchLocationModel)
    ]
    $ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ PaddingVertical safeMarginTop safeMarginBottom
        , accessibility if state.data.settingSideBar.opened /= SettingSideBar.CLOSED then DISABLE_DESCENDANT else DISABLE
        , orientation VERTICAL
        ]
        [ if not state.props.rideRequestFlow then homeScreenTopIconView push state else emptyTextView state ]

----------------------------------------- UPDATED HOME SCREEN -------------------------------------------

homeScreenViewV2 :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
homeScreenViewV2 push state = 
  relativeLayout
    [ height $ V (screenHeight unit)
    , width $ V (screenWidth unit)
    ][ linearLayout
        [ height $ V ((screenHeight unit)/ 3)
        , width MATCH_PARENT
        , background state.data.config.homeScreen.primaryBackground 
        , padding $ (PaddingTop (safeMarginTop))
        ][] 
      , homescreenHeader push state
      , linearLayout 
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , margin $ MarginTop 20
          , orientation VERTICAL
          ][ coordinatorLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              ][ bottomSheetLayout
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , peakHeight $ if state.data.peekHeight == 0 || state.data.peekHeight == 500 then getPeekHeight state else state.data.peekHeight
                  , halfExpandedRatio 0.99
                  , sheetState state.props.homeScreenSheetState
                  , enableShift false
                  , onSlide 
                    ( \action -> do
                        case action of 
                          Scroll val -> if ((state.props.currSlideIndex < truncate 1 val) && not state.props.isHomescreenExpanded) || ((state.props.currSlideIndex > truncate 1 val) && state.props.isHomescreenExpanded) then push (Scroll (truncate 1 val)) 
                                        else pure unit 
                          _ -> push action
                        pure unit
                        )
                    (Scroll)
                  ][ relativeLayout
                      [ width MATCH_PARENT
                      , height MATCH_PARENT
                      , orientation VERTICAL
                      , clipChildren false
                      ][ linearLayout
                          [ height MATCH_PARENT
                          , width WRAP_CONTENT
                          , background Color.white900
                          , margin $ MarginTop 32 
                          , padding $ PaddingTop 30 
                          , stroke if state.data.config.homeScreen.header.showSeparator then "1," <> Color.borderGreyColor else "0," <> Color.borderGreyColor
                          , gradient if os == "IOS" then (Linear 270.0 [Color.white900 , Color.grey700]) else (Linear 180.0 [Color.white900 , Color.grey700])
                          ][ scrollView
                              [ height $ if os == "IOS" then (V (getHeightFromPercent 90)) else MATCH_PARENT
                              , width MATCH_PARENT
                              , padding $ PaddingBottom 70
                              , nestedScrollView true
                              ][ linearLayout
                                  [ width $ V (screenWidth unit)
                                  , height WRAP_CONTENT
                                  , orientation VERTICAL
                                  ] $ [savedLocationsView state push] <> 
                                    (if not state.props.isSrcServiceable && state.props.currentStage == HomeScreen then
                                      [locationUnserviceableView push state]
                                    else 
                                      [if isHomeScreenView state then mapView push state "CustomerHomeScreenMap" else emptyTextView state
                                      , if state.data.config.feature.enableZooTicketBookingFlow
                                          then zooTicketBookingBanner state push 
                                          else linearLayout[visibility GONE][]
                                      , shimmerView state
                                      , suggestionsView push state
                                      , emptySuggestionsBanner state push
                                      , footerView push state])
                              ]
                          ]
                        , whereToButtonView push state
                      ]
                    ]  
                ]
            ]
      ]

isHomeScreenView :: HomeScreenState -> Boolean
isHomeScreenView state = state.props.currentStage == HomeScreen

footerView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
footerView push state = 
  linearLayout  
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding $ Padding 24 5 24 30
    , gravity CENTER
    , accessibilityHint $  getString BOOK_AND_MOVE <>  getString ANYWHERE_IN_THE_CITY
    ][
       textView $ 
        [ text $ getString BOOK_AND_MOVE
        , color Color.black700
        , gravity CENTER
        ]  <> FontStyle.h1 TypoGraphy
      , textView $ 
        [ text $ getString ANYWHERE_IN_THE_CITY
        , gravity CENTER
        , color Color.black700
        ] <> FontStyle.h1 TypoGraphy

      , linearLayout
        [ height $ V 2
        , width MATCH_PARENT
        , background Color.grey800
        , margin $ MarginVertical 24 24
        ][]
      , linearLayout  
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER_VERTICAL
          , padding $ Padding 16 15 16 15
          , stroke $ "1," <> Color.grey900
          , cornerRadii $ Corners 6.0 true true true true
          , onClick push $ const OpenLiveDashboard
          , visibility if state.data.config.dashboard.enable then VISIBLE else GONE
          ][
            imageView
              [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_live_stats"
              , height $ V 20
              , width $ V 20
              , margin $ MarginRight 7
              ]
            , textView
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ getString CHECKOUT_OUR_LIVE_STATS 
                , color Color.blue900
                , textSize FontSize.a_16
                , gravity CENTER_VERTICAL
                ]
          ]
      , textView $
        [ text $ getString $ MOST_LOVED_APP "MOST_LOVED_APP"
        , gravity CENTER
        , color Color.black600
        , margin $ MarginTop 16
        ] <> FontStyle.body1 TypoGraphy
    ]

homescreenHeader :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
homescreenHeader push state = 
  linearLayout 
    [height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingTop safeMarginTop
    , id $ getNewIDWithTag "homescreenHeader"
    , afterRender push $ const UpdatePeekHeight
    ][ pickupLocationView push state]

whereToButtonView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
whereToButtonView push state  =
  let whereToButtonConfig = state.data.config.homeScreen.whereToButton
  in
  linearLayout  
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , background Color.white900
      , margin $ getMarginFromConfig whereToButtonConfig.margin 
      , cornerRadii $ Corners 8.0 true true true true
      , shadow $ getShadowFromConfig whereToButtonConfig.shadow
      ][
        PrestoAnim.animationSet
        [ Anim.fadeIn  ( state.props.isHomescreenExpanded) 
        , Anim.fadeOut (not state.props.isHomescreenExpanded) 
        ] $ 
        linearLayout
          [ width WRAP_CONTENT
          , height MATCH_PARENT
          , padding $ Padding 16 16 0 16
          , gravity CENTER_VERTICAL
          , accessibilityHint "Menu Button"
          , visibility $ boolToVisibility state.props.isHomescreenExpanded
          , onClick push $ const OpenSettings
          , rippleColor Color.rippleShade
          ][ imageView
            [ height $ V 20
            , width $ if state.props.isHomescreenExpanded then V 20 else V 0
            , gravity CENTER_VERTICAL
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_menu_dark"
            ]
          , linearLayout
                [ width $ V 1
                , height $ V 20
                , margin $ if state.props.isHomescreenExpanded then MarginLeft 12 else MarginLeft 0 
                , gravity CENTER_VERTICAL
                , background Color.grey900
                ][]
          ] 
          
        , PrestoAnim.animationSet
          [ translateInXAnim
              (if os == "IOS" 
                then
                  animConfig
                        { fromX = 20
                        , toX = 0
                        , duration = 300
                        , ifAnim = state.props.isHomescreenExpanded 
                        }
                else
                  animConfig
                    { fromX =  10
                    , toX =  0
                    , duration = 300
                    , ifAnim = not state.props.isHomescreenExpanded 
                    })
          , translateOutXAnim
              (if os == "IOS" 
                then
                  animConfig
                        { toX =  25
                        , duration = 300
                        , ifAnim = state.props.isHomescreenExpanded 
                        }
                else
                  animConfig
                        { toX = 10
                        , duration = 300
                        , ifAnim = state.props.isHomescreenExpanded 
                        })
          ]
          $ linearLayout
          [ height WRAP_CONTENT
          , weight 1.0
          , onClick push $ const WhereToClick
          , padding $ Padding (if state.props.isHomescreenExpanded then 0 else 16) 16 16 16
          , gravity CENTER_VERTICAL
          , accessibilityHint "Where To Button"
          , clickable $ state.props.isSrcServiceable
          , alpha if state.props.isSrcServiceable then 1.0 else 0.4 
          ][ 
            imageView
              [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_destination"
              , height $ V 20
              , width $ V 20
              , margin (Margin 5 5 5 5)
              , accessibility DISABLE
              , onClick push $ if state.props.isSrcServiceable then (const $ OpenSearchLocation) else (const $ NoAction)
              , gravity BOTTOM
              ]
            , textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ getString WHERE_TO
                , color Color.black900
                ] <> FontStyle.subHeading1 TypoGraphy

          ]
      ]
    where
      getMarginFromConfig :: MarginConfig -> Margin
      getMarginFromConfig marginConfig = 
        Margin marginConfig.left marginConfig.top marginConfig.right marginConfig.bottom

      getShadowFromConfig :: ShadowConfig -> Shadow
      getShadowFromConfig shadowConfig = 
        Shadow shadowConfig.x shadowConfig.y shadowConfig.blur shadowConfig.spread shadowConfig.color shadowConfig.opacity


pickupLocationView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
pickupLocationView push state = 
  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , padding (Padding 16 20 16 16)
      ][
        PrestoAnim.animationSet
        [ Anim.fadeIn  (not state.props.isHomescreenExpanded) 
        , Anim.fadeOut state.props.isHomescreenExpanded 
        ] $ 
        linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation HORIZONTAL
          , margin $ MarginBottom 8
          , gravity CENTER
          ][
            linearLayout
              [ width WRAP_CONTENT 
              , height WRAP_CONTENT
              , gravity CENTER_VERTICAL
              , disableClickFeedback true
              , clickable $ not (state.props.currentStage == SearchLocationModel)
              , visibility if not state.data.config.terminateBtnConfig.visibility then GONE
                           else if state.props.isHomescreenExpanded then INVISIBLE 
                           else VISIBLE
              , onClick push (const TerminateApp)
              , margin $ MarginRight 8
              , padding $ Padding 8 8 8 8 
              , background $ state.data.config.terminateBtnConfig.backgroundColor
              , cornerRadius 8.0
              ]
              [ imageView
                  [ imageWithFallback state.data.config.terminateBtnConfig.imageUrl
                  , height $ V 23
                  , width $ V 23
                  , visibility $ boolToVisibility state.data.config.terminateBtnConfig.visibility
                  ]
              ]
          , linearLayout
              [ width WRAP_CONTENT 
              , height WRAP_CONTENT
              , gravity CENTER_VERTICAL
              , disableClickFeedback true
              , clickable $ not (state.props.currentStage == SearchLocationModel)
              , visibility if state.props.isHomescreenExpanded then INVISIBLE else VISIBLE
              , onClick push $ const OpenSettings
              , padding $ Padding 8 8 8 8 
              , background $ state.data.config.homeScreen.header.menuButtonBackground
              , cornerRadius 8.0
              ]
              [ imageView
                  [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_menu"
                  , height $ V 23
                  , width $ V 23
                  , accessibility if state.props.emergencyHelpModal || state.props.currentStage == ChatWithDriver || state.props.isCancelRide || state.props.isLocationTracking || state.props.callSupportPopUp || state.props.cancelSearchCallDriver then DISABLE else ENABLE
                  , accessibilityHint "Navigation : Button"
                  ]
              ] 
          
            , linearLayout
                [ height WRAP_CONTENT
                , weight 1.0
                , gravity CENTER_VERTICAL
                , layoutGravity "center_vertical"
                , visibility if state.props.isHomescreenExpanded then INVISIBLE else VISIBLE
                ][ imageView
                    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_logo_light"
                    , height $ V 50
                    , width $ V 110
                    , margin $ MarginHorizontal 10 10
                    , visibility $ if state.data.config.homeScreen.header.showLogo then VISIBLE else GONE
                    ]
                  , textView $
                    [ text $ getString BOOK_YOUR_RIDE
                    , color $ state.data.config.homeScreen.header.titleColor
                    , width MATCH_PARENT
                    , gravity CENTER
                    , visibility $ if state.data.config.homeScreen.header.showLogo then GONE else VISIBLE
                    ] <> FontStyle.h3 TypoGraphy
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , background Color.transparentBlue
                , padding $ Padding 12 8 12 8
                , gravity CENTER_VERTICAL
                , cornerRadius 8.0
                , layoutGravity "center_vertical"
                $ boolToVisibility $ not $ (not state.data.config.feature.enableReferral) || ((state.props.isReferred && state.props.currentStage == RideStarted) || state.props.hasTakenRide || state.props.currentSheetState == EXPANDED)
                , onClick push $ const $ if state.props.isReferred then ReferralFlowNoAction else ReferralFlowAction
                ][ textView
                    [ text $ if not state.props.isReferred then  getString HAVE_A_REFFERAL else (getString REFERRAL_CODE_APPLIED)
                    , color Color.blue800
                    , textSize FontSize.a_14  
                    ]
                ]
            ]
        , 
        PrestoAnim.animationSet
        [ Anim.fadeIn  (not state.props.isHomescreenExpanded) 
        , Anim.fadeOut state.props.isHomescreenExpanded 
        ] $ 
        linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , background $ state.data.config.homeScreen.pickUpViewColor
            , padding $ Padding 16 7 16 7
            , cornerRadii $ Corners 8.0 true true true true
            , gravity CENTER_VERTICAL
            , visibility if state.props.isHomescreenExpanded then INVISIBLE else VISIBLE
            , onClick push $ if state.props.isSrcServiceable then (const $ OpenSearchLocation) else (const $ NoAction)
            ][ imageView
                  [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_green_circle"
                  , height $ V 12
                  , width $ V 12
                  , margin (Margin 10 5 9 5)
                  , accessibility DISABLE
                  , onClick push $ if state.props.isSrcServiceable then (const $ OpenSearchLocation) else (const $ NoAction)
                  , gravity BOTTOM
                  ]
              , textView $
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text $ (getString PICKUP_) <> ( if state.props.isSrcServiceable then (getString CURRENT_LOCATION) else getString NOT_SERVICEABLE)
                  , color state.data.config.homeScreen.pickupLocationTextColor
                  ] <> FontStyle.paragraphText TypoGraphy
            ]
        ]

mapView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> String -> PrestoDOM (Effect Unit) w
mapView push state idTag = 
  let mapDimensions = getMapDimensions state
  in
  PrestoAnim.animationSet [ fadeInWithDelay 250 true ] $
  relativeLayout
    [ height mapDimensions.height
    , width mapDimensions.width 
    , margin if state.props.currentStage == HomeScreen then (Margin 16 16 16 0) else (Margin 0 0 0 0)
    , onAnimationEnd
            ( \action -> do
                _ <- push action
                _ <- getCurrentPosition push CurrentLocation
                _ <- showMap (getNewIDWithTag idTag) isCurrentLocationEnabled "satellite" zoomLevel push MAPREADY
                if state.props.openChatScreen && state.props.currentStage == RideAccepted then push OpenChatScreen
                else pure unit
                case state.props.currentStage of
                  HomeScreen -> if ((getSearchType unit) == "direct_search") then push DirectSearch else pure unit
                  _ -> pure unit
            )
            (const MapReadyAction)
    ][ linearLayout
        [ height  $ mapDimensions.height
        , width $ mapDimensions.width 
        , accessibility DISABLE_DESCENDANT
        , id (getNewIDWithTag idTag)
        , visibility if state.props.isSrcServiceable then VISIBLE else GONE
        , cornerRadius if state.props.currentStage == HomeScreen then 16.0 else 0.0
        , clickable $ not isHomeScreenView state 
        ][]
     , linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , alignParentBottom "true,-1"
        , gravity RIGHT
        , padding $ Padding 0 0 16 16
        , visibility $ if isHomeScreenView state then VISIBLE else GONE
        ][ imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_recenter_btn"
            , accessibility DISABLE
            , onClick
                ( \action -> do
                    _ <- push action
                    _ <- getCurrentPosition push UpdateCurrentLocation
                    _ <- pure $ logEvent state.data.logField "ny_user_recenter_btn_click"
                    pure unit
                )
                (const $ RecenterCurrentLocation)
            , height $ V 40
            , width $ V 40
            ]

        ]
    ]
getMapDimensions :: HomeScreenState -> {height :: Length, width :: Length}
getMapDimensions state = 
  let mapHeight = if (any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithDriver ] && os /= "IOS") then 
                    getMapHeight state
                  else if ( (suggestionViewVisibility state) && (isBannerVisible state)|| suggestionViewVisibility state) then 
                    V (getHeightFromPercent 27)
                  else if (isHomeScreenView state) then
                    V (getHeightFromPercent 45)
                  else
                    MATCH_PARENT
      mapWidth =  if state.props.currentStage /= HomeScreen then MATCH_PARENT else V ((screenWidth unit)-32)
  in {height : mapHeight, width : mapWidth}

suggestionsView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
suggestionsView push state = 
  linearLayout 
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $ PaddingBottom 10
  , margin $ Margin 8 14 8 0
  , visibility $ boolToVisibility $ suggestionViewVisibility state && not (state.props.showShimmer && null state.data.tripSuggestions)
  ]
  [ let isTripSuggestionsEmpty = null state.data.tripSuggestions
    in linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity CENTER_VERTICAL
      ][ textView $
        [ height MATCH_PARENT
        , width WRAP_CONTENT
        , text if isTripSuggestionsEmpty then getString SUGGESTED_DESTINATION else getString RECENT_RIDES
        , color Color.black800
        , gravity CENTER_VERTICAL
        , padding $ PaddingHorizontal 8 8 
        , accessibilityHint 
          if isTripSuggestionsEmpty 
          then "Suggested Destinations" 
          else "Recent Rides"
        ] <> FontStyle.subHeading1 TypoGraphy
      , pillTagView 
          if isTripSuggestionsEmpty 
          then { text: getString SMART, image: "ny_ic_filled_stars" } 
          else { text: getString ONE_CLICK, image: "ny_ic_filled_lightning" }
      ]
  , textView $
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , text if null state.data.tripSuggestions then getString DISCOVER_AWESOME_SPOTS_TAILORED_JUST_FOR_YOU else getString ONE_CLICK_BOOKING_FOR_YOUR_FAVOURITE_JOURNEYS
    , color Color.black600
    , padding $ PaddingHorizontal 8 8 
    , accessibilityHint 
      if null state.data.tripSuggestions 
      then "Places you might like to go to." 
      else "One click booking for your favourite journeys!"
    , margin $ MarginBottom 7
    ] <> FontStyle.body3 TypoGraphy
  , if null state.data.tripSuggestions 
    then suggestedLocationCardView push state
    else repeatRideCardParentView push state 
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , visibility $
        if length state.data.tripSuggestions > 2 
        then VISIBLE 
        else if not $ null state.data.tripSuggestions 
          then GONE
          else if length state.data.destinationSuggestions > 2 
            then VISIBLE
            else GONE
    , padding $ PaddingVertical 10 6
    , gravity CENTER_HORIZONTAL
    , onClick push $ const ShowMoreSuggestions
    ]
    [ textView $ 
      [ text if state.props.suggestionsListExpanded then getString VIEW_LESS else getString VIEW_MORE
      , color Color.blue900
      ] <> FontStyle.tags TypoGraphy
    ]
  ]

shimmerView :: forall w. HomeScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  shimmerFrameLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.transparent
    , visibility $ boolToVisibility $ state.props.showShimmer && null state.data.tripSuggestions
    ] 
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ width $ V 200
            , height $ V 20
            , margin $ Margin 16 10 16 0
            , background Color.greyDark
            ]
            []
        , linearLayout
            [ width $ V 300
            , height $ V 20
            , margin $ Margin 16 7 16 0
            , background Color.greyDark
            ]
            []
        , linearLayout
            [ width MATCH_PARENT
            , height $ V 80
            , margin $ Margin 16 16 16 10
            , cornerRadius 8.0
            , background Color.greyDark
            ]
            []
        , linearLayout
            [ width MATCH_PARENT
            , height $ V 80
            , margin $ MarginHorizontal 16 16 
            , cornerRadius 8.0
            , background Color.greyDark
            ]
            []
        ]
    ]

movingRightArrowView :: forall w. String -> PrestoDOM (Effect Unit) w
movingRightArrowView viewId =
  lottieAnimationView
      [ id (getNewIDWithTag viewId)
      , afterRender (\action-> do
                    void $ pure $ startLottieProcess lottieAnimationConfig{ rawJson =  (getAssetsBaseUrl FunctionCall) <> "lottie/right_arrow.json" , speed = 1.0,lottieId = (getNewIDWithTag viewId) }
                    pure unit)(const NoAction)
      , height $ V 40
      , width $ V 40
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_yellow_arrow"
      , gravity CENTER_HORIZONTAL
      , accessibility DISABLE
      ]
      
suggestedLocationCardView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
suggestedLocationCardView push state = 
  let takeValue = if state.props.suggestionsListExpanded then state.data.config.suggestedTripsAndLocationConfig.maxLocationsToBeShown else state.data.config.suggestedTripsAndLocationConfig.minLocationsToBeShown
  in
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , clipChildren false
    ]( mapWithIndex ( \index item -> suggestedDestinationCard push state index item ) (take takeValue state.data.destinationSuggestions))

suggestedDestinationCard ::  forall w. (Action -> Effect Unit) -> HomeScreenState ->Int -> LocationListItemState -> PrestoDOM (Effect Unit) w
suggestedDestinationCard push state index suggestion = 
  PrestoAnim.animationSet
    [ Anim.fadeIn true] $
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , stroke $ "1,"<> Color.grey800
    , margin $ Margin 8 8 8 8
    , shadow $ Shadow 0.1 0.1 7.0 24.0 Color.greyBackDarkColor 0.5 
    , padding $ Padding 16 16 16 16
    , background Color.white900
    , gravity CENTER_VERTICAL
    , cornerRadius 16.0
    , onClick push $ const (SuggestedDestinationClicked suggestion)
    ][ linearLayout
        [ height $ V 26
        , width $ V 26
        , gravity CENTER
        , padding (Padding 3 3 3 3)
        , margin $ MarginRight 4
        ][ imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_sd"
            , height $ V 20
            , width $ V 20
            ]
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        , margin $ MarginRight 24
        , orientation VERTICAL
        ][ textView $
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text suggestion.title
            , color Color.black800
            , ellipsize true
            , margin $ MarginBottom 5
            , singleLine true
            ] <> FontStyle.body1 TypoGraphy
          , textView $
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text suggestion.subTitle
            , color Color.black700
            , ellipsize true
            , singleLine true
            ] <> FontStyle.body3 TypoGraphy
        ]
      , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation VERTICAL
          , layoutGravity "center_vertical"
          , gravity CENTER
          , background Color.yellow900
          , cornerRadius if os == "IOS" then 20.0 else 22.5
          ][ movingRightArrowView ("movingArrowView" <> show index)]
    ]

repeatRideCardParentView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
repeatRideCardParentView push state = 
  let takeValue = if state.props.suggestionsListExpanded then state.data.config.suggestedTripsAndLocationConfig.maxTripsToBeShown else state.data.config.suggestedTripsAndLocationConfig.minTripsToBeShown
  in
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , clipChildren false
    ]( mapWithIndex ( \index item -> repeatRideCard push state index item ) (take takeValue state.data.tripSuggestions))


repeatRideCard :: forall w. (Action -> Effect Unit) -> HomeScreenState ->Int -> Trip -> PrestoDOM (Effect Unit) w
repeatRideCard push state index trip = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , stroke $ "1,"<> Color.grey800
    , margin $ Margin 8 8 8 8
    , shadow $ Shadow 0.1 0.1 7.0 24.0 Color.greyBackDarkColor 0.5 
    , padding $ Padding 16 16 16 16
    , background Color.white900
    , gravity CENTER_VERTICAL
    , cornerRadii $ Corners 16.0 true true true true
    , onClick push $ const (RepeatRide index trip)
    ][ linearLayout
        [ height $ V 26
        , width $ V 26
        , gravity CENTER
        , padding (Padding 3 3 3 3)
        , margin $ MarginRight 4
        ][ imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_repeat_trip"
            , height $ V 20
            , width $ V 20
            ]
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        , orientation VERTICAL
        , margin $ MarginRight 24
        ][ textView $
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text $ getTripTitle trip.destination 
            , color Color.black800
            , ellipsize true
            , margin $ MarginBottom 5
            , singleLine true
            ] <> FontStyle.body1 TypoGraphy
          , textView $
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text $ getTripSubTitle trip.destination
            , color Color.black700
            , ellipsize true
            , singleLine true
            ] <> FontStyle.body3 TypoGraphy
        ]
      , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation VERTICAL
          , layoutGravity "center_vertical"
          , gravity CENTER
          , background Color.yellow900
          , cornerRadius if os == "IOS" then 20.0 else 22.5
          ][ movingRightArrowView ("movingArrowView" <> show index)]
    ]
  where
    getTripTitle :: String -> String
    getTripTitle destination = 
      maybe "" identity $ head $ DS.split (DS.Pattern ",") destination
    
    getTripSubTitle :: String -> String
    getTripSubTitle destination = 
      (DS.drop ((fromMaybe 0 (DS.indexOf (DS.Pattern ",") (destination))) + 2) (destination))

pillTagView :: forall w. {text :: String, image :: String} -> PrestoDOM (Effect Unit) w
pillTagView config = 
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , background Color.yellow800
    , cornerRadius 14.0
    , gravity CENTER
    , padding $ Padding 8 4 8 4
    ][  imageView
        [ height $ V 13
        , width $ V 13
        , margin $ MarginRight 4
        , imageWithFallback $ fetchImage FF_ASSET config.image
        ]
      , textView $
        [ text $ config.text
        , width WRAP_CONTENT
        , gravity CENTER
        , color Color.black900
        , height WRAP_CONTENT
        ] <> FontStyle.tags LanguageStyle
      ]

suggestionViewVisibility :: HomeScreenState -> Boolean
suggestionViewVisibility state =  ((length state.data.tripSuggestions  > 0 || length state.data.destinationSuggestions  > 0) && isHomeScreenView state)

isBannerVisible :: HomeScreenState -> Boolean
isBannerVisible state = getValueToLocalStore DISABILITY_UPDATED == "false" && state.data.config.showDisabilityBanner && isHomeScreenView state

reAllocateConfirmation :: forall action. (action -> Effect Unit) -> HomeScreenState -> action -> Number -> Flow GlobalState Unit
reAllocateConfirmation push state action duration = do
  when state.props.reAllocation.showPopUp $
    void $ delay $ Milliseconds duration
  doAff do liftEffect $ push $ action

updateMapPadding :: HomeScreenState -> Effect Unit
updateMapPadding state = 
  if state.props.currentStage /= HomeScreen then 
    void $ setMapPadding 0 0 0 0
  else 
    void $ setMapPadding 0 0 0 requiredViewHeight
  where
    recentViewHeight = (runFn1 getLayoutBounds (getNewIDWithTag "buttonLayout")).height + 200
    iosScale = runFn1 getPixels FunctionCall
    iosNativeScale = runFn1 getDefaultPixels ""
    displayZoomFactor = iosNativeScale / iosScale
    pixels = runFn1 getPixels FunctionCall
    density = (runFn1 getDeviceDefaultDensity FunctionCall) / defaultDensity
    requiredViewHeight = if os /= "IOS" 
                         then ceil ((toNumber recentViewHeight / pixels) * density) 
                         else ceil ((toNumber recentViewHeight / displayZoomFactor) / iosScale)



locationUnserviceableView ::  forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
locationUnserviceableView push state = 
  linearLayout[
    height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 17 24 17 0
  , clickable true
  , orientation VERTICAL
  , gravity CENTER
  ][
    relativeLayout[
      height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_HORIZONTAL
    , cornerRadius 16.0
    ][
      linearLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER
      ][
        imageView[
          imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_white_blur_map"
        , width WRAP_CONTENT
        , height $ WRAP_CONTENT
        , gravity CENTER
        ]
      ]
    , linearLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT 
      , orientation VERTICAL
      , gravity CENTER
      , margin $ Margin 22 83 22 83 
      , background Color.transparent
      ][
        imageView[
          imageWithFallback $ fetchImage FF_ASSET "ny_ic_location_unserviceable"
        , width $ V 108
        , height $ V 101
        ]
      , textView  $ [
          text $ getString LOCATION_UNSERVICEABLE
        , width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER 
        , color Color.black800
        , margin $ MarginTop 10
        ] <> (FontStyle.h1 TypoGraphy)
      , textView $ [
          text $ getString WE_ARE_NOT_LIVE_IN_YOUR_AREA
        , width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , color Color.black700
        , margin $ MarginTop 8
        ] <> (FontStyle.paragraphText TypoGraphy)
      ]
    ]
  , linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER
    ][
      textView $ [
        text $ getString FACING_PROBLEM_WITH_APP
      , color Color.black700
      ] <> (FontStyle.tags TypoGraphy)
    , textView $ [
        textFromHtml $ getString TAP_HERE_TO_REPORT 
      , color Color.blue900
      , margin $ MarginLeft 4
      , onClick push $ const ReportIssueClick
      ] <> (FontStyle.tags TypoGraphy)
    ]
  ]