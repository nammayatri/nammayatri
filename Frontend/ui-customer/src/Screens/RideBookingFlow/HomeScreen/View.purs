{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.View where

import Components.MessagingView.Common.Types
import Components.MessagingView.Common.View
import ConfigProvider
import Constants.Configs
import Data.FoldableWithIndex
import Data.Tuple
import Engineering.Helpers.BackTrack
import Locale.Utils
import Mobility.Prelude
import Mobility.Prelude
import PrestoDOM.Core
import PrestoDOM.List
import Screens.RideBookingFlow.HomeScreen.Config
import Services.API hiding (Followers(..))
import SuggestionUtils
import Accessor
import Accessor
import Timers
import Types.App
import Accessor (_lat, _lon, _selectedQuotes, _fareProductType, _contents, _stopLocation, _toLocation)
import Animation (fadeInWithDelay, fadeIn, fadeOut, translateYAnimFromTop, scaleAnim, translateYAnimFromTopWithAlpha, translateInXAnim, translateOutXAnim, translateInXForwardAnim, translateOutXBackwardAnimY, translateInXSidebarAnim, emptyScreenAnimation, fadeInWithDuration, fadeOutWithDuration, scaleYAnimWithDelay, shimmerAnimation)
import Animation as Anim
import Animation.Config (AnimConfig, animConfig)
import Animation.Config (Direction(..), translateFullYAnimWithDurationConfig, translateYAnimHomeConfig, messageInAnimConfig, messageOutAnimConfig)
import CarouselHolder as CarouselHolder
import Common.Types.App (LazyCheck(..), YoutubeData, CarouselData, City(..))
import Common.Types.App as CT
import Common.Types.App as CTP
import Components.Banner.Controller as BannerConfig
import Components.Banner.View as Banner
import Components.BannerCarousel as BannerCarousel
import Components.ChooseVehicle as ChooseVehicle
import Components.ChooseYourRide as ChooseYourRide
import Components.CommonComponentConfig as CommonComponentConfig
import Components.DriverInfoCard as DriverInfoCard
import Components.EmergencyHelp as EmergencyHelp
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import Components.ErrorModal as ErrorModal
import Components.FavouriteLocationModel as FavouriteLocationModel
import Components.LocationListItem.View as LocationListItem
import Components.LocationTagBar as LocationTagBar
import Components.LocationTagBarV2 as LocationTagBarV2
import Components.MenuButton as MenuButton
import Components.MessagingView as MessagingView
import Components.PopUpModal as PopUpModal
import Components.PopupWithCheckbox.View as PopupWithCheckbox
import Components.PricingTutorialModel as PricingTutorialModel
import Components.PrimaryButton as PrimaryButton
import Components.QuoteListModel.View as QuoteListModel
import Components.RateCard as RateCard
import Components.Referral as ReferralComponent
import Components.DeliveryParcelImageAndOtp as DeliveryParcelImageAndOtp
import Components.RequestInfoCard as RequestInfoCard
import Components.RideCompletedCard as RideCompletedCard
import Components.SaveFavouriteCard as SaveFavouriteCard
import Components.SearchLocationModel as SearchLocationModel
import Components.SelectListModal as CancelRidePopUp
import Components.ServiceTierCard.View as ServiceTierCard
import Components.SettingSideBar as SettingSideBar
import Components.SourceToDestination as SourceToDestination
import Constants (defaultDensity)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (any, length, mapWithIndex, take, (!!), head, filter, cons, null, tail, drop, elem)
import Data.Array as Arr
import Data.Either (Either(..),either)
import Data.Function.Uncurried (runFn1)
import Data.Function.Uncurried (runFn1, runFn2)
import Data.Function.Uncurried (runFn3)
import Data.Int (ceil, floor, fromNumber, fromString, toNumber, pow)
import Data.Int as INT
import Data.Lens ((^.))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe, isNothing)
import Data.Number as NUM
import Data.String as DS
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Debug (spy)
import DecodeUtil (getAnyFromWindow, removeFromWindow)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1, runEffectFn2,runEffectFn3, runEffectFn9)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, liftFlow, os, safeMarginBottom, safeMarginTop, screenHeight, isPreviousVersion, screenWidth, camelCaseToSentenceCase, truncate, getExpiryTime, getDeviceHeight, getScreenPpi, safeMarginTopWithDefault, markPerformance, getValueFromIdMap, updatePushInIdMap, getCurrentUTC, convertUTCtoISC, compareUTCDate, getCurrentUTC, getMarkerCallback)
import Engineering.Helpers.Events as Events
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.Suggestions (getMessageFromKey, getSuggestionsfromKey, chatSuggestion, emChatSuggestion)
import Engineering.Helpers.Utils (showAndHideLoader, getCityFromString)
import Font.Size as FontSize
import Font.Style as FontStyle
import Halogen.VDom.DOM.Prop (Prop)
import Helpers.API as HelpersAPI
import Helpers.Pooling (delay)
import Helpers.SpecialZoneAndHotSpots (specialZoneTagConfig, zoneLabelIcon, findSpecialPickupZone, getConfirmLocationCategory)
import Helpers.Utils (fetchImage, FetchImageFrom(..), decodeError, fetchAndUpdateCurrentLocation, getAssetsBaseUrl, getCurrentLocationMarker, getLocationName, getNewTrackingId, getSearchType, parseFloat, storeCallBackCustomer, didReceiverMessage, getPixels, getDefaultPixels, getDeviceDefaultDensity, getCityConfig, getVehicleVariantImage, getImageBasedOnCity, getDefaultPixelSize, getVehicleVariantImage, getRouteMarkers, TrackingType(..), getDistanceBwCordinates, decodeBookingTimeList, disableChat, isParentView)
import JBridge (showMarker, animateCamera, reallocateMapFragment, clearChatMessages, drawRoute, enableMyLocation, firebaseLogEvent, generateSessionId, getArray, getCurrentPosition, getExtendedPath, getHeightFromPercent, getLayoutBounds, initialWebViewSetUp, isCoordOnPath, isInternetAvailable, isMockLocation, lottieAnimationConfig, removeAllPolylines, removeMarker, requestKeyboardShow, scrollOnResume, showMap, startChatListenerService, startLottieProcess, stopChatListenerService, storeCallBackMessageUpdated, storeCallBackOpenChatScreen, storeKeyBoardCallback, updateRoute, addCarousel, updateRouteConfig, addCarouselWithVideoExists, storeCallBackLocateOnMap, storeOnResumeCallback, setMapPadding, getKeyInSharedPrefKeys, locateOnMap, locateOnMapConfig, defaultMarkerConfig, currentPosition, defaultMarkerImageConfig, defaultActionImageConfig, differenceBetweenTwoUTCInMinutes, ActionImageConfig(..), handleLocateOnMapCallback, differenceBetweenTwoUTC, mkRouteConfig)
import JBridge as JB
import Engineering.Helpers.Utils as EHU
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import LocalStorage.Cache (getValueFromCache)
import Log (printLog, logStatus)
import MerchantConfig.Types (MarginConfig, ShadowConfig)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (Unit, bind, const, discard, map, negate, not, pure, show, unit, void, when, ($), (&&), (*), (+), (-), (/), (/=), (<), (<<<), (<=), (<>), (==), (>), (||), (<$>), identity, (>=), (<*>))
import Presto.Core.Types.API (ErrorResponse)
import Presto.Core.Types.Language.Flow (Flow, doAff, modifyState, getState)
import PrestoDOM (BottomSheetState(..), Gradient(..), Gravity(..), Length(..), Accessiblity(..), Margin(..), Accessiblity(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Shadow(..), scrollBarY, adjustViewWithKeyboard, afterRender, alignParentBottom, background, clickable, color, cornerRadius, disableClickFeedback, ellipsize, fontStyle, frameLayout, gradient, gravity, halfExpandedRatio, height, id, imageView, imageWithFallback, lineHeight, linearLayout, lottieAnimationView, margin, maxLines, onBackPressed, onClick, orientation, padding, peakHeight, relativeLayout, scaleType, singleLine, stroke, text, textFromHtml, textSize, textView, url, visibility, webView, weight, width, layoutGravity, accessibilityHint, accessibility, accessibilityFocusable, focusable, scrollView, onAnimationEnd, clipChildren, enableShift, horizontalScrollView, shadow, onStateChanged, scrollBarX, clipToPadding, onSlide, rotation, rippleColor, shimmerFrameLayout, imageUrl)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Properties (cornerRadii, sheetState, alpha, nestedScrollView)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Resources.LocalizableV2.Strings (getEN, getStringV2)
import Screens.AddNewAddressScreen.Controller as AddNewAddress
import Screens.HomeScreen.Controller ( checkCurrentLocation, checkSavedLocations, dummySelectedQuotes, eval2, flowWithoutOffers, getPeekHeight, checkRecentRideVariant, findingQuotesSearchExpired)
import Screens.HomeScreen.PopUpConfigs as PopUpConfigs
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.HomeScreen.Transformer (transformSavedLocations, getActiveBooking, getDriverInfo, getFormattedContacts, getFareProductType, formatContacts)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.ContactsList (contactCardView)
import Screens.RideBookingFlow.HomeScreen.BannerConfig (getBannerConfigs)
import Screens.Types (FareProductType(..)) as FPT
import Screens.Types (Followers(..), CallType(..), HomeScreenState, LocationListItemState, PopupType(..), SearchLocationModelType(..), SearchResultType(..), Stage(..), ZoneType(..), SheetState(..), Trip(..), SuggestionsMap(..), Suggestions(..), BottomNavBarIcon(..), NewContacts, ReferralStatus(..), VehicleViewType(..))
import Screens.Types as ST
import Services.API (GetDriverLocationResp(..), GetQuotesRes(..), GetRouteResp(..), LatLong(..), RideAPIEntity(..), RideBookingRes(..), Route(..), SavedLocationsListRes(..), SearchReqLocationAPIEntity(..), SelectListRes(..), Snapped(..), GetPlaceNameResp(..), PlaceName(..), RideBookingListRes(..))
import Services.Backend (getDriverLocation, getQuotes, getRoute, makeGetRouteReq, rideBooking,ridebookingStatus, selectList, walkCoordinates, walkCoordinate, getSavedLocationList)
import Services.Backend as Remote
import Services.FlowCache as FlowCache
import Storage (KeyStore(..), getValueToLocalStore, isLocalStageOn, setValueToLocalStore, updateLocalStage, getValueToLocalNativeStore)
import Styles.Colors as Color
import Types.App (FlowBT, GlobalState(..), defaultGlobalState)
import Halogen.VDom.DOM.Prop (Prop)
import Data.String as DS
import Data.Function.Uncurried (runFn1, runFn2)
import Components.CommonComponentConfig as CommonComponentConfig
import Constants.Configs
import Common.Resources.Constants (zoomLevel, chatService)
import Resources.Constants (markerArrowSize)
import Constants (defaultDensity)
import Resources.Constants (getEditDestPollingCount)
import RemoteConfig as RemoteConfig
import RemoteConfig.Utils (getCustomerVoipConfig)
import Animation as Anim
import Animation.Config (AnimConfig, animConfig)
import Components.SourceToDestination as SourceToDestination
import Data.Map as Map
import SuggestionUtils
import MerchantConfig.Types (MarginConfig, ShadowConfig)
import Mobility.Prelude
import Timers
import PrestoDOM.Core
import Locale.Utils
import CarouselHolder as CarouselHolder
import PrestoDOM.List
import Components.BannerCarousel as BannerCarousel
import Components.MessagingView.Common.Types
import Components.MessagingView.Common.View
import Data.FoldableWithIndex
import Engineering.Helpers.BackTrack (liftFlowBT)
import LocalStorage.Cache (getValueFromCache)
import Components.PopupWithCheckbox.View as PopupWithCheckbox
import Services.FlowCache as FlowCache
import Engineering.Helpers.BackTrack
import Engineering.Helpers.Events as Events
import Types.App
import Mobility.Prelude
import Screens.Types as ST
import Helpers.SpecialZoneAndHotSpots (specialZoneTagConfig, zoneLabelIcon, findSpecialPickupZone, getConfirmLocationCategory)
import Components.ServiceTierCard.View as ServiceTierCard
import Common.Types.App as CTP
import JBridge as JB
import Common.Types.App as CT
import Effect.Unsafe (unsafePerformEffect)
import Screens.Types (FareProductType(..)) as FPT
import Helpers.Utils (decodeBookingTimeList, getLanguageBasedCityName)
import Resources.LocalizableV2.Strings (getEN)
import Screens.HomeScreen.PopUpConfigs as PopUpConfigs
import Screens.HomeScreen.Controllers.Types
import Helpers.Utils as HU
import Screens.NammaSafetyFlow.Components.SafetyUtils as SU
import Screens.HomeScreen.ScreenData (dummyNewContacts)
import Engineering.Helpers.Commons as EHC
import Resources.LocalizableV2.Types

screen :: HomeScreenState -> Screen Action HomeScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "HomeScreen"
  , globalEvents:
       [ (\push -> do
          if initialState.props.currentStage == HomeScreen
            then do
              let currLocation = runFn3 getAnyFromWindow "current_location" Nothing Just
                  _ = removeFromWindow "current_location"
              case currLocation of
                Just loc -> push $ UpdateCurrentLocation loc.lat loc.lon
                Nothing -> do
                  if (initialState.props.sourceLat == 0.0 && initialState.props.sourceLong == 0.0)
                    then startTimer initialState.props.shimmerViewTimer "shimmerTimer" "1" push ShimmerTimer
                    else pure unit
            else pure unit
          pure $ runEffectFn1 clearTimerWithIdEffect "shimmerTimer")
      , ( \push -> do
            _ <- pure $ printLog "storeCallBackCustomer initially" "."
            _ <- pure $ printLog "storeCallBackCustomer callbackInitiated" initialState.props.callbackInitiated
            -- push NewUser -- TODO :: Handle the functionality
            _ <- if initialState.data.config.enableMockLocation then isMockLocation push IsMockLocation else pure unit
            _ <- launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ checkForLatLongInSavedLocations push UpdateSavedLoc initialState
            when (initialState.props.followsRide && isNothing initialState.data.followers) $ void $ launchAff $ flowRunner defaultGlobalState $ getFollowRide push UpdateFollowers
            when initialState.props.showDeliveryImageAndOtpModal $ void $ launchAff $ flowRunner defaultGlobalState $ do
              doAff do liftEffect $ push $ RefreshDelveryParcelImage
            if (not initialState.props.callbackInitiated) then do
              _ <- pure $ printLog "storeCallBackCustomer initiateCallback" "."
              _ <- storeCallBackCustomer push NotificationListener "HomeScreen" Just Nothing
              _ <- pure $ runFn2 storeOnResumeCallback push OnResumeCallback
              _ <- runEffectFn3 JB.storeCallBackInternetAction push InternetCallBackCustomer "HomeScreen"
              _ <- runEffectFn2 JB.storeNoInternetAction push UpdateNoInternet
              _ <- runEffectFn2 storeKeyBoardCallback push KeyboardCallback
              push HandleCallback
            else do
              pure unit
            when (isNothing initialState.data.bannerData.bannerItem) $ void $ launchAff $ flowRunner defaultGlobalState $ computeListItem push
            when (isNothing initialState.data.rideCompletedData.issueReportData.bannerItem) $ void $ launchAff $ flowRunner defaultGlobalState $  computeIssueReportBanners push
            case initialState.props.currentStage of
              SearchLocationModel -> case initialState.props.isSearchLocation of
                LocateOnMap -> do
                  void $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreen")
                  void $ runEffectFn2 storeCallBackLocateOnMap (\key lat lon -> push $ LocateOnMapCallBack key lat lon) (handleLocateOnMapCallback "HomeScreen")
                  pure unit
                _ -> do
                  case initialState.props.isSource of
                    Just index -> do
                      _ <- pure $ requestKeyboardShow (if index then (getNewIDWithTag "SourceEditText") else (getNewIDWithTag "DestinationEditText"))
                      pure unit
                    Nothing -> pure unit
                  pure unit
              EditingDestinationLoc -> case initialState.props.isSearchLocation of
                LocateOnMap -> do
                  void $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenEditDest")
                  void $ runEffectFn2 storeCallBackLocateOnMap (\key lat lon -> push $ LocateOnMapCallBack key lat lon) (handleLocateOnMapCallback "HomeScreen")
                  pure unit
                _ -> pure unit
              FindingEstimate -> do
                logStatus "find_estimate" ("searchId : " <> initialState.props.searchId)
                void $ pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                void $ pure $ setValueToLocalStore BOOSTED_SEARCH "false"
                estimatesPolling <- runEffectFn1 getValueFromIdMap "EstimatePolling"

                if initialState.data.currentCityConfig.iopConfig.enable then do
                  when (not $ getValueToLocalStore STARTED_ESTIMATE_SEARCH == "TRUE") do -- Check if estimate search is already started
                    void $ pure $ setValueToLocalStore STARTED_ESTIMATE_SEARCH "TRUE"
                    void $ launchAff $ flowRunner defaultGlobalState $ getEstimatePolling (getValueToLocalStore TRACKING_ID) GetEstimates CheckFlowStatusAction 3 2000.0 push initialState
                else if estimatesPolling.shouldPush then do
                     void $ launchAff $ flowRunner defaultGlobalState $ getEstimate GetEstimates CheckFlowStatusAction 40 1000.0 push initialState estimatesPolling.id
                else pure unit
              FindingQuotes -> do
                when ((getValueToLocalStore FINDING_QUOTES_POLLING) == "false") $ do
                  void $ pure $ setValueToLocalStore FINDING_QUOTES_POLLING "true"
                  void $ pure $ setValueToLocalStore LOCAL_STAGE (show FindingQuotes)
                  void $ pure $ setValueToLocalStore AUTO_SELECTING "false"
                  void $ startTimer initialState.props.searchExpire "findingQuotes" "1" push SearchExpireCountDown
                  void $ pure $ setValueToLocalStore GOT_ONE_QUOTE "FALSE"
                  void $ pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
                  let pollingCount = ceil ((toNumber initialState.props.searchExpire)/((fromMaybe 0.0 (NUM.fromString (getValueToLocalStore TEST_POLLING_INTERVAL))) / 1000.0))
                  void $ launchAff $ flowRunner defaultGlobalState $ getQuotesPolling (getValueToLocalStore TRACKING_ID) GetQuotesList Restart pollingCount (fromMaybe 0.0 (NUM.fromString (getValueToLocalStore TEST_POLLING_INTERVAL))) push initialState
              ConfirmingRide -> do
                void $ pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
                void $ launchAff $ flowRunner defaultGlobalState $ confirmRide (getValueToLocalStore TRACKING_ID) GetRideConfirmation CheckFlowStatusAction GoToHomeScreen 5 3000.0 push initialState
              ConfirmingQuotes -> do
                when ((getValueToLocalStore CONFIRM_QUOTES_POLLING) == "false") $ do
                  void $ pure $ setValueToLocalStore CONFIRM_QUOTES_POLLING "true"
                  let pollingCount = ceil ((toNumber $ findingQuotesSearchExpired false false false)/((fromMaybe 0.0 (NUM.fromString (getValueToLocalStore CONFIRM_QUOTES_POLLING_COUNT))) / 1000.0))
                  void $ pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
                  void $ launchAff $ flowRunner defaultGlobalState $ confirmRide (getValueToLocalStore TRACKING_ID) GetRideConfirmation CheckFlowStatusAction GoToHomeScreen pollingCount 3000.0 push initialState
              HomeScreen -> do
                fetchAndUpdateCurrentLocation push UpdateLocAndLatLong RecenterCurrentLocation
                void $ pure $ setValueToLocalStore CONFIRM_QUOTES_POLLING_COUNT "100"
                let suggestionsMap = getSuggestionsMapFromLocal FunctionCall
                if (getValueToLocalStore UPDATE_REPEAT_TRIPS == "true" && Map.isEmpty suggestionsMap) then do
                  void $ launchAff $ flowRunner defaultGlobalState $ updateRecentTrips UpdateRepeatTrips push Nothing
                else pure unit
                when (isJust initialState.data.rideHistoryTrip) $ do
                  push $ RepeatRide 0 (fromMaybe HomeScreenData.dummyTrip initialState.data.rideHistoryTrip)
                _ <- pure $ setValueToLocalStore SESSION_ID (generateSessionId unit)
                _ <- pure $ removeAllPolylines ""
                _ <- pure $ enableMyLocation true
                _ <- pure $ setValueToLocalStore NOTIFIED_CUSTOMER "false"
                pure unit
                -- when (initialState.props.scheduledRidePollingDelay < 1800.0 && initialState.props.scheduledRidePollingDelay > 0.0) do
                --   _ <- launchAff $ flowRunner defaultGlobalState $ do
                --         doAff do liftEffect $ push $ StartScheduledRidePolling
                --   pure unit
                -- when (initialState.props.startScheduledRidePolling) do
                --   void $ pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
                --   let polling_count = ceil $ initialState.props.scheduledRidePollingDelay/(120.0)
                --   void $ launchAff $ flowRunner defaultGlobalState $ confirmRide (getValueToLocalStore TRACKING_ID) GetRideConfirmation CheckFlowStatusAction GoToHomeScreen polling_count 120000.0 push initialState
              SettingPrice -> do
                void $ pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                void $ pure $ setValueToLocalStore BOOSTED_SEARCH "false"

                let isRepeatRideVariantAvailable =  (isNothing initialState.props.repeatRideServiceTierName) || (checkRecentRideVariant initialState)
                if (initialState.props.isRepeatRide && isRepeatRideVariantAvailable && initialState.data.iopState.hasTopProviderEstimate )then do
                  -- check if already timer is running for repeat ride
                  let isBlindPerson = getValueToLocalStore DISABILITY_NAME == "BLIND_LOW_VISION"
                  when (DS.null initialState.props.repeatRideTimerId && initialState.props.repeateRideTimerStoped == false && not isBlindPerson) $  startTimer initialState.data.config.suggestedTripsAndLocationConfig.repeatRideTime "repeatRide" "1" push RepeatRideCountDown
                else if (initialState.props.isRepeatRide && not isRepeatRideVariantAvailable) then do
                    void $ pure $ EHU.showToast $ getString LAST_CHOSEN_VARIANT_NOT_AVAILABLE
                else pure unit
                when initialState.data.iopState.providerSelectionStage $
                  startTimer initialState.data.currentCityConfig.iopConfig.autoSelectTime "providerSelectionStage" "1" push ProviderAutoSelected
              PickUpFarFromCurrentLocation ->
                void $ pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
              RideAccepted -> do
                initVoipIfEnabled initialState
                when
                  (initialState.data.config.notifyRideConfirmationConfig.notify && any (_ == getValueToLocalStore NOTIFIED_CUSTOMER) ["false" , "__failed" , "(null)"])
                    $ startTimer 5 "notifyCustomer" "1" push NotifyDriverStatusCountDown
                _ <- pure $ enableMyLocation true
                if ((getValueToLocalStore DRIVER_ARRIVAL_ACTION) == "TRIGGER_WAITING_ACTION") then do
                  void $ waitingCountdownTimerV2 initialState.data.driverInfoCardState.driverArrivalTime "1" "countUpTimerId" push WaitingTimeAction
                else
                  when
                    (initialState.data.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE || initialState.props.isOtpRideFlow) $ do
                      let secondsLeft = initialState.data.config.driverInfoConfig.specialZoneQuoteExpirySeconds - (getExpiryTime initialState.data.driverInfoCardState.createdAt true)
                      void $ startTimer secondsLeft "SpecialZoneOTPExpiry" "1" push SpecialZoneOTPExpiryAction
                if ((getValueToLocalStore TRACKING_DRIVER) == "False") then do
                  _ <- pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                  _ <- pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
                  when (initialState.props.zoneType.priorityTag == SPECIAL_PICKUP && initialState.data.config.feature.enableSpecialPickup && os /= "IOS") $ do
                    let specialPickupZone = findSpecialPickupZone initialState.props.locateOnMapProps.sourceGeoJson initialState.props.locateOnMapProps.sourceGates initialState.data.driverInfoCardState.sourceLat initialState.data.driverInfoCardState.sourceLng
                    case specialPickupZone of
                      Just pickUpZone -> runEffectFn1 locateOnMap locateOnMapConfig { geoJson = pickUpZone.geoJson, points = pickUpZone.gates, locationName = pickUpZone.locationName, navigateToNearestGate = false }
                      Nothing -> pure unit
                  void $ launchAff $ flowRunner defaultGlobalState $ driverLocationTracking push UpdateCurrentStage DriverArrivedAction DriverReachedDestinationAction UpdateETA 3000.0 (getValueToLocalStore TRACKING_ID) initialState "pickup" 1
                else pure unit
                when (not initialState.props.chatcallbackInitiated && initialState.data.fareProductType /= FPT.ONE_WAY_SPECIAL_ZONE && disableChat initialState.data.fareProductType) $ do
                  -- @TODO need to revert once apk update is done
                  --when (initialState.data.driverInfoCardState.providerType == CTP.ONUS) $ void $ JB.showInAppNotification JB.inAppNotificationPayload{title = "Showing Approximate Location", message = "Driver locations of other providers are only approximate", channelId = "ApproxLoc", showLoader = true}
                  case initialState.data.contactList of
                    Nothing -> void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ fetchContactsForMultiChat push initialState
                    Just contacts -> do
                      push $ UpdateChatWithEM false $ fromMaybe dummyNewContacts $ Arr.head contacts
                      let uuid = initialState.data.driverInfoCardState.bppRideId
                          chatUser = if uuid == initialState.data.driverInfoCardState.bppRideId then "Customer" else (getValueFromCache (show CUSTOMER_ID) getKeyInSharedPrefKeys)
                      checkAndStartChatService push uuid chatUser false initialState
                      pure unit
                void $ push $ DriverInfoCardActionController DriverInfoCard.NoAction
                push LoadMessages
                void $ pure $ setValueToLocalStore DRIVER_REACHED_DESTINATION_ACTION "TRIGGER_DRIVER_REACHED_DESTINATION"
              EditPickUpLocation -> do
                void $ pure $ enableMyLocation true
                void $ runEffectFn2 storeCallBackLocateOnMap (\key lat lon -> push $ UpdatePickupLocation key lat lon) (handleLocateOnMapCallback "HomeScreen")
                pure unit
              RideStarted -> do
                void $ push $ DriverInfoCardActionController DriverInfoCard.NoAction
                if ((getValueToLocalStore TRACKING_DRIVER) == "False") then do
                  _ <- pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                  _ <- pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
                  _ <- launchAff $ flowRunner defaultGlobalState $ driverLocationTracking push UpdateCurrentStage DriverArrivedAction DriverReachedDestinationAction UpdateETA 10000.0 (getValueToLocalStore TRACKING_ID) initialState "trip" 1
                  pure unit
                else
                  pure unit
                let isRental = initialState.data.fareProductType == FPT.RENTAL
                case initialState.data.contactList of
                  Nothing -> void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ updateEmergencyContacts push initialState
                  Just contacts -> do
                    void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
                      emergencySettings <- lift $ lift $ fetchEmergencySettings push initialState
                      case emergencySettings of
                        Just (GetEmergencySettingsRes settings) -> do
                          let contacts = formatContacts settings.defaultEmergencyNumbers
                          void $ liftFlowBT $ validateAndStartChat contacts push initialState settings.safetyCheckStartTime settings.safetyCheckEndTime
                          pure unit
                        Nothing -> pure unit
                -- when (initialState.data.fareProductType /= FPT.RENTAL) $ do
                --   void $ push RemoveChat
                pure unit
                if initialState.data.fareProductType == FPT.RENTAL then
                  void $ rideDurationTimer (runFn2 differenceBetweenTwoUTC (getCurrentUTC "") initialState.data.driverInfoCardState.rentalData.startTimeUTC ) "1" "RideDurationTimer" push (RideDurationTimer)
                  else pure unit
                void $ push $ DriverInfoCardActionController DriverInfoCard.NoAction
              ChatWithDriver -> do
                if (getValueToLocalStore DRIVER_ARRIVAL_ACTION) == "TRIGGER_WAITING_ACTION" then waitingCountdownTimerV2 initialState.data.driverInfoCardState.driverArrivalTime "1" "countUpTimerId" push WaitingTimeAction else pure unit
                when ((not initialState.props.chatcallbackInitiated) && initialState.data.fareProductType /= FPT.ONE_WAY_SPECIAL_ZONE && disableChat initialState.data.fareProductType) $ do
                  case initialState.data.contactList of
                    Nothing -> void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ fetchContactsForMultiChat push initialState
                    Just _ -> do
                      let uuid = initialState.data.driverInfoCardState.currentChatRecipient.uuid
                          chatUser = if initialState.data.driverInfoCardState.currentChatRecipient.uuid == initialState.data.driverInfoCardState.bppRideId then "Customer" else (getValueFromCache (show CUSTOMER_ID) getKeyInSharedPrefKeys)
                      checkAndStartChatService push uuid chatUser false initialState
                      pure unit
                  push LoadMessages
              ConfirmingLocation -> do
                void $ pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
              GoToConfirmLocation -> do
                void $ pure $ enableMyLocation true
                void $ pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                void $ runEffectFn2 storeCallBackLocateOnMap (\key lat lon -> push $ UpdatePickupLocation key lat lon) (handleLocateOnMapCallback "HomeScreen")
                void $ push $ GoToConfirmingLocationStage
              ConfirmingEditDestinationLoc -> do
                when ((getValueToLocalStore FINDING_EDIT_LOC_RESULTS) /= "true") $ do
                  void $ pure $ setValueToLocalStore FINDING_EDIT_LOC_RESULTS "true"
                  void $ pure $ setValueToLocalStore LOCAL_STAGE (show ConfirmingEditDestinationLoc)
                  void $ pure $ setValueToLocalStore EDIT_DEST_POLLING_ID (getNewTrackingId unit)
                  let pollingCount = if any ( _ == getValueToLocalStore EDIT_DEST_POLLING_COUNT) ["_failed", "(null)"] then INT.round getEditDestPollingCount else INT.round (fromMaybe getEditDestPollingCount (NUM.fromString (getValueToLocalStore EDIT_DEST_POLLING_COUNT)))
                  void $ launchAff $ flowRunner defaultGlobalState $ getEditLocResults (getValueToLocalStore EDIT_DEST_POLLING_ID) GetEditLocResult BackPressed pollingCount (fromMaybe 2000.0 (NUM.fromString (getValueToLocalStore EDIT_DEST_POLLING_INTERVAL))) push initialState
              TryAgain -> do
                logStatus "find_estimate" ("searchId : " <> initialState.props.searchId)
                estimatesPolling <- runEffectFn1 getValueFromIdMap "EstimatePolling"
                when estimatesPolling.shouldPush $ void $ launchAff $ flowRunner defaultGlobalState $ getEstimate EstimatesTryAgain CheckFlowStatusAction 40 1000.0 push initialState estimatesPolling.id
                pure unit
              FindEstimateAndSearch -> do
                push $ SearchForSelectedLocation
                pure unit
              ReAllocated -> do
                void $ launchAff $ flowRunner defaultGlobalState $ reAllocateConfirmation push initialState ReAllocate 3000.0
              ShortDistance -> do
                when (initialState.props.suggestedRideFlow || initialState.props.isRepeatRide) $ push $ ShortDistanceActionController PopUpModal.OnButton2Click
              GoToConfirmgDelivery -> do
                push $ ConfirmDeliveryRide
                pure unit
              _ -> pure unit
            if ((initialState.props.sourceLat /= (-0.1)) && (initialState.props.sourceLong /= (-0.1))) then do
              case initialState.props.sourceLat, initialState.props.sourceLong of
                0.0, 0.0 -> do
                  _ <- getCurrentPosition push CurrentLocation
                  pure (pure unit)
                _, _ -> pure (pure unit)
            else
              pure (pure unit)
        ),
        (\push -> do
            when (Arr.elem initialState.props.currentStage [RideStarted, RideAccepted]) $ do
              push UpdateRateCardCache
              runEffectFn3 JB.initialiseShakeListener push ShakeActionCallback JB.defaultShakeListenerConfig
              pure unit
            when (Arr.elem initialState.props.currentStage [RideAccepted, RideCompleted]) $ void $ launchAff $ flowRunner defaultGlobalState $ fetchEmergencySettings push initialState
            pure (pure unit))

      ]
  , eval:
      \action state -> do
        let _ = spy "HomeScreen action " action
        let _ = spy "HomeScreen state " state
        eval2 action state
  }


isCurrentLocationEnabled :: Boolean
isCurrentLocationEnabled = isLocalStageOn HomeScreen

initVoipIfEnabled :: HomeScreenState -> Effect Unit
initVoipIfEnabled state = do
  let voipConfig = getCustomerVoipConfig $ DS.toLower $ getValueToLocalStore CUSTOMER_LOCATION
  if voipConfig.customer.enableVoipFeature 
    then void $ pure $ JB.initSignedCall state.data.driverInfoCardState.bppRideId false
    else pure unit

view :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
view push state =
  let
    showLabel = not (state.props.defaultPickUpPoint == "" && DS.null state.props.markerLabel)
    isEditDestination = any (_ == state.props.currentStage) [ConfirmEditDestinationLoc, ConfirmingEditDestinationLoc, RevisedEstimate, EditingDestinationLoc]
    extraPadding = if state.props.currentStage == ConfirmingLocation then getDefaultPixelSize (if os == "IOS" then 50 else 112) else (if os == "IOS" then 50 else 0)
    confirmingLocOrEditPickupStage = any (_ == state.props.currentStage) [ConfirmingLocation, EditPickUpLocation]
  in
  (if os == "IOS" then emptyScreenAnimation else PrestoAnim.animationSet[])  $
  frameLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , onBackPressed push (const BackPressed)
    , clickable true
    , afterRender
        (\action -> do
          void $ markPerformance "HOME_SCREEN_RENDER"
          void $ Events.endMeasuringDuration "onCreateToHomeScreenRenderDuration"
          void $ Events.endMeasuringDuration "initAppToHomeScreenRenderDuration"
          push action
        ) (const AfterRender)
    , accessibility DISABLE
    ]
    [  linearLayout
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
                  , clickable false
                  , background Color.transparent
                  , accessibility if any (_ == state.props.currentStage) [RideAccepted, RideStarted, HomeScreen] && not isAnyOverlayEnabled state then ENABLE else DISABLE
                  , accessibilityHint $ camelCaseToSentenceCase (show state.props.currentStage)
                  ][]
                , linearLayout
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , background Color.transparent
                  ][
                    if isHomeScreenView state then homeScreenViewV2 push state else emptyTextView state
                  , if isEditDestination || isHomeScreenView state then emptyTextView state else mapView' push state $  "CustomerHomeScreen"
                  , if isEditDestination then mapView' push state "CustomerHomeScreenEditDest" else emptyTextView state
                    ]
                , if not state.data.config.feature.enableSpecialPickup then
                    linearLayout
                    [ width MATCH_PARENT
                    , height MATCH_PARENT
                    , background Color.transparent
                    , padding $ PaddingBottom $ (if os == "IOS" then 60 + (if showLabel then 20 else 0) else 70) + extraPadding
                    , gravity CENTER
                    , accessibility DISABLE
                    , orientation VERTICAL
                    , visibility $ boolToVisibility $ not state.data.config.feature.enableSpecialPickup
                    ]
                    [ textView
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , background Color.black800
                        , color $ Color.white900
                        , accessibility DISABLE_DESCENDANT
                        , text if state.props.markerLabel /= "" then state.props.markerLabel
                               else if DS.length state.props.defaultPickUpPoint > state.data.config.mapConfig.labelTextSize then
                                  (DS.take (state.data.config.mapConfig.labelTextSize - 3) state.props.defaultPickUpPoint) <> "..."
                               else
                                  state.props.defaultPickUpPoint
                        , padding (Padding 5 5 5 5)
                        , margin (MarginBottom 5)
                        , cornerRadius 5.0
                        , visibility $ boolToInvisibility $ showLabel && (confirmingLocOrEditPickupStage || state.props.locateOnMap)
                        , id (getNewIDWithTag "LocateOnMapPin")
                        ]
                    , imageView
                        [ width $ V 35
                        , height $ V 35
                        , accessibility DISABLE
                        , imageWithFallback $ fetchImage FF_COMMON_ASSET $ locateOnMapPinImage
                        , visibility $ boolToVisibility (confirmingLocOrEditPickupStage ||  state.props.locateOnMap)
                        ]
                    ]
                  else
                    linearLayout
                    [ width MATCH_PARENT
                    , height MATCH_PARENT
                    , background Color.transparent
                    , padding $ PaddingBottom $ 95 + extraPadding
                    , gravity CENTER
                    , accessibility DISABLE
                    , orientation VERTICAL
                    , visibility $ boolToVisibility $ state.data.config.feature.enableSpecialPickup && (confirmingLocOrEditPickupStage || state.props.locateOnMap)
                    ][ imageView
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , accessibility DISABLE_DESCENDANT
                        , visibility $ boolToInvisibility (showLabel && (confirmingLocOrEditPickupStage || state.props.locateOnMap))
                        , id (getNewIDWithTag "LocateOnMapPin")
                        ]
                     ]
                , linearLayout
                    [ width MATCH_PARENT
                    , height MATCH_PARENT
                    , background Color.transparent
                    , padding $ PaddingBottom $ (if os == "IOS" then 26 else 36) + extraPadding
                    , gravity CENTER
                    , accessibility DISABLE
                    , orientation VERTICAL
                    , visibility $ boolToVisibility $ state.data.config.feature.enableSpecialPickup && (confirmingLocOrEditPickupStage || state.props.locateOnMap)
                    ]
                    [ imageView
                        [ width $ V 35
                        , height $ V 35
                        , accessibility DISABLE
                        , imageWithFallback $ fetchImage FF_COMMON_ASSET $ locateOnMapPinImage
                        , visibility $ boolToVisibility (confirmingLocOrEditPickupStage || state.props.locateOnMap)
                        ]
                    ]
                ]
            , topLeftIconView state push
            , rideRequestFlowView push state
            , preferenceView push state
            , if state.props.currentStage == PricingTutorial then (pricingTutorialView push state) else emptyTextView state
            , if (any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithDriver] && onUsRide && not (state.props.currentStage == RideAccepted && state.props.isOtpRideFlow && any (_ == state.data.fareProductType) [FPT.RENTAL, FPT.INTER_CITY])) then messageWidgetView push state else emptyTextView state
            , if (any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithDriver]) then rideDetailsBottomView push state else emptyTextView state
            , if state.props.currentStage == ChatWithDriver then messagingView push state else emptyTextView state
            , if state.props.currentStage /= RideRating && state.props.isMockLocation && (getMerchant FunctionCall == NAMMAYATRI) && state.props.currentStage == HomeScreen then (sourceUnserviceableView push state) else emptyTextView state
            , if state.data.config.feature.enableZooTicketBookingFlow then bottomNavBarView push state else emptyTextView state
            , if state.data.settingSideBar.opened /= SettingSideBar.CLOSED then settingSideBarView push state else emptyTextView state
            , if (state.props.currentStage == SearchLocationModel || state.props.currentStage == FavouriteLocationModel || state.props.currentStage == FavouriteLocationModelEditDest ) then searchLocationView push state else emptyTextView state
            , if (any (_ == state.props.currentStage) [ FindingQuotes, QuoteList, TryAgain, ConfirmingQuotes ] || state.data.iopState.providerSelectionStage) then (quoteListModelView push state) else emptyTextView state
            , if (state.props.currentStage == EditingDestinationLoc || state.props.currentStage == ConfirmEditDestinationLoc || state.props.currentStage == ConfirmingEditDestinationLoc || state.props.currentStage == RevisedEstimate) then searchLocationView push state else emptyTextView state
            , if (state.props.isCancelRide) then (cancelRidePopUpView push state) else emptyTextView state
            , if (state.props.showConfirmEditDestPopUp) then confirmEditDestPopUp push state else emptyTextView state
            , if (state.props.isContactSupportPopUp) then contactSupportPopUpView push state else emptyTextView state
            , if (state.props.isPopUp /= NoPopUp) then (logOutPopUpView push state) else emptyTextView state
            , if (state.props.isLocationTracking) then (locationTrackingPopUp push state) else emptyTextView state
            , if (state.props.isEstimateChanged) then (estimateChangedPopUp push state) else emptyTextView state
            , if state.props.currentStage == PickUpFarFromCurrentLocation then (pickUpFarFromCurrLocView push state) else emptyTextView state
            , if state.props.currentStage == DistanceOutsideLimits then (distanceOutsideLimitsView push state) else emptyTextView state
            , if state.props.currentStage == ShortDistance && (not state.props.suggestedRideFlow) && (not state.props.isRepeatRide)then (shortDistanceView push state) else emptyTextView state
            , if state.props.isSaveFavourite then saveFavouriteCardView push state else emptyTextView state
            , if state.props.showShareAppPopUp && state.data.config.feature.enableShareApp then shareAppPopUp push state else emptyTextView state
            , if state.props.showLiveDashboard then showLiveStatsDashboard push state else emptyTextView state
            , if state.props.showCallPopUp then (driverCallPopUp push state) else emptyTextView state
            , if state.props.cancelSearchCallDriver then cancelSearchPopUp push state else emptyTextView state
            , if state.props.showRateCard then (rateCardView push state) else emptyTextView state
            , if state.props.currentStage == GoToTripSelect then (selectTripViewIntercity push state) else emptyTextView state
            -- , if state.props.zoneTimerExpired then zoneTimerExpiredView state push else emptyTextView state
            , if state.props.callSupportPopUp then callSupportPopUpView push state else emptyTextView state
            , if state.props.showDisabilityPopUp &&  (getValueToLocalStore DISABILITY_UPDATED == "true") then disabilityPopUpView push state else emptyTextView state
            , if state.data.waitTimeInfo && state.props.currentStage == RideAccepted then waitTimeInfoPopUp push state else emptyTextView state
            , if isJust state.props.safetyAlertType && state.props.currentStage == RideStarted then safetyAlertPopup push state else  emptyTextView state
            , if state.props.showShareRide then PopupWithCheckbox.view (push <<< ShareRideAction) (shareRideConfig state) else emptyTextView state
            , if state.props.referral.referralStatus /= NO_REFERRAL || state.props.referral.showAddReferralPopup then referralPopUp push state else emptyTextView state
            , if state.props.showRentalInfo then rentalInfoPopUp push state else emptyTextView state
            , if state.props.showSpecialZoneInfoPopup then specialZoneInfoPopup push state else emptyTextView state
            , if showAcView state then isAcWorkingView push state else emptyTextView state
            , if state.props.showIntercityUnserviceablePopUp || state.props.showNormalRideNotSchedulablePopUp then intercityInSpecialZonePopupView push state else emptyTextView state
            , if state.props.showDeliveryImageAndOtpModal then deliveryParcelImageAndOtpView push state else emptyTextView state
            , if state.props.zoneOtpExpired then zoneTimerExpiredView state push else emptyTextView state
            , if state.props.showScheduledRideExistsPopUp then scheduledRideExistsPopUpView push state else emptyTextView state
            , if state.data.toll.showAmbiguousPopUp then PopUpModal.view (push <<< TollChargeAmbigousPopUpAction) (PopUpConfigs.finalFareExcludesToll state) else emptyTextView state
            , if state.data.toll.showIncludedPopUp then PopUpModal.view (push <<< TollChargeIncludedPopUpAction) (PopUpConfigs.tollChargesIncluded state) else emptyTextView state
            , if state.props.showEditPickupPopupOnCancel then PopUpModal.view (push <<< EditPickupPopupOnCancelAC) (editPickupPopupOnCancel state) else emptyTextView state
            , if state.props.searchLocationModelProps.showRideInfo then rideInfoCardView push state  else emptyTextView state
            , if state.data.intercityBus.showPermissionPopUp then PopUpModal.view (push <<< IntercityBusPermissionAction) (PopUpConfigs.intercityBusPhoneNumberPermission state) else emptyTextView state
            , if state.data.intercityBus.showWebView && isJust state.data.intercityBus.url then intercityWebView push state else emptyTextView state
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
                    else []
              <> if state.props.bookAmbulanceModal then [PopUpModal.view (push <<< AgreePopUp) (PopUpConfigs.bookAmbulanceModalConfig state)] else [])
        ]
  ]
  where
    showAcView :: HomeScreenState -> Boolean
    showAcView state = ((getValueFromCache (show AC_POPUP_SHOWN_FOR_RIDE) getKeyInSharedPrefKeys) /= state.data.driverInfoCardState.rideId )
                        && state.props.currentStage == RideStarted
                        && (((not isAcRide)
                         || (runFn2 differenceBetweenTwoUTCInMinutes (getCurrentUTC "") state.data.startedAtUTC > acPopupConfig.showAfterTime)))
                        && state.props.showAcWorkingPopup
                        && ((isAcRide && acPopupConfig.enableAcPopup) || (not isAcRide && acPopupConfig.enableNonAcPopup))
                        && (not $ state.data.driverInfoCardState.serviceTierName `Arr.elem` [Just "Auto", Just "Bike Taxi"])
                        && (not $ state.data.driverInfoCardState.vehicleVariant `Arr.elem` ["AMBULANCE_TAXI" , "AMBULANCE_TAXI_OXY"])
                        && state.data.currentCityConfig.enableAcViews
                        && state.data.fareProductType /= FPT.DELIVERY

    showSafetyAlertPopup = Arr.notElem (getValueToLocalNativeStore SAFETY_ALERT_TYPE) ["__failed", "false", "(null)"]
    onUsRide = state.data.driverInfoCardState.providerType == CTP.ONUS
    isAcRide = ServiceTierCard.showACDetails (fromMaybe "" state.data.driverInfoCardState.serviceTierName) Nothing state.data.fareProductType
    acPopupConfig = state.data.config.acPopupConfig
    locateOnMapPinImage =
      let confirmingLocOrEditPickupStage = any (_ == state.props.currentStage) [ConfirmingLocation, EditPickUpLocation]
      in case state.data.fareProductType of
            FPT.DELIVERY -> if state.props.isSource == Just true then "ny_ic_src_marker" else "ny_ic_dest_marker"
            _ -> case confirmingLocOrEditPickupStage || state.props.isSource == (Just true) of
                        true  -> "ny_ic_src_marker"
                        false -> "ny_ic_dest_marker"


rideDetailsBottomView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rideDetailsBottomView push state =
  let
    brandingBannerVis = boolToVisibility $ state.data.currentCityConfig.iopConfig.enable
    onUsRide = state.data.driverInfoCardState.providerType == CTP.ONUS
  in
  relativeLayout [
    width MATCH_PARENT
  , height MATCH_PARENT
  ][ rideTrackingView push state
    , DriverInfoCard.brandingBannerView state.data.config.driverInfoConfig brandingBannerVis (Just "BrandingBanner") onUsRide state.data.driverInfoCardState.providerName
  ]

scheduledRideExistsPopUpView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
scheduledRideExistsPopUpView push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , accessibility DISABLE
  ][PopUpModal.view (push <<< ScheduledRideExistsAction) (scheduledRideExistsPopUpConfig state)]

bottomNavBarView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
bottomNavBarView push state = let
  viewVisibility = boolToVisibility $ state.props.currentStage == HomeScreen
  enableBusBooking = state.data.config.feature.enableBusBooking -- && isJust (Arr.find (\service -> service.type == RemoteConfig.BUS) (nammaServices FunctionCall))
  in
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.transparent
    , alignParentBottom "true,-1"
    , visibility viewVisibility
    , gravity BOTTOM
    , orientation VERTICAL
    ][  separator (V 1) Color.grey900 state.props.currentStage
      , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , padding $ PaddingVertical 10 (10+safeMarginBottom)
          , background Color.white900
          ](map (\item ->
              linearLayout
              [ height WRAP_CONTENT
              , weight 1.0
              , gravity CENTER
              , onClick push $ const $ BottomNavBarAction item.id
              , orientation VERTICAL
              , alpha if (state.props.focussedBottomIcon == item.id) then 1.0 else 0.5
              ][  imageView
                    [ height $ V 24
                    , width $ V 24
                    , imageWithFallback $ fetchImage FF_ASSET $ item.image
                    ]
                , textView $
                    [ text item.text
                    , height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , color $ Color.black800
                    ] <> FontStyle.body9 TypoGraphy

              ]
            ) ([{text : "Mobility" , image : "ny_ic_vehicle_unfilled_black", id : MOBILITY}]
                <> (if enableBusBooking then [{text : "Bus" , image : "ny_ic_bus_black", id : BUS_}] else [])
                <> [{text : "Ticketing" , image : "ny_ic_ticket_black", id : TICKETING }]
              )
            )
    ]
getMapHeight :: HomeScreenState -> Length
getMapHeight state = V (if state.data.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE then (((screenHeight unit)/ 4)*3)
                            else if (state.props.currentStage == RideAccepted || state.props.currentStage == ChatWithDriver) then ((screenHeight unit) - (getInfoCardPeekHeight state)) + 50
                            else (((screenHeight unit)/ 15)*10))


getCarouselConfig ∷ ListItem → HomeScreenState → Array (BannerCarousel.Config (BannerCarousel.Action → Action)) → CarouselHolder.CarouselHolderConfig BannerCarousel.PropConfig Action
getCarouselConfig view state banners =
  let
    isBlindPerson = getValueToLocalStore DISABILITY_NAME == "BLIND_LOW_VISION"
  in {
    view
  , items : BannerCarousel.bannerTransformer banners
  , orientation : HORIZONTAL
  , currentPage : state.data.bannerData.currentPage
  , autoScroll : state.data.config.bannerCarousel.enableAutoScroll && not isBlindPerson
  , autoScrollDelay : state.data.config.bannerCarousel.autoScrollDelay
  , id : "bannerCarousel"
  , autoScrollAction : Just UpdateBanner
  , onPageSelected : Just BannerChanged
  , onPageScrollStateChanged : Just BannerStateChanged
  , onPageScrolled : Nothing
  , currentIndex : state.data.bannerData.currentBanner
  , showScrollIndicator : true
  , layoutHeight : V 137
  , overlayScrollIndicator : false
}

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

confirmEditDestPopUp :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
confirmEditDestPopUp push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , accessibility DISABLE
  ][PopUpModal.view (push <<< RequestEditAction) (confirmRequestEditConfig state)]

messageWidgetView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
messageWidgetView push state =
  let isWidgetVisible = ((any (_ == state.props.currentStage)) [ RideAccepted, ChatWithDriver] || state.props.isChatWithEMEnabled || state.data.fareProductType == FPT.RENTAL) && state.data.fareProductType /= FPT.ONE_WAY_SPECIAL_ZONE && disableChat state.data.fareProductType && not (state.props.currentStage == RideAccepted && state.props.isOtpRideFlow && any (_ == state.data.fareProductType) [FPT.RENTAL, FPT.INTER_CITY]) && state.data.config.feature.enableChat && state.data.config.feature.enableSuggestions && not state.props.removeNotification
  in
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , accessibility if state.data.settingSideBar.opened /= SettingSideBar.CLOSED || state.props.currentStage == ChatWithDriver || state.props.isCancelRide || state.props.isLocationTracking || state.props.callSupportPopUp || state.props.cancelSearchCallDriver || state.props.showCallPopUp || state.props.showRateCard || state.props.bottomSheetState == STATE_EXPANDED || state.data.waitTimeInfo then DISABLE_DESCENDANT else DISABLE
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
     ][ messageNotificationView push (getMessageNotificationViewConfig state)
      , linearLayout
        [ height $ V $ ((getInfoCardPeekHeight state) - if isWidgetVisible then 196 else 0)
        , width $ MATCH_PARENT
        , accessibility DISABLE
        ][]
     ]
  ]
  where disableSuggestions :: HomeScreenState -> Boolean
        disableSuggestions state = state.data.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE || not state.data.config.feature.enableChat || not state.data.config.feature.enableSuggestions || disableChat state.data.fareProductType

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
            ( mapWithIndex
                ( \index item ->
                    linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , orientation VERTICAL
                      ]
                      [ trackingCardCallView push state item
                      , if (index == 0) then linearLayout
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
  [ { text: (getString DIRECT_CALL)
    , imageWithFallback: fetchImage FF_ASSET "ic_direct_call"
    , type: DIRECT_CALLER
    , data: (getString YOUR_NUMBER_WILL_BE_VISIBLE_TO_THE_DRIVER_USE_IF_NOT_CALLING_FROM_REGISTERED_NUMBER)
    }
  , { text: (getString ANONYMOUS_CALL)
    , imageWithFallback: fetchImage FF_ASSET "ic_anonymous_call"
    , type: ANONYMOUS_CALLER
    , data: (getString YOUR_NUMBER_WILL_NOT_BE_SHOWN_TO_THE_DRIVER_THE_CALL_WILL_BE_RECORDED_FOR_COMPLIANCE)
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
        , if(item.type == ANONYMOUS_CALLER) then labelView push state (getString RECOMMENDED) state.data.config.showRecommendedText else labelView push state (getStringV2 faster) state.data.config.showFasterText
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

labelView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> String -> Boolean ->  PrestoDOM (Effect Unit) w
labelView push state label toShow =
    linearLayout[
      height WRAP_CONTENT
    , width WRAP_CONTENT
    , cornerRadii $ Corners 8.0 true true true true
    , background Color.green900
    , margin (MarginHorizontal 10 10)
    , visibility $ boolToVisibility $ toShow
    ][
      textView $ [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , color Color.white900
      , gravity CENTER
      , padding (Padding 8 1 8 1)
      , textSize FontSize.a_13
      , text label
      ]
    ]

searchLocationView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
searchLocationView push state =
  let isEditDestStage = any (_ == state.props.currentStage) [EditingDestinationLoc, ConfirmEditDestinationLoc, ConfirmingEditDestinationLoc, RevisedEstimate]
  in
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background if (any (_ == state.props.currentStage) [SearchLocationModel, EditingDestinationLoc] && state.props.isSearchLocation == LocateOnMap) || (any (_ == state.props.currentStage) [ConfirmEditDestinationLoc, ConfirmingEditDestinationLoc, RevisedEstimate]) then Color.transparent else Color.grey800
  ] [ if state.props.currentStage == SearchLocationModel then (searchLocationModelView push state) else emptyTextView state
    , if state.props.currentStage == FavouriteLocationModel || state.props.currentStage == FavouriteLocationModelEditDest then (favouriteLocationModel push state) else emptyTextView state
    , if isEditDestStage then (editingDestinationLocView push state) else emptyTextView state
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
        , visibility $ boolToVisibility $ state.props.rideRequestFlow && any (_ == state.props.currentStage) [ConfirmingLocation, EditPickUpLocation]
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
  let removeRefferal = (not state.data.config.feature.enableReferral) || (((any (_ == state.props.currentStage)) [RideAccepted, RideStarted, ChatWithDriver, SettingPrice]) || state.props.hasTakenRide || state.props.currentSheetState == EXPANDED)
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

sosView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
sosView push state = do
  let sosViewVisibility = boolToVisibility $ (state.props.currentStage == RideStarted || (state.props.currentStage == RideAccepted && state.data.driverInfoCardState.driverArrived )) && state.data.config.feature.enableSafetyFlow && (state.data.fareProductType /= FPT.DELIVERY || (state.props.currentStage == RideAccepted && state.data.driverInfoCardState.driverArrived) || state.data.driverInfoCardState.destinationReached )
  relativeLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER
    , visibility sosViewVisibility
    , margin $ MarginRight 16
    ]
    [ linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , cornerRadius 20.0
        , clipChildren false
        , background Color.blue900
        , orientation VERTICAL
        , gravity CENTER
        , margin $ Margin 12 12 12 8
        ]
        [ if onUsRide then safetyCenterView push INVISIBLE else linearLayout[visibility GONE][]
        , textView
            $ [ text $ getString NEW <> "✨"
              , color Color.white900
              , margin $ MarginVertical 5 3
              , gravity CENTER
              , visibility GONE -- Not required now.
              ]
            <> FontStyle.body17 TypoGraphy
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width $ WRAP_CONTENT
        , shadow $ Shadow 0.1 2.0 10.0 24.0 Color.greyBackDarkColor 0.5
        , background if state.props.currentStage == RideStarted && onUsRide then Color.blue900 else Color.white900
        , cornerRadius 20.0
        , onClick push $ const (if onUsRide then OpenEmergencyHelp else OpenOffUsSOS)
        -- , clickable onUsRide -- need to remove once @Kavyashree's changes are megred
        , rippleColor Color.rippleShade
        , padding $ Padding 12 8 12 8
        ]
        [ safetyCenterView push VISIBLE
        ]
    ]
  where
  onUsRide = state.data.driverInfoCardState.providerType == CTP.ONUS
  safetyCenterView push vis =
    linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER
      , visibility vis
      , accessibilityHint $ "Emergency Button"
      , accessibility ENABLE
      ]
      [ imageView
          [ imageWithFallback $ fetchImage FF_ASSET shieldImage
          , height $ V 24
          , width $ V 24
          , margin $ MarginRight 8
          , accessibility DISABLE
          , onClick push $ const OpenEmergencyHelp
          -- , clickable onUsRide -- need to remove once @Kavyashree's changes are megred
          ]
      , textView
          $ [ text $ getString EMERGENCY
            , color emergencyTextColor
            , margin $ MarginBottom 1
            , accessibility DISABLE
            ]
          <> FontStyle.body6 TypoGraphy
      ]
  emergencyTextColor = case (not onUsRide), state.props.currentStage of
                        true, _ -> Color.black800
                        false, RideAccepted -> Color.blue900
                        false, RideStarted -> Color.white900
                        _,_ -> Color.white900

  shieldImage = if onUsRide
                  then if state.props.currentStage == RideStarted
                         then "ny_ic_shield_blue"
                         else "ny_ic_sos"
                  else "ny_ic_sos_related"

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
        [ RateCard.view (push <<< RateCardAction) (getCardConfig state) ]
  where
  getCardConfig state = do
    if (state.data.fareProductType == FPT.INTER_CITY) then (intercityRateCardConfig state) else (rateCardConfig state)

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
      , gradient (Linear 0.0 ["#FFFFFF" , "#FFFFFF" , "#FFFFFF", Color.transparent])
      , orientation VERTICAL
      , padding $ PaddingTop 16
      ] $ maybe ([]) (\item -> [bannersCarousal item state push]) state.data.bannerData.bannerItem
      <> [ PrimaryButton.view (push <<< PrimaryButtonActionController) (whereToButtonConfig state)
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
  , cornerRadii $ Corners (4.0) true true false false
  ]([ if (not hideSavedLocsView) then savedLocationsView state push else linearLayout[visibility GONE][]
    , shimmerView state
    , additionalServicesView push state
    , if (isJust state.data.rentalsInfo && (isLocalStageOn HomeScreen || state.props.currentStage == HomeScreen)) then rentalBanner push state else linearLayout[visibility GONE][]
    , suggestionsView push state
    , emptySuggestionsBanner state push
    ]
    )

bannersCarousal :: forall w. ListItem -> HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bannersCarousal view state push =
  let banners = getBannerConfigs state BannerCarousel
      len = length banners
  in if len > 0
      then
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ MarginTop 12
        ][CarouselHolder.carouselView push $ getCarouselConfig view state banners]
      else dummyView state


emptySuggestionsBanner :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
emptySuggestionsBanner state push =
  let appName = fromMaybe state.data.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
      dimension = if state.data.config.feature.enableAdditionalServices then 230 else 250
  in linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 12.0
    , margin $ Margin 16 0 16 0
    , gravity CENTER_HORIZONTAL
    , orientation VERTICAL
    , visibility $ boolToVisibility $ (not (suggestionViewVisibility state)) && not (state.props.showShimmer && null state.data.tripSuggestions) && state.data.config.homeScreen.bannerViewVisibility
    ][  imageView
        [ height $ V dimension
        , width $ V dimension
        , imageWithFallback $ getImageBasedOnCity "ny_ic_home_illustration"
        ]
      , textView $
        [ text $ getVarString WELCOME_TEXT $ Arr.singleton appName
        , gravity CENTER
        , width MATCH_PARENT
        , margin $ MarginBottom 4
        , color Color.black900
        ] <> (FontStyle.subHeading1 LanguageStyle)
      , textView $
        [ text $ getString TAP_WHERE_TO_TO_BOOK_RIDE
        , height WRAP_CONTENT
        , gravity CENTER
        , width MATCH_PARENT
        , color Color.black700
        ] <> (FontStyle.body1 LanguageStyle)
     ]
    where
      getHomeScreenIllustration :: HomeScreenState -> String
      getHomeScreenIllustration state = let
        cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalStore CUSTOMER_LOCATION)
        in (if state.data.config.autoVariantEnabled && cityConfig.enableCabs then "ny_ic_home_illustration_cab_auto"
          else if state.data.config.autoVariantEnabled then "ny_ic_home_illustration_auto"
          else if cityConfig.enableCabs then "ny_ic_home_illustration_cab"
          else "ny_ic_home_illustration_auto")

savedLocationsView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
savedLocationsView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , clickable (state.props.isSrcServiceable && not state.props.userBlocked)
    , visibility $ boolToVisibility $ not $ state.props.showShimmer
    , padding $ PaddingHorizontal 16 16
    ]
    [ PrestoAnim.animationSet [ fadeIn true ] $
      linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , margin $ MarginVertical marginTop 8
        , alpha if (state.props.isSrcServiceable && not state.props.userBlocked) then 1.0 else 0.4
        , onClick push (const NoAction)
        , onAnimationEnd
             ( \action -> do
                 push $ MAPREADY "" "" ""
                 case state.props.currentStage of
                   HomeScreen -> if ((getSearchType unit) == "direct_search") then push DirectSearch else pure unit
                   _ -> pure unit
                 _ <- getCurrentPosition push CurrentLocation
                 push action
                 pure unit
             )(const MapReadyAction)
        ]
        [ LocationTagBar.view (push <<< SavedAddressClicked) { savedLocations: state.data.savedLocations } ]
    ]
  where
    marginTop = if not $ null $ getBannerConfigs state BannerCarousel then 20 else 24

recentSearchesView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
recentSearchesView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 16
    , padding $ PaddingHorizontal 16 16
    , visibility $ boolToVisibility $ not $ null state.data.destinationSuggestions
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
                    , accessibility if state.props.currentStage == ChatWithDriver || state.props.isCancelRide || state.props.isLocationTracking || state.props.callSupportPopUp || state.props.cancelSearchCallDriver then DISABLE else ENABLE
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
                , onClick push $ if (state.props.isSrcServiceable && not state.props.userBlocked) then (const $ OpenSearchLocation) else (const $ NoAction)
                , gravity BOTTOM
                ]
            , linearLayout
                [ orientation VERTICAL
                , width MATCH_PARENT
                , height WRAP_CONTENT
                , disableClickFeedback true
                , onClick push $ if (state.props.isSrcServiceable && not state.props.userBlocked) then (const $ OpenSearchLocation) else (const $ NoAction)
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
                      , text if (state.props.isSrcServiceable && not state.props.userBlocked) then
                              (if state.data.source /= "" then state.data.source else (getString CURRENT_LOCATION))
                             else
                               getString APP_NOT_SERVICEABLE
                      , maxLines 1
                      , ellipsize true
                      , color if (state.props.isSrcServiceable && not state.props.userBlocked) then Color.black800 else Color.greyDark
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
    , visibility $ boolToVisibility $ isStageInList state.props.currentStage [ SettingPrice, ConfirmingLocation, RideCompleted, FindingEstimate, ConfirmingEditDestinationLoc, ConfirmingRide, FindingQuotes, TryAgain, RideRating, ReAllocated, LoadMap, RevisedEstimate, EditPickUpLocation]
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
        , if isStageInList state.props.currentStage [ FindingEstimate, ConfirmingRide, TryAgain, FindingQuotes, ConfirmingEditDestinationLoc, ReAllocated, LoadMap] then
            (loaderView push state)
          else
            emptyTextView state
        ]
    ]
    where
      getViewBasedOnStage :: (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
      getViewBasedOnStage push state = do
        if state.props.currentStage == SettingPrice then
          if state.props.isRepeatRide && (isNothing state.props.repeatRideServiceTierName || checkRecentRideVariant state)
            then estimatedFareView push state
          else
            ChooseYourRide.view (push <<< ChooseYourRideAction) (chooseYourRideConfig state)
        else if state.props.currentStage == RevisedEstimate then revisedEstimatedFareView push state
        else if (state.props.currentStage == ConfirmingLocation || state.props.currentStage == EditPickUpLocation) then
          confirmPickUpLocationView push state
        else
          emptyTextView state

      isCityInList :: City -> Array City -> Boolean
      isCityInList city = any (_ == city)

isStageInList :: Stage -> Array Stage -> Boolean
isStageInList stage = any (_ == stage)

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
  let image = if (any (_ == state.props.currentStage) [ SettingPrice, ConfirmingLocation, PricingTutorial, EditPickUpLocation, DistanceOutsideLimits ]) then fetchImage FF_COMMON_ASSET "ny_ic_chevron_left" else if state.data.config.dashboard.enable && (checkVersion "LazyCheck") then fetchImage FF_ASSET "ic_menu_notify" else fetchImage FF_ASSET "ny_ic_hamburger"
      onClickAction = if (any (_ == state.props.currentStage) [ SettingPrice, ConfirmingLocation, PricingTutorial, EditPickUpLocation, DistanceOutsideLimits ]) then const BackPressed else const OpenSettings
      isBackPress = (any (_ == state.props.currentStage) [ SettingPrice, ConfirmingLocation, PricingTutorial, EditPickUpLocation, DistanceOutsideLimits ])
      followerBar = (showFollowerBar (fromMaybe [] state.data.followers) state) && (any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithDriver])
      isEditDestination = any (_ == state.props.currentStage) [EditingDestinationLoc, ConfirmEditDestinationLoc, ConfirmingEditDestinationLoc, RevisedEstimate]
      isVisible = state.data.config.showHamMenu && not isEditDestination && not ((not state.props.rideRequestFlow) || any (_ == state.props.currentStage) [ FindingEstimate, ConfirmingRide, HomeScreen]) && (not ((isParentView FunctionCall) &&  any (_ == state.props.currentStage)[ RideAccepted , RideStarted]))
      manuallySharedFollowers = fromMaybe [] state.data.manuallySharedFollowers
  in
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , visibility $ boolToVisibility isVisible
    , margin $ MarginTop if followerBar then 0 else safeMarginTop
    ]
    $ []
    <> ( if isFollowEnabled state && followerBar then [ followRideBar push (getFollowers state) (MATCH_PARENT) true false] else []
      )
    <> ( [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , visibility $ boolToVisibility  state.data.config.showHamMenu
            , margin $ Margin 16 20 0 0
            , accessibility if state.data.settingSideBar.opened /= SettingSideBar.CLOSED || state.props.currentStage == ChatWithDriver || state.props.isCancelRide || state.props.isLocationTracking || state.props.callSupportPopUp || state.props.cancelSearchCallDriver || state.props.showCallPopUp || state.props.showRateCard || state.data.waitTimeInfo then DISABLE_DESCENDANT else DISABLE
            ]
            [ linearLayout
                [ height $ V 48
                , width $ V 48
                , stroke ("1," <> Color.grey900)
                , background Color.white900
                , gravity CENTER
                , cornerRadius 24.0
                , visibility $ boolToVisibility $ not (any (_ == state.props.currentStage) [ FindingEstimate, ConfirmingRide, FindingQuotes, TryAgain, RideCompleted, RideRating, ReAllocated, SearchLocationModel ])
                , clickable true
                , onClick push $ if isBackPress then const BackPressed else const OpenSettings
                , accessibilityHint if isBackPress then "Back : Button" else "Menu : Button"
                , accessibility ENABLE
                , rippleColor Color.rippleShade
                ]
                [ imageView
                    [ imageWithFallback image
                    , height $ V 25
                    , accessibility DISABLE
                    , clickable true
                    , onClick push $ onClickAction
                    , width $ V 25
                    ]
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , weight 1.0
                ]
                []
            , sosView push state
            , if (not state.data.config.dashboard.enable) || (isPreviousVersion (getValueToLocalStore VERSION_NAME) (if os == "IOS" then "1.2.5" else "1.2.1")) then emptyTextView state else liveStatsDashboardView push state
            ]
        ]
      )
    <> (case state.data.upcomingRideDetails of
          Nothing -> []
          Just rideDetails -> [upcomingRideBanner push rideDetails])
  where
  upcomingRideBanner push rideDetails =
    linearLayout
          [ height $ WRAP_CONTENT
          , width MATCH_PARENT
          , background Color.blueGreen
          , margin $ Margin 16 16 16 0
          , padding $ Padding 2 4 2 4
          , cornerRadius 8.0
          , gravity CENTER
          ][  textView $
              [ textFromHtml $ getString $ YOU_HAVE_AN_UPCOMING_BOOKING (rideDetails.rideScheduledAt)
                , color Color.white900
              ] <> FontStyle.tags TypoGraphy ]
----------- estimatedFareView ----------------
estimatedFareView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
estimatedFareView push state =
  let tagConfig = specialZoneTagConfig state.props.zoneType.priorityTag
      showTag = any (_ == state.props.zoneType.priorityTag) [SPECIAL_PICKUP, METRO]
  in
  linearLayout
  [ orientation VERTICAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , background tagConfig.backgroundColor
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
      , padding (Padding 8 4 8 4)
      , visibility $ boolToVisibility showTag
      , clickable $ isJust tagConfig.infoPopUpConfig
      , onClick push $ const $ SpecialZoneInfoTag
      ] [ imageView
          [ width (V 15)
          , height (V 15)
          , margin (MarginRight 6)
          , imageWithFallback $ fetchImage COMMON_ASSET tagConfig.icon
          ]
        , textView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , textSize FontSize.a_14
          , text tagConfig.text
          , color Color.white900
          , accessibility DISABLE
          ]
        , imageView
          [ width (V 18)
          , height (V 18)
          , visibility if isJust tagConfig.infoPopUpConfig then VISIBLE else GONE
          , margin (MarginLeft 6)
          , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_white_info"
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


revisedEstimatedFareView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
revisedEstimatedFareView push state =
  let tagConfig = specialZoneTagConfig state.props.zoneType.priorityTag
      showTag = any (_ == state.props.zoneType.priorityTag) [SPECIAL_PICKUP, METRO]
  in
  linearLayout
  [ orientation VERTICAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , background tagConfig.backgroundColor
  , clickable true
  , visibility $ boolToVisibility (state.props.currentStage == RevisedEstimate)
  , stroke ("1," <> Color.grey900)
  , gravity CENTER
  , cornerRadii $ Corners 24.0 true true false false
  ][ linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.white900
      , clickable true
      , accessibility if state.props.showRevisedFareDetails then DISABLE_DESCENDANT else ENABLE
      , accessibilityHint $ "Fare Updated to " <> (show (fromMaybe 0 state.data.newEstimatedFare)) <> "Previously fare was " <> (show state.data.driverInfoCardState.price) <> "New Distance is " <> (show ((fromMaybe 0.0 state.data.newEstimatedDistance)/1000.0)) <> "km ( was " <> state.data.driverInfoCardState.estimatedDistance <> "km " <> ")"
      , padding (Padding 16 7 16 24)
      , stroke ("1," <> Color.grey900)
      , gravity CENTER
      , cornerRadii $ Corners 24.0 true true false false
      ][
          linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , gravity CENTER_HORIZONTAL
          ]
          [ textView $
              [ text $ getString ROUTE_AND_FARE_UPDATED
              , color Color.black800
              , gravity CENTER_HORIZONTAL
              , height WRAP_CONTENT
              , width MATCH_PARENT
              ]
              <> FontStyle.h1 TypoGraphy
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , gravity CENTER
          , cornerRadius 8.0
          , stroke $ "1," <> Color.grey900
          , margin $ MarginTop 16
          ][
            textView $
            [
              text $ (getCurrency appConfig) <> (show (fromMaybe 0 state.data.newEstimatedFare))
              , color Color.black900
              , gravity CENTER_HORIZONTAL
              , height WRAP_CONTENT
              , margin $ MarginTop 8
              , visibility $ boolToVisibility $ isJust state.data.newEstimatedFare
              , width MATCH_PARENT
            ]<> FontStyle.heading TypoGraphy
            , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , gravity CENTER_HORIZONTAL
            , onClick push $ const ShowRevisedFareDetails
            ]
            [ textView $
              [
                text $ getString DETAILS
                , color Color.black700
                , gravity CENTER
                , height WRAP_CONTENT
                , margin $ MarginTop 8
                , accessibilityHint "Details : Button"
                , accessibility ENABLE
                , visibility $ boolToVisibility $ isJust state.data.newEstimatedFare
                , width WRAP_CONTENT
              ]<> FontStyle.paragraphText TypoGraphy
              , imageView
              [
                imageWithFallback $ fetchImage FF_ASSET $ if state.props.showRevisedFareDetails then "ny_ic_chevron_up" else "ny_ic_chevron_down"
                , height $ V 16
                , width $ V 16
                , margin $ Margin 6 10 0 0
              ]
            ]
            ,  if state.props.showRevisedFareDetails then olderFareDetails state else emptyTextViewNoHeight state
            ,  if state.props.showRevisedFareDetails then oldDistanceDetails state else emptyTextViewNoHeight state
            , linearLayout[
                height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity CENTER_HORIZONTAL
              , margin $ Margin 10 12 10 12
              , cornerRadius 10.0
              , background Color.yellow800
            ]
            [ textView $
              [
                text $ getString PLEASE_CONFIRM_WITH_YOUR_AFTER_REQUESTING
                , color Color.black900
                , gravity CENTER_HORIZONTAL
                , padding $ Padding 8 8 8 8
                , height WRAP_CONTENT
                , width MATCH_PARENT
              ]<> FontStyle.paragraphText TypoGraphy
            ]
          ]
        , requestRideButtonView push state
      ]
  ]
  where
    olderFareDetails :: HomeScreenState -> PrestoDOM (Effect Unit) w
    olderFareDetails state =
      PrestoAnim.animationSet [ Anim.fadeIn true]
        $ textView $
          [
            text $ getString PREVIOUS_FARE <> " :" <> (getCurrency appConfig) <> (show state.data.driverInfoCardState.price)
            , color Color.black800
            , gravity CENTER_HORIZONTAL
            , height WRAP_CONTENT
            , margin $ MarginVertical 8 8
            , width MATCH_PARENT
            , visibility $ boolToVisibility state.props.showRevisedFareDetails
          ]<> FontStyle.paragraphText TypoGraphy
    oldDistanceDetails :: HomeScreenState -> PrestoDOM (Effect Unit) w
    oldDistanceDetails state =
      PrestoAnim.animationSet [ Anim.fadeIn true]
        $ textView $
          [
            text $ (show (fromMaybe 0.0 state.data.newEstimatedDistance)) <> "km ( " <> (getString PREVIOUSLY) <> state.data.driverInfoCardState.estimatedDistance <> "km " <> ")"
            , color Color.black800
            , gravity CENTER_HORIZONTAL
            , height WRAP_CONTENT
            , width MATCH_PARENT
            , visibility $ boolToVisibility state.props.showRevisedFareDetails
          ]<> FontStyle.paragraphText TypoGraphy


estimateHeaderView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
estimateHeaderView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER_HORIZONTAL
    ]
    [ textView $
        [ text$ getString CONFIRM_YOUR_RIDE
        , color Color.black800
        , gravity CENTER_HORIZONTAL
        , height WRAP_CONTENT
        , width MATCH_PARENT
        , accessibility ENABLE
        ]
        <> FontStyle.h1 TypoGraphy
    , estimatedTimeDistanceView push state
    , extraChargesView
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , margin $ MarginTop 12
        , background Color.grey900
        ][]
    ]
  where
    extraChargesView =
      linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER_HORIZONTAL
      , margin $ MarginTop 4
      , visibility $  boolToVisibility $ state.data.selectedEstimatesObject.hasTollCharges || state.data.selectedEstimatesObject.hasParkingCharges
      ][
        linearLayout [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , padding $ Padding 8 8 8 8
        , background Color.grey700
        , orientation HORIZONTAL
        , cornerRadius 16.0
        , gravity CENTER_VERTICAL
        ][
          imageView[
            height $ V 16
          , width $ V 16
          , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET if state.data.selectedEstimatesObject.hasTollCharges then "ny_ic_black_toll" else "ny_ic_parking_logo_grey"
          , margin $ MarginRight 4
          ]
        , textView $ [
            textFromHtml case  state.data.selectedEstimatesObject.hasTollCharges, state.data.selectedEstimatesObject.hasParkingCharges of
              true, true -> getString APP_TOLL_PARKING_CHARGES
              true, false -> getString APP_TOLL_CHARGES
              false, true -> getString APP_PARKING_CHARGES
              _, _ -> ""
          , color Color.black800
          , gravity CENTER_HORIZONTAL
          , height WRAP_CONTENT
          , gravity CENTER_HORIZONTAL
          , width MATCH_PARENT
          ] <> FontStyle.body1 TypoGraphy
        ]
      ]


estimatedTimeDistanceView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
estimatedTimeDistanceView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , margin $ MarginTop 4
    , accessibility ENABLE
    , accessibilityHint $ "Estimated distance is : " <> state.data.rideDistance <> "and Estimated time is : " <> state.data.rideDuration
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

menuButtonView :: forall w action . (Action -> Effect Unit) -> String -> String -> Boolean -> HomeScreenState -> Action -> Action -> Boolean -> PrestoDOM (Effect Unit) w
menuButtonView push menuText menuImage faster state action infixIconAC isActive =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , gravity CENTER
  ][ linearLayout
     [ height $ V 30
     , width $ V 30
     , onClick push $ const action
     , gravity CENTER
     ][ linearLayout
        [ height $ V 20
        , width $ V 20
        , stroke if isActive then ("2," <> state.data.config.primaryBackground) else ("2," <> Color.black600)
        , cornerRadius 10.0
        , gravity CENTER
        ][  linearLayout
            [ width $ V 10
            , height $ V 10
            , cornerRadius 5.0
            , background $ state.data.config.primaryBackground
            , visibility $ boolToVisibility isActive
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
      , onClick push $ const action
      ] <> FontStyle.paragraphText LanguageStyle
    , if faster then
        linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , background state.data.config.autoSelectBackground
        , cornerRadius 14.0
        , gravity CENTER
        , margin $ MarginLeft 8
        , padding $ Padding 10 6 10 6
        ][  imageView
            [ height $ V 12
            , width $ V 12
            , margin $ MarginRight 4
            , imageWithFallback menuImage
            ]
          , textView $
            [ text $ getString RECOMMENDED
            , width WRAP_CONTENT
            , gravity CENTER
            , color Color.white900
            , height WRAP_CONTENT
            ] <> FontStyle.body15 LanguageStyle
          ]
        else
          imageView
          [ height $ V 20
          , width $ V 20
          , imageWithFallback menuImage
          , onClick push $ const infixIconAC
          , padding $ Padding 2 2 2 2
          , margin $ MarginHorizontal 5 5
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
      confirmLocationCategory = getConfirmLocationCategory state
      tagConfig = specialZoneTagConfig confirmLocationCategory
      tagVisibility = confirmLocationCategory /= NOZONE && (not (DS.null state.props.defaultPickUpPoint) || any (_ == confirmLocationCategory) [HOTSPOT true, HOTSPOT false])
  in
  linearLayout
    [ orientation VERTICAL
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , disableClickFeedback true
    , background Color.transparent
    , accessibility DISABLE
    , visibility if state.props.currentStage == ConfirmingLocation || state.props.currentStage == EditPickUpLocation then VISIBLE else GONE
    , padding $ PaddingTop 16
    , cornerRadii $ Corners 24.0 true true false false
    , gravity CENTER
    , clickable true
    , onClick push $ const NoAction
    ]
    [ recenterButtonView push state
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , stroke $ "1," <> Color.grey900
        , cornerRadii $ Corners 24.0 true true false false
        , background tagConfig.backgroundColor
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , gravity CENTER
            , padding (Padding zonePadding 4 zonePadding 4)
            , cornerRadii $ Corners 24.0 true true false false
            , visibility $ boolToVisibility tagVisibility
            , clickable $ isJust tagConfig.infoPopUpConfig
            , onClick push $ const $ SpecialZoneInfoTag
            ] [ imageView
                [ width (V 15)
                , height (V 15)
                , margin (MarginRight 6)
                , imageWithFallback $ fetchImage COMMON_ASSET tagConfig.icon
                ]
              , textView
                [ width if os == "IOS" && confirmLocationCategory == AUTO_BLOCKED then (V 230) else WRAP_CONTENT
                , height WRAP_CONTENT
                , gravity CENTER
                , textSize FontSize.a_14
                , text tagConfig.text
                , color Color.white900
                , accessibility DISABLE
                ]
              , imageView
                [ width (V 18)
                , height (V 18)
                , visibility if isJust tagConfig.infoPopUpConfig then VISIBLE else GONE
                , margin (MarginLeft 6)
                , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_white_info"
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
                [ text if state.props.currentStage == EditPickUpLocation then getString CHANGE_PICKUP_LOCATION else if state.data.fareProductType == FPT.DELIVERY then (getString CONFIRM_PICKUP_AND_DROP_LOCATION) else (getString CONFIRM_PICKUP_LOCATION)
                , color Color.black800
                , accessibility DISABLE
                , gravity CENTER_HORIZONTAL
                , height WRAP_CONTENT
                , width MATCH_PARENT
                ] <> (if state.data.fareProductType == FPT.DELIVERY then FontStyle.h2 TypoGraphy else FontStyle.h1 TypoGraphy)
              , textView $ [
                  text $ getString MOVE_PIN_TO_THE_DESIRED_PICKUP_POINT
                , color Color.black800
                , accessibility DISABLE
                , gravity CENTER_HORIZONTAL
                , height WRAP_CONTENT
                , visibility $ boolToVisibility $ isLocalStageOn EditPickUpLocation
                , width MATCH_PARENT
                ] <> FontStyle.body2 TypoGraphy
              , deliveryPickupAndDropLocationView push state
              , currentLocationView push state $ pickupLocationConfig state
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
                          ConfirmingEditDestinationLoc -> getString GETTING_REVISED_ESTIMATE
                          _ -> getString GETTING_ESTIMATES_FOR_YOU
      loaderVisibility = (any (_ == state.props.currentStage) [ FindingEstimate, ConfirmingRide, TryAgain, ReAllocated, ConfirmingEditDestinationLoc]) || (state.props.currentStage == LoadMap && state.props.isRepeatRide)
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
      , visibility $ boolToVisibility $ loaderVisibility
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
      , PrestoAnim.animationSet [ translateYAnimFromTopWithAlpha $ translateFullYAnimWithDurationConfig 300 true ]
          $ separator (V 1) Color.grey900 state.props.currentStage
      , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , visibility if (any (_ == state.props.currentStage) [ FindingEstimate, TryAgain, LoadMap]) then VISIBLE else GONE
          , orientation VERTICAL
          , gravity CENTER
          ]
          [ PrestoAnim.animationSet [ translateYAnimFromTopWithAlpha $ translateFullYAnimWithDurationConfig 300 true ]
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
    ][ SearchLocationModel.view (push <<< SearchLocationModelActionController) $ searchLocationModelViewState state]

editingDestinationLocView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
editingDestinationLocView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background if state.props.isRideServiceable then Color.transparent else Color.white900
    ]
    [ SearchLocationModel.view (push <<< EditDestSearchLocationModelActionController) $ editDestSearchLocationModelViewState state]


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


------------------------ emptyTextView ---------------------------
emptyTextViewNoHeight :: forall w. HomeScreenState ->  PrestoDOM (Effect Unit) w
emptyTextViewNoHeight state = textView [text "", width $ if os == "IOS" then V 1 else V 0, height $ if os == "IOS" then V 1 else V 0]

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
      topMargin = 60 + if os == "IOS" then safeMarginTop else 20
  in
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (Padding 0 0 0 0)
    , margin $ MarginTop topMargin
    , background Color.transparent
    , accessibility if (state.data.settingSideBar.opened /= SettingSideBar.CLOSED) || state.props.currentStage == ChatWithDriver || state.props.cancelSearchCallDriver || state.props.showCallPopUp || state.props.isCancelRide || state.props.isLocationTracking || state.props.callSupportPopUp || (state.props.showShareAppPopUp && state.data.config.feature.enableShareApp) || state.data.waitTimeInfo then DISABLE_DESCENDANT else DISABLE
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
                        ] <> (if lowVisionDisability || (os == "ANDROID") then
                            [onStateChanged push $ ScrollStateChanged
                            , sheetState state.props.currentSheetState]
                            else case state.props.sheetState of
                                    Nothing -> []
                                    Just state -> [sheetState state]))
                        [ linearLayout
                            [ height WRAP_CONTENT
                            , width MATCH_PARENT
                            ]
                            [

                              if state.props.currentStage == RideAccepted && (any (_ ==  state.data.fareProductType) [FPT.RENTAL, FPT.INTER_CITY]) && state.props.isOtpRideFlow then
                               let driverInfoCardState = driverInfoCardViewState state
                               in DriverInfoCard.view (push <<< DriverInfoCardActionController) $ driverInfoCardState { props { isOtpRideFlow = true}}
                              else if (any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithDriver]) then
                                DriverInfoCard.view (push <<< DriverInfoCardActionController) $ driverInfoCardViewState state
                              else
                                emptyTextView state
                            ]
                        ]
            ]
        ]
    ]
  where
    sheetHeight = toNumber (runFn1 getLayoutBounds $ getNewIDWithTag "BottomSheetLayout").height
    halfExpanded = (toNumber (getInfoCardPeekHeight state)) / if sheetHeight == 0.0 then 611.0 else sheetHeight
    bottomPadding = if os == "IOS" && safeMarginBottom == 0 then 24 else safeMarginBottom

getMessageNotificationViewConfig :: HomeScreenState -> MessageNotificationView Action
getMessageNotificationViewConfig state =
  let getSuggestionKey = if state.props.isChatWithEMEnabled then emChatSuggestion else chatSuggestion
      toChatComponent { message, sentBy, timeStamp, type: type_, delay } = { message, sentBy, timeStamp, type: type_, delay}
  in {
      showChatNotification : state.props.showChatNotification
    , enableChatWidget : state.props.enableChatWidget
    , isNotificationExpanded :state.props.isNotificationExpanded
    , fareProductType : state.data.fareProductType
    , config : state.data.config
    , rideStarted : state.props.currentStage == RideStarted
    , lastMessage : toChatComponent state.data.lastMessage
    , lastSentMessage : state.data.lastSentMessage
    , lastReceivedMessage : state.data.lastReceivedMessage
    , removeNotificationAction : RemoveNotification
    , messageViewAnimationEnd : MessageViewAnimationEnd
    , messageReceiverAction : MessageDriver
    , sendQuickMessageAction : SendQuickMessage
    , timerCounter : state.data.triggerPatchCounter
    , messageExpiryAction : MessageExpiryTimer
    , chatSuggestions : getChatSuggestions state
    , messages : state.data.messages
    , removeNotification : state.props.removeNotification
    , currentStage : state.props.currentStage
    , suggestionKey : getSuggestionKey
    , user : mkUser
    , isOtpRideFlow : state.props.isOtpRideFlow
  }
  where
    mkUser =
      { userName : mkUserHelpers state.data.driverInfoCardState.driverName
      , receiver : mkUserHelpers "Driver"
      }

    mkUserHelpers fallbackName =
      let primaryContact = head $ filter (\item -> item.contactPersonId == state.data.driverInfoCardState.currentChatRecipient.contactPersonId) $ fromMaybe [] state.data.contactList
      in if state.props.isChatWithEMEnabled
          then case primaryContact of
              Nothing -> fallbackName
              Just contact -> contact.name
          else fallbackName


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

contactSupportPopUpView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
contactSupportPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , accessibility DISABLE
    ]
    [ PopUpModal.view (push <<< ContactSupportAction) (CommonComponentConfig.contactSupportPopUpConfig state.data.config) ]

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
    , visibility if any (_ == currentStage) [FindingQuotes] then GONE else VISIBLE
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
            void $ pure $ startLottieProcess lottieAnimationConfig {speed = 1.5, rawJson = state.data.config.estimateAndQuoteConfig.genericLoaderLottie, lottieId = (getNewIDWithTag "lottieLoader") }
            pure unit
        )
        (const LottieLoaderAction)
    , height $ V state.data.config.searchLocationConfig.lottieHeight
    , width $ V state.data.config.searchLocationConfig.lottieWidth
    ]

getEstimate :: forall action. (GetQuotesRes -> Int -> action) -> action -> Int -> Number -> (action -> Effect Unit) -> HomeScreenState  -> Int -> Flow GlobalState Unit
getEstimate action flowStatusAction count duration push state id = do
  noEstimatePolling <- liftFlow $ runEffectFn1 getValueFromIdMap "EstimatePolling"
  if ((isLocalStageOn FindingEstimate) || (isLocalStageOn TryAgain))  && noEstimatePolling.id == id then do
    let _ = runFn2 updatePushInIdMap "EstimatePolling" false
    if (count > 0) then do
      resp <- getQuotes (state.props.searchId)
      _ <- pure $ printLog "caseId" (state.props.searchId)
      case resp of
        Right response -> do
          _ <- pure $ printLog "api Results " response
          let (GetQuotesRes resp) = response
              valueAddNp = filter (\(EstimateAPIEntity estimate) -> fromMaybe true estimate.isValueAddNP) resp.estimates
          if not (null resp.quotes) || not (null valueAddNp) then do
            doAff do liftEffect $ push $ action response count
            pure unit
          else do
            if (count == 1) then do
              _ <- pure $ updateLocalStage SearchLocationModel
              doAff do liftEffect $ push $ action response count
            else do
              void $ delay $ Milliseconds duration
              getEstimate action flowStatusAction (count - 1) duration push state id
        Left err -> do
          let errResp = err.response
              codeMessage = decodeError errResp.errorMessage "errorMessage"
          if ( err.code == 400 && codeMessage == "ACTIVE_BOOKING_ALREADY_PRESENT" ) then do
            -- _ <- pure $ logEvent state.data.logField "ny_fs_active_booking_found_on_search"
            void $ pure $ EHU.showToast "ACTIVE BOOKING ALREADY PRESENT"
            doAff do liftEffect $ push $ flowStatusAction
          else do
            void $ delay $ Milliseconds duration
            if (count == 1) then do
              let response = GetQuotesRes { quotes: [], estimates: [], fromLocation: SearchReqLocationAPIEntity { lat: 0.0, lon: 0.0 }, toLocation: Nothing }
              _ <- pure $ updateLocalStage SearchLocationModel
              doAff do liftEffect $ push $ action response count
            else do
              getEstimate action flowStatusAction (count - 1) duration push state id
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
      if  usableCount > 0 then do
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
            getQuotesPolling pollingId action retryAction (usableCount - 1) duration push state
      else do
        let response = SelectListRes { selectedQuotes: Nothing, bookingId : Nothing }
        _ <- pure $ updateLocalStage QuoteList
        doAff do liftEffect $ push $ action response

getEditLocResults :: forall action. String -> (GetEditLocResultResp -> action) -> action -> Int -> Number -> (action -> Effect Unit) -> HomeScreenState -> Flow GlobalState Unit
getEditLocResults pollingId action exitAction count duration push state = do
  when (pollingId == (getValueToLocalStore EDIT_DEST_POLLING_ID) && (isLocalStageOn ConfirmingEditDestinationLoc)) $ do
    internetCondition <- liftFlow $ isInternetAvailable unit
    when (internetCondition && state.props.bookingUpdateRequestId /= Nothing)  $ do
      let usableCount = count
      if (spy "USABLECOUNT :- " usableCount > 0) then do
        resp <- HelpersAPI.callApi $ Remote.makeEditLocationResultRequest (fromMaybe "" state.props.bookingUpdateRequestId)
        case resp of
          Right response -> do
            let (GetEditLocResultResp resp') = response
            let (BookingUpdateRequestDetails bookingUpdateRequestDetails) = resp'.bookingUpdateRequestDetails
            if (bookingUpdateRequestDetails.estimatedDistance /= Nothing && bookingUpdateRequestDetails.estimatedFare /= Nothing) then
              doAff do liftEffect $ push $ action response
            else if isJust bookingUpdateRequestDetails.errorObj then do
              let (ErrorObj err) = fromMaybe (ErrorObj {errorCode : "SOMETHING WENT WRONG. PLEASE TRY AGAIN LATER", errorMessage : ""}) bookingUpdateRequestDetails.errorObj
              void $ pure $ setValueToLocalStore FINDING_EDIT_LOC_RESULTS "false"
              void $ pure $ EHU.showToast err.errorCode
              doAff do liftEffect $ push $ exitAction
            else do
              void $ delay $ Milliseconds duration
              getEditLocResults pollingId action exitAction (usableCount - 1) duration push state
          Left err -> do
            let errResp = err.response
                codeMessage = decodeError errResp.errorMessage "errorMessage"
            if err.code == 400 then do
              if codeMessage == "EDIT_LOCATION_ATTEMPTS_EXHAUSTED" then void $ pure $ EHU.showToast "TRIP UPDATE REQUEST LIMIT EXCEEDED."
              else if codeMessage == "RIDE_NOT_SERVICEABLE" then void $ pure $ EHU.showToast "RIDE NOT SERVICEABLE"
              else void $ pure $ EHU.showToast codeMessage
              void $ pure $ setValueToLocalStore FINDING_EDIT_LOC_RESULTS "false"
              doAff do liftEffect $ push $ exitAction
            else do
              void $ delay $ Milliseconds duration
              getEditLocResults pollingId action exitAction (usableCount - 1) duration push state
      else do
        void $ pure $ setValueToLocalStore FINDING_EDIT_LOC_RESULTS "false"
        void $ pure $ EHU.showToast "SOMETHING WENT WRONG. PLEASE TRY AGAIN LATER"
        doAff do liftEffect $ push $ exitAction

-- Polling for IOP estimates
getEstimatePolling :: forall action. String -> (GetQuotesRes -> Int -> action) -> action  -> Int -> Number -> (action -> Effect Unit) -> HomeScreenState -> Flow GlobalState Unit
getEstimatePolling pollingId action flowStatusAction count duration push state = do
  let isValidCase = (isLocalStageOn FindingEstimate || isLocalStageOn SettingPrice ) && (not $ isLocalStageOn ProviderSelection) && ((getValueToLocalStore STARTED_ESTIMATE_SEARCH) == "TRUE")
  if isValidCase then do
    if (count > 0) then do
        resp <- getQuotes (state.props.searchId)
        if isValidCase then -- race Condition check for the case when user has already moved to next screen
          case resp of
              Right response -> do
                  void $ pure $ printLog "api Results new" response
                  let (GetQuotesRes resp) = response
                      topProvider = filter (\(EstimateAPIEntity estimate) -> maybe false (\valueAdd -> valueAdd) estimate.isValueAddNP) resp.estimates
                  if not (null resp.quotes) || not (null topProvider) then do
                      doAff do liftEffect $ push $ action response count
                  else do
                      if (count == 1) then do
                        _ <- pure $ updateLocalStage SearchLocationModel
                        doAff do liftEffect $ push $ action response count
                      else do
                        void $ delay $ Milliseconds duration
                        pure unit
                  void $ delay $ Milliseconds duration
                  getEstimatePolling pollingId action flowStatusAction (count - 1) duration push state
              Left err -> do
                  let errResp = err.response
                      codeMessage = decodeError errResp.errorMessage "errorMessage"
                  if ( err.code == 400 && codeMessage == "ACTIVE_BOOKING_ALREADY_PRESENT" ) then do
                      void $ pure $ EHU.showToast "ACTIVE BOOKING ALREADY PRESENT"
                      doAff do liftEffect $ push $ flowStatusAction
                  else do
                      void $ delay $ Milliseconds duration
                      if (count == 1) then do
                        let response = GetQuotesRes { quotes: [], estimates: [], fromLocation: SearchReqLocationAPIEntity { lat: 0.0, lon: 0.0 }, toLocation: Nothing }
                        _ <- pure $ updateLocalStage SearchLocationModel
                        doAff do liftEffect $ push $ action response count
                      else do
                        getEstimatePolling pollingId action flowStatusAction (count - 1) duration push state
        else
          void $ pure $ setValueToLocalStore STARTED_ESTIMATE_SEARCH "FALSE"
    else
      void $ pure $ setValueToLocalStore STARTED_ESTIMATE_SEARCH "FALSE"
  else
    void $ pure $ setValueToLocalStore STARTED_ESTIMATE_SEARCH "FALSE"

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
      rideBookingListResponse <- Remote.rideBookingListWithStatus "30" "0" "COMPLETED" Nothing
      void $ pure $ setValueToLocalStore UPDATE_REPEAT_TRIPS "false"
      case rideBookingListResponse of
        Right listResp -> do
          handleResponse listResp
        Left _ -> liftFlow $ push $ action (RideBookingListRes {list : []} )

driverLocationTracking :: (Action -> Effect Unit) -> (String -> RideBookingRes -> Action) -> (String -> Action) -> (String -> Action) -> (Int -> Int -> Action) -> Number -> String -> HomeScreenState -> String -> Int -> Flow GlobalState Unit
driverLocationTracking push action driverArrivedAction driverReachedDestinationAction updateState duration trackingId state routeState expCounter = do
  _ <- pure $ printLog "trackDriverLocation2_function" trackingId
  let hideMarkerCallback = state.data.fareProductType == FPT.DELIVERY && HU.isParentView FunctionCall
  (GlobalState gbState) <- getState
  if (any (\stage -> isLocalStageOn stage) [ RideAccepted, RideStarted, ChatWithDriver]) && ((getValueToLocalStore TRACKING_ID) == trackingId) then do
    let bookingId = if state.props.bookingId == "" then gbState.homeScreen.props.bookingId else state.props.bookingId
        destinationLat = gbState.homeScreen.props.destinationLat
        destinationLong = gbState.homeScreen.props.destinationLong
        isBookingUpdated = gbState.homeScreen.data.isBookingUpdated
    if bookingId /= "" && (destinationLat /= 0.0 && destinationLong /= 0.0)
      then do
        if not isBookingUpdated then do
          respBooking <- ridebookingStatus bookingId
          either (const $ pure unit) handleRideBookingStatus respBooking
        else do
          respBooking <- rideBooking bookingId
          either (const $ pure unit) handleRideBookingResp respBooking
      else do
        mbResp <- getActiveBooking
        case mbResp of
          Nothing -> pure unit
          Just resp -> do
            void $ liftFlow $ push $ UpdateBookingDetails resp
            handleRideBookingResp resp
    if (state.data.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE) && (isLocalStageOn RideAccepted) then do
      _ <- pure $ enableMyLocation true
      _ <- pure $ removeAllPolylines ""
      _ <- doAff $ liftEffect $ animateCamera state.data.driverInfoCardState.sourceLat state.data.driverInfoCardState.sourceLng zoomLevel "ZOOM"
      _ <- doAff $ liftEffect $ JB.showMarker defaultMarkerConfig{ markerId = "ny_ic_src_marker", pointerIcon = "ny_ic_src_marker" } state.data.driverInfoCardState.sourceLat state.data.driverInfoCardState.sourceLng 110 0.5 0.9 (getNewIDWithTag "CustomerHomeScreen")
      void $ delay $ Milliseconds duration
      driverLocationTracking push action driverArrivedAction driverReachedDestinationAction updateState duration trackingId state routeState expCounter
      else do
        (GlobalState gbState) <- getState
        let rideId = if state.data.driverInfoCardState.rideId == "" then gbState.homeScreen.data.driverInfoCardState.rideId else state.data.driverInfoCardState.rideId
        response <- getDriverLocation rideId
        case response of
          Right (GetDriverLocationResp resp) -> do
            -- (GlobalState currentState) <- getState Fget
            let
              srcLat = (resp ^. _lat)
              srcLon = (resp ^. _lon)
              mbPreviousDropLat = gbState.homeScreen.data.driverInfoCardState.driversPreviousRideDropLocLat
              mbPreviousDropLon = gbState.homeScreen.data.driverInfoCardState.driversPreviousRideDropLocLon
              hasCurrentLocAndPrevDropLoc = isJust mbPreviousDropLat && isJust mbPreviousDropLon
              Tuple dstLat dstLon =
                case (any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver]), mbPreviousDropLat, mbPreviousDropLon of
                  true, Just previousDropLat, Just previousDropLon  -> Tuple previousDropLat previousDropLon
                  true, _, _ -> Tuple state.data.driverInfoCardState.sourceLat state.data.driverInfoCardState.sourceLng
                  false, _, _ -> Tuple gbState.homeScreen.data.driverInfoCardState.destinationLat gbState.homeScreen.data.driverInfoCardState.destinationLng
              trackingType = case Tuple (any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver]) hasCurrentLocAndPrevDropLoc of
                                Tuple true true -> ADVANCED_RIDE_TRACKING
                                Tuple false _ -> RIDE_TRACKING
                                _ -> DRIVER_TRACKING

              markers = getRouteMarkers state.data.driverInfoCardState.vehicleVariant state.props.city trackingType state.data.fareProductType (Just state.props.currentStage)
              markers' = getRouteMarkers state.data.driverInfoCardState.vehicleVariant state.props.city DRIVER_TRACKING state.data.fareProductType (Just state.props.currentStage)
              sourceSpecialTagIcon = zoneLabelIcon state.props.zoneType.sourceTag
              destSpecialTagIcon = zoneLabelIcon state.props.zoneType.destinationTag
              onUsRide = state.data.driverInfoCardState.providerType == CTP.ONUS
              routeType = if onUsRide then "LineString" else "DASH"
              specialLocationTag =  if (any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver]) then
                                      specialLocationConfig destSpecialTagIcon sourceSpecialTagIcon onUsRide getPolylineAnimationConfig
                                    else
                                      specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon false getPolylineAnimationConfig
              mapRouteConfig = if onUsRide then specialLocationTag else specialLocationTag{dashUnit = 30, gapUnit = 20}
            -- TODO :: may use in future
            -- if isSpecialPickupZone then do
            --   let srcMarkerConfig = defaultMarkerConfig{ pointerIcon = markers.srcMarker }
            --       destMarkerConfig = defaultMarkerConfig{ pointerIcon = markers.destMarker, primaryText = getMarkerPrimaryText 0 }
            --   _ <- pure $ removeAllPolylines ""
            --   _ <- liftFlow $ runEffectFn9 drawRoute (walkCoordinate srcLat srcLon dstLat dstLon) ( if os == "IOS" then "LineString" else "DOT") "#323643" false srcMarkerConfig destMarkerConfig 8 "DRIVER_LOCATION_UPDATE" specialLocationTag
            --   void $ delay $ Milliseconds duration
            --   driverLocationTracking push action driverArrivedAction updateState duration trackingId state routeState expCounter
            if ((getValueToLocalStore TRACKING_ID) == trackingId) then do
              if (getValueToLocalStore TRACKING_ENABLED) == "False" then do
                let srcMarkerConfig = defaultMarkerConfig{ markerId = markers.srcMarker, pointerIcon = markers.srcMarker }
                    destMarkerConfig = defaultMarkerConfig{ markerId = markers.destMarker, pointerIcon = markers.destMarker, anchorV = 1.0}
                _ <- pure $ setValueToLocalStore TRACKING_DRIVER "True"

                if (srcLat /= 0.0 && srcLon /= 0.0 && dstLat /= 0.0 && dstLon /= 0.0) then do
                  void $ pure $ removeAllPolylines ""
                  let routeConfig = mkRouteConfig (walkCoordinate srcLat srcLon dstLat dstLon) srcMarkerConfig destMarkerConfig Nothing "DRIVER_LOCATION_UPDATE" ( if os == "IOS" then "LineString" else "DOT") false JB.DEFAULT mapRouteConfig
                      deliveryRouteOnRideAccepted = deliveryDestRouteConfig state
                  when (not $ isLocalStageOn EditPickUpLocation) $ void $ liftFlow $ drawRoute ([routeConfig] <> deliveryRouteOnRideAccepted) (getNewIDWithTag "CustomerHomeScreen")
                  else pure unit
                void $ delay $ Milliseconds duration
                driverLocationTracking push action driverArrivedAction driverReachedDestinationAction updateState duration trackingId state routeState expCounter
              else if ((getValueToLocalStore TRACKING_DRIVER) == "False" || not (isJust state.data.route)) || (hasCurrentLocAndPrevDropLoc && isNothing state.data.routeCacheForAdvancedBooking) || hasCurrentLocAndPrevDropLoc /= state.data.previousRideDrop then do
                _ <- pure $ setValueToLocalStore TRACKING_DRIVER "True"
                routeResponse <- getRoute routeState $ makeGetRouteReq srcLat srcLon dstLat dstLon (if routeState == "pickup" then Just rideId else Nothing) 
                when (not $ isLocalStageOn EditPickUpLocation) $ do
                    routeResponseAdvanced <- do
                      case state.data.routeCacheForAdvancedBooking, mbPreviousDropLat, mbPreviousDropLon, routeResponse of
                        Nothing , Just previousDropLat, Just previousDropLon, Right (GetRouteResp routeResp) -> do
                          let routes = maybe (Nothing) (\(Route route) -> Just route) (routeResp !! 0)
                          {points, route, routeDistance, routeDuration} <- createRouteHelper routeState srcLat srcLon dstLat dstLon routes rideId
                          let normalRoutePoints = fromMaybe {points : []} points
                              lastPointInRoute = Arr.last normalRoutePoints.points
                              previousDropLat' = maybe previousDropLat (\resp -> resp.lat) lastPointInRoute
                              previousDropLon' = maybe previousDropLon (\resp -> resp.lng) lastPointInRoute
                          let routeReq = makeGetRouteReq previousDropLat' previousDropLon' state.data.driverInfoCardState.sourceLat state.data.driverInfoCardState.sourceLng (if routeState == "pickup" then Just rideId else Nothing) 
                          Just <$> getRoute routeState routeReq
                        Just advRoute, Just previousDropLat, Just previousDropLon, _ -> pure $ Just (Right (GetRouteResp ([advRoute])))
                        _, _, _, _ -> pure $ Just (Right (GetRouteResp []))

                    case routeResponse, routeResponseAdvanced of
                      Right (GetRouteResp routeResp), (Just (Right (GetRouteResp routeRespAdvanced)))  -> do
                        case ((routeResp) !! 0), ((routeRespAdvanced) !! 0), hasCurrentLocAndPrevDropLoc of
                          Just (Route routes), Just (Route routesAdvanced), true -> do
                            when (not $ isLocalStageOn EditPickUpLocation) $ void $ pure $ removeAllPolylines ""
                            {points, route, routeDistance, routeDuration} <- createRouteHelper routeState srcLat srcLon dstLat dstLon (Just routes) rideId
                            let newPoints = points
                                newRoute = route
                                routeDistanceNormal = fromMaybe 0 routeDistance
                                routeDurationNormal = fromMaybe 0 routeDuration
                            {points, route, routeDistance, routeDuration} <- createRouteHelper routeState dstLat dstLon state.data.driverInfoCardState.sourceLat state.data.driverInfoCardState.sourceLng (Just routesAdvanced) rideId
                            let newPointsAdv = points
                                newRouteAdv = route
                                routeDistanceAdvanced = fromMaybe 0 routeDistance
                                routeDurationAdvanced = fromMaybe 0 routeDuration
                                distanceBwDriverAndPickup = (getDistanceBwCordinates srcLat srcLon state.data.driverInfoCardState.sourceLat state.data.driverInfoCardState.sourceLng) * 1000.0
                                driverWithinPickupThreshold = distanceBwDriverAndPickup > state.data.config.mapConfig.locateOnMapConfig.editPickUpThreshold
                                callback = if hideMarkerCallback then "" else runFn2 getMarkerCallback push MarkerLabelOnClick
                                destMarkerPrimaryText = if (any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver]) then state.data.driverInfoCardState.source else state.data.driverInfoCardState.destination
                                srcMarkerConfig = defaultMarkerConfig{ markerId = markers'.srcMarker, pointerIcon = markers'.srcMarker }
                                srcMarkerConfig' = defaultMarkerConfig{ markerId = "dummy_src", pointerIcon = "" , primaryText = "", anchorV = 1.0  }
                                destMarkerConfig = defaultMarkerConfig{ markerId = markers.destMarker, pointerIcon = markers.destMarker, primaryText = "Completing ride nearby" , anchorV = 0.8, markerSize = if os == "IOS" then 30.0 else 70.0, labelMaxWidth = 350, useMarkerSize = true, useAnchorConfig = true, markerSizeWidth = 30.0 , markerSizeHeight = 30.0}  --- markerSizeWidth, markerSizeHeight use in case of ios
                                destMarkerConfig' = defaultMarkerConfig{ markerId = markers'.destMarker, pointerIcon = markers'.destMarker, primaryText = destMarkerPrimaryText, anchorV = 1.0, markerCallback = callback, actionImage = getMarkerActionImageConifg state driverWithinPickupThreshold}
                                normalRoutePoints = fromMaybe {points : []} newPoints
                                normalAdvRoutePoints = fromMaybe {points : []} newPointsAdv
                                normalRoute = mkRouteConfig normalRoutePoints srcMarkerConfig destMarkerConfig Nothing "DRIVER_LOCATION_UPDATE" "LineString" true JB.DEFAULT mapRouteConfig{isAnimation = true}
                                normalAdvRouteConfig = mkRouteConfig normalAdvRoutePoints srcMarkerConfig' destMarkerConfig' Nothing "ADVANCED_ROUTE" ( if os == "IOS" then "LineString" else "DOT") true JB.ADVANCED mapRouteConfig{isAnimation = false}
                            void $ pure $ setValueToLocalStore ADVANCED_ROUTE_DISTANCE (show routesAdvanced.distance)
                            void $ pure $ setValueToLocalStore DRIVER_WITHIN_PICKUP_THRESHOLD $ show driverWithinPickupThreshold
                            when (not $ isLocalStageOn EditPickUpLocation) $ liftFlow $ drawRoute [normalRoute,normalAdvRouteConfig] (getNewIDWithTag "CustomerHomeScreen")
                            _ <- doAff do liftEffect $ push $ updateState (routes.duration + routesAdvanced.duration) (routes.distance + routesAdvanced.distance)
                            void $ delay $ Milliseconds duration
                            driverLocationTracking push action driverArrivedAction driverReachedDestinationAction updateState duration trackingId state { data { route = newRoute, routeCacheForAdvancedBooking = newRouteAdv, previousRideDrop = true, speed = (routeDistanceNormal + routeDistanceAdvanced) / (routeDurationNormal + routeDurationAdvanced) } } routeState expCounter
                          Just (Route routes), Nothing, false -> do
                            rentalPoints <- if state.data.fareProductType == FPT.RENTAL && isLocalStageOn RideAccepted then getRentalPoints routeState dstLat dstLon state rideId else pure Nothing
                            let callback = if hideMarkerCallback then "" else runFn2 getMarkerCallback push MarkerLabelOnClick
                                distanceBwDriverAndPickup = (getDistanceBwCordinates srcLat srcLon state.data.driverInfoCardState.sourceLat state.data.driverInfoCardState.sourceLng) * 1000.0
                                driverWithinPickupThreshold = distanceBwDriverAndPickup > state.data.config.mapConfig.locateOnMapConfig.editPickUpThreshold
                                destMarkerPrimaryText = if (any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver]) then state.data.driverInfoCardState.source else state.data.driverInfoCardState.destination
                                destLat = if state.data.fareProductType == FPT.RENTAL && isLocalStageOn RideStarted then (maybe dstLat (\loc -> loc.lat) state.props.stopLoc) else dstLat
                                destLon = if state.data.fareProductType == FPT.RENTAL && isLocalStageOn RideStarted then (maybe dstLon (\loc -> loc.lng) state.props.stopLoc) else dstLon
                            {points, route, routeDistance, routeDuration} <- createRouteHelper routeState srcLat srcLon destLat destLon (Just routes) rideId
                            if (srcLat /= 0.0 && srcLon /= 0.0 && destLat /= 0.0 && destLon /= 0.0) then do
                              when (not $ isLocalStageOn EditPickUpLocation) $ void $ pure $ removeAllPolylines ""
                              let srcMarkerConfig = defaultMarkerConfig{ markerId = markers.srcMarker, pointerIcon = markers.srcMarker, primaryText = getMarkerPrimaryText routes.distance }
                                  destMarkerConfig = defaultMarkerConfig{ markerId = markers.destMarker, pointerIcon = markers.destMarker, primaryText = destMarkerPrimaryText, anchorV = 1.0, markerCallback = callback, actionImage = getMarkerActionImageConifg state driverWithinPickupThreshold }
                                  srcRentalMarkerConfig = defaultMarkerConfig{ markerId = "", pointerIcon = "" , primaryText = ""}
                                  destRentalMarkerConfig = defaultMarkerConfig{ markerId = "ny_ic_blue_marker", pointerIcon = "ny_ic_blue_marker", primaryText = "", anchorV = 1.0  }
                                  normalRoutePoints = fromMaybe {points : []} points
                                  rentalRoutePoints = fromMaybe {points : []} rentalPoints
                                  normalRoute = mkRouteConfig  normalRoutePoints srcMarkerConfig destMarkerConfig Nothing "DRIVER_LOCATION_UPDATE" "LineString" true JB.DEFAULT mapRouteConfig
                                  rentalRouteConfig = mkRouteConfig rentalRoutePoints srcRentalMarkerConfig destRentalMarkerConfig Nothing "DRIVER_LOCATION_UPDATE" ( if os == "IOS" then "LineString" else "DOT") true JB.RENTAL mapRouteConfig{isAnimation = false}
                                  deliveryRouteOnRideAccepted = deliveryDestRouteConfig state
                                  routeArray = ([normalRoute] <> (if isNothing rentalPoints then [] else [rentalRouteConfig]) <> deliveryRouteOnRideAccepted)
                              void $ pure $ setValueToLocalStore ADVANCED_ROUTE_DISTANCE "0"
                              void $ pure $ setValueToLocalStore DRIVER_WITHIN_PICKUP_THRESHOLD $ if driverWithinPickupThreshold then "true" else "false"
                              when (not $ isLocalStageOn EditPickUpLocation) $  liftFlow $ drawRoute routeArray (getNewIDWithTag "CustomerHomeScreen")
                            else pure unit
                            _ <- doAff do liftEffect $ push $ updateState (fromMaybe 1 routeDuration) $ fromMaybe 1 routeDistance
                            let duration' = if isJust route then duration else getDuration state.data.config.driverLocationPolling.retryExpFactor expCounter
                                expCounter' = if isJust route then expCounter else expCounter + 1
                            void $ delay $ Milliseconds duration'
                            driverLocationTracking push action driverArrivedAction driverReachedDestinationAction updateState duration trackingId state { data { route = route, speed = (fromMaybe 0 routeDistance) / (fromMaybe 1 routeDuration), routeCacheForAdvancedBooking = Nothing, previousRideDrop = false } } routeState expCounter'
                          _, _, _ -> do
                            if (state.data.fareProductType == FPT.RENTAL && not hasCurrentLocAndPrevDropLoc && srcLat /= 0.0 && srcLon /= 0.0) then do
                              when (not $ isLocalStageOn EditPickUpLocation) $  void $ pure $ removeAllPolylines ""
                              void $ doAff $ liftEffect $ JB.showMarker defaultMarkerConfig{ markerId = markers.srcMarker, pointerIcon = markers.srcMarker } srcLat srcLon 160 0.5 0.5 (getNewIDWithTag "CustomerHomeScreen")
                              void $ doAff $ liftEffect $ animateCamera srcLat srcLon zoomLevel "ZOOM"
                            else pure unit
                            void $ delay $ Milliseconds $ getDuration state.data.config.driverLocationPolling.retryExpFactor expCounter
                            driverLocationTracking push action driverArrivedAction driverReachedDestinationAction updateState duration trackingId state { data { route = Nothing, routeCacheForAdvancedBooking = Nothing, previousRideDrop = false } } routeState (expCounter + 1)
                      _ , _-> do
                        void $ delay $ Milliseconds $ getDuration state.data.config.driverLocationPolling.retryExpFactor expCounter
                        driverLocationTracking push action driverArrivedAction driverReachedDestinationAction updateState duration trackingId state { data { route = Nothing, routeCacheForAdvancedBooking = Nothing, previousRideDrop = false } } routeState (expCounter + 1)
              else do
                  when (not $ isLocalStageOn EditPickUpLocation) $ do
                    case state.data.route, (state.data.previousRideDrop == hasCurrentLocAndPrevDropLoc )of
                      Just (Route route), _  -> do
                            locationResp <- liftFlow $ isCoordOnPath (walkCoordinates route.points) (resp ^. _lat) (resp ^. _lon) (state.data.speed)
                            if locationResp.isInPath then do
                              let newPoints = { points : locationResp.points}
                                  distanceBwDriverAndPickup = (getDistanceBwCordinates srcLat srcLon state.data.driverInfoCardState.sourceLat state.data.driverInfoCardState.sourceLng) * 1000.0
                                  driverWithinPickupThreshold = distanceBwDriverAndPickup > state.data.config.mapConfig.locateOnMapConfig.editPickUpThreshold
                                  specialLocationTag =  if (any (\stage -> isLocalStageOn stage) [ RideAccepted, ChatWithDriver]) then
                                                          specialLocationConfig "" sourceSpecialTagIcon onUsRide getPolylineAnimationConfig
                                                        else
                                                          specialLocationConfig "" destSpecialTagIcon false getPolylineAnimationConfig
                                  mapRouteConfig = if onUsRide then specialLocationTag else specialLocationTag{dashUnit = 30, gapUnit = 20}
                                  callback = if hideMarkerCallback then "" else runFn2 getMarkerCallback push MarkerLabelOnClick
                                  destMarkerConfig =  defaultMarkerConfig { markerId = markers.destMarker, pointerIcon = if hasCurrentLocAndPrevDropLoc then "dummy_dest" else markers.destMarker, markerCallback = callback, actionImage = getMarkerActionImageConifg state driverWithinPickupThreshold}
                                  eta = if not hasCurrentLocAndPrevDropLoc || any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver] then getMarkerPrimaryText (locationResp.distance + fromMaybe 0 (fromString $  getValueToLocalStore ADVANCED_ROUTE_DISTANCE)) else ""
                              void $ pure $ setValueToLocalStore DRIVER_WITHIN_PICKUP_THRESHOLD $ if driverWithinPickupThreshold then "true" else "false"
                              liftFlow $ runEffectFn1 updateRoute updateRouteConfig { json = newPoints, destMarkerConfig = destMarkerConfig, locationName = if (any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver]) then state.data.driverInfoCardState.source else state.data.driverInfoCardState.destination, eta = eta, srcMarker = markers.srcMarker, specialLocation = mapRouteConfig, zoomLevel = zoomLevel, pureScriptID = (getNewIDWithTag "CustomerHomeScreen"),  polylineKey = "DEFAULT"}
                              _ <- doAff do liftEffect $ push $ updateState locationResp.eta locationResp.distance
                              void $ delay $ Milliseconds duration
                              driverLocationTracking push action driverArrivedAction driverReachedDestinationAction updateState duration trackingId state routeState expCounter
                            else do
                              driverLocationTracking push action driverArrivedAction driverReachedDestinationAction updateState duration trackingId state { data { route = Nothing, routeCacheForAdvancedBooking = Nothing, previousRideDrop = false } } routeState expCounter
                      _ , _ -> driverLocationTracking push action driverArrivedAction driverReachedDestinationAction updateState duration trackingId state { data { route = Nothing, routeCacheForAdvancedBooking = Nothing, previousRideDrop = false } } routeState expCounter
            else pure unit
          Left _ -> do
            void $ delay $ Milliseconds $ getDuration state.data.config.driverLocationPolling.retryExpFactor expCounter
            driverLocationTracking push action driverArrivedAction driverReachedDestinationAction updateState duration trackingId state { data { route = Nothing, routeCacheForAdvancedBooking = Nothing, previousRideDrop = false } } routeState (expCounter + 1)
  else do
    pure unit
  where
    getDuration factor counter = duration * (toNumber $ pow factor counter)
    isSpecialPickupZone = state.props.currentStage == RideAccepted && state.props.zoneType.priorityTag == SPECIAL_PICKUP && isJust state.data.driverInfoCardState.sourceAddress.area && state.data.config.feature.enableSpecialPickup

    handleRideBookingStatus (RideBookingStatusRes respBooking) = do
      (GlobalState state') <- getState
      if respBooking.isBookingUpdated || respBooking.bookingStatus == "REALLOCATED" then do
        void $ modifyState \(GlobalState globalState) -> GlobalState $ globalState { homeScreen {data{isBookingUpdated = true} } }
        updatedResp <- rideBooking respBooking.id
        either (const $ pure unit) handleRideBookingResp updatedResp
      else do
        let
            rideScheduledTime = state'.homeScreen.data.driverInfoCardState.rideScheduledAtUTC
            isScheduledRide = ((fromMaybe (getCurrentUTC "") rideScheduledTime) > (getCurrentUTC ""))
            scheduledTimeDiff = compareUTCDate (getCurrentUTC "") (fromMaybe (getCurrentUTC "") rideScheduledTime)
            waitTimeStartTime =
                              if isScheduledRide then
                                case respBooking.driverArrivalTime ,rideScheduledTime of
                                  Just driverArrivalTime, Just rideScheduledTime ->
                                    if (compareUTCDate driverArrivalTime rideScheduledTime > 0) then
                                      driverArrivalTime
                                      else
                                      rideScheduledTime
                                  Just driverArrivalTime ,  Nothing -> driverArrivalTime
                                  _ , _ -> ""
                                else fromMaybe "" respBooking.driverArrivalTime
        if ((isScheduledRide && scheduledTimeDiff > 0 && (isJust respBooking.driverArrivalTime ))|| ((not isScheduledRide )&& respBooking.driverArrivalTime /= Nothing && (getValueToLocalStore DRIVER_ARRIVAL_ACTION) == "TRIGGER_DRIVER_ARRIVAL" )) then do
              let _ = spy "WAIT_TIME_TRIGERRED" waitTimeStartTime
              doAff do liftEffect $ push $ driverArrivedAction waitTimeStartTime
        else do
          let _ = spy "WAIT_TIME_TRIGERRED" waitTimeStartTime
          pure unit
        case respBooking.rideStatus of
          Just rideStatus -> do
            void $ modifyState \(GlobalState globalState) -> GlobalState $ globalState { homeScreen {data{driverInfoCardState { driversPreviousRideDropLocLat = respBooking.driversPreviousRideDropLocLat, driversPreviousRideDropLocLon = respBooking.driversPreviousRideDropLocLon } } }, followRideScreen { data { sosStatus = respBooking.sosStatus } } }
            doAff do liftEffect $ push $ UpdateCurrentStageStatus rideStatus ( RideBookingStatusRes respBooking)
          Nothing -> pure unit

    handleRideBookingResp (RideBookingRes respBooking) = do
      let bookingStatus = respBooking.status
          fareProductType' = getFareProductType $ respBooking.bookingDetails ^._fareProductType
          stopLocation = if fareProductType' == FPT.RENTAL then _stopLocation else _toLocation
          stopLocationDetails = fromMaybe dummyBookingDetails (respBooking.bookingDetails ^._contents^.stopLocation)
          isScheduledRide = respBooking.isScheduled || ((fromMaybe (getCurrentUTC "") respBooking.rideScheduledTime) > (getCurrentUTC ""))
          (BookingLocationAPIEntity toLocation) = stopLocationDetails
      void $ modifyState \(GlobalState globalState) -> GlobalState $ globalState { homeScreen {props{bookingId = respBooking.id,destinationLat=toLocation.lat,destinationLong=toLocation.lon}, data{driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes respBooking) (state.data.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE) state.data.driverInfoCardState} } }
      let fareProductType = respBooking.bookingDetails ^. _fareProductType
      case bookingStatus of
        "REALLOCATED" -> do
            doAff do liftEffect $ push $ action bookingStatus ( RideBookingRes respBooking)
        _             -> do
            case (respBooking.rideList !! 0) of
              Just (RideAPIEntity res) -> do
                let rideStatus = res.status
                doAff do liftEffect $ push $ action rideStatus ( RideBookingRes respBooking)
                let scheduledTimeDiff = compareUTCDate (getCurrentUTC "") (fromMaybe (getCurrentUTC "") respBooking.rideScheduledTime)
                    waitTimeStartTime =
                      if isScheduledRide then
                        case res.driverArrivalTime , respBooking.rideScheduledTime of
                          Just driverArrivalTime, Just rideScheduledTime ->
                            if (compareUTCDate driverArrivalTime rideScheduledTime > 0) then
                              driverArrivalTime
                              else
                              rideScheduledTime
                          Just driverArrivalTime ,  Nothing -> driverArrivalTime
                          _ , _ -> ""
                        else fromMaybe "" res.driverArrivalTime
                if ((isScheduledRide && scheduledTimeDiff > 0 && (isJust res.driverArrivalTime ))|| ((not isScheduledRide )&& res.driverArrivalTime /= Nothing && (getValueToLocalStore DRIVER_ARRIVAL_ACTION) == "TRIGGER_DRIVER_ARRIVAL" )) then
                  doAff do liftEffect $ push $ driverArrivedAction waitTimeStartTime
                else pure unit
                if (res.destinationReachedAt /= Nothing && (getValueToLocalStore DRIVER_REACHED_DESTINATION_ACTION) == "TRIGGER_DRIVER_REACHED_DESTINATION") then
                  doAff do liftEffect $ push $ driverReachedDestinationAction $ fromMaybe "" res.destinationReachedAt
                else pure unit
              Nothing -> pure unit

    getMarkerPrimaryText distance =
      if isSpecialPickupZone then
        fromMaybe "" state.data.driverInfoCardState.sourceAddress.area
      else
        metersToKm distance (state.props.currentStage == RideStarted)

    getRentalPoints routeState dstLat dstLon state rideId = do
      {points, route, routeDistance, routeDuration} <- createRouteHelper routeState dstLat dstLon ( maybe (0.0) (\loc -> loc.lat) state.props.stopLoc) (maybe 0.0 (\loc -> loc.lng) state.props.stopLoc) Nothing rideId--state.data.driverInfoCardState.destinationLng
      pure points

confirmRide :: forall action. String -> (RideBookingRes -> action) -> action -> action -> Int -> Number -> (action -> Effect Unit) -> HomeScreenState -> Flow GlobalState Unit
confirmRide trackingId rideConfirmationAction checkFlowStatusAction goToHomeScreenAction count duration push state = do
  let bookingId = if (DS.null state.props.bookingId) then maybe "" (\bookingTime -> bookingTime.bookingId) $ head $ decodeBookingTimeList FunctionCall else state.props.bookingId
  if (count > 0) && (isLocalStageOn ConfirmingRide || isLocalStageOn ConfirmingQuotes || state.props.startScheduledRidePolling) && bookingId /= "" && trackingId == (getValueToLocalStore TRACKING_ID) then do
    resp <- rideBooking bookingId
    _ <- pure $ printLog "response to confirm ride:- " (state.props.searchId)
    case resp of
      Right response -> do
        _ <- pure $ printLog "api Results " response
        let (RideBookingRes resp) = response
            fareProductType = getFareProductType $ (resp.bookingDetails) ^. _fareProductType
            otpCode = ((resp.bookingDetails) ^. _contents ^. _otpCode)
            isSpecialZoneRide = fareProductType == FPT.ONE_WAY_SPECIAL_ZONE || isJust otpCode
            status = if isSpecialZoneRide then "CONFIRMED" else "TRIP_ASSIGNED"
        if status == resp.status && (isSpecialZoneRide || not (null resp.rideList)) then do
          doAff do liftEffect $ push $ rideConfirmationAction response
          pure unit
        else if resp.status == "CANCELLED" then do
          doAff do liftEffect $ push $ checkFlowStatusAction
          pure unit
        else if resp.status == "REALLOCATED" then do
          doAff do liftEffect $ push $ checkFlowStatusAction
        else do
          void $ delay $ Milliseconds duration
          confirmRide trackingId rideConfirmationAction checkFlowStatusAction goToHomeScreenAction (count - 1) duration push state
      Left err -> do
        _ <- pure $ printLog "api error " err
        void $ delay $ Milliseconds duration
        confirmRide trackingId rideConfirmationAction checkFlowStatusAction goToHomeScreenAction (count - 1) duration push state
  else if (count <= 0) && (isLocalStageOn ConfirmingRide) then
    doAff do liftEffect $ push $ goToHomeScreenAction
  else do
    pure unit

cancelRidePopUpView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
cancelRidePopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , accessibility DISABLE
    ][ CancelRidePopUp.view (push <<< CancelRidePopUpAction) (cancelRidePopUpConfig state)]

checkForLatLongInSavedLocations :: forall action. (action -> Effect Unit) -> (Array LocationListItemState -> action) -> HomeScreenState -> FlowBT String Unit
checkForLatLongInSavedLocations push action state = do
  void $ setValueToLocalStore RELOAD_SAVED_LOCATION "false"
  void $ transformSavedLocations state.data.savedLocations
  if getValueToLocalStore RELOAD_SAVED_LOCATION == "true" then do
    (SavedLocationsListRes savedLocationResp )<- FlowCache.updateAndFetchSavedLocations false
    liftFlowBT $ push $ action $ AddNewAddress.getSavedLocations savedLocationResp.list
  else pure unit
  void $ setValueToLocalStore RELOAD_SAVED_LOCATION "false"

zoneTimerExpiredView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
zoneTimerExpiredView state push =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity CENTER
  ][ PopUpModal.view (push <<< ZoneTimerExpired) (zoneTimerExpiredConfig state)]

rentalInfoPopUp :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rentalInfoPopUp push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity CENTER
  ][ PopUpModal.view (push <<< RentalInfoAction) (rentalInfoViewConfig state)]

editButtontView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
editButtontView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , stroke $ "1," <> state.data.config.confirmPickUpLocationBorder
  , cornerRadius if (os == "IOS") then 15.0 else 20.0
  , padding (Padding 10 6 10 6)
  , margin $ MarginLeft 10
  , accessibilityHint "Edit Pickup Location : Button"
  , visibility $ boolToVisibility $ state.props.currentStage /= EditPickUpLocation && state.data.fareProductType /= FPT.DELIVERY
  ][ textView
      $
      [ text (getString EDIT)
      , color Color.black800
      , gravity CENTER_VERTICAL
      , accessibility DISABLE
      ]
      <> FontStyle.body1 TypoGraphy
  ]

currentLocationView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> CurrentLocationConfig -> PrestoDOM (Effect Unit) w
currentLocationView push state config =
  linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , margin $ MarginVertical 20 10
            , clickable $ config.isClickable
            , onClick push $ const config.click
            , padding $ PaddingHorizontal 15 15
            , stroke config.border
            , gravity CENTER_VERTICAL
            , accessibility DISABLE
            , alpha config.alpha
            , cornerRadius $ if state.data.fareProductType == FPT.DELIVERY then 12.0 else 5.0
            , visibility $ boolToVisibility config.visibility
            ]
            [ imageView
                [ imageWithFallback $ fetchImage FF_COMMON_ASSET config.image
                , height $ V 16
                , width $ V 16
                , gravity CENTER_VERTICAL
                , accessibility DISABLE
                ]
            , textView
                $
                  [ text config.text
                  , ellipsize true
                  , maxLines (if state.data.fareProductType == FPT.DELIVERY then 1 else 2)
                  , accessibility ENABLE
                  , accessibilityHint config.accessibilityText
                  , gravity LEFT
                  , weight 1.0
                  , padding (Padding 10 16 10 16)
                  , color Color.black800
                  ]
                <> (if state.data.fareProductType == FPT.DELIVERY then FontStyle.body1 TypoGraphy else FontStyle.subHeading1 TypoGraphy)
              , editButtontView push state
            ]

deliveryPickupAndDropLocationView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
deliveryPickupAndDropLocationView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , visibility $ boolToVisibility $ state.data.fareProductType == FPT.DELIVERY && state.props.currentStage /= EditPickUpLocation
    ]
    [ currentLocationView push state $ deliveryPickupLocationConfig state
    , currentLocationView push state $ deliveryDropLocationConfig state
    ]

nearByPickUpPointsView :: forall w . HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
nearByPickUpPointsView state push =
  scrollView
  [ height $ V $ getPickUpViewHeight state.data.nearByPickUpPoints
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ Padding 5 20 0 5
  , visibility $ boolToVisibility $ (not (DS.null state.props.defaultPickUpPoint)) && state.data.fareProductType /= FPT.DELIVERY
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
  where
    getPickUpViewHeight nearByPickUpPoints =
      let
        menuBtnHeight = 56 -- Update Menu Button Height
        padding = 28
        allowMaxLen = if state.props.currentStage /= EditPickUpLocation then 3 else 2
        len = if (length nearByPickUpPoints > allowMaxLen) then allowMaxLen else length nearByPickUpPoints
        removeExtraPadding = if len > 1 then padding else 0
        pickUpPointViewHeight = len * menuBtnHeight + len * padding - removeExtraPadding
        finalHeight = if os == "IOS"
                            then pickUpPointViewHeight - len * 10
                            else pickUpPointViewHeight
      in
      finalHeight


isAnyOverlayEnabled :: HomeScreenState -> Boolean
isAnyOverlayEnabled state = state.data.settingSideBar.opened /= SettingSideBar.CLOSED || state.props.cancelSearchCallDriver || state.props.isCancelRide || state.props.isLocationTracking || state.props.callSupportPopUp || state.props.showCallPopUp || state.props.showRateCard || (state.props.showShareAppPopUp && state.data.config.feature.enableShareApp || state.data.waitTimeInfo)

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


----------------------------------------- UPDATED HOME SCREEN -------------------------------------------

homeScreenViewV2 :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
homeScreenViewV2 push state =
    relativeLayout
      [ height MATCH_PARENT
      , width $ V (screenWidth unit)
      ][ linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , orientation VERTICAL
          ][  homescreenHeader push state
            , linearLayout
              [ weight 1.0
              ][ scrollView
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , nestedScrollView true
                  , scrollBarY false
                  ][ linearLayout
                      [ width $ V (screenWidth unit)
                      , height WRAP_CONTENT
                      , orientation VERTICAL
                      ] [ relativeLayout
                          [ width MATCH_PARENT
                          , height WRAP_CONTENT
                          , orientation VERTICAL
                          , gravity $ CENTER_HORIZONTAL
                          ][ locationUnserviceableOrUserBlockedView push state
                           , homeScreenContent push state
                           , PrestoAnim.animationSet [ Anim.triggerOnAnimationEnd true] $
                                linearLayout
                                [ height WRAP_CONTENT
                                , width MATCH_PARENT
                                , onAnimationEnd push (const MapReadyAction)
                                ][shimmerView state]
                      ]
                  ]
              ]
          ]
        ]
      ]

homeScreenContent :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
homeScreenContent push state =  let
  cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalStore CUSTOMER_LOCATION)
  in
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.white900
    , visibility $ boolToVisibility $ (state.props.isSrcServiceable && not state.props.userBlocked) && not state.props.showShimmer
    ][ linearLayout
       [ width MATCH_PARENT
       , height WRAP_CONTENT
       , orientation VERTICAL
       , id $ getNewIDWithTag "homescreenContent"
      ][ mapView' push state "CustomerHomeScreenMap"
        , if (isJust state.data.rentalsInfo && (isLocalStageOn HomeScreen || state.props.currentStage == HomeScreen)) then rentalBanner push state else linearLayout[visibility GONE][]
        , suggestionsView push state
        , servicesView push state
        , contentView state
        , exploreCitySection push state
      ]
    , footerView push state
    ]
  where
    contentView state =
      let banners = getBannerConfigs state BannerCarousel
      in
      linearLayout
      [ width $ V $ screenWidth unit
      , height $ V 172
      , visibility $ boolToVisibility $ state.props.city /= AnyCity && (not $ null banners)
      ]$[
        -- imageView
        --   [ imageWithFallback "ny_ic_cab_banner,https://assets.moving.tech/beckn/nammayatri/nammayatricommon/images/ny_ic_cab_banner.png"
        --   , height $ V 135
        --   , width MATCH_PARENT
        --   , gravity CENTER_VERTICAL
        --   , margin $ Margin 16 0 16 8
        --   , onClick push $ const WhereToClick
        --   , accessibility DISABLE
        --   , visibility $ boolToVisibility $ state.data.config.banners.homeScreenCabLaunch && Arr.elem state.props.city [ST.Bangalore, ST.Tumakuru, ST.Mysore]
        --   ]

      ] <> maybe [] (\item -> [bannersCarousal item state push]) state.data.bannerData.bannerItem

servicesView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
servicesView push state =
  let itemLen = length $ spy "nammaServices-codex" $ nammaServices FunctionCall
      appName = fromMaybe state.data.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
      firstName = fromMaybe "Yatri " (head (DS.split (DS.Pattern " ") appName))
  in
  linearLayout
  [ height WRAP_CONTENT
  , orientation VERTICAL
  , width MATCH_PARENT
  , margin $ Margin 16 20 16 0
  , visibility $ boolToVisibility $ if itemLen < 1 then false else true
  ]  [
      linearLayout[
          height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        ][  textView $
              [ text $ firstName <> " " <> getEN SERVICES
              , color Color.black900
              , padding $ PaddingLeft 8
              , height MATCH_PARENT
              ] <> FontStyle.subHeading1 TypoGraphy
          ],
    linearLayout[
      height WRAP_CONTENT
      , margin $ MarginTop $ if itemLen > 2 then 9 else 12
      , width MATCH_PARENT
    ] ( mapWithIndex ( \index item -> (if itemLen > 2 then verticalServiceView else horizontalServiceView) push index item ) (nammaServices FunctionCall))
  ]


horizontalServiceView :: forall w. (Action -> Effect Unit) -> Int -> RemoteConfig.Service -> PrestoDOM (Effect Unit) w
horizontalServiceView push index service =
  linearLayout
  [ height if service.hasSecondaryPill then WRAP_CONTENT else MATCH_PARENT
  , weight 1.0
  , orientation VERTICAL
  , gravity CENTER
  , margin $ MarginLeft $ if index == 0 then 0 else 16
  , accessibility ENABLE
  , accessibility DISABLE_DESCENDANT
  , accessibilityHint $ getEN service.name
  , onClick push $ const $ ServicesOnClick service
  ][linearLayout
    [ height if service.hasSecondaryPill then WRAP_CONTENT else MATCH_PARENT
    , width MATCH_PARENT
    , padding $ Padding 16 8 16 8
    , background service.backgroundColor
    , cornerRadius 12.0
    , gravity CENTER
    ][ imageView
      [ imageWithFallback $ service.image
      , height $ V 32
      , width $ V 32
      , margin $ MarginRight 8
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , orientation VERTICAL
      ][ textView $ 
        [ text $ getString $ service.name
        , color Color.black800
        , singleLine false
        ] <> FontStyle.body20 TypoGraphy
      , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER_HORIZONTAL
        , visibility $ boolToVisibility service.hasSecondaryPill
        ][ textView $
            [ text $ getString $ METRO_FREE_TICKET_EVENT $ getNumberWithSuffix $ 3
            , color Color.white900
            , padding $ Padding 6 3 6 3
            , background service.secondaryPillColor
            , cornerRadius 41.0
            , gravity CENTER
            , margin $ MarginTop 3
            ] <> FontStyle.captions TypoGraphy
        ]
      ]
    ]
  ]

verticalServiceView :: forall w. (Action -> Effect Unit) -> Int -> RemoteConfig.Service -> PrestoDOM (Effect Unit) w
verticalServiceView push index service = 
  relativeLayout
  [ height if service.hasSecondaryPill then WRAP_CONTENT else MATCH_PARENT
  , weight 1.0
  , orientation VERTICAL
  , gravity CENTER
  , accessibility ENABLE
  , accessibility DISABLE_DESCENDANT
  , accessibilityHint $ getEN service.name
  , margin $ MarginLeft $ if index == 0 then 0 else 12
  , onClick push $ const $ ServicesOnClick service
  ][linearLayout 
    [ height if service.hasSecondaryPill then WRAP_CONTENT else MATCH_PARENT
    , width MATCH_PARENT
    , gravity CENTER
    , visibility $ boolToVisibility service.hasSecondaryPill
    ][ textView $
        [ text $ getString OFFERS
        , color Color.white900
        , gravity CENTER
        , padding $ Padding 6 3 6 3
        , background service.secondaryPillColor
        , cornerRadius 41.0
        ] <> FontStyle.captions TypoGraphy
    ]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 5 5 5 5
    , background service.backgroundColor
    , margin $ MarginVertical 9 16
    , cornerRadius 12.0
    , gravity CENTER_HORIZONTAL
    , orientation VERTICAL
    ] [ imageView 
      [ imageWithFallback $ service.image
      , height $ V 32
      , width $ V 32
      ]
    ]
    , textView $ 
      [ text $ getString $ service.name
      , color Color.black800
      , width MATCH_PARENT
      , gravity CENTER
      , singleLine true
      , alignParentBottom "true,-1"
      ] <> FontStyle.body33 TypoGraphy
  ]

isHomeScreenView :: HomeScreenState -> Boolean
isHomeScreenView state = state.props.currentStage == HomeScreen || (state.props.currentStage == SearchLocationModel && state.props.isSearchLocation == SearchLocation && state.props.homeScreenPrimaryButtonLottie)

footerView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
footerView push state =
  let headerBounds = (runFn1 getLayoutBounds (getNewIDWithTag "homescreenHeader"))
      contentBounds = (runFn1 getLayoutBounds (getNewIDWithTag "homescreenContent"))
      contentHeight = contentBounds.height
      dynamicMargin =  screenHeight unit - getDefaultPixelSize (headerBounds.height + contentHeight )
      suggestions = if null state.data.tripSuggestions then length state.data.destinationSuggestions else length state.data.tripSuggestions
      marginTop = if state.props.suggestionsListExpanded || (os /= "IOS" && not state.props.suggestionsListExpanded && suggestions >= 3)
                      then getDefaultPixelSize 150
                  else if dynamicMargin > getDefaultPixelSize 150
                      then dynamicMargin
                  else getDefaultPixelSize 150
      bottomPadding = if state.data.config.feature.enableZooTicketBookingFlow then 50 else 0
  in
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 24
    , padding $ Padding 24 5 24 (30+bottomPadding+safeMarginBottom)
    , gravity CENTER
    , accessibilityHint $  getString BOOK_AND_MOVE <>  getString ANYWHERE_IN_THE_CITY
    ][
       textView $
        [ text $ getString BOOK_AND_MOVE
        , color Color.grey900
        , gravity CENTER
        ]  <> FontStyle.h1 TypoGraphy
      , textView $
        [ text $ getString ANYWHERE_IN_THE_CITY
        , gravity CENTER
        , color Color.grey900
        ] <> FontStyle.h1 TypoGraphy

      , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , margin $ MarginVertical 32 32
        , gradient (Linear 90.0 [Color.manatee5, Color.manatee33, Color.manatee5])
        ][]
      , linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER_VERTICAL
          , padding $ Padding 16 8 16 8
          , stroke $ "1," <> Color.grey900
          , cornerRadius 8.0
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
        , margin $ MarginTop 32
        ] <> FontStyle.body1 TypoGraphy
      , textView $
        [ text $ "#beOpen #chooseOpen"
        , gravity CENTER
        , color Color.black600
        , margin $ MarginTop 16
        ] <> FontStyle.paragraphText TypoGraphy
    ]

homescreenHeader :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
homescreenHeader push state =
  linearLayout
    [height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingTop safeMarginTop
    , background Color.white900
    , id $ getNewIDWithTag "homescreenHeader"
    , afterRender push $ const UpdatePeekHeight
    ][ pickupLocationView push state]


getHeaderLogo :: HomeScreenState -> String
getHeaderLogo state =
  fetchImage FF_ASSET $ if DS.null state.data.config.appData.logoLight then do
    if DS.null state.data.currentCityConfig.appLogoLight then "ny_ic_logo_dark"
    else state.data.currentCityConfig.appLogoLight
  else state.data.config.appData.logoLight

pickupLocationView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
pickupLocationView push state =
  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      ][
        linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation HORIZONTAL
          , padding (Padding 0 20 16 0)
          ][
            linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , gravity CENTER_VERTICAL
              , disableClickFeedback true
              , clickable $ not (state.props.currentStage == SearchLocationModel)
              , visibility if not state.data.config.terminateBtnConfig.visibility then GONE
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
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , padding $ Padding 16 0 12 14
              , onClick push $ const OpenSettings
              , clickable $ not (state.props.currentStage == SearchLocationModel)
              ][linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , gravity CENTER_VERTICAL
                , disableClickFeedback true
                , clickable $ not (state.props.currentStage == SearchLocationModel)
                , padding $ Padding 8 10 8 10
                , onClick push $ const OpenSettings
                , background $ state.data.config.homeScreen.header.menuButtonBackground
                , cornerRadius 8.0
                , stroke $ "1," <> Color.grey700
                , rippleColor Color.rippleShade
                ][ imageView
                    [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_menu_black"
                    , height $ V 12
                    , width $ V 17
                    , accessibility if state.props.currentStage == ChatWithDriver || state.props.isCancelRide || state.props.isLocationTracking || state.props.callSupportPopUp || state.props.cancelSearchCallDriver then DISABLE else ENABLE
                    , accessibilityHint "Navigation : Button"
                    ]
                ]
              ]
            , linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              ][ imageView
                  [ imageWithFallback $ getHeaderLogo state
                  , height $ V 32
                  , width $ V 93
                  , margin $ MarginRight 12
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
              , width MATCH_PARENT
              , gravity RIGHT
              ][ linearLayout
                 [ width WRAP_CONTENT
                 , height WRAP_CONTENT
                 , cornerRadius 8.0
                 , background Color.blue600
                 , onClick push $ const $ if state.props.isReferred then ReferralFlowNoAction else ReferralFlowAction
                 , visibility $ boolToVisibility $ not $ (not state.data.config.feature.enableReferral) || ((state.props.isReferred && state.props.currentStage == RideStarted) || state.props.hasTakenRide)
                 ][ textView
                    [ text $ if not state.props.isReferred then  getString HAVE_A_REFFERAL else (getString REFERRAL_CODE_APPLIED)
                    , color Color.blue800
                    , gravity CENTER_HORIZONTAL
                    , textSize FontSize.a_14
                    , padding $ Padding 12 8 12 8
                    ]
                 ]
              ]
            ]
        ]


mapView' :: forall w. (Action -> Effect Unit) -> HomeScreenState -> String -> PrestoDOM (Effect Unit) w
mapView' push state idTag =
  let mapDimensions = getMapDimensions state
      bottomPadding = if state.props.currentStage == ConfirmingLocation then getDefaultPixelSize extraBottomPadding else 0
      -- banners = getBannerConfigs state BannerCarousel
      enableActions = isHomeScreenView state && state.props.isSrcServiceable
      buttonPadding = if os == "IOS" then Padding 16 16 16 12 else Padding 16 16 16 16
      initialLottieView = getValueToLocalStore MapViewLottie == "true"
      mapViewLottieConfig = mapLottieConfig FunctionCall
  in
  PrestoAnim.animationSet[scaleYAnimWithDelay 5000] $
  Keyed.relativeLayout
    [ height mapDimensions.height
    , width mapDimensions.width
    , cornerRadius if state.props.currentStage == HomeScreen then 16.0 else 0.0
    , padding $ PaddingBottom $ bottomPadding
    ]$[ Tuple ("MapView" <> idTag) $ linearLayout
          ([ height mapDimensions.height
          , width mapDimensions.width
          , accessibility DISABLE_DESCENDANT
          , id (getNewIDWithTag idTag)
          , visibility if (state.props.isSrcServiceable && not state.props.userBlocked) then VISIBLE else GONE
          , cornerRadius if state.props.currentStage == HomeScreen && os == "IOS" then 16.0 else 0.0
          , background Color.white900
          , afterRender
            ( \action -> do
                _ <- push action
                if state.props.sourceLat == 0.0 && state.props.sourceLong == 0.0 then do
                  void $ getCurrentPosition push CurrentLocation
                else pure unit
                if state.props.mapLottieViewVisibility && isHomeScreenView state && initialLottieView && mapViewLottieConfig.visibility
                  then do
                    void $ startTimer 18 "MapLottieViewTimer" "1" push ToggleMapLottieView
                    void $ pure $ startLottieProcess lottieAnimationConfig{ rawJson = mapViewLottieConfig.lottieUrl, lottieId = getNewIDWithTag "HomeScreenLottieViewOverMap"}
                  else 
                    pure unit
                _ <- showMap (getNewIDWithTag idTag) (isHomeScreenView state) "satellite" zoomLevel state.props.sourceLat state.props.sourceLong push MAPREADY
                if os == "IOS" then
                  case state.props.currentStage of
                    HomeScreen -> void $ setMapPadding 0 0 0 0
                    ConfirmingLocation -> void $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = false, lat = state.props.sourceLat, lon = state.props.sourceLong, geoJson = state.data.polygonCoordinates, points = state.data.nearByPickUpPoints, zoomLevel = zoomLevel, labelId = getNewIDWithTag "LocateOnMapPin" }
                    _ -> pure unit
                else pure unit
                if state.props.openChatScreen && state.props.currentStage == RideAccepted then push OpenChatScreen
                else pure unit
                case state.props.currentStage of
                  HomeScreen -> if ((getSearchType unit) == "direct_search") then push DirectSearch else pure unit
                  _ -> pure unit
                pure unit
            )
            (const MapReadyAction)
          ])[]
     , Tuple "LottieViewOverMap" $ lottieAnimationView
        [ id (getNewIDWithTag "HomeScreenLottieViewOverMap")
        , visibility $ boolToVisibility $ state.props.mapLottieViewVisibility && isHomeScreenView state && initialLottieView && mapViewLottieConfig.visibility
        , width MATCH_PARENT
        , padding $ PaddingBottom 40
        ]
     , Tuple "BottomGradient" $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , alignParentBottom "true,-1"
        , gravity RIGHT
        , orientation VERTICAL
        , visibility $ boolToVisibility $ enableActions
        ][ linearLayout
          [ height $ V 60
          , width MATCH_PARENT
          , alignParentBottom "true,-1"
          , gradient (Linear 0.0 [Color.white900, Color.transparent])
          ][]
       ]
      , Tuple "TopGradient" $ relativeLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , visibility $ boolToVisibility $ enableActions
        ][ linearLayout
            [ height $ V 35
            , width MATCH_PARENT
            , gradient (Linear 180.0 [Color.white900, Color.transparent])
            ][]
          , if isFollowEnabled state
              then followView push $ getFollowers state
              else linearLayout[visibility GONE][]
        ]
      , Tuple "WhereTo" $ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity RIGHT
        , alignParentBottom "true,-1"
        , orientation VERTICAL
        , visibility $ boolToVisibility $ isHomeScreenView state
        ][ imageView
           [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_recenter_btn"
           , accessibility DISABLE
           , clickable true
           , margin $ MarginRight 16
           , onClick
               ( \action -> do
                   _ <- push action
                   _ <- getCurrentPosition push UpdateCurrentLocation
                   _ <- pure $ logEvent state.data.logField "ny_user_recenter_btn_click"
                   pure unit
               )
               (const $ RecenterCurrentLocation)
           , height $ V 32
           , width $ V 32
           ]
          , highCancellationBanner push state
          ,relativeLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , margin $ Margin 16 16 16 0
            , padding buttonPadding
            , alignParentBottom "true,-1"
            , gradient (Linear 180.0 [Color.gunMetal, Color.eerieBlack])
            , cornerRadius 12.0
            , rippleColor "#000000"
            , onClick ( \action -> do
                    void $ pure $ startLottieProcess lottieAnimationConfig{ rawJson = "primary_button_loader.json", lottieId = getNewIDWithTag "HomeScreenNewPrimaryButtonWithLottie"}
                    push action
                ) $ const OpenSearchLocation
            , gravity CENTER_VERTICAL
            , visibility $ boolToVisibility $ enableActions
            , accessibility ENABLE
            , accessibilityHint "Where are you going? : Button"
            ][
              linearLayout
                [ height WRAP_CONTENT
                , visibility $ boolToInvisibility $ not state.props.homeScreenPrimaryButtonLottie
                , gravity CENTER_VERTICAL
                , width MATCH_PARENT
                ][
                  imageView
                  [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_search_yellow"
                  , height $ V 16
                  , width $ V 16
                  , gravity CENTER_VERTICAL
                  , margin $ MarginRight 12
                  ]
                , textView $
                  [ text $ getString WHERE_ARE_YOU_GOING
                  , color Color.yellow900
                  , singleLine false
                  , padding $ if (getLanguageLocale languageKey) == "EN_US" && os /= "IOS" then PaddingBottom 4 else PaddingBottom 0
                  ] <> FontStyle.subHeading3 TypoGraphy
                ]
            , linearLayout
              ([ height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity CENTER
              ])
              [ lottieAnimationView
                  [ id (getNewIDWithTag "HomeScreenNewPrimaryButtonWithLottie")
                  , visibility $ boolToInvisibility state.props.homeScreenPrimaryButtonLottie
                  , height $ V 27
                  , width $ V 100
                  ]
              ]
            ]
      ]
  ]
  where
    followView :: forall w. (Action -> Effect Unit) -> Array Followers -> PrestoDOM (Effect Unit) w
    followView push followers =
      linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , padding $ PaddingHorizontal 16 16
        , margin $ MarginTop 20
        ][ followRideBar push followers MATCH_PARENT false true ]

showFollowerBar :: Array Followers -> HomeScreenState -> Boolean
showFollowerBar followers state = state.props.followsRide && followers /= []

followRideBar :: forall w. (Action -> Effect Unit) -> Array Followers -> Length -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w
followRideBar push followers customWidth addSafePadding useCornerRadius =
  linearLayout
    ([ height WRAP_CONTENT
    , width customWidth
    , background Color.blue800
    , gravity CENTER
    , padding $ Padding 16 (if addSafePadding then safeMarginTopWithDefault 8 else 8) 16 8
    , onClick push (const GoToFollowRide)
    ] <> if useCornerRadius
          then [cornerRadius 8.0]
          else [])
    [ imageView
      [ height $ V 20
      , width $ V 20
      , margin $ MarginRight 8
      , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_location_arrow"
      ]
    , textView $
      [ text $ getFollowersTitle
      , color Color.white900
      , weight 1.0
      , gravity LEFT
      ] <> FontStyle.tags TypoGraphy
    , imageView
      [ height $ V 16
      , width $ V 16
      , margin $ MarginLeft 8
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right_white"
      ]
    ]
  where
  followerCount = length followers

  getFollowersTitle = if followerCount == 1 then getString $ HAS_SHARED_A_RIDE_WITH_YOU followersName else (getString $ HAVE_SHARED_RIDE_WITH_YOU followersName) <> ". " <> getString TAP_TO_FOLLOW

  followersName =
    foldlWithIndex
      ( \idx acc item -> case item.name of
          Nothing -> acc
          Just name -> acc <> name <> (if (idx + 1) >= followerCount then "" else if (idx + 1) == (followerCount - 1) then " & " else ", ")
      )
      ""
      followers

getMapDimensions :: HomeScreenState -> {height :: Length, width :: Length}
getMapDimensions state =
  let mapViewLottieConfig = mapLottieConfig FunctionCall
      mapHeight = if (any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithDriver ] && os /= "IOS") then
                    getMapHeight state
                  else if (isHomeScreenView state) then
                    if state.props.mapLottieViewVisibility && getValueToLocalStore MapViewLottie == "true" && mapViewLottieConfig.visibility then V (getHeightFromPercent 40) else V (getHeightFromPercent 35)
                  else if (state.data.fareProductType == FPT.RENTAL) then
                    V (screenHeight unit - 100)
                  else MATCH_PARENT
      mapWidth = MATCH_PARENT
  in {height : mapHeight, width : mapWidth}

highCancellationBanner :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
highCancellationBanner push state =
  let appName = fromMaybe state.data.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
  in
  linearLayout 
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , background Color.brownishYellow
  , margin $ Margin 16 8 16 2
  , cornerRadius 12.0
  , visibility $ boolToVisibility $ showCancellationHighBanner state.data.cancellationRate
  ]
  [ linearLayout 
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ Margin 14 8 8 8
    , id $ EHC.getNewIDWithTag "cancellationBannerText"
    ]
    [ textView $
      [ text $ getString YOUR_CANCELLATION_RATE_IS_HIGH
      , color Color.black900
      , width $ V (((EHC.screenWidth unit) * 2)/ 3)
      , height WRAP_CONTENT
      ] <> FontStyle.subHeading3 TypoGraphy
    , textView $
      [ text $ getString $ AVOID_FURTHER_CANCELLATIONS_TO_KEEP_USING_APP appName
      , color Color.black900
      , width $ V (((EHC.screenWidth unit) * 2)/ 3)
      , singleLine false
      , height WRAP_CONTENT
      , margin $ MarginVertical 4 4
      ] <> FontStyle.body1 TypoGraphy
    ]
    , linearLayout[weight 1.0][]
    , linearLayout
      [ width WRAP_CONTENT
      , orientation VERTICAL
      , height $ V (if contentLayout.height == 0 then 80 else contentLayoutHeight)
      ]
      [ linearLayout[weight 1.0][]
      , imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_cancellation_high"
        , accessibility DISABLE
        , margin $ MarginRight 12
        , height $  V 80
        , width $ V 80
        ]
      ]
    ]
    where 
      showCancellationHighBanner :: Maybe Number -> Boolean
      showCancellationHighBanner cancellationRate =
        let rate = fromMaybe 0.0 cancellationRate
            cancellationThresholdConfig = RemoteConfig.getCancellationBannerThresholdConfig $  DS.toLower $ getValueToLocalStore CUSTOMER_LOCATION
        in cancellationThresholdConfig.showBanner && rate >= cancellationThresholdConfig.percentage
      contentLayoutHeight = HU.getDefaultPixelSize(contentLayout.height + 20)
      contentLayout = runFn1 JB.getLayoutBounds $ EHC.getNewIDWithTag "cancellationBannerText"
        

suggestionsView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
suggestionsView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ Margin 16 16 16 0
  , visibility $ boolToVisibility $ suggestionViewVisibility state && not (state.props.showShimmer && null state.data.tripSuggestions)
  ]
  [ let isTripSuggestionsEmpty = null state.data.tripSuggestions
    in linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity CENTER_VERTICAL
      , margin $ MarginBottom 12
      ][ textView $
        [ height MATCH_PARENT
        , width WRAP_CONTENT
        , text if isTripSuggestionsEmpty then getString SUGGESTED_DESTINATION else getString ONE_TAP_BOOKINGS
        , color Color.black800
        , gravity CENTER_VERTICAL
        , padding $ PaddingHorizontal 8 8
        , accessibilityHint
          if isTripSuggestionsEmpty
          then "Suggested Destinations"
          else "One Click Bookings"
        ] <> FontStyle.subHeading1 TypoGraphy
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
    , visibility GONE
    ] <> FontStyle.body3 TypoGraphy
  , if null state.data.tripSuggestions
    then suggestedLocationCardView push state
    else repeatRideCardParentView push state
  ]

shimmerView :: forall w. HomeScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.white900
    , visibility $ boolToVisibility state.props.showShimmer
    ]
    [ shimmerHelper 320 MATCH_PARENT false $ Margin 16 16 16 10
    , shimmerHelper 80 MATCH_PARENT false $ Margin 16 16 16 10
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      ]
      [ shimmerHelper 70 WRAP_CONTENT true $ Margin 16 16 16 10
      , shimmerHelper 70 WRAP_CONTENT true $ Margin 0 16 16 10
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ Margin 16 16 16 10
      ]
      ( map
          ( \_ ->
              shimmerHelper 130 (V 160) false $ MarginRight 12
          )
          (1 Arr... 3)
      )
    ]

shimmerHelper :: forall w. Int -> Length -> Boolean -> Margin -> PrestoDOM (Effect Unit) w
shimmerHelper ht width' useWeight margin' =
  shimmerFrameLayout
    ([ height $ V ht
    , cornerRadius 8.0
    , margin margin'
    , background Color.greyDark
    ] <> if useWeight then [weight 1.0] else [width width'])
    []


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
      destinations = take takeValue state.data.destinationSuggestions
      destLen = length destinations
  in
  horizontalScrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , clipChildren false
    , scrollBarX false
    ][ linearLayout
       [ height WRAP_CONTENT
       , width MATCH_PARENT
       , clipChildren false
       ] ( mapWithIndex ( \index item -> suggestedDestinationCard push state index item destLen) (destinations))
    ]

suggestedDestinationCard ::  forall w. (Action -> Effect Unit) -> HomeScreenState ->Int -> LocationListItemState -> Int -> PrestoDOM (Effect Unit) w
suggestedDestinationCard push state index suggestion len =
  PrestoAnim.animationSet
    [ Anim.fadeIn true] $
  linearLayout
    [ height WRAP_CONTENT
    , width $ if len == 1 then V ((screenWidth unit) - 32) else MATCH_PARENT
    , orientation HORIZONTAL
    , margin $ MarginLeft (if index == 0 then 0 else 12)
    , stroke $ "1,"<> Color.grey800
    , padding $ Padding 16 14 16 14
    , background Color.white900
    , gravity CENTER_VERTICAL
    , cornerRadius 16.0
    , onClick push $ const (SuggestedDestinationClicked suggestion false)
    , accessibilityHint $ "Trip to" <> suggestion.title <> suggestion.subTitle
    , rippleColor Color.rippleShade
    ][ linearLayout
        [ height $ V 26
        , width $ V 26
        , gravity CENTER
        , margin $ MarginRight 12
        ][ imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_green_loc_tag"
            , height $ V 20
            , width $ V 20
            ]
        ]
      , linearLayout
        ([ height WRAP_CONTENT
        , orientation VERTICAL
        ] <> if len == 1 then [weight 1.0] else [width $ V (JB.getWidthFromPercent 60)])
        [ textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text suggestion.title
            , color Color.black800
            , ellipsize true
            , margin $ MarginBottom 1
            , singleLine true
            ] <> FontStyle.body1 TypoGraphy
          , textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text suggestion.subTitle
            , color Color.black700
            , ellipsize true
            , singleLine true
            ] <> FontStyle.body3 TypoGraphy
        ]
    ]

repeatRideCardParentView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
repeatRideCardParentView push state =
  let takeValue = state.data.config.suggestedTripsAndLocationConfig.maxTripsToBeShown
      repeatRides = take takeValue state.data.tripSuggestions
      ridesLen = length repeatRides
  in
  horizontalScrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , scrollBarX false
    , clipChildren false
    ][ linearLayout
       [ height WRAP_CONTENT
       , width MATCH_PARENT
       , clipChildren false
       ] ( mapWithIndex ( \index item -> repeatRideCard push state index item ridesLen) (repeatRides))
    ]

repeatRideCard :: forall w. (Action -> Effect Unit) -> HomeScreenState ->Int -> Trip -> Int -> PrestoDOM (Effect Unit) w
repeatRideCard push state index trip len =
  linearLayout
    [ height WRAP_CONTENT
    , width $ if len == 1 then V ((screenWidth unit) - 32) else MATCH_PARENT
    , orientation HORIZONTAL
    , stroke $ "1,"<> Color.grey700
    , margin $ MarginLeft (if index == 0 then 0 else 12)
    , clipChildren false
    , padding $ Padding 12 8 16 8
    , background Color.white900
    , gravity CENTER_VERTICAL
    , cornerRadius 16.0
    , onClick push $ const (RepeatRide index trip)
    , accessibilityHint $ (if vehicleVariant /= "" && isJust trip.serviceTierNameV2 then vehicleVariant else "") <>" : trip to " <> getTripTitle trip.destination <> getTripSubTitle trip.destination
    , rippleColor Color.rippleShade
    ][ frameLayout
        [ height $ V 45
        , width WRAP_CONTENT
        , margin $ MarginRight 8
        , gravity CENTER
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , gravity CENTER
            , margin margin'
            ][ imageView
                [ imageWithFallback imageName
                , height imageDimensions.height
                , width imageDimensions.width
                ]
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , gravity BOTTOM
            , alignParentBottom "true,-1"
            , margin margin'
            , visibility $ boolToVisibility pillTagVisibility
            ][ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                ][ linearLayout -- tag
                    [ width MATCH_PARENT
                    , margin margin'
                    , height WRAP_CONTENT
                    , background Color.lightGrey1
                    , cornerRadius 10.0
                    , gravity CENTER
                    , layoutGravity "center_horizontal"
                    , padding $ Padding 7 3 7 3
                    ][ textView $
                        [ text $ vehicleVariant
                        , width MATCH_PARENT
                        , gravity CENTER
                        , singleLine true
                        , color Color.lightGreyBlue2
                        , height WRAP_CONTENT
                        ] <> FontStyle.captions LanguageStyle
                      ]
                  ]
              ]
          ]
      , linearLayout
        ([ height WRAP_CONTENT
        , orientation VERTICAL
        ] <> if len == 1 then [weight $ if os == "IOS" then 3.0 else 1.0] else [width $ V (JB.getWidthFromPercent 55)])
        [ textView $
           [ height WRAP_CONTENT
           , width WRAP_CONTENT
           , text $ getTripTitle trip.destination
           , color Color.black800
           , ellipsize true
           , margin $ MarginBottom 1
           , singleLine true
           ] <> FontStyle.body1 TypoGraphy
         , textView $
           [ height WRAP_CONTENT
           , width WRAP_CONTENT
           , text $ getTripSubTitle trip.destination
           , color Color.black700
           , ellipsize true
           , singleLine true
           ] <> FontStyle.body3 TypoGraphy
        ]
    ]
  where
    getTripTitle :: String -> String
    getTripTitle destination =
      maybe "" identity $ head $ DS.split (DS.Pattern ",") destination

    getTripSubTitle :: String -> String
    getTripSubTitle destination =
      (DS.drop ((fromMaybe 0 (DS.indexOf (DS.Pattern ",") (destination))) + 2) (destination))

    imageName = do
      case trip.vehicleVariant, isJust trip.serviceTierNameV2 of
                  Just variant, true -> getVehicleVariantImage variant RIGHT_VIEW
                  _,_ -> fetchImage FF_ASSET "ny_ic_green_loc_tag"

    pillTagVisibility = isJust trip.serviceTierNameV2 && isJust trip.vehicleVariant

    imageDimensions = if isVariantStored
                        then {height : V 33, width : V 50}
                        else {height : V 33, width : V 20}


    isVariantStored = isJust trip.serviceTierNameV2 && isJust trip.vehicleVariant

    paddingLeft = if not isVariantStored then 25 else 0

    vehicleVariant = fromMaybe "" trip.serviceTierNameV2

    margin' = if not isVariantStored then Margin 5 6 5 0 else MarginHorizontal 5 5

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



locationUnserviceableOrUserBlockedView ::  forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
locationUnserviceableOrUserBlockedView push state =
  let userBlockedViewConfig = {viewHeader : ACCOUNT_BLOCKED, viewHeaderTypography : FontStyle.h2 TypoGraphy, viewContent : YOU_CAN_STILL_ACCESS, viewImage : fetchImage FF_ASSET "ny_ic_account_blocked", viewImageWidth : V 159, viewImageHeight : V 117, contentViewMargin : Margin 40 75 40 83 }
      locationUnserviceableViewConfig = {viewHeader : LOCATION_UNSERVICEABLE, viewHeaderTypography : FontStyle.h1 TypoGraphy, viewContent : WE_ARE_NOT_LIVE_IN_YOUR_AREA, viewImage : fetchImage FF_ASSET "ny_ic_location_unserviceable", viewImageWidth : V 108, viewImageHeight : V 101, contentViewMargin : Margin 22 83 22 83 }
      currentViewConfig =
        if state.props.userBlocked then userBlockedViewConfig
        else if not state.props.isSrcServiceable then locationUnserviceableViewConfig
        else userBlockedViewConfig
  in
  linearLayout[
    height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 17 24 17 0
  , clickable true
  , orientation VERTICAL
  , gravity CENTER
  , visibility $ boolToVisibility $ (not (state.props.isSrcServiceable && not state.props.userBlocked) || state.props.userBlocked)
  ][
    relativeLayout[
      height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_HORIZONTAL
    , cornerRadius 16.0
    ]$[
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
      , margin currentViewConfig.contentViewMargin
      , background Color.transparent
      ][
        imageView[
          imageWithFallback currentViewConfig.viewImage
        , width currentViewConfig.viewImageWidth
        , height currentViewConfig.viewImageHeight
        ]
      , textView  $ [
          text $ getString currentViewConfig.viewHeader
        , width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , color Color.black800
        , margin $ MarginTop 10
        ] <> currentViewConfig.viewHeaderTypography
      , textView $ [
          text $ getString currentViewConfig.viewContent
        , width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , color Color.black700
        , margin $ MarginTop 8
        ] <> (FontStyle.paragraphText TypoGraphy)
      ]
    ] <> if isFollowEnabled state then
            [ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , gravity CENTER
              ]
              [ followRideBar push (getFollowers state) (V 328) false false]
            ]
          else
            []
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

rentalBanner :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rentalBanner push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 8 0 8 0
    , margin $ Margin 8 24 8 0
    , visibility $ boolToVisibility $ (isJust state.data.rentalsInfo && (isLocalStageOn HomeScreen || state.props.currentStage == HomeScreen))
    , gradient (Linear 0.0 [Color.transparent, "#FFFFFF" , "#FFFFFF" , "#FFFFFF"])
    ][  if state.props.showShimmer then
          textView[]
          else rentalBannerView state push]

rentalBannerView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rentalBannerView state push =
  let
    bannerText = (maybe "" (\rentalsInfo ->
                              if rentalsInfo.multipleScheduled then getString UPCOMING_BOOKINGS
                              else if (isJust rentalsInfo.driverInformation) then fetchDriverDetails rentalsInfo
                              else do
                                let timeUTC = rentalsInfo.rideScheduledAtUTC
                                    fpt = rentalsInfo.fareProductType
                                    bookingInfoString = if fpt == FPT.INTER_CITY then YOU_HAVE_UPCOMING_INTERCITY_BOOKING
                                                        else YOU_HAVE_UPCOMING_RENTAL_BOOKING
                                getString $ bookingInfoString $ convertUTCtoISC timeUTC "D" <> " " <> convertUTCtoISC timeUTC "MMMM" <> " " <> convertUTCtoISC timeUTC "YYYY" <> " , " <> convertUTCtoISC timeUTC "HH" <> ":" <> convertUTCtoISC timeUTC "mm"
                          ) state.data.rentalsInfo)
    fpt = (maybe FPT.ONE_WAY (\rentalsInfo -> rentalsInfo.fareProductType) state.data.rentalsInfo)
    {textColor,backGroundColor,bannerImg} = ( maybe {textColor : Color.black800,backGroundColor:Color.grey700,bannerImg:"ny_ic_mutiple_bg"}
                                            (\rentalsInfo -> if rentalsInfo.multipleScheduled then {textColor : Color.black800,backGroundColor:Color.grey700,bannerImg:"ny_ic_mutiple_bg"}
                                                            else do
                                                                case fpt of
                                                                  FPT.RENTAL -> {textColor : Color.blueGreen,backGroundColor:Color.blueGreenBg,bannerImg:"ny_ic_rental_banner_bg"}
                                                                  FPT.INTER_CITY -> {textColor : Color.blue800,backGroundColor:Color.blue600,bannerImg:"ny_ic_intercity_bg_img"}
                                                                  _ -> {textColor : Color.black800,backGroundColor:Color.grey700,bannerImg:"ny_ic_mutiple_bg"}
                                            ) state.data.rentalsInfo)

  in
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , background $ backGroundColor
    , padding $ PaddingLeft 16
    , cornerRadius 12.0
    , onClick push $ const RentalBannerClick
    ][  textView $
        [ weight 1.0
        , height WRAP_CONTENT
        , textFromHtml $ bannerText
        , color $ textColor
        , accessibility ENABLE
        , accessibilityHint $ bannerText
        ]<> FontStyle.tags TypoGraphy
      , frameLayout
        [ height WRAP_CONTENT
        , gravity CENTER
        , width WRAP_CONTENT
        ][
          imageView
            [ height $ V 60
            , width $ V 115
            , imageWithFallback $ fetchImage FF_COMMON_ASSET bannerImg
            ]
          , imageView
            [ imageWithFallback $ getVehicleVariantImage (maybe "" (\item -> item.vehicleVariant) state.data.rentalsInfo) LEFT_VIEW
            , height $ V 56
            , width $ V 77
            , layoutGravity "center"
            ]
        ]

    ]
    where
      fetchDriverDetails rentalsInfo =
        case (rentalsInfo.driverInformation) of
          Just resp -> do
            let
              driverName =  if DS.length resp.driverName > 12 then DS.take 12 resp.driverName <> ".." else resp.driverName
              vehicleNumber = resp.vehicleNumber
              fpt = rentalsInfo.fareProductType
              bookingInfoString = case fpt of
                                      FPT.INTER_CITY -> (getString INTER_CITY_)
                                      FPT.RENTAL -> (getString RENTAL_STR)
                                      _ -> (getString SCHEDULED)
            (getString DRIVER_ASSIGNED) <> " • " <> bookingInfoString <> "<br> <b>" <> driverName <> " • " <> vehicleNumber <> "</b>"
          _ -> do
            let timeUTC = rentalsInfo.rideScheduledAtUTC
                fpt = rentalsInfo.fareProductType
                bookingInfoString = if fpt == FPT.INTER_CITY then YOU_HAVE_UPCOMING_INTERCITY_BOOKING
                                    else YOU_HAVE_UPCOMING_RENTAL_BOOKING
            getString $ bookingInfoString $ convertUTCtoISC timeUTC "D" <> " " <> convertUTCtoISC timeUTC "MMMM" <> " " <> convertUTCtoISC timeUTC "YYYY" <> " , " <> convertUTCtoISC timeUTC "HH" <> ":" <> convertUTCtoISC timeUTC "mm"


additionalServicesView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
additionalServicesView push state = let
  showNewTag = true
  cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalStore CUSTOMER_LOCATION)
  appName = fromMaybe state.data.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
  firstName = fromMaybe "Yatri " (head (DS.split (DS.Pattern " ") appName))
  showAdditionalServices = (state.data.config.feature.enableAdditionalServices || cityConfig.enableRentals) && state.props.currentStage == HomeScreen
  in
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingHorizontal 16 16
    , visibility $ boolToVisibility $ showAdditionalServices && (not state.props.showShimmer)
    , margin $ MarginVertical 20 15
    ][  linearLayout[
          height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        ][  textView $
              [ text $ firstName <> " " <> getEN SERVICES
              , color Color.black900
              , height MATCH_PARENT
              ] <> FontStyle.subHeading1 TypoGraphy
          , newView push state
          ]
      , linearLayout
          [ height WRAP_CONTENT
          , margin $ MarginTop 16
          , width MATCH_PARENT
          , gravity CENTER
          ][  LocationTagBarV2.view (push <<< LocationTagBarAC) (locationTagBarConfig state)]
      ]


computeListItem :: (Action -> Effect Unit) -> Flow GlobalState Unit
computeListItem push = do
  bannerItem <- preComputeListItem $ BannerCarousel.view push (BannerCarousel.config BannerCarousel)
  void $ liftFlow $ push (SetBannerItem bannerItem)

computeIssueReportBanners :: (Action -> Effect Unit) -> Flow GlobalState Unit
computeIssueReportBanners push = do
  bannerItem <- preComputeListItem $ RideCompletedCard.customerIssueCarousalView (push <<< RideCompletedAC)
  void $ liftFlow $ push $ SetIssueReportBannerItems bannerItem

updateEmergencyContacts :: (Action -> Effect Unit) -> HomeScreenState -> FlowBT String Unit
updateEmergencyContacts push state = do
      emergencySettings <- lift $ lift $ fetchEmergencySettings push state
      case emergencySettings of
        Just (GetEmergencySettingsRes settings) -> do
          let contacts = formatContacts settings.defaultEmergencyNumbers
          void $ liftFlowBT $ push (UpdateContacts contacts)
          void $ liftFlowBT $ validateAndStartChat contacts push state settings.safetyCheckStartTime settings.safetyCheckEndTime
        Nothing -> pure unit

fetchContactsForMultiChat :: (Action -> Effect Unit) -> HomeScreenState -> FlowBT String Unit
fetchContactsForMultiChat push state = do
  contacts <- getFormattedContacts
  void $ liftFlowBT $ push $ UpdateContacts contacts
  let uuid = state.data.driverInfoCardState.currentChatRecipient.uuid
      chatUser = if state.props.isChatWithEMEnabled then (getValueFromCache (show CUSTOMER_ID) getKeyInSharedPrefKeys) else "Customer"
  liftFlowBT $ checkAndStartChatService push uuid chatUser false state

validateAndStartChat :: Array NewContacts ->  (Action -> Effect Unit) -> HomeScreenState -> Maybe Int -> Maybe Int -> Effect Unit
validateAndStartChat contacts push state safetyCheckStartSeconds safetyCheckEndSeconds = do
  if state.data.fareProductType == FPT.RENTAL && not state.props.chatcallbackInitiated
    then checkAndStartChatService push state.data.driverInfoCardState.currentChatRecipient.uuid "Customer" false state
  else do

    if (length contacts) == 0
      then push RemoveChat
      else do
        if (not $ state.props.chatcallbackInitiated ) then do
          let primaryContact = fromMaybe dummyNewContacts $ Arr.head $ Arr.filter (\item -> (isJust item.contactPersonId)) contacts
          push $ UpdateChatWithEM true primaryContact
          let
              uuid = if primaryContact.priority == 0 then state.data.driverInfoCardState.rideId else state.data.driverInfoCardState.rideId <> "$" <> (fromMaybe "" primaryContact.contactPersonId)
              chatUser = (getValueFromCache (show CUSTOMER_ID) getKeyInSharedPrefKeys)
          checkAndStartChatService push uuid chatUser true state else pure unit


checkAndStartChatService ::  (Action -> Effect Unit) -> String -> String -> Boolean -> HomeScreenState -> Effect Unit
checkAndStartChatService push chatChannelId chatUser rideStarted state = do
  let isChatServiceRunning = runFn1 JB.isServiceRunning chatService
  if os /= "IOS" && chatChannelId /= getValueToLocalStore CHAT_CHANNEL_ID && isChatServiceRunning then do

    stopChatListenerService
    checkChatService push 5 chatChannelId chatUser rideStarted state
  else
    startChatServices push chatChannelId chatUser rideStarted

startChatServices ::  (Action -> Effect Unit) -> String -> String -> Boolean -> Effect Unit
startChatServices push chatChannelId chatUser rideStarted = do
    void $ clearChatMessages
    void $ storeCallBackMessageUpdated push chatChannelId chatUser UpdateMessages AllChatsLoaded
    void $ storeCallBackOpenChatScreen push OpenChatScreen
    void $ startChatListenerService
    void $ scrollOnResume push ScrollToBottom
    push InitializeChat

checkChatService :: forall w . (Action -> Effect Unit) -> Int -> String -> String -> Boolean -> HomeScreenState -> Effect Unit
checkChatService push retry channelId chatUser rideStarted state =
  when (retry > 0) $ do
    let isChatServiceRunning = runFn1 JB.isServiceRunning chatService
    if isChatServiceRunning then do
      void $ pure $ delay $ Milliseconds 2000.0
      checkChatService push (retry-1) channelId chatUser rideStarted state
    else
      startChatServices push channelId chatUser rideStarted



safetyAlertPopup :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
safetyAlertPopup push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][PopUpModal.view (push <<< SafetyAlertAction) (safetyAlertConfig state)]

shareRideOptionView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> Int -> NewContacts -> PrestoDOM (Effect Unit) w
shareRideOptionView push state index contact =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding $ Padding 16 6 16 6
    , onClick push $ const $ ToggleShare index
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET $ if contact.isSelected then "ny_ic_checkbox_selected" else "ny_ic_checkbox_unselected"
        , height $ V 16
        , width $ V 16
        , margin $ MarginRight 10
        ]
    , ContactCircle.view (ContactCircle.getContactConfig contact index false false) (push <<< ContactAction)
    , textView
        $ [ text contact.name
          , color Color.black800
          , gravity CENTER_VERTICAL
          , margin $ MarginLeft 8
          ]
        <> FontStyle.body1 TypoGraphy
    ]


getFollowRide :: forall action. (action -> Effect Unit) -> (FollowRideRes -> action) -> Flow GlobalState Unit
getFollowRide push action = do
  resp <- Remote.getFollowRide ""
  case resp of
    Right response -> liftFlow $ push $ action response
    Left err -> do
      _ <- pure $ printLog "api error " err
      pure unit

referralPopUp :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
referralPopUp push state =
  ReferralComponent.view (push <<< ReferralComponentAction) (referralPopUpConfig state)

specialZoneInfoPopup :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
specialZoneInfoPopup push state =
  let zoneType = if isLocalStageOn ConfirmingLocation then
                    if state.props.locateOnMapProps.isSpecialPickUpGate then
                      SPECIAL_PICKUP
                    else
                      state.props.confirmLocationCategory
                 else state.props.zoneType.priorityTag
      tagConfig = specialZoneTagConfig zoneType
  in case tagConfig.infoPopUpConfig of
        Just infoPopUpConfig ->
          PrestoAnim.animationSet [ Anim.fadeIn true ]
            $ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                ][ RequestInfoCard.view (push <<< RequestInfoCardAction) (specialZoneInfoPopupConfig infoPopUpConfig) ]
        Nothing -> emptyTextView state

newView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
newView push state =
  textView $
    [ text $ "✨ " <> getString NEW_
    , color Color.white900
    , cornerRadius if os == "IOS" then 12.0 else 16.0
    , background Color.blue900
    , padding $ Padding 6 4 6 4
    , margin $ MarginLeft 8
    , gravity CENTER_VERTICAL
    , visibility $ boolToVisibility $ state.data.config.homeScreen.showAdditionalServicesNew
    ] <> FontStyle.tags TypoGraphy

extraBottomPadding :: Int
extraBottomPadding =  if os == "IOS" then 60 + safeMarginBottom else 112

isAcWorkingView ::  forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
isAcWorkingView push state =
  PopUpModal.view (push <<< AcWorkingPopupAction) (acWorkingPopupConfig state)

preferenceView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
preferenceView push state =
  let dimLayout = state.data.iopState.providerPrefVisible || state.data.iopState.providerPrefInfo
      providerPrefVisibility = state.data.iopState.showPrefButton
      bookingPrefVisibility = (not state.data.currentCityConfig.iopConfig.enable) && state.data.config.estimateAndQuoteConfig.enableBookingPreference
      isProviderPrefView = state.data.currentCityConfig.iopConfig.enable
      followerBar = (showFollowerBar (fromMaybe [] state.data.followers) state) && (any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithDriver])
      startTimeUTC = if state.data.startTimeUTC == "" then (getCurrentUTC "") else (state.data.startTimeUTC)
      compareStartTime = (compareUTCDate startTimeUTC (getCurrentUTC "")) < 30 * 60
      blackListedFareProductType = any (_ == state.data.fareProductType) [FPT.INTER_CITY]
      isVisible = (any (_ == state.props.currentStage) [SettingPrice]) && compareStartTime && (not blackListedFareProductType)
  in
    relativeLayout [
      width MATCH_PARENT
    , height MATCH_PARENT
    , visibility $ boolToVisibility isVisible
    ] [
      linearLayout[
          width MATCH_PARENT
        , height MATCH_PARENT
        , clickable dimLayout
        , background if dimLayout then Color.blackLessTrans else Color.transparent
        , onClick push $ const $ if dimLayout then BackPressed else NoRender
      ][]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , padding $ Padding 0 20 18 0
        , margin $ MarginTop if followerBar then 0 else safeMarginTop
        , orientation VERTICAL
        , gravity RIGHT
        ] $ [ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , gravity RIGHT
              ][  linearLayout[weight 1.0][]
                , linearLayout
                  [ height $ V 48
                  , width $ V 48
                  , stroke $ "1," <> Color.grey900
                  , background Color.white900
                  , gravity CENTER
                  , cornerRadius 24.0
                  , clickable true
                  , visibility $ boolToVisibility if isProviderPrefView then providerPrefVisibility else bookingPrefVisibility
                  , accessibility DISABLE
                  , onClick push $ const ShowPref
                  , rippleColor Color.rippleShade
                  ]
                  [ imageView
                      [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_pref"
                      , height $ V 25
                      , width $ V 25
                      ]
                  ]
              ]
        ] <> if state.data.iopState.providerPrefVisible then [if isProviderPrefView then providerPreferenceOptions push state else bookingPrefOptions push state] else []
          <> if state.data.iopState.providerPrefInfo then [requestInfoCardView push state isProviderPrefView] else []
    ]

requestInfoCardView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> Boolean -> PrestoDOM (Effect Unit) w
requestInfoCardView push state providerPrefInfo =
  PrestoAnim.animationSet [ fadeIn true ]
    $ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ MarginTop 10
        ][ RequestInfoCard.view (push <<< RequestInfoCardAction) infoCardConfig ]
        where infoCardConfig = if providerPrefInfo then multipleProvidersInfo state else requestInfoCardConfig state

providerPreferenceOptions :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
providerPreferenceOptions push state =
  PrestoAnim.animationSet [ fadeIn true ] $
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity RIGHT
  , margin $ MarginTop 10
  ][ linearLayout[weight 1.0][]
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , cornerRadius 20.0
      , orientation VERTICAL
      , gravity LEFT
      , padding $ Padding 16 16 16 16
      , background Color.white900
      ][  menuButtonView push (getString BOOK_TOP_PROVIDER) (fetchImage FF_ASSET "ny_ic_thumbs_up") true state (ShowMultipleProvider false) (NoAction) (not btnActive)
        , menuButtonView push (getString CHOOSE_FROM_PROVIDERS) (fetchImage FF_ASSET "ny_ic_info") false  state (ShowMultipleProvider true) (ShowProviderInfo true) btnActive
      ]
    ]
    where btnActive = state.data.iopState.showMultiProvider

bookingPrefOptions :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
bookingPrefOptions push state =
  PrestoAnim.animationSet [ fadeIn true ] $
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity RIGHT
  , margin $ MarginTop 10
  ][ linearLayout[weight 1.0][]
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , cornerRadius 20.0
      , orientation VERTICAL
      , gravity LEFT
      , padding $ Padding 16 16 16 16
      , background Color.white900
      ][  menuButtonView push (getString AUTO_ASSIGN_DRIVER) (fetchImage FF_ASSET "ny_ic_faster_lightning") true state (CheckBoxClick true) (NoAction) (btnActive true)
        , menuButtonView push (getString CHOOSE_BETWEEN_MULTIPLE_DRIVERS) (fetchImage FF_ASSET "ny_ic_info") false  state (CheckBoxClick false) (ShowProviderInfo true) (btnActive false)
      ]
    ]
    where btnActive autoAssign = (state.props.flowWithoutOffers && autoAssign || not state.props.flowWithoutOffers && not autoAssign)



createRouteHelper routeState startLat startLon endLat endLon mbRoute rideId = do
  route <- do
    case mbRoute of
      Just routeCalculated  -> pure $ Just routeCalculated
      Nothing -> do
        routeResp <- getRoute routeState $ makeGetRouteReq startLat startLon endLat endLon (if routeState == "pickup" then Just rideId else Nothing) 
        case routeResp of
          Right (GetRouteResp resp) -> do
            case (head resp) of
              Just (Route route) -> pure $ Just route
              Nothing -> pure $ Nothing
          Left _ -> pure $ Nothing
  case route of
    Just route' -> do
      if (startLat /= 0.0 && startLon /= 0.0 && endLat /= 0.0 && endLon /= 0.0 ) then do
        let (Snapped routePts) = route'.points
            newPts = if (length routePts > 1 && route'.distance <= 50000) then
                        getExtendedPath $ walkCoordinates (route'.points)
                        else
                          walkCoordinate startLat startLon endLat endLon
            newRoute = route' {points = Snapped (map (\item -> LatLong { lat : item.lat, lon : item.lng}) newPts.points)}
        pure $ {points : Just newPts, route : Just (Route newRoute), routeDuration : Just $ route'.duration, routeDistance : Just $ route'.distance}
      else pure $ {points : Nothing, route : Nothing, routeDuration : Nothing, routeDistance : Nothing}
    Nothing -> pure $ {points : Nothing, route : Nothing, routeDuration : Nothing, routeDistance : Nothing}


intercityInSpecialZonePopupView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
intercityInSpecialZonePopupView push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][PopUpModal.view (push <<< IntercitySpecialZone) (intercityInSpecialZonePopupConfig state)]

exploreCitySection :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
exploreCitySection push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingHorizontal 16 16
    , margin $ MarginTop 20
    , visibility $ boolToVisibility  $ not $ null state.data.famousDestinations
    ][  textView $
          [ text $ getString EXPLORE
          , color Color.black900
          ] <> FontStyle.h3 TypoGraphy
      , horizontalScrollView
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , scrollBarX false
          ][ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , margin $ MarginTop 16
              , orientation HORIZONTAL
              ][ exploreCityCardView push state]
          ]
      ]
    where
      city = getValueToLocalStore CUSTOMER_LOCATION

exploreCityCardView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
exploreCityCardView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    ](mapWithIndex ( \index item -> exploreCityCard push state index item) state.data.famousDestinations)

exploreCityCard :: forall w . (Action -> Effect Unit) -> HomeScreenState -> Int -> LocationListItemState -> PrestoDOM (Effect Unit) w
exploreCityCard push state index locationItem =
  linearLayout
    [ height WRAP_CONTENT
    , width $ V 160
    , orientation VERTICAL
    , margin $ MarginRight 12
    , onClick push $ const $ SuggestedDestinationClicked locationItem true
    , accessibilityHint $ "Go to " <> locationItem.title
    ][ imageView
        [ imageUrl locationItem.postfixImageUrl
        , height $ V 110
        , width MATCH_PARENT
        , cornerRadius 12.0
        , accessibility DISABLE
        ]
      , textView $
          [ text locationItem.address
          , color Color.black800
          , margin $ MarginTop 8
          , accessibility DISABLE
          ] <> FontStyle.body1 TypoGraphy
      , textView $
          [ text locationItem.description
          , color Color.black700
          , accessibility DISABLE
          ] <> FontStyle.body3 TypoGraphy
    ]

deliveryParcelImageAndOtpView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
deliveryParcelImageAndOtpView push state =
  linearLayout
    [
      height MATCH_PARENT
    , width MATCH_PARENT
    ] [
        PrestoAnim.animationSet
        [
          Anim.fadeInWithDelay 2 true
        ] $ DeliveryParcelImageAndOtp.view (push <<< DeliveryParcelImageOtpAction) (deliveryParcelImageAndOtpConfig state)
      ]

fetchEmergencySettings :: (Action -> Effect Unit) -> HomeScreenState -> Flow GlobalState (Maybe GetEmergencySettingsRes)
fetchEmergencySettings push state = do
  case state.props.safetySettings of
    Just settings -> pure $ Just settings
    Nothing -> do
      resp <- Remote.getEmergencySettings ""
      case resp of
        Right response -> do
          liftFlow $ push $ UpdateSafetySettings response
          pure $ Just response
        Left err -> pure Nothing

isFollowEnabled :: HomeScreenState -> Boolean
isFollowEnabled state = (state.props.followsRide && isJust state.data.followers) || isJust state.data.manuallySharedFollowers

getFollowers :: HomeScreenState -> Array Followers
getFollowers state = do
  let automaticallySharedFollowers = fromMaybe [] state.data.followers
      manuallySharedFollowers = fromMaybe [] state.data.manuallySharedFollowers
  Arr.nubByEq (\a b -> a.bookingId == b.bookingId) $ Arr.union automaticallySharedFollowers manuallySharedFollowers


--- select Trip view  --

selectTripViewIntercity :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
selectTripViewIntercity push state =
  linearLayout [
    width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , background  Color.red
][ SearchLocationModel.selectTripView (push <<< SearchLocationModelActionController) $ searchLocationModelViewState state]

rideInfoCardConfig :: forall w. HomeScreenState ->  RequestInfoCard.Config
rideInfoCardConfig state = let
  config = RequestInfoCard.config
  rideInfoCardConfig' = config{
    title {
      text = (getString ROUND_TRIP_POLICY) ,
      accessibilityHint = (getEN ROUND_TRIP_POLICY),
      textStyle = FontStyle.Heading1
    }
  , imageConfig {
      imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_baggage",
      height = V 130,
      width = V 130,
      padding = Padding 0 2 2 0,
      visibility = VISIBLE
    }
  , bulletPoints = [ getString FOR_EVERY_EXTRA_HOUR_YOU_ADD , getString BY_DEFAULT_ONE_HOUR ]
  , buttonConfig {
      text = getString GOT_IT,
      padding = PaddingVertical 16 20,
      accessibilityHint = (getEN GOT_IT) <> " : Button"
    }
  }
  in rideInfoCardConfig'

rideInfoCardView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rideInfoCardView push state =
  PrestoAnim.animationSet [ fadeIn true ]
  $ linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , accessibility DISABLE
  ][ RequestInfoCard.view (push <<< RequestInfoCardAction) (rideInfoCardConfig state) ]


--- Intercity Confirmation view----
intercityWebView :: forall w . (Action -> Effect Unit) -> HomeScreenState-> PrestoDOM (Effect Unit) w
intercityWebView push state  =
  case state.data.intercityBus.url of
    Just url' ->
      linearLayout [
        height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      , clickable true
      ][
        linearLayout[
          width MATCH_PARENT
        , height WRAP_CONTENT
        , margin $ Margin 16 10 16 10
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ][
          linearLayout[
            height WRAP_CONTENT
          , width WRAP_CONTENT
          , stroke $ "1," <> Color.grey900
          , gravity CENTER
          , padding $ Padding 4 4 4 4
          , rippleColor Color.rippleShade
          , onClick push $ const $ HideIntercityBusView ""
          , cornerRadius 8.0
          ][
            imageView [
              imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left"
            , height $ V 24
            , width $ V 24
            ]
          ]
        , imageView [
            imageWithFallback $ getHeaderLogo state
          , height $ V 32
          , width $ V 93
          , margin $ MarginLeft 10
          ]
        , linearLayout[
            height WRAP_CONTENT
          , weight 1.0
          ][]
        , linearLayout[
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , padding $ Padding 2 2 2 2
          , cornerRadius 1.0
          ][
            imageView [
                imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_refresh_black"
              , onClick (\_ ->  runEffectFn2 EHC.loadWebViewWithURL (getNewIDWithTag "intercityWebView")  url') (const NoAction)
              , height $ V 24
              , width $ V 24
              , rippleColor Color.rippleShade
            ]
          ]
        ]
      , linearLayout[
          weight 1.0
        , width MATCH_PARENT
        , background Color.grey800
        , afterRender( \_ ->initialWebViewSetUp push (getNewIDWithTag "intercityWebView") HideIntercityBusView) (const NoAction)
        ][
          webView[
            url $ url'
          , height MATCH_PARENT
          , width MATCH_PARENT
          , id $ getNewIDWithTag "intercityWebView"
          ]
        ]
      ]
    Nothing -> emptyTextView state


pickupLocationConfig :: HomeScreenState -> CurrentLocationConfig
pickupLocationConfig state =
  let showCurrentLocationView = DS.null state.props.defaultPickUpPoint && (state.data.fareProductType /= FPT.DELIVERY || state.props.currentStage == EditPickUpLocation)
  in {
        isClickable: state.props.currentStage /= EditPickUpLocation,
        click: GoBackToSearchLocationModal,
        border: "1," <> state.data.config.confirmPickUpLocationBorder,
        image: "ny_ic_source_dot",
        accessibilityText:  "Pickup Location is " <>  (DS.replaceAll (DS.Pattern ",") (DS.Replacement " ") state.data.source),
        text: state.data.source,
        visibility: showCurrentLocationView,
        alpha: 1.0
      }

deliveryPickupLocationConfig :: HomeScreenState -> CurrentLocationConfig
deliveryPickupLocationConfig state =
  {
    isClickable: true,
    click: ToggleCurrentPickupDropCurrentLocation true,
    border: "1," <> (if state.props.isSource == Just true then Color.blue800 else Color.grey900),
    image: "ny_ic_source_dot",
    accessibilityText:  "Pickup Location is " <>  (DS.replaceAll (DS.Pattern ",") (DS.Replacement " ") state.data.source),
    text: state.data.source,
    visibility: state.data.fareProductType == FPT.DELIVERY,
    alpha: 1.0
   }

deliveryDropLocationConfig :: HomeScreenState -> CurrentLocationConfig
deliveryDropLocationConfig state =
  {
    isClickable: true,
    click: ToggleCurrentPickupDropCurrentLocation false,
    border: "1," <> (if (state.props.isSource /= Just true) then Color.blue800 else Color.grey900),
    image: "ny_ic_dest_dot",
    accessibilityText:  "Destination Location is " <>  (DS.replaceAll (DS.Pattern ",") (DS.Replacement " ") state.data.destination),
    text: state.data.destination,
    visibility: state.data.fareProductType == FPT.DELIVERY,
    alpha: if state.props.isSource == Just true then 0.3 else 1.0
   }

type CurrentLocationConfig = { isClickable :: Boolean, click :: Action, border :: String, image :: String, accessibilityText :: String, text :: String, visibility :: Boolean, alpha :: Number }

deliveryDestRouteConfig :: HomeScreenState -> Array JB.RouteConfig
deliveryDestRouteConfig state =

  if state.data.fareProductType == FPT.DELIVERY && state.props.currentStage == RideAccepted then
    let srcMarkerConfig = JB.defaultMarkerConfig{ markerId = "", pointerIcon = "" }
        destMarkerConfig = JB.defaultMarkerConfig{ markerId = "ny_ic_dest_marker", pointerIcon = "ny_ic_dest_marker", primaryText = "", anchorU = 0.5, anchorV = 1.0}
        srcLat = state.data.driverInfoCardState.sourceLat
        srcLon = state.data.driverInfoCardState.sourceLng
        dstLat = state.data.driverInfoCardState.destinationLat
        dstLon = state.data.driverInfoCardState.destinationLng
        routeConfig = mkRouteConfig (walkCoordinate srcLat srcLon dstLat dstLon) srcMarkerConfig destMarkerConfig Nothing "NORMAL_ROUTE" "DOT" false JB.DELIVERY_DESTINATION $ specialLocationConfig "" "" false getPolylineAnimationConfig
    in [routeConfig]
  else []