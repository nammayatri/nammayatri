{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Flow where

import Accessor
import Engineering.Helpers.LogEvent
import Screens.TicketBookingFlow.TicketBooking.Transformer
import Services.API
import Services.API as API
import Common.Resources.Constants (zoomLevel)
import Common.Types.App (ChatFCMData(..), GlobalPayload(..), SignatureAuthData(..), Payload(..), Version(..), LocationData(..), EventPayload(..), ClevertapEventParams, OTPChannel(..), LazyCheck(..), FCMBundleUpdate, ProviderType(..), CategoryListType(..),TicketType(..), Confidence(..), TripCategory(..), TripCategoryTag(..), City(..))
import Common.Types.App as CTA
import Components.ChatView.Controller (makeChatComponent')
import Components.LocationListItem.Controller (locationListStateObj, dummyAddress)
import Components.SavedLocationCard.Controller (getCardType)
import Components.ChooseVehicle.Controller as ChooseVehicle
import Components.SettingSideBar.Controller as SettingSideBarController
import Constants as Constants
import Control.Monad.Except (runExcept)
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (runBackT)
import Control.Transformers.Back.Trans as App
import Data.Array (catMaybes, reverse, filter, length, null, snoc, (!!), any, sortBy, head, uncons, last, concat, all, elemIndex, mapWithIndex, elem, nubByEq, foldl, (:),all,notElem)
import Data.Array as Arr
import Helpers.Utils as HU
import Data.Either (Either(..), either)
import Data.Function.Uncurried (runFn3, runFn2, runFn1, mkFn1)
import Data.Int as INT
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Number (fromString)
import Data.Ord (compare)
import Data.String (Pattern(..), Replacement(..), drop, indexOf, split, toLower, trim, take, joinWith)
import Data.String (length) as STR
import Data.String as DS
import Data.String.Common (joinWith, split, toUpper, trim, replaceAll)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), makeAff, nonCanceler, launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1, runEffectFn2, runEffectFn9, runEffectFn3)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (liftFlow, os, getNewIDWithTag, getExpiryTime, convertUTCtoISC, getCurrentUTC, getWindowVariable, flowRunner, resetIdMap, markPerformance, splitString)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Events as Events
import Engineering.Helpers.Utils (compareDate, loaderText, toggleLoader, showToast, saveObject, reboot, showSplash,getAndRemoveLatestNotificationType, fetchLanguage, handleUpdatedTerms, getReferralCode, (?), getCityFromString)
import Engineering.Helpers.GeoHash (encodeGeohash, geohashNeighbours)
import Foreign (MultipleErrors, unsafeToForeign)
import Foreign.Class (class Encode)
import Foreign.Class (class Encode, encode)
import Foreign.Generic (decodeJSON, encodeJSON)
import JBridge (getCurrentLatLong, showMarker, cleverTapSetLocation, currentPosition, drawRoute, emitJOSEvent, enableMyLocation, factoryResetApp, firebaseLogEvent, firebaseLogEventWithParams, firebaseLogEventWithTwoParams, firebaseUserID, generateSessionId, getLocationPermissionStatus, getVersionCode, getVersionName, hideKeyboardOnNavigation, hideLoader, initiateLocationServiceClient, isCoordOnPath, isInternetAvailable, isLocationEnabled, isLocationPermissionEnabled, launchInAppRatingPopup, locateOnMap, locateOnMapConfig, metaLogEvent, openNavigation, reallocateMapFragment, removeAllPolylines, removeAllPolygons, saveSuggestionDefs, saveSuggestions, setCleverTapUserProp, stopChatListenerService, toggleBtnLoader, updateRoute, updateMarker, extractReferrerUrl, getLocationNameV2, getLatLonFromAddress, showDialer, cleverTapCustomEventWithParams, cleverTapCustomEvent, showKeyboard, differenceBetweenTwoUTCInMinutes, shareTextMessage, defaultMarkerConfig, Location, setMapPadding, defaultMarkerImageConfig, timeValidity, removeMarker, setCleverTapProfileData, loginCleverTapUser, defaultMarkerImageConfig, destroySignedCall)
import JBridge as JB
import Helpers.Utils (convertUTCToISTAnd12HourFormat, decodeError, addToPrevCurrLoc, addToRecentSearches, adjustViewWithKeyboard, checkPrediction, differenceOfLocationLists, drawPolygon, filterRecentSearches, fetchImage, FetchImageFrom(..), getCurrentDate, getNextDateV2, getNextDate, getCurrentLocationMarker, getCurrentLocationsObjFromLocal, getDistanceBwCordinates, getGlobalPayload, getMobileNumber, getNewTrackingId, getObjFromLocal, getPrediction, getRecentSearches, getScreenFromStage, getSearchType, parseFloat, parseNewContacts, removeLabelFromMarker, requestKeyboardShow, saveCurrentLocations, seperateByWhiteSpaces, setText, showCarouselScreen, sortPredictionByDistance, toStringJSON, triggerRideStatusEvent, withinTimeRange, fetchDefaultPickupPoint, updateLocListWithDistance, getCityCodeFromCity, getCityNameFromCode, getDistInfo, getExistingTags, getMetroStationsObjFromLocal, updateLocListWithDistance, getCityConfig, getMockFollowerName, getMetroConfigFromAppConfig, encodeBookingTimeList, decodeBookingTimeList, bufferTimePerKm, invalidBookingTime, normalRoute, breakPrefixAndId, editPickupCircleConfig)
import Language.Strings (getString)
import Helpers.SpecialZoneAndHotSpots (zoneLabelIcon, transformGeoJsonFeature, getSpecialTag, getZoneType, transformHotSpotInfo, mapSpecialZoneGates)
import Language.Types (STR(..)) as STR
import Log (logInfo, logStatus)
import Data.Bounded (top)
import MerchantConfig.Types (AppConfig(..), MetroConfig(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import MerchantConfig.Utils as MU
import Prelude (Unit, bind, discard, map, mod, negate, not, pure, show, unit, void, when, identity, otherwise, ($), (&&), (+), (-), (/), (/=), (<), (<=), (<>), (==), (>), (>=), (||), (<$>), (<<<), ($>), (>>=), (*), max, min, (>>>), flip, (<#>))
import Mobility.Prelude (capitalize, boolToInt, startsWith)
import ModifyScreenState (modifyScreenState, updateSafetyScreenState, updateRepeatRideDetails, FlowState(..))
import Presto.Core.Types.Language.Flow (doAff, fork, setLogField)
import Helpers.Pooling (delay)
import Presto.Core.Types.Language.Flow (getLogFields)
import Resources.Constants (DecodeAddress(..), decodeAddress, encodeAddress, getKeyByLanguage, getValueByComponent, getWard, ticketPlaceId, dummyPrice, estimateLabelMaxWidth, markerArrowSize, estimateLabelMaxWidth, markerArrowSize)
import Screens (getScreen)
import Resources.Constants (DecodeAddress(..), decodeAddress, encodeAddress, getKeyByLanguage, getValueByComponent, getWard, ticketPlaceId, getAddressFromBooking, dummyPrice)
import Screens.AccountSetUpScreen.ScreenData as AccountSetUpScreenData
import Screens.AccountSetUpScreen.Transformer (getDisabilityList)
import Screens.AddNewAddressScreen.Controller (encodeAddressDescription, getSavedLocations, getSavedTags, getLocationList, calculateDistance, getSavedTagsFromHome, validTag, isValidLocation, getLocTag, savedLocTransformer) as AddNewAddress
import Screens.AddNewAddressScreen.ScreenData (dummyLocation) as AddNewAddressScreenData
import Screens.ChooseLanguageScreen.Controller (ScreenOutput(..))
import Screens.EmergencyContactsScreen.ScreenData as EmergencyContactsScreenData
import Screens.EnterMobileNumberScreen.Controller (ScreenOutput(..))
import Screens.EnterMobileNumberScreen.ScreenData as EnterMobileNumberScreenData
import Screens.Handlers as UI
import Screens.HelpAndSupportScreen.ScreenData as HelpAndSupportScreenData
import Screens.HelpAndSupportScreen.Transformer (reportIssueMessageTransformer)
import Screens.HomeScreen.Controller (flowWithoutOffers, getSearchExpiryTime, findingQuotesSearchExpired, tipEnabledState, getCachedEstimates)
import Screens.InvoiceScreen.Controller (ScreenOutput(..)) as InvoiceScreenOutput
import Screens.HomeScreen.ScreenData (dummyRideBooking)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.FollowRideScreen.ScreenData as FollowRideScreenData
import Screens.SelectLanguageScreen.ScreenData as SelectLanguageScreenData
import Screens.HomeScreen.Transformer (getLocationList, dummyRideAPIEntity, encodeAddressDescription, getPlaceNameResp, getUpdatedLocationList, transformContactList, getTripFromRideHistory, getFormattedContacts, getFareProductType, getEstimateIdFromSelectedServices, getTripDetailsState)
import Screens.MyProfileScreen.ScreenData as MyProfileScreenData
import Screens.ReferralScreen.ScreenData as ReferralScreen
import Screens.TicketInfoScreen.ScreenData as TicketInfoScreenData
import Screens.Types (RiderRideCompletedScreenState(..), TicketBookingScreenStage(..), CardType(..), AddNewAddressScreenState(..), SearchResultType(..), CurrentLocationDetails(..), CurrentLocationDetailsWithDistance(..), DeleteStatus(..), HomeScreenState, LocItemType(..), PopupType(..), SearchLocationModelType(..), Stage(..), LocationListItemState, LocationItemType(..), NewContacts, NotifyFlowEventType(..), FlowStatusData(..), ErrorType(..), ZoneType(..), TipViewData(..), TripDetailsGoBackType(..), DisabilityT(..), UpdatePopupType(..), PermissionScreenStage(..), TicketBookingItem(..), TicketBookings(..), TicketBookingScreenData(..), TicketInfoScreenData(..), IndividualBookingItem(..), SuggestionsMap(..), Suggestions(..), Address(..), LocationDetails(..), TipViewStage(..), Trip(..), SearchLocationTextField(..), SearchLocationScreenState, SearchLocationActionType(..), SearchLocationStage(..), LocationInfo, BottomNavBarIcon(..), FollowRideScreenStage(..), ReferralStatus(..), LocationType(..), Station(..), TicketBookingStage(..), MetroStations(..), SearchResultType(..), RentalScreenStage(..))
import Screens.RentalBookingFlow.RideScheduledScreen.Controller (ScreenOutput(..)) as RideScheduledScreenOutput
import Screens.NammaSafetyFlow.Controller as NammaSafetyFlowScreenOutput
import Screens.DataExplainWithFetch.ScreenData as DataExplainWithFetchSD
import Screens.DataExplainWithFetch.Controller as DataExplainWithFetchC
import Screens.RentalBookingFlow.RideScheduledScreen.ScreenData as RideScheduledScreenData
import Screens.ReportIssueChatScreen.ScreenData as ReportIssueChatScreenData
import Screens.RideBookingFlow.HomeScreen.Config (specialLocationConfig, getTipViewData,fetchRideDetails,fetchExtraFares)
import Screens.SavedLocationScreen.Controller (getSavedLocationForAddNewAddressScreen)
import Screens.SearchLocationScreen.Controller as SearchLocationController
import Screens.SearchLocationScreen.ScreenData as SearchLocationScreenData
import Screens.TicketBookingFlow.MetroTicketDetails.ScreenData as MetroTicketDetailsScreenData
import Screens.TicketBookingFlow.PlaceDetails.Controller as PlaceDetailsC
import Screens.TicketBookingFlow.PlaceDetails.View as PlaceDetailsS
import Screens.TicketBookingFlow.PlaceList.Controller as PlaceListC
import Screens.DataExplainWithFetch.ComponentConfig as DataExplainWithFetchCC
import Screens.TicketBookingFlow.PlaceList.ScreenData as PlaceListData
import Screens.TicketBookingFlow.PlaceList.View as PlaceListS
import Screens.TicketBookingFlow.TicketBooking.ScreenData as TicketBookingScreenData
import Screens.Types (Gender(..)) as Gender
import Screens.Types as ST
import Screens.Types
import Services.Backend as Remote
import Services.Config (getBaseUrl, getSupportNumber)
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalNativeStore, setUserCity, getValueToLocalStore, isLocalStageOn, setValueToLocalNativeStore, setValueToLocalStore, updateLocalStage)
import Effect.Aff (Milliseconds(..), makeAff, nonCanceler, launchAff)
import Types.App
import Types.App (ScreenType(..), ABOUT_US_SCREEN_OUTPUT(..), ACCOUNT_SET_UP_SCREEN_OUTPUT(..), ADD_NEW_ADDRESS_SCREEN_OUTPUT(..), GlobalState(..), CONTACT_US_SCREEN_OUTPUT(..), FlowBT, HOME_SCREEN_OUTPUT(..), MY_PROFILE_SCREEN_OUTPUT(..), MY_RIDES_SCREEN_OUTPUT(..), PERMISSION_SCREEN_OUTPUT(..), REFERRAL_SCREEN_OUPUT(..), SAVED_LOCATION_SCREEN_OUTPUT(..), SELECT_LANGUAGE_SCREEN_OUTPUT(..), ScreenType(..), TRIP_DETAILS_SCREEN_OUTPUT(..), EMERGECY_CONTACTS_SCREEN_OUTPUT(..), TICKET_BOOKING_SCREEN_OUTPUT(..), WELCOME_SCREEN_OUTPUT(..), APP_UPDATE_POPUP(..), TICKET_BOOKING_SCREEN_OUTPUT(..), TICKET_INFO_SCREEN_OUTPUT(..), defaultGlobalState, TICKETING_SCREEN_SCREEN_OUTPUT(..), METRO_TICKET_SCREEN_OUTPUT(..), METRO_TICKET_DETAILS_SCREEN_OUTPUT(..), METRO_MY_TICKETS_SCREEN_OUTPUT(..), METRO_MY_TICKETS_SCREEN_OUTPUT(..), METRO_TICKET_STATUS_SCREEN_OUTPUT(..), SELECT_BUS_ROUTE_SCREEN_OUTPUT(..))
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Screens.AccountSetUpScreen.Transformer (getDisabilityList)
import Constants.Configs
import PrestoDOM (initUI, Visibility(..))
import Common.Resources.Constants (zoomLevel,locateOnMapLabelMaxWidth)
import PaymentPage
import Screens.TicketBookingFlow.TicketBooking.Transformer
import Domain.Payments as PP
import PrestoDOM.Core (terminateUI)
import Helpers.Storage.Flow.BaseApp
import Helpers.Storage.Flow.SearchStatus
import Helpers.Logs
import Helpers.Auth
import Helpers.Version
import Helpers.Ride (customerFeedbackPillData, getFlowStatusData, checkRideStatus)
import Helpers.Firebase
import Foreign.Class (class Encode)
import SuggestionUtils
import ConfigProvider
import Components.ChatView.Controller (makeChatComponent')
import Components.MessagingView (ChatComponent)
import Mobility.Prelude (capitalize, boolToVisibility)
import Screens.HelpAndSupportScreen.Transformer (reportIssueMessageTransformer)
import Timers
import Screens.TicketBookingFlow.PlaceList.View as PlaceListS
import Screens.TicketBookingFlow.PlaceDetails.View as PlaceDetailsS
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Control.Transformers.Back.Trans as App
import Locale.Utils
import Screens.RentalBookingFlow.RideScheduledScreen.Controller (ScreenOutput(..)) as RideScheduledScreenOutput
import Screens.SearchLocationScreen.Controller as SearchLocationController
import Screens.SearchLocationScreen.ScreenData as SearchLocationScreenData
import Screens.RentalBookingFlow.RentalScreen.Controller as RentalScreenController
import Screens.RentalBookingFlow.RentalScreen.ScreenData as RentalScreenData
import Screens.RideBookingFlow.RiderRideCompletedCard.ScreenData as RiderRideCompletedScreenData
import Screens (ScreenName(..), getScreen) as Screen
import MerchantConfig.DefaultConfig (defaultCityConfig)
import Screens.NammaSafetyFlow.SafetySettingsScreen.Controller as SafetySettingsScreen
import Screens.NammaSafetyFlow.SetupSafetySettingsScreen.Controller as SetupSafetySettingsScreen
import Screens.NammaSafetyFlow.ActivateSafetyScreen.Controller as ActivateSafetyScreen
import Screens.NammaSafetyFlow.SosActiveScreen.Controller as SosActiveScreen
import Screens.NammaSafetyFlow.SafetyEducationScreen.Controller as SafetyEducationScreen
import Screens.NammaSafetyFlow.Components.SafetyUtils
-- import Screens.NammaSafetyFlow.Controller as NammaSafetyFlow
import RemoteConfig as RC
import RemoteConfig.Utils (getCustomerVoipConfig)
import Engineering.Helpers.RippleCircles (clearMap)
import Data.Array (groupBy, fromFoldable, singleton, sort)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Types.App
import Screens.TicketBookingFlow.TicketStatus.ScreenData as TicketStatusScreenData
import Screens.Types
import Screens.TicketBookingFlow.TicketStatus.Transformer as TicketStatusTransformer
import Screens.TicketBookingFlow.MetroTicketStatus.Transformer
import Screens.TicketBookingFlow.MetroTicketDetails.Transformer
import Screens.TicketBookingFlow.MetroMyTickets.Transformer
import Screens.TicketBookingFlow.MetroTicketBooking.ScreenData as MetroTicketBookingScreenData
import Screens.NammaSafetyFlow.ScreenData (defaultTimerValue, initData) as SafetyScreenData
import Services.Config (getNumbersToWhiteList)
import SessionCache (getValueFromWindow, setValueInWindow)
import LocalStorage.Cache (clearCache, setInCache)
import DecodeUtil (getAnyFromWindow)
import Screens.ReportIssueChatScreen.ScreenData as ReportIssueChatScreenData
import Screens.RideSummaryScreen.ScreenData as RideSummaryScreenData
import Screens.FollowRideScreen.Controller (deleteDismisedMockDrills)
import Data.Map as Map
import PrestoDOM.Core (getPushFn)
import Foreign.Object (lookup)
import Screens.RideSelectionScreen.Transformer (myRideListTransformer)
import Services.FlowCache as FlowCache
import Data.HashMap as DHM
import Helpers.API as HelpersAPI
import Helpers.Referral (applyReferralCode)
import Helpers.SpecialZoneAndHotSpots
import Components.ChooseVehicle.Controller as ChooseVehicle
import Screens.HelpAndSupportScreen.Transformer (getUpdatedIssueList, getApiIssueList)
import Screens.RentalBookingFlow.RideScheduledScreen.ScreenData as RideScheduledScreenData
import Helpers.API (callApiBT)
import Effect.Unsafe (unsafePerformEffect)
import Screens.Types (SearchResultType(..)) as SearchResultType
import Screens.Types (FareProductType(..)) as FPT
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import Helpers.TipConfig (isTipEnabled, setTipViewData)
import Presto.Core.Types.API (ErrorResponse(..))
import AssetsProvider (renewFile)
import Data.Tuple
import Screens.HomeScreen.Controllers.Types
import Data.Array as DA
import Language.Strings (getVarString)
import Language.Types as LT
import Components.ServiceTierCard.View as ServiceTierCard
import Screens.RideBookingFlow.HomeScreen.Config (getFareUpdatedStr)
import Components.MessagingView.Controller as CMC
import Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.Controller as ParcelDeliveryScreenController
import Screens.Types (TripDetailsGoBackType(..))
import DecodeUtil
import Styles.Colors as Color
import Data.Int (ceil)
import RemoteConfig as RemoteConfig
import Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.ScreenData as ParcelDeliveryScreenData
import Helpers.PrestoUtils
import Common.RemoteConfig (fetchRemoteConfigString)
import Engineering.Helpers.Events as EHE
import Screens.TicketBookingFlow.BusTicketBooking.Controller as BusTicketBookingController
import Screens.TicketBookingFlow.BusTicketBooking.ScreenData as BusTicketBookingScreenData
import Screens.TicketBookingFlow.BusTrackingScreen.Controller as BusTrackingScreen
import Screens.TicketBookingFlow.BusTrackingScreen.Transformer (getStationsFromBusRoute)
import Screens.AadhaarVerificationScreen.ScreenData as AadhaarVerificationScreenData
import Screens.TicketBookingFlow.BusTrackingScreen.ScreenData as BusTrackingScreenData
import Helpers.FrfsUtils (getFirstRoute, getAllFirstRoutes, getSortedStops)
import DecodeUtil (decodeForeignAny,parseJSON)

baseAppFlow :: GlobalPayload -> Boolean -> FlowBT String Unit
baseAppFlow gPayload callInitUI = do
  when callInitUI $ initUIWrapper ""
  let bundleSplashConfig = RemoteConfig.getBundleSplashConfig "lazy"
      hybridInit = fromMaybe true $ gPayload ^. _payload ^. _show_splash
  if callInitUI && bundleSplashConfig.enable && hybridInit then toggleSetupSplash true else pure unit
  if isJust (gPayload ^. _payload ^. _appToken) then upateTokenFromHybridFlow $ gPayload ^. _payload ^. _appToken else pure unit
  let _ = setKeyInWindow "forceAppToNoInternetScreen" true
  let _ = EHE.addEvent (EHE.defaultEventObject "splash_screen_loaded") { module = "onboarding"}
  if callInitUI && bundleSplashConfig.enable then void $ lift $ lift $ fork $ EHE.runLogTracking else pure unit
  lift $ lift $ void $ fork $ doAff $ makeAff \cb -> runEffectFn3 renewFile "v1-assets_downloader.jsa" "https://assets.moving.tech/beckn/bundles/mobility-core/0.0.11/v1-assets_downloader.jsa" (cb <<< Right) $> nonCanceler
  liftFlowBT $ markPerformance "BASE_APP_FLOW"
  -- checkVersion
  baseAppStorage -- TODO:: Restructure the files and names
  baseAppLogs
  liftFlowBT $ runEffectFn1 resetIdMap ""
  liftFlowBT $ resetAllTimers
  void $ pure $ spy "DEBUG: gPayload" gPayload
  tokenValidity <- validateToken signatureAuthData
  lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)
  if tokenValidity then
    if isJust (gPayload ^. _payload ^. _chatMessageData)
      then handleChatMessage $ gPayload ^. _payload ^. _chatMessageData
      else handleDeepLinks (Just gPayload) false
  else
    validateAuthData $ signatureAuthData
  where
  signatureAuthData = gPayload ^. _payload ^. _signatureAuthData

  validateAuthData signatureAuthData = case signatureAuthData of
    Just signatureAuth -> do
      response <- lift $ lift $ Remote.triggerSignatureBasedOTP signatureAuth
      validationStatus <- validateSignaturePayload signatureAuth response
      when validationStatus $ handleDeepLinks (Just gPayload) false
    Nothing ->
      if showCarouselScreen FunctionCall then
        welcomeScreenFlow
      else
        enterMobileNumberScreenFlow

dataFetchScreenFlow :: NammaSafetyStage -> Int -> FlowBT String Unit
dataFetchScreenFlow stageConfig stepVal = do
  let stageConfigSteps = DataExplainWithFetchSD.getStepsNumber stageConfig
      newHeaderValue = DataExplainWithFetchSD.getHeaderValue stageConfig
      newHomeScreenStage homescreenState = if homescreenState.props.currentStage == ChatWithDriver then homescreenState.props.stageBeforeChatScreen else homescreenState.props.currentStage
  modifyScreenState $ DataFetchScreenStateType (\dataFetchScreen -> dataFetchScreen { config { stage = stageConfig, stageSteps = stageConfigSteps, currentStep = stepVal}, data { headerValue = newHeaderValue } })
  flow <- UI.dataFetchScreen

  case flow of
    DataExplainWithFetchC.AddEmergencyContacts state -> do
      modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactScreen -> emergencyContactScreen { props { fromNewSafetyFlow= true} })
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { safetySettings = Nothing, chatcallbackInitiated = false, currentStage = newHomeScreenStage homeScreen}, data{contactList = Nothing }} )
      emergencyScreenFlow
    DataExplainWithFetchC.UpdateEmergencyContacts state -> do
      void $ Remote.emergencyContactsBT $ Remote.postContactsReq state.data.emergencyContactsList
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { safetySettings = Nothing, chatcallbackInitiated = false, currentStage = newHomeScreenStage homeScreen}, data{contactList = Nothing } })
      void $ lift $ lift $ showToast $  "ContactsUpdated"
      nammaSafetyFlow
    DataExplainWithFetchC.Exit state -> do
      updateSafetySettings state
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { safetySettings = Nothing } })
      nammaSafetyFlow
    DataExplainWithFetchC.GoToSafetyDrill state -> do
      modifyScreenState $ NammaSafetyScreenStateType (\safetyScreen -> safetyScreen { props { triggeringSos = false, timerValue = SafetyScreenData.defaultTimerValue, showTestDrill = true, showShimmer = true, confirmTestDrill = false, isAudioRecordingActive = false, showMenu = false, isFromSafetyCenter = true, recordedAudioUrl = Nothing, audioRecordingStatus = CTA.NOT_RECORDING, recordingTimer = "00 : 00", defaultCallPopup = false } })
      activateSafetyScreenFlow
    _ -> homeScreenFlow

nammaSafetyFlow :: FlowBT String Unit
nammaSafetyFlow = do
  flow <- UI.nammaSafetyScreen
  case flow of
    NammaSafetyFlowScreenOutput.Navigate updatedState navigationConfig -> do
      updateDataFetchScreenState updatedState navigationConfig.isCompleted Nothing
      case navigationConfig.navigation of
        TrustedContacts _ -> do
          let emergencyContactLength = Arr.length updatedState.data.emergencyContactsList
          modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactScreen -> emergencyContactScreen { data{ selectedContacts = updatedState.data.emergencyContactsList },props { showDropDown = false, fromNewSafetyFlow= true, saveEmergencyContacts = true, getDefaultContacts = emergencyContactLength > 0 } })
          emergencyScreenFlow
        SafetyCheckIn _ -> do
          if not navigationConfig.isCompleted
            then
              modifyScreenState
                $ DataFetchScreenStateType
                    ( \dataFetchScreen ->
                        dataFetchScreen
                          { data
                            { unExpectedEventChecks = API.SHARE_WITH_TIME_CONSTRAINTS
                            , postRideCheck = API.SHARE_WITH_TIME_CONSTRAINTS
                            }
                          }
                    )
            else pure unit
          dataFetchScreenFlow (DataExplainWithFetchSD.stageData $ SafetyCheckIn []) 0
        EmergencyActions _ -> do
          if not navigationConfig.isCompleted
            then modifyScreenState $ DataFetchScreenStateType( \dataFetchScreen -> dataFetchScreen{ data{ autoCallDefaultContact = true, emergencySOSShake = true}})
            else pure unit
          dataFetchScreenFlow (DataExplainWithFetchSD.stageData $ EmergencyActions []) 0
        SafetyDrill _ -> dataFetchScreenFlow (DataExplainWithFetchSD.stageData $ SafetyDrill []) 0
        TrustedContactsActions _ -> dataFetchScreenFlow (DataExplainWithFetchSD.stageData $ TrustedContactsActions []) 0
        DriverSafetyStandards _ -> dataFetchScreenFlow (DataExplainWithFetchSD.stageData $ DriverSafetyStandards []) 0
    _ -> homeScreenFlow

updateSafetySettings :: ST.DataFetchScreenState -> FlowBT String Unit
updateSafetySettings state = do
  let
    setUpCompletedConditions = (not $ null state.data.emergencyContactsList) && (DataExplainWithFetchCC.getBooleanFromOptions state.data.postRideCheck || DataExplainWithFetchCC.getBooleanFromOptions state.data.unExpectedEventChecks || (state.data.informPoliceSos || state.data.notifySafetyTeam))
    req =
      UpdateEmergencySettingsReq
        { shareEmergencyContacts: Nothing
        , shareTripWithEmergencyContactOption: Nothing
        , nightSafetyChecks: Nothing
        , hasCompletedSafetySetup:  Just setUpCompletedConditions
        , autoCallDefaultContact: Just state.data.autoCallDefaultContact
        , enableOtpLessRide: Nothing
        , enablePostRideSafetyCheck: Just state.data.postRideCheck
        , enableUnexpectedEventsCheck: Just state.data.unExpectedEventChecks
        , informPoliceSos: Just state.data.informPoliceSos
        , notifySafetyTeamForSafetyCheckFailure: Just state.data.notifySafetyTeam
        , notifySosWithEmergencyContacts: Nothing
        , shakeToActivate: Just state.data.emergencySOSShake
        , shareTripWithEmergencyContacts: Nothing
        , hasCompletedMockSafetyDrill : Just state.data.hasCompletedMockSafetyDrill
        }
  void $ lift $ lift $ Remote.updateEmergencySettings req
  when setUpCompletedConditions $
      modifyScreenState $ HomeScreenStateType $ \homeScreen → homeScreen
        { data
          { settingSideBar
            { hasCompletedSafetySetup = true
            }
          }
        }
  pure unit


updateDataFetchScreenState :: ST.NammaSafetyScreenState -> Boolean -> Maybe API.GetEmergencySettingsRes -> FlowBT String Unit
updateDataFetchScreenState safetyScreenState setupCompleted apiResponse = do
  let (API.GetEmergencySettingsRes emergencySettings) = safetyScreenState.data.settingsAPIResponse
  modifyScreenState
    $ DataFetchScreenStateType
        ( \dataFetchScreen ->
            dataFetchScreen
              { data
                { unExpectedEventChecks = emergencySettings.enableUnexpectedEventsCheck
                , postRideCheck = emergencySettings.enablePostRideSafetyCheck
                , notifySafetyTeam = emergencySettings.notifySafetyTeamForSafetyCheckFailure
                , emergencySOSShake = emergencySettings.shakeToActivate
                , autoCallDefaultContact = emergencySettings.autoCallDefaultContact
                , informPoliceSos = emergencySettings.informPoliceSos
                , notifySosWithEmergencyContacts = emergencySettings.notifySosWithEmergencyContacts
                , emergencyContactsList = safetyScreenState.data.emergencyContactsList
                , hasCompletedMockSafetyDrill = emergencySettings.hasCompletedMockSafetyDrill
                },
                props { stageSetUpCompleted = setupCompleted}
              }
        )
  pure unit

handleChatMessage :: Maybe ChatFCMData -> FlowBT String Unit
handleChatMessage chatFCMData = do
  case chatFCMData of
    Just (ChatFCMData chatData) ->
      if (fromMaybe "" chatData.source) == "TRUSTED_CONTACT"
        then do
          contacts <- getFormattedContacts
          let filteredContact = getContact contacts $ fromMaybe "" chatData.personId
          void $ pure $ updateLocalStage ChatWithDriver
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {
            data
              { channelIdFromFCM = fromMaybe "" chatData.channelId
              , personIdFromFCM = fromMaybe "" chatData.personId
              , sourceFromFCM = fromMaybe "" chatData.source
              , chatPersonId = getValueToLocalStore CUSTOMER_ID
              , driverInfoCardState =
                homeScreen.data.driverInfoCardState{
                  currentChatRecipient{ name = filteredContact.name
                    , number = ""
                    , uuid = fromMaybe "" chatData.channelId
                    , recipient = CMC.USER
                    , enableForShareRide = true
                    , contactPersonId = chatData.personId
                    , notifiedViaFCM = Nothing
                    , shareTripWithEmergencyContactOption = filteredContact.shareTripWithEmergencyContactOption.key
                  }
                }
              },
            props {
              isChatWithEMEnabled = true
            }
          })
          currentFlowStatus false
        else updateFollower true false Nothing
    Nothing -> currentFlowStatus false
  where
    getContact contactList userId =
      case Arr.find (\contact ->  (fromMaybe "" contact.contactPersonId) == userId) contactList of
        Just contact -> contact
        Nothing -> HomeScreenData.dummyNewContacts

handleDeepLinks :: Maybe GlobalPayload -> Boolean -> FlowBT String Unit
handleDeepLinks mBGlobalPayload skipDefaultCase = do
  liftFlowBT $ markPerformance "HANDLE_DEEP_LINKS"
  handleExternalLocations mBGlobalPayload
  case mBGlobalPayload of
    Just globalPayload -> case globalPayload ^. _payload ^. _view_param of
      Just screen -> case screen of
        "rides" -> hideSplashAndCallFlow myRidesScreenFlow
        "abt" -> hideSplashAndCallFlow aboutUsScreenFlow
        "fvrts" -> hideSplashAndCallFlow $ savedLocationFlow HomeScreenFlow
        "help" -> hideSplashAndCallFlow $ flowRouter HelpAndSupportScreenFlow
        "prof" -> hideSplashAndCallFlow myProfileScreenFlow
        "lang" -> hideSplashAndCallFlow $ selectLanguageScreenFlow HomeScreenFlow
        "tkts" -> hideSplashAndCallFlow placeListFlow
        "safety" -> hideSplashAndCallFlow nammaSafetyFlow
        "rentals" -> hideSplashAndCallFlow $ hybridFlow screen
        "intercity" -> hideSplashAndCallFlow $ hybridFlow screen
        "favourites" -> hideSplashAndCallFlow $ hybridFlow screen
        "safetytools" -> hideSplashAndCallFlow $ hybridFlow screen
        "rideConfirmed" -> hideSplashAndCallFlow $ hybridFlow screen
        "rideCompleted" -> hideSplashAndCallFlow $ hybridFlow screen
        "tripDetail" -> hideSplashAndCallFlow $ hybridFlow screen
        "addHome" -> addFavLocFlow SearchLocationScreenData.initData "HOME_TAG"
        "addWork" -> addFavLocFlow SearchLocationScreenData.initData "WORK_TAG"
        "driverprofile" -> hideSplashAndCallFlow $ hybridFlow screen
        "ticketing" ->  hideSplashAndCallFlow $ hybridFlow screen
        "waitingFordriver" -> hideSplashAndCallFlow $ currentFlowStatus false
        "smd" -> do
          modifyScreenState $ NammaSafetyScreenStateType (\safetyScreen -> safetyScreen { props { showTestDrill = true } })
          hideSplashAndCallFlow activateSafetyScreenFlow
        "sedu" -> do
          case globalPayload ^. _payload ^. _deepLinkJSON of
            Just (CTA.QueryParam queryParam) -> do
              if isJust queryParam.option then do
                let
                  videoList = RC.safetyBannerVideoConfigData (DS.toLower $ getValueToLocalStore CUSTOMER_LOCATION) $ fetchLanguage $ getLanguageLocale languageKey
                modifyScreenState $ NammaSafetyScreenStateType (\safetyScreen -> safetyScreen { data { videoList = videoList }, props { showVideoView = true, educationViewIndex = Just 0, fromBannerLink = true } })
              else
                modifyScreenState $ NammaSafetyScreenStateType (\safetyScreen -> safetyScreen { props { fromDeepLink = true } })
            _ -> pure unit
          hideSplashAndCallFlow safetyEducationFlow
        "mt" -> hideSplashAndCallFlow metroTicketBookingFlow
        "shareRide" ->  do
          case globalPayload ^. _payload ^. _deepLinkJSON of
            Just (CTA.QueryParam queryParam) -> do
              case queryParam.rideId of
                Just rId -> do
                  res <- lift $ lift $ HelpersAPI.callApi $ GetManuallySharedDetailsReq rId
                  case res of
                    Right (GetManuallySharedDetailsRes rideDetails) -> do
                      let follower = { name : Just rideDetails.customerName, bookingId : rideDetails.bookingId, mobileNumber : rideDetails.customerPhone, priority : 0, isManualFollower : true, personId : rideDetails.customerId }
                      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { followers = Just [follower], manuallySharedFollowers = Just [follower] } })
                      void $ pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                      updateFollower false true Nothing
                    Left _ -> currentFlowStatus false
                _ -> currentFlowStatus false
            _ -> pure unit
        "delivery" -> do
            if getValueToLocalNativeStore PARCEL_INSTRUCTIONS_VISITED /= "true" then do
              modifyScreenState $ ParcelDeliveryScreenStateType (\_ -> ParcelDeliveryScreenData.initData { data { currentStage = ST.DELIVERY_INSTRUCTIONS}})
              parcelDeliveryFlow 
            else do
              void $ pure $ updateLocalStage SearchLocationModel
              modifyScreenState $ ParcelDeliveryScreenStateType (\_ -> ParcelDeliveryScreenData.initData)
              modifyScreenState $ HomeScreenStateType (\updatedState-> updatedState { 
                props { homeScreenPrimaryButtonLottie = true, isSource = Just true, currentStage = SearchLocationModel, isSearchLocation = SearchLocation, searchLocationModelProps{crossBtnSrcVisibility = true},  rideSearchProps{ sessionId = generateSessionId unit } }
              , data {fareProductType = FPT.DELIVERY, source="", locationList = updatedState.data.recentSearchs.predictionArray} 
              })
              homeScreenFlow
        "bt" -> do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { sourceLong =  (fromMaybe 0.0 $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LON) ,sourceLat =  (fromMaybe 0.0 $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LAT)  } })
          (GlobalState getstate)<- getState
          setValueToLocalStore SESSION_ID (generateSessionId unit)
          modifyScreenState $ BusTicketBookingScreenStateType (\_ -> BusTicketBookingScreenData.initData { data {ticketServiceType = BUS} , props {srcLat =  getstate.homeScreen.props.sourceLat , srcLong = getstate.homeScreen.props.sourceLong}})
          hideSplashAndCallFlow busTicketBookingFlow
        _ -> do
          let _ = spy "startwith" screen
          if startsWith "safetytools" screen then do
            let safetyParam = DS.split (DS.Pattern "$$") screen
                rideId = fromMaybe "" $ safetyParam !! 1
                vehicleNumber = fromMaybe "" $ safetyParam !! 2
            modifyScreenState
              $ NammaSafetyScreenStateType
                  ( \nammaSafetyScreen ->
                      nammaSafetyScreen
                        { props
                          { triggeringSos = false
                          , timerValue = SafetyScreenData.defaultTimerValue
                          , showTestDrill = false
                          , showShimmer = true
                          , confirmTestDrill = false
                          , isSafetyCenterDisabled = false
                          , checkPastRide = false
                          , isAudioRecordingActive = false
                          , showCallPolice = false
                          , showMenu = false
                          , recordedAudioUrl = Nothing
                          , audioRecordingStatus = CTA.NOT_RECORDING
                          , recordingTimer = "00 : 00"
                          , defaultCallPopup = false
                          , reportPastRide = false
                          }
                        , data
                          { rideId = rideId
                          , vehicleDetails = vehicleNumber
                          }
                        }
                  )
            activateSafetyScreenFlow
          else if startsWith "tripDetail" screen then do
            (GlobalState state) <- getState
            let tripDetailParam = DS.split (DS.Pattern "$$") screen
                bookingId = fromMaybe "" $ tripDetailParam !! 1
            (RideBookingRes rideBookingResponse) <- Remote.rideBookingBT bookingId
            let bookingResp = (RideBookingRes rideBookingResponse)
            modifyScreenState $ TripDetailsScreenStateType (\_ -> getTripDetailsState bookingResp state.tripDetailsScreen)
            tripDetailsScreenFlow
          else if startsWith "driverprofile" screen then do
            let safetyParam = DS.split (DS.Pattern "$$") screen
                rideId = fromMaybe "" $ safetyParam !! 1
            modifyScreenState $ DriverProfileScreenCommonStateType ( \driverProfileScreen -> driverProfileScreen { props { rideId = rideId } } )
            driverProfileScreenFlow
          else if startsWith "emergencyContactScreen" screen then do 
            (GetEmergContactsResp res) <- Remote.getEmergencyContactsBT GetEmergContactsReq
            let
              contacts =
                getDefaultPriorityList
                  $ map
                      ( \(ContactDetails item) ->
                          { number: item.mobileNumber
                          , name: item.name
                          , isSelected: true
                          , enableForFollowing: fromMaybe false item.enableForFollowing
                          , enableForShareRide: fromMaybe false item.enableForShareRide
                          , shareTripWithEmergencyContactOption: EmergencyContactsScreenData.getRideOptionFromKeyEM $ fromMaybe API.NEVER_SHARE item.shareTripWithEmergencyContactOption
                          , onRide: fromMaybe false item.onRide
                          , priority: fromMaybe 1 item.priority
                          , contactPersonId : item.contactPersonId
                          , isFollowing : Nothing
                          , notifiedViaFCM : item.notifiedViaFCM
                          }
                      )
                      res.defaultEmergencyNumbers
            let emergencyContactLength = Arr.length contacts
            modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactScreen -> emergencyContactScreen { data{ selectedContacts = contacts,  emergencyContactsList = contacts },props { showDropDown = false, fromNewSafetyFlow= true, saveEmergencyContacts = true, getDefaultContacts = emergencyContactLength > 0 } })
            emergencyScreenFlow
          else if startsWith "reportIssue" screen then do 
            let issueParam = DS.split (DS.Pattern "$$") screen
                rideId = fromMaybe "" (issueParam !! 1)
            let
              language = fetchLanguage $ getLanguageLocale languageKey
            (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language "f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0" "" rideId ""
            let
              getOptionsRes' = mapWithIndex (\index (Option optionObj) -> optionObj { option = (show (index + 1)) <> ". " <> (reportIssueMessageTransformer optionObj.option) }) getOptionsRes.options

              messages' = mapWithIndex (\index (Message currMessage) -> makeChatComponent' (reportIssueMessageTransformer currMessage.message) currMessage.messageTitle currMessage.messageAction currMessage.label "Bot" (getCurrentUTC "") "Text" (500 * (index + 1))) getOptionsRes.messages

              chats' =
                map
                  ( \(Message currMessage) ->
                      Chat
                        { chatId: currMessage.id
                        , chatType: "IssueMessage"
                        , timestamp: (getCurrentUTC "")
                        }
                  )
                  getOptionsRes.messages
            void $ pure $ cleverTapCustomEvent "ny_user_report_safety_issue_activated"
            modifyScreenState $ ReportIssueChatScreenStateType (\_ -> ReportIssueChatScreenData.initData { data { entryPoint = ReportIssueChatScreenData.SafetyScreen, chats = chats', tripId = Just rideId, selectedCategory = { categoryName : "Safety Related Issue", categoryId : "f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0", categoryImageUrl : Nothing, categoryAction : Nothing, isRideRequired : false, maxAllowedRideAge : Nothing, categoryType : "Category", allowedRideStatuses : Nothing} , options = getOptionsRes', chatConfig { messages = messages' }, selectedRide = Nothing } })
            flowRouter IssueReportChatScreenFlow
          else
            case breakPrefixAndId screen of
              Just ( Tuple "metroBooking" bookingId )-> do
                case bookingId of
                  Just id -> hideSplashAndCallFlow $ viewTicketDetialsFlow (Just id)
                  _ -> pure unit
              _ -> if skipDefaultCase then pure unit else currentFlowStatus false
      Nothing -> currentFlowStatus false
    Nothing -> do
      let
        mBPayload = getGlobalPayload Constants.globalPayload
      case mBPayload of
        Just _ -> handleDeepLinks mBPayload skipDefaultCase
        Nothing -> pure unit

handleExternalLocations :: Maybe GlobalPayload -> FlowBT String Unit
handleExternalLocations mBGlobalPayload = do
  case mBGlobalPayload of
    Just globalPayload ->
      case globalPayload ^. _payload ^. _destination of
        Just (LocationData destinationObj) -> do
          (ServiceabilityRes dest) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq (destinationObj.lat) (destinationObj.lon)) DESTINATION
          case dest.serviceable of
            true -> do
              case destinationObj.name of
                Just src -> updateDataInState destinationObj.lat destinationObj.lon src (encodeAddress src [] Nothing destinationObj.lat destinationObj.lon) Nothing
                Nothing -> do
                    mbDestination <- getPlaceName destinationObj.lat destinationObj.lon HomeScreenData.dummyLocation true
                    case mbDestination of
                      Just (PlaceName destination) -> updateDataInState destinationObj.lat destinationObj.lon destination.formattedAddress (encodeAddress destination.formattedAddress destination.addressComponents destination.placeId destinationObj.lat destinationObj.lon) destination.placeId
                      Nothing -> pure unit
            false -> pure unit
        Nothing -> pure unit
    Nothing -> pure unit
  where
    updateDataInState lat lon addressString address placeId = do
      void $ updateLocalStage GoToConfirmLocation
      modifyScreenState $ HomeScreenStateType (\homescreen -> homescreen{
          props{currentStage = GoToConfirmLocation, destinationLat = lat,destinationLong = lon,destinationPlaceId = placeId,sourceLong =  (fromMaybe 0.0 $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LON) ,sourceLat =  (fromMaybe 0.0 $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LAT) ,isSource = Just false,isSharedLocationFlow = true}
        , data{ source = (getString STR.CURRENT_LOCATION),destination = addressString, destinationAddress =address}
      })
hideSplashAndCallFlow :: FlowBT String Unit -> FlowBT String Unit
hideSplashAndCallFlow flow = do
  hideLoaderFlow
  flow

hybridFlow :: String -> FlowBT String Unit
hybridFlow flow = do
  case spy "HybridFlow: " flow of
    "rentals" -> do
      currentLocation <- lift $ lift $ doAff do liftEffect getCurrentLatLong
      latestScheduledRides <- FlowCache.fetchAndUpdateScheduledRides true
      modifyScreenState
        $ RentalScreenStateType
            ( \_ ->
                RentalScreenData.initData
                  { data
                    { pickUpLoc
                      { address = getString STR.CURRENT_LOCATION
                      , city = CTA.Bangalore
                      , lat = Just currentLocation.lat
                      , lon = Just currentLocation.lng
                      , placeId = Nothing
                      }
                    , latestScheduledRides = latestScheduledRides
                    }
                  }
            )
      rentalScreenFlow
    "intercity" -> do
      void $ updateLocalStage SearchLocationModel
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { source=(getString STR.CURRENT_LOCATION), rentalsInfo = Nothing}, props{isIntercityFlow = true,isSource = Just false, canScheduleRide = false, isSearchLocation = SearchLocation, currentStage = SearchLocationModel, searchLocationModelProps{crossBtnSrcVisibility = false }}})
      homeScreenFlow
    "favourites" -> savedLocationFlow HomeScreenFlow
    "rideConfirmed" -> do
      checkRideStatus true false 
      homeScreenFlow
    "rideCompleted" -> do
      checkRideStatus false false
      homeScreenFlow
    "tripDetail" -> do
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen { props { fromMyRides = RideCompletedScreen } })
      tripDetailsScreenFlow
    "metro" ->  metroTicketBookingFlow
    "ticketing" -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData { props { navigateToHome = true } })
      modifyScreenState $ TicketingScreenStateType (\_ -> PlaceListData.initData { props { hideMyTickets = false } })
      placeListFlow
    "safetytools" -> do
      modifyScreenState
        $ NammaSafetyScreenStateType
            ( \nammaSafetyScreen ->
                nammaSafetyScreen
                  { props
                    { triggeringSos = false
                    , timerValue = SafetyScreenData.defaultTimerValue
                    , showTestDrill = false
                    , showShimmer = true
                    , confirmTestDrill = false
                    , isSafetyCenterDisabled = false
                    -- , checkPastRide = state.props.currentStage == HomeScreen
                    , isAudioRecordingActive = false
                    , showCallPolice = false
                    , showMenu = false
                    , recordedAudioUrl = Nothing
                    , audioRecordingStatus = CTA.NOT_RECORDING
                    , recordingTimer = "00 : 00"
                    , defaultCallPopup = false
                    , reportPastRide = false
                    }
                  , data
                    { rideId = "" -- TODO:: Need to handle this case
                    -- , vehicleDetails =
                    }
                  }
            )
      activateSafetyScreenFlow
    _ -> pure unit


hideLoaderFlow :: FlowBT String Unit
hideLoaderFlow = do
  toggleSetupSplash false
  void $ lift $ lift $ toggleLoader false
  liftFlowBT $ hideLoader ""

toggleSetupSplash :: Boolean -> FlowBT String Unit
toggleSetupSplash =
  if _ then do
    UI.splashScreen
  else do
    pure unit

riderRideCompletedScreenFlow :: FlowBT String Unit
riderRideCompletedScreenFlow = do
  flow <- UI.riderRideCompletedScreen
  case flow of
    RIDER_DETAILS_SCREEN state -> do
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen { props { fromMyRides = RideCompletedScreen } })
      tripDetailsScreenFlow
    GO_TO_HELP_AND_SUPPORTS -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen { data { fromScreen = "RideCompleted" } })
      flowRouter HelpAndSupportScreenFlow
    HOME_SCREENS rideId -> do
      void $ lift $ lift $ fork $ Remote.notifyFlowEvent $ Remote.makeNotifyFlowEventReq "RATE_DRIVER_SKIPPED"
      setValueToLocalStore RATING_SKIPPED rideId
      (GlobalState state) <- getState
      when (isLocalStageOn FindingQuotes)
        $ do
            cancelEstimate state.homeScreen.props.estimateId
      let markerName = getCurrentLocationMarker $ getValueToLocalStore VERSION_NAME
          markerConfig = defaultMarkerConfig{ markerId = markerName, pointerIcon = markerName }
      void $ pure $ removeAllPolylines ""
      void $ lift $ lift $ liftFlow $ showMarker markerConfig 9.9 9.9 160 0.5 0.9 (getNewIDWithTag "CustomerHomeScreen")
      void $ pure $ currentPosition ""
      void $ updateLocalStage HomeScreen
      updateUserInfoToState state.homeScreen
      modifyScreenState $ ReportIssueChatScreenStateType (\_ -> ReportIssueChatScreenData.initData)
      modifyScreenState $ RiderRideCompletedScreenStateType (\_ -> RiderRideCompletedScreenData.initData)
      currentFlowStatus false    
    GOTO_NAMMASAFETY _ triggerSos showtestDrill -> do
      (GlobalState state) <- getState
      updateSafetyScreenState state.homeScreen SafetyScreenData.defaultTimerValue showtestDrill triggerSos
      case (triggerSos || showtestDrill) of
        true -> do
          let
            isRideCompleted = state.homeScreen.props.currentStage == RideCompleted
          modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { props { reportPastRide = isRideCompleted, fromScreen = Just RideCompletedScreen }, data { lastRideDetails = if isRideCompleted then Arr.head $ myRideListTransformer true [ state.homeScreen.data.ratingViewState.rideBookingRes ] state.homeScreen.data.config Nothing else Nothing } })
          activateSafetyScreenFlow
        false -> nammaSafetyFlow
    GO_TO_ISSUE_REPORT_CHAT_SCREEN_WITH_ISSUE updatedState issueType -> do
      let (RideBookingRes resp) = updatedState.ratingViewState.rideBookingRes
          checkIfSafetyEnabled = EHC.getExpiryTime (fromMaybe "" resp.rideEndTime) true / 60 < updatedState.config.safety.pastRideInterval
      if issueType == CTA.Accessibility then
        homeScreenFlow
      else if checkIfSafetyEnabled && issueType == CTA.NightSafety then do
        modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> SafetyScreenData.initData { props { reportPastRide = true, fromScreen = Just RideCompletedScreen }, data { lastRideDetails = Arr.head $ myRideListTransformer true [ updatedState.ratingViewState.rideBookingRes ] updatedState.config Nothing  } })
        activateSafetyScreenFlow
      else do
        let
          language = fetchLanguage $ getLanguageLocale languageKey
          categoryId = case issueType of
            CTA.NightSafety -> "f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0"
            _ -> "ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj"

          categoryName = case issueType of
            CTA.NightSafety -> "Safety Related Issue"
            _ -> "Ride related"

        (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language  categoryId "" updatedState.rideRatingState.rideId  ""
        getOptionsResp <- if (issueType == CTA.DemandExtraTollAmount) then do
            let getOptionsRes' = DA.find(\(Option x) -> x.label == "TOLL_RELATED_ISSUES" )getOptionsRes.options
            (GetOptionsRes getOptionsRes'') <- case getOptionsRes' of 
              Just (Option resp) -> Remote.getOptionsBT language  categoryId resp.issueOptionId updatedState.rideRatingState.rideId  ""
              Nothing -> pure (GetOptionsRes getOptionsRes)
            pure getOptionsRes''
          else pure getOptionsRes
        let 
          getOptionsRes'' = mapWithIndex (\index (Option optionObj) -> optionObj {option = (show (index + 1)) <> ". " <> (reportIssueMessageTransformer optionObj.option) }) getOptionsResp.options
          messages' = mapWithIndex (\index (Message currMessage) -> makeChatComponent' (reportIssueMessageTransformer currMessage.message) currMessage.messageTitle currMessage.messageAction currMessage.label "Bot" (getCurrentUTC "") "Text" (500*(index + 1))) getOptionsResp.messages
          chats' = map (\(Message currMessage) -> Chat {chatId : currMessage.id, chatType : "IssueMessage", timestamp : (getCurrentUTC "")} )getOptionsResp.messages

        modifyScreenState $ ReportIssueChatScreenStateType (\_ -> ReportIssueChatScreenData.initData {
          data {
            entryPoint = ReportIssueChatScreenData.RiderRideCompletedScreen
          , chats = chats'
          , tripId = Just updatedState.rideRatingState.rideId
          , selectedCategory = {
              categoryName : categoryName
            , categoryId : categoryId
            , categoryImageUrl : Nothing
            , categoryAction : Nothing
            , isRideRequired : false
            , maxAllowedRideAge : Nothing
            , categoryType : "Category"
            , allowedRideStatuses : Nothing
          }
          , options = getOptionsRes''
          , chatConfig {
              messages = messages'
            }
          , selectedRide = Nothing 
          }
        })
        void $ pure $ toggleBtnLoader "" false
        flowRouter IssueReportChatScreenFlow
    GO_TO_DRIVER_PROFILE state -> do
      modifyScreenState $ DriverProfileScreenCommonStateType ( \driverProfileScreen -> driverProfileScreen { props { rideId = state.rideRatingState.rideId } } )
      driverProfileScreenFlow
    SUBMIT_RATINGS state audio -> do
      logField_ <- lift $ lift $ getLogFields
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_ride_give_feedback" $ [ { key: "Rating", value: unsafeToForeign state.ratingCard.rating } ]
      void $ lift $ lift $ fork do
        (_ :: (Either ErrorResponse RideFeedbackRes)) <- HelpersAPI.callApi $ Remote.makeRideFeedBackReq state.rideRatingState.rideId state.ratingCard.feedbackList
        pure unit
      void $ lift $ lift $ fork do
        (_ :: (Either ErrorResponse APISuccessResp)) <- HelpersAPI.callApi $ getfeedbackReqs state audio
        pure unit
      void $ updateLocalStage HomeScreen
      let
        finalAmount = if state.topCard.finalAmount == 0 then state.rideRatingState.finalAmount else state.topCard.finalAmount
      let
        bookingId = if state.bookingId == "" then state.rideRatingState.bookingId else state.bookingId
      pure $ runFn3 emitJOSEvent "java" "onEvent" $ encode
        $ EventPayload
            { event: "process_result"
            , payload:
                Just
                  { action: "feedback_submitted"
                  , trip_amount: Just finalAmount
                  , trip_id: Just bookingId
                  , ride_status: Nothing
                  , screen: Just "RiderRideCompletedScreenState"
                  , exit_app: false
                  }
            }
      (GlobalState globalState) <- getState
      updateUserInfoToState globalState.homeScreen
      if (getSearchType unit) == "direct_search" then do
        void $ updateLocalStage SearchLocationModel
        checkAndUpdateLocations
        modifyScreenState $ ReportIssueChatScreenStateType (\_ -> ReportIssueChatScreenData.initData)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { currentStage = HomeScreen } })
        searchLocationFlow
      else
       pure unit
      if state.ratingCard.rating == 5 then do
        void $ pure $ launchInAppRatingPopup unit
        pure unit
      else
        pure unit
      modifyScreenState $ ReportIssueChatScreenStateType (\_ -> ReportIssueChatScreenData.initData)
      modifyScreenState $ RiderRideCompletedScreenStateType (\_ -> RiderRideCompletedScreenData.initData)
      currentFlowStatus false


currentFlowStatus :: Boolean -> FlowBT String Unit
currentFlowStatus prioritizeRating = do
  logField_ <- lift $ lift $ getLogFields
  liftFlowBT $ markPerformance "CURRENT_FLOW_STATUS"
  void $ lift $ lift $ toggleLoader false
  liftFlowBT $ markPerformance "VERIFY_PROFILE_CALL_API"
  verifyProfile "LazyCheck"
  liftFlowBT $ markPerformance "FLOW_STATUS_CALL_API"
  flowStatus <- Remote.flowStatusBT "LazyCheck"
  liftFlowBT $ markPerformance "RIDE_LIST_CALL_API"
  case flowStatus ^. _currentStatus of
    WAITING_FOR_DRIVER_OFFERS currentStatus -> goToFindingQuotesStage currentStatus.estimateId (flowStatus ^. _isValueAddNP) currentStatus.otherSelectedEstimates false currentStatus.tripCategory
    DRIVER_OFFERED_QUOTE currentStatus -> goToFindingQuotesStage currentStatus.estimateId (flowStatus ^. _isValueAddNP) (Just []) true Nothing
    WAITING_FOR_DRIVER_ASSIGNMENT currentStatus -> goToConfirmRide currentStatus
    RIDE_ASSIGNED _ -> checkRideStatus true prioritizeRating
    PENDING_RATING _ -> do
      firstRideCompletedEvent ""
      checkRideStatus false prioritizeRating
    _ -> checkRideStatus false prioritizeRating
  liftFlowBT $ markPerformance "HIDE_LOADER_FLOW"
  hideLoaderFlow
  void $ pure $ hideKeyboardOnNavigation true -- TODO:: Why is this added here @ashkriti?
  (GlobalState globalState) <- getState
  if globalState.homeScreen.props.currentStage == RideCompleted then riderRideCompletedScreenFlow else homeScreenFlow

  where
  goToConfirmingQuotesStage :: { bookingId :: String, validTill :: String, fareProductType :: Maybe String, tripCategory :: Maybe TripCategory } -> FlowBT String Unit
  goToConfirmingQuotesStage currentStatus = do
    let
      currentTimeToValid = EHC.getUTCAfterNSeconds (getCurrentUTC "") 1800
      diffFromValidToCurrent = EHC.compareUTCDate currentStatus.validTill (getCurrentUTC "")
    when (diffFromValidToCurrent < 3600 && diffFromValidToCurrent > 0 ) do --change depending on time
      setValueToLocalStore CONFIRM_QUOTES_POLLING "false"
      updateLocalStage ConfirmingQuotes
      hideLoaderFlow
      case (getFlowStatusData "LazyCheck") of
        Just (FlowStatusData flowStatusData) -> do
          modifyScreenState
            $ HomeScreenStateType
                ( \homeScreen ->
                    homeScreen
                      { props
                        { sourceLat = flowStatusData.source.lat
                        , sourceLong = flowStatusData.source.lng
                        , destinationLat = flowStatusData.destination.lat
                        , destinationLong = flowStatusData.destination.lng
                        , currentStage = ConfirmingQuotes
                        , rideRequestFlow = true
                        , selectedQuote = Nothing
                        , bookingId = currentStatus.bookingId
                        , city = getCityNameFromCode flowStatusData.source.city
                        }
                      , data
                        { source = flowStatusData.source.place
                        , destination = flowStatusData.destination.place
                        , sourceAddress = flowStatusData.sourceAddress
                        , destinationAddress = flowStatusData.destinationAddress
                        , fareProductType = case currentStatus.tripCategory of
                            Nothing -> homeScreen.data.fareProductType
                            Just (TripCategory tripCategory) -> 
                              if tripCategory.tag == CTA.Delivery then FPT.DELIVERY 
                              else if tripCategory.tag == CTA.Rental then FPT.RENTAL 
                              else homeScreen.data.fareProductType
                        }
                      }
                )
        Nothing -> updateFlowStatus SEARCH_CANCELLED
      homeScreenFlow
    pure unit

  verifyProfile :: String -> FlowBT String Unit
  verifyProfile dummy = do
    liftFlowBT $ markPerformance "VERIFY_PROFILE"
    response <- Remote.getProfileBT ""
    config <- getAppConfigFlowBT appConfig
    updateVersion (response ^. _clientVersion) (response ^. _bundleVersion)
    updateFirebaseToken (response ^. _maskedDeviceToken) getUpdateToken
    updatePersonInfo response
    updateFlowStatusStorage response
    updateCTEventData response
    setValueToLocalStore CUSTOMER_REFERRAL_CODE (fromMaybe "" (response ^. _customerReferralCode))
    if isNothing (response ^. _firstName) then do
      void $ updateLocalStage HomeScreen
      hideLoaderFlow
      when (response ^. _referralCode /= Nothing) $ void $ modifyScreenState $ AccountSetUpScreenStateType (\state -> state {data {isReferred = Verified, referralCode = (fromMaybe "" (response ^. _referralCode))}})
      accountSetUpScreenFlow
      handleDeepLinks Nothing true
    else do
      tag <- maybe (pure "") pure (response ^. _disability)
      let
        hasCompletedSafetySetup = fromMaybe false $ response ^. _hasCompletedSafetySetup

        hasCompletedMockSafetyDrill = fromMaybe false $ response ^. _hasCompletedMockSafetyDrill

        sosBannerType = case hasCompletedSafetySetup, hasCompletedMockSafetyDrill of
          false, _ -> Just ST.SETUP_BANNER
          true, false -> Just ST.MOCK_DRILL_BANNER
          _, _ -> Nothing
      modifyScreenState $ HomeScreenStateType
        $ \homeScreen →
            homeScreen
              { data
                { disability = Just { tag: tag, id: "", description: "" }
                , followers = Nothing
                , settingSideBar
                  { name = fromMaybe "" (response ^. _firstName)
                  , gender = response ^. _gender
                  , email = response ^. _email
                  , hasCompletedSafetySetup = hasCompletedSafetySetup
                  }
                , cancellationRate = response ^. _cancellationRate
                }
              , props
                { isBanner = false
                , sosBannerType = sosBannerType
                , followsRide = fromMaybe false (response ^. _followsRide)
                , isSafetyCenterDisabled = fromMaybe false (response ^. _isSafetyCenterDisabled)
                , userBlocked = fromMaybe false (response ^. _isBlocked)
                }
              }

  getUpdateToken :: String -> FlowBT String Unit --TODO:: Move this to common library
  getUpdateToken token =
    let
      UpdateProfileReq initialData = Remote.mkUpdateProfileRequest FunctionCall

      requiredData = initialData { deviceToken = Just token }
    in
      void $ lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)

  updatePersonInfo :: GetProfileRes -> FlowBT String Unit
  updatePersonInfo response = do
    config <- getAppConfigFlowBT appConfig
    let
      referralStatus = getValueToLocalStore REFERRAL_STATUS
      language = response ^. _language
      deviceIdRes = response ^. _deviceId
      androidIdRes = response ^. _androidId
      deviceId = JB.getDeviceID unit
      androidId = JB.getAndroidId unit
      updateDeviceId = (isNothing deviceIdRes && deviceId /= "NO_DEVICE_ID")
      updateAndroidId = (isNothing androidIdRes && androidId /= "NO_ANDROID_ID")
      mbDeviceId = if deviceId /= "NO_DEVICE_ID" then Just deviceId else Nothing
      mbAndroidId = if androidId /= "NO_ANDROID_ID" then Just androidId else Nothing
    let
      referralCode =
        if config.feature.enableReferral && config.feature.enableAutoReferral then do
          case referralStatus of
            "NOT_REFERRED_NOT_TAKEN_RIDE" -> getReferralCode (getValueToLocalStore REFERRER_URL)
            _ -> Nothing
        else
          Nothing
    if isNothing language || (getKeyByLanguage (fromMaybe "ENGLISH" language) /= (getLanguageLocale languageKey)) || isJust referralCode || updateDeviceId || updateAndroidId then do
      case referralCode of
        Just code
          | referralStatus == "NOT_REFERRED_NOT_TAKEN_RIDE" -> do
            referralAppliedStatus <- applyReferralCode code
            case referralAppliedStatus of
              REFERRAL_APPLIED -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { referral { referralStatus = REFERRAL_APPLIED }, isReferred = true } })
              REFERRAL_INVALID -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { referral { referralStatus = REFERRAL_INVALID } } })
              _ -> pure unit
        _ -> do
          let (UpdateProfileReq initialData) = Remote.mkUpdateProfileRequest FunctionCall
              requiredData = if updateDeviceId || updateAndroidId then initialData{deviceId = mbDeviceId, androidId = mbAndroidId} else initialData
          void $ lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
    else
      pure unit

  goToFindingQuotesStage :: String -> Maybe Boolean -> Maybe (Array String) -> Boolean -> Maybe TripCategory -> FlowBT String Unit
  goToFindingQuotesStage estimateId mbIsValueAddNP otherSelectedEstimates driverOfferedQuote tripCategory = do
    let
      providerType = maybe CTA.ONUS (\valueAdd -> if valueAdd then CTA.ONUS else CTA.OFFUS) mbIsValueAddNP -- This defines whether quote selected was ours or not after kill and relaunch
    removeChatService ""
    if any (_ == (getValueToLocalStore FINDING_QUOTES_START_TIME)) [ "__failed", "" ] then do
      updateFlowStatus SEARCH_CANCELLED
    else do
      let
        searchExpiryTime = getSearchExpiryTime true

        secondsLeft = findingQuotesSearchExpired driverOfferedQuote true true
      if secondsLeft > 0 then do
        let
          stage = if isLocalStageOn ReAllocated then ReAllocated else FindingQuotes
        updateLocalStage stage
        setValueToLocalStore AUTO_SELECTING ""
        setValueToLocalStore FINDING_QUOTES_POLLING "false"
        setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
        (GlobalState currentState) <- getState
        let enableBoostSearch = fetchRemoteConfigString "enable_boost_search" == "true"
            enableTipView = any (_ /= currentState.homeScreen.data.fareProductType) [FPT.ONE_WAY, FPT.DRIVER_OFFER] && not enableBoostSearch
        let
          tipViewData = case (getTipViewData "LazyCheck") of
            Just (TipViewData tipView) -> do
              currentState.homeScreen.props.tipViewProps { stage = tipView.stage, activeIndex = tipView.activeIndex, isVisible = tipView.activeIndex >= 0 && enableTipView }
            Nothing -> do
              currentState.homeScreen.props.tipViewProps
          estimates = if length currentState.homeScreen.data.specialZoneQuoteList > 0 then currentState.homeScreen.data.specialZoneQuoteList else map (\item -> item{validTill = ""}) (getCachedEstimates "")
          selectedVariant = getValueToLocalStore SELECTED_VARIANT
          selectedEstimate = case selectedVariant of 
                                "BOOK_ANY" -> fromMaybe ChooseVehicle.config $ DA.find (\item -> item.vehicleVariant == "BOOK_ANY") estimates
                                _ -> fromMaybe ChooseVehicle.config $ DA.find (\item -> item.id == estimateId) estimates
          fareProductType = case tripCategory of
            Nothing -> currentState.homeScreen.data.fareProductType
            Just (TripCategory tripCategory') -> 
              if tripCategory'.tag == CTA.Delivery then FPT.DELIVERY 
              else if tripCategory'.tag == CTA.Rental then FPT.RENTAL 
              else currentState.homeScreen.data.fareProductType

        case (getFlowStatusData "LazyCheck") of
          Just (FlowStatusData flowStatusData) -> do
            modifyScreenState
              $ HomeScreenStateType
                  ( \homeScreen ->
                      homeScreen
                        { props
                          { sourceLat = flowStatusData.source.lat
                          , sourceLong = flowStatusData.source.lng
                          , destinationLat = flowStatusData.destination.lat
                          , destinationLong = flowStatusData.destination.lng
                          , currentStage = stage
                          , searchExpire = secondsLeft
                          , estimateId = estimateId
                          , rideRequestFlow = true
                          , selectedQuote = Nothing
                          , tipViewProps = tipViewData
                          , city = getCityNameFromCode flowStatusData.source.city
                          , findingQuotesProgress = 1.0 - (INT.toNumber secondsLeft) / (INT.toNumber searchExpiryTime)
                          , locateOnMapProps { sourceLocationName = flowStatusData.source.address, sourceGeoJson = flowStatusData.sourceGeoJson, sourceGates = flowStatusData.sourceGates }
                          }
                        , data
                          { source = flowStatusData.source.place
                          , destination = flowStatusData.destination.place
                          , sourceAddress = flowStatusData.sourceAddress
                          , otherSelectedEstimates = fromMaybe [] otherSelectedEstimates
                          , destinationAddress = flowStatusData.destinationAddress
                          , specialZoneQuoteList = estimates
                          , selectedEstimatesObject = selectedEstimate {vehicleVariant = selectedVariant, providerType = providerType}
                          , fareProductType = fareProductType
                          }
                        }
                  )
          Nothing -> updateFlowStatus SEARCH_CANCELLED
      else
        updateFlowStatus SEARCH_CANCELLED

  goToConfirmRide :: { bookingId :: String, validTill :: String, fareProductType :: Maybe String, tripCategory :: Maybe TripCategory } -> FlowBT String Unit
  goToConfirmRide currentStatus =
    let
      bookingId = currentStatus.bookingId
      _ = spy "inside gotoconfirmride" currentStatus
      fareProductType = case currentStatus.tripCategory of
        Nothing -> currentStatus.fareProductType
        Just (TripCategory tripCategory) -> 
          if tripCategory.tag == CTA.Delivery then Just "DELIVERY"
          else if tripCategory.tag == CTA.Rental then Just "RENTAL"
          else currentStatus.fareProductType
    in
      if any (_ == fareProductType) [ Just "ONE_WAY_SPECIAL_ZONE", Nothing ] then
        checkRideStatus false false
      else if (fareProductType == Just "ONE_WAY") then do
        hideLoaderFlow
        updateLocalStage ConfirmingRide
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { currentStage = ConfirmingRide, bookingId = bookingId, isPopUp = NoPopUp } })
        homeScreenFlow
      else
        goToConfirmingQuotesStage currentStatus

enterMobileNumberScreenFlow :: FlowBT String Unit
enterMobileNumberScreenFlow = do
  let _ = setKeyInWindow "forceAppToNoInternetScreen" false
  config <- getAppConfigFlowBT appConfig
  let currentCityConfig = getCityConfig config.cityConfig $ getValueToLocalStore CUSTOMER_LOCATION
  hideLoaderFlow -- Removed initial choose langauge screen
  if (any (_ == getLanguageLocale languageKey) [ "__failed", "(null)" ]) then void $ pure $ setLanguageLocale config.defaultLanguage else pure unit
  logField_ <- lift $ lift $ getLogFields
  if any (_ == getValueToLocalStore REFERRER_URL) [ "__failed", "(null)" ] then
    void $ pure $ extractReferrerUrl unit
  else
    pure unit
  void $ lift $ lift $ toggleLoader false
  void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_enter_mob_num_scn_view"
  setValueToLocalStore T_AND_C_VERSION (show config.termsVersion)
  flow <- UI.enterMobileNumberScreen
  case flow of
    GoToAccountSetUp state -> do
            void $ lift $ lift $ loaderText (getString STR.VERIFYING_OTP) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)  -- TODO : Handlde Loader in IOS Side
            void $ lift $ lift $ toggleLoader true
            let generatedID = "generated_" <> (generateSessionId unit)
            (resp) <- lift $ lift $  Remote.verifyToken (Remote.makeVerifyOTPReq state.data.otp generatedID) state.data.tokenId
            case resp of
              Right resp -> do
                    void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_verify_otp"
                    let eventLog = if state.props.autoFillOTPEnabled then  "otp_auto_detected_success" else "otp_manual_success"
                    let _ = EHE.addEvent (EHE.defaultEventObject eventLog) { module = "onboarding"}
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false}})
                    let (VerifyTokenResp response) = resp
                        customerId = ((response.person)^. _id)
                    if (customerId == "__failed") then do
                      void $ lift $ lift $ setLogField "customer_id" $ encode ("null")
                      pure unit
                      else do
                        void $ lift $ lift $ setLogField "customer_id" $ encode (customerId)
                        pure unit
                    setValueToLocalStore CUSTOMER_ID customerId
                    void $ liftFlowBT $ setCleverTapProfileData "Identity" customerId
                    void $ liftFlowBT $ setCleverTapProfileData "Preferred Language" "ENGLISH"
                    setValueToLocalStore REGISTERATION_TOKEN response.token
                    setValueToLocalStore USER_NAME $ (fromMaybe "" $ response.person ^. _firstName) <> " " <> (fromMaybe "" $ response.person ^. _middleName) <> " " <> (fromMaybe "" $ response.person ^. _lastName)
                    void $ liftFlowBT $ loginCleverTapUser unit
                    if isNothing (response.person ^. _firstName) then currentFlowStatus false else handleDeepLinks Nothing false
              Left err -> do
                let eventLog = if state.props.autoFillOTPEnabled then  "otp_auto_detected_failure" else "otp_manual_failure"
                let _ = EHE.addEvent (EHE.defaultEventObject eventLog) { module = "onboarding"}
                pure $ setText (getNewIDWithTag "EnterOTPNumberEditText") ""
                let errResp = err.response
                    codeMessage = decodeError errResp.errorMessage "errorCode"
                if ( err.code == 400 && codeMessage == "TOKEN_EXPIRED") then do
                    void $ lift $ lift $ showToast (getString STR.OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN)
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> enterMobileNumber{data{otp=""}, props{enterOTP = false, wrongOTP = false}})
                else if ( err.code == 400 && codeMessage == "INVALID_AUTH_DATA") then do
                    let attemptsLeft = decodeError errResp.errorMessage "errorPayload"
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> enterMobileNumber{props{wrongOTP = true, btnActiveOTP = false, attemptLeft = attemptsLeft}, data{otp=""}})
                else if ( err.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then do
                    void $ lift $ lift $ showToast (getString STR.TOO_MANY_LOGIN_ATTEMPTS_PLEASE_TRY_AGAIN_LATER)
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false, wrongOTP = false}, data{otp=""}})
                else do
                    void $ lift $ lift $ showToast (getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false,wrongOTP = false}, data{otp=""}})
                enterMobileNumberScreenFlow
    GoToOTP state -> do
      when ((getValueToLocalStore MOBILE_NUMBER) /= state.data.mobileNumber)
        $ do
            deleteValueFromLocalStore SUGGESTIONS_MAP
            deleteValueFromLocalStore RECENT_SEARCHES
      void $ pure $ setValueInWindow (show MOBILE_NUMBER) state.data.mobileNumber
      setValueToLocalStore COUNTRY_CODE (state.data.countryObj.countryCode)
      void $ liftFlowBT $ setCleverTapProfileData "Phone" (state.data.countryObj.countryCode <> (getValueToLocalStore MOBILE_NUMBER))
      (TriggerOTPResp triggerOtpResp) <- Remote.triggerOTPBT (Remote.makeTriggerOTPReq state.data.mobileNumber state.data.countryObj.countryCode (show state.data.otpChannel) currentCityConfig.allowBlockedUserLogin)
      void $ lift $ lift $ showToast (getString if state.data.otpChannel == SMS then STR.SENT_OTP_VIA_SMS else STR.SENT_OTP_VIA_WHATSAPP)
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen { data { tokenId = triggerOtpResp.authId, attempts = triggerOtpResp.attempts }, props { enterOTP = true, resendEnable = false } })
      modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen { data { settingSideBar { number = state.data.mobileNumber } }, props { userBlocked = triggerOtpResp.isPersonBlocked } })
      enterMobileNumberScreenFlow
    ResendOTP state -> do
      (ResendOTPResp resendResp) <- Remote.resendOTPBT state.data.tokenId
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen { data { tokenId = resendResp.authId, attempts = resendResp.attempts } })
      enterMobileNumberScreenFlow
    GoBack state -> do
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen { data { timer = 30 }, props { enterOTP = false, resendEnable = false } })
      enterMobileNumberScreenFlow
    GoToWelcomeScreen state -> welcomeScreenFlow

welcomeScreenFlow :: FlowBT String Unit
welcomeScreenFlow = do
  hideLoaderFlow
  flow <- UI.welcomeScreen
  case flow of
    GoToMobileNumberScreen -> enterMobileNumberScreenFlow

accountSetUpScreenFlow :: FlowBT String Unit
accountSetUpScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  disabilityListT <- updateDisabilityList "Account_Set_Up_Screen"
  modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> accountSetUpScreen { data { disabilityOptions { disabilityOptionList = disabilityListT } } })
  flow <- UI.accountSetUpScreen
  case flow of
    GO_HOME state -> do
      void $ lift $ lift $ toggleLoader false
      let
        gender = getGenderValue state.data.gender

        selectedDisability = state.data.disabilityOptions.selectedDisability

        deviceId = JB.getDeviceID unit

        androidId = JB.getAndroidId unit

        (UpdateProfileReq initialData) = Remote.mkUpdateProfileRequest FunctionCall

        requiredData =
          initialData
            { firstName = (Just state.data.name)
            , gender = gender
            , hasDisability = Just (isJust selectedDisability)
            , disability =
              case selectedDisability of
                Just disability -> Just (Remote.mkDisabilityData disability (fromMaybe "" state.data.disabilityOptions.otherDisabilityReason))
                _ -> Nothing
            , deviceId = if deviceId /= "NO_DEVICE_ID" then Just deviceId else Nothing
            , androidId = if androidId /= "NO_ANDROID_ID" then Just androidId else Nothing
            }
      setValueToLocalStore DISABILITY_UPDATED "true"
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { showDisabilityPopUp = (isJust selectedDisability) }, data { disability = selectedDisability } })
      when (state.data.isReferred == NotVerified && DS.length state.data.referralCode >= 6) $ void $ applyReferralCode state.data.referralCode
      resp <- lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
      case resp of
        Right response -> do
          setValueToLocalStore USER_NAME state.data.name
          void $ pure $ setCleverTapUserProp  [{key : "Name", value :  unsafeToForeign state.data.name}]
          case gender of
            Just value -> do
              void $ pure $ setCleverTapUserProp [{key : "gender", value : unsafeToForeign value}]
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { settingSideBar { gender = Just value } }, props { isBanner = false, referral { showAddReferralPopup = false } } })
            Nothing -> pure unit
          void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_onboarded"
          void $ pure $ metaLogEvent "ny_user_onboarded"
          let _ = EHE.addEvent (EHE.defaultEventObject "home_screen_loaded") { module = "onboarding"}
          pure unit
        Left err -> do
          void $ lift $ lift $ showToast (getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
          modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> state { props { btnActive = true }, data { name = state.data.name } })
          accountSetUpScreenFlow
    GO_BACK -> do
      void $ pure $ deleteValueFromLocalStore REGISTERATION_TOKEN
      void $ pure $ deleteValueFromLocalStore MOBILE_NUMBER
      modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> AccountSetUpScreenData.initData)
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen -> enterMobileNumberScreen { data { otp = "" } })
      enterMobileNumberScreenFlow
    APPLY_REFERRAL referralCode -> do
      referralAppliedStatus <- applyReferralCode referralCode
      case referralAppliedStatus of
        REFERRAL_APPLIED -> do
          void $ pure $ hideKeyboardOnNavigation true
          modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> accountSetUpScreen { data {isReferred = Verified, referralTextFocussed = true} })
        REFERRAL_INVALID -> do
          modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> accountSetUpScreen { data {isReferred = ReferralFailed, referralTextFocussed = true} })
        _ -> pure unit
      accountSetUpScreenFlow

updateDisabilityList :: String -> FlowBT String (Array DisabilityT)
updateDisabilityList screenType = do
  response <- Remote.disabilityList
  case response of
    Right (GetDisabilityListResp resp) -> pure $ getDisabilityList resp
    Left err -> pure $ getDisabilityList []

homeScreenFlow :: FlowBT String Unit
homeScreenFlow = do
  liftFlowBT $ markPerformance "HOME_SCREEN_FLOW"
  logField_ <- lift $ lift $ getLogFields
  (GlobalState currentState) <- getState
  void $ checkAndUpdateSavedLocations currentState.homeScreen
  let
    isRideCompleted = getAndRemoveLatestNotificationType unit == "TRIP_FINISHED"
  when isRideCompleted $ currentFlowStatus (isJust currentState.homeScreen.data.upcomingRideDetails)
  void $ pure $ cleverTapSetLocation unit
  -- TODO: REQUIRED ONCE WE NEED TO STORE RECENT CURRENTLOCATIONS
  -- resp <- lift $ lift $ getCurrentLocationsObjFromLocal currentState.homeScreen
  -- modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{previousCurrentLocations = resp}})
  -- TODO: HANDLE LOCATION LIST INITIALLY
  void $ pure $ firebaseUserID (getValueToLocalStore CUSTOMER_ID)
  void $ lift $ lift $ toggleLoader false
  resp <- FlowCache.fetchAndUpdateScheduledRides true
  let
    bookingTimeList = decodeBookingTimeList FunctionCall

    nearestScheduledBookingTime = maybe (getCurrentUTC "") (\bookingTimeList -> bookingTimeList.rideStartTime) $ head $ bookingTimeList

    _ = runFn2 EHC.updatePushInIdMap "bannerCarousel" true

    currentCityConfig = getCityConfig currentState.homeScreen.data.config.cityConfig $ getValueToLocalStore CUSTOMER_LOCATION

    currTime = (getCurrentUTC "")

    diffInSeconds = (INT.toNumber $ fromMaybe 0 $ head $ filter (\item -> item >= 0) $ sort $ map (\date -> EHC.compareUTCDate date.rideStartTime currTime) bookingTimeList)

    famousDestinations = if null currentState.homeScreen.data.famousDestinations
                          then fetchFamousDestinations FunctionCall
                          else currentState.homeScreen.data.famousDestinations
  updateRideScheduledTime resp ""
  modifyScreenState
    $ HomeScreenStateType
        ( \homeScreen ->
            homeScreen
              { props { scheduledRidePollingDelay = diffInSeconds, hasTakenRide = (getValueToLocalStore REFERRAL_STATUS == "HAS_TAKEN_RIDE"), isReferred = (getValueToLocalStore REFERRAL_STATUS == "REFERRED_NOT_TAKEN_RIDE"), city = getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION }
              , data { currentCityConfig = currentCityConfig, famousDestinations = famousDestinations , latestScheduledRides = resp }
              }
        )
  flow <- UI.homeScreen
  void $ lift $ lift $ fork $ Remote.pushSDKEvents
  case flow of
    HybridAppExit -> pure unit
    STAY_IN_HOME_SCREEN -> do
      updateLocalStage HomeScreen
      modifyScreenState $ HomeScreenStateType (\_ -> HomeScreenData.initData)
      homeScreenFlow
    GO_TO_SEARCH_LOCATION_SCREEN state isSource -> do
      { currentLoc, sourceLoc, destLoc, address, destAddress } <- fetchSrcAndDestLoc state
      (GlobalState globalState) <- getState
      modifyScreenState
        $ SearchLocationScreenStateType
            ( \slsState ->
                SearchLocationScreenData.initData
                  { data { srcLoc = Just sourceLoc, currentLoc = currentLoc, destLoc = destLoc, locationList = globalState.globalProps.cachedSearches, rideType = slsState.data.rideType }
                  , props { focussedTextField = if isSource then Just SearchLocPickup else Just SearchLocDrop, actionType = SearchLocationAction, areBothLocMandatory = true }
                  }
            )
      searchLocationFlow
    ADD_STOP state -> do
      (GlobalState globalState) <- getState
      let
        updatedState = { lat: Just state.data.driverInfoCardState.sourceLat, lon: Just state.data.driverInfoCardState.sourceLng, city: state.props.city, addressComponents: encodeAddress state.data.driverInfoCardState.source [] Nothing state.data.driverInfoCardState.sourceLat state.data.driverInfoCardState.sourceLng, placeId: Nothing, address: state.data.driverInfoCardState.source, metroInfo: Nothing,  busStopInfo : Nothing,stationCode: "" }
      modifyScreenState
        $ SearchLocationScreenStateType
            ( \slsState ->
                SearchLocationScreenData.initData
                  { props { focussedTextField = Just SearchLocDrop, areBothLocMandatory = false, searchLocStage = PredictionsStage, actionType = AddingStopAction}
                  , data { locationList = globalState.globalProps.cachedSearches, fromScreen = (Screen.getScreen Screen.HOME_SCREEN), srcLoc = Just updatedState, rideType = slsState.data.rideType }
                  }
            )
      searchLocationFlow
    CHECK_FLOW_STATUS -> currentFlowStatus false
    GO_TO_MY_RIDES fromBanner -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> myRidesScreen { data { offsetValue = 0 }, props { fromNavBar = true, fromBanner = fromBanner } , prestoListArrayItems =[],itemsRides = []})
      myRidesScreenFlow
    GO_TO_HELP -> do
      void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_help"
      flowRouter HelpAndSupportScreenFlow
    CHANGE_LANGUAGE -> selectLanguageScreenFlow HomeScreenFlow
    GO_TO_ABOUT -> aboutUsScreenFlow
    GO_TO_MY_TICKETS -> do
      (GetAllBookingsRes bookedRes) <- Remote.getAllBookingsBT Booked
      (GetAllBookingsRes pendingRes) <- Remote.getAllBookingsBT Pending
      (GetAllBookingsRes cancelledRes) <- Remote.getAllBookingsBT Cancelled
      void $ pure $ spy "bookedRes" bookedRes
      void $ pure $ spy "pendingRes" pendingRes
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData { props { navigateToHome = true, currentStage = ViewTicketStage, previousStage = ViewTicketStage, ticketBookingList = getTicketBookings (buildBookingDetails bookedRes) (buildBookingDetails pendingRes) (buildBookingDetails cancelledRes) } })
      modifyScreenState $ TicketingScreenStateType (\ticketingScreen -> ticketingScreen { props { hideMyTickets = true } })
      ticketListFlow
    GO_TO_MY_PROFILE updateProfile -> do
      void $ lift $ lift $ liftFlow $ logEvent logField_ (if updateProfile then "safety_banner_clicked" else "ny_user_profile_click")
      modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState -> MyProfileScreenData.initData { props { fromHomeScreen = updateProfile, updateProfile = updateProfile, changeAccessibility = true, isBtnEnabled = true, genderOptionExpanded = false, showOptions = false, expandEnabled = true } })
      myProfileScreenFlow
    GO_TO_FIND_ESTIMATES updatedState -> do
      rideSearchRequestFlow updatedState
    GO_TO_TRIP_TYPE_SELECTION updatedState -> do
      findEstimates updatedState
    GO_TO_RIDE_SUMMARY_SCREEN updatedState -> do
      modifyScreenState $ RideSummaryScreenStateType (\rideSummaryScreen -> RideSummaryScreenData.initData{ data { rideDetails = fetchRideDetails updatedState,extraFare = fetchExtraFares updatedState,fromScreen = (Screen.getScreen Screen.RIDE_SUMMARY_SCREEN)},props{pickUpOpen = true,shimmerVisibility=false}} )
      let
        isRoundTrip = updatedState.props.searchLocationModelProps.tripType == ROUND_TRIP
        currentSelectedEstimatesObject = updatedState.data.selectedEstimatesObject
        currIndex = currentSelectedEstimatesObject.index
        selectedEstimatesObject = if (currIndex == 0) then (fromMaybe currentSelectedEstimatesObject ((updatedState.data.quoteList)!!0)) else currentSelectedEstimatesObject
        startTime = if updatedState.data.startTimeUTC == "" then (getCurrentUTC "") else updatedState.data.startTimeUTC
        isScheduledRideSearch = startTime > (getCurrentUTC "")
      liftFlowBT $ logEventWithMultipleParams logField_ "user_intercity_ride_details_shown"
                                                                $ [ { key: "Vehicle Variant", value: unsafeToForeign selectedEstimatesObject.vehicleVariant }
                                                                  , { key: "RoundTrip", value: unsafeToForeign isRoundTrip }
                                                                  , { key: "Estimated Fare", value: unsafeToForeign selectedEstimatesObject.price}
                                                                  , { key: "ScheduledRide", value: unsafeToForeign isScheduledRideSearch }
                                                                  ]
                                                                <> (maybe [] (\rideVehicleServiceTier -> [{ key: "Vehicle ServiceTierName", value: unsafeToForeign $ rideVehicleServiceTier}]) selectedEstimatesObject.serviceTierName )
      rideSummaryScreenFlow
    RETRY_FINDING_QUOTES showLoader estimateId -> do
      void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS) -- TODO : Handled Loader in IOS Side
      void $ lift $ lift $ toggleLoader showLoader
      (GlobalState newState) <- getState
      let
        state = newState.homeScreen
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_tip_search" $ [ { key: "Tip amount (₹)", value: unsafeToForeign state.props.customerTip.tipForDriver } ]
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_rider_retry_request_quote"
        $ [ { key: "Request Type", value: unsafeToForeign if (getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then "Auto Assign" else "Manual Assign" }
          , { key: "Estimate Fare (₹)", value: unsafeToForeign (state.data.suggestedAmount + state.data.rateCard.additionalFare) }
          , { key: "Customer tip (₹)", value: unsafeToForeign state.props.customerTip.tipForDriver }
          , { key: "Estimated Ride Distance", value: unsafeToForeign state.data.rideDistance }
          , { key: "Night Ride", value: unsafeToForeign state.data.rateCard.isNightShift }
          ]
      if (not (isLocalStageOn QuoteList)) then do
        void $ pure $ firebaseLogEvent "ny_user_cancel_and_retry_request_quotes"
        cancelEstimate estimateId
      else do
        void $ pure $ firebaseLogEvent "ny_user_retry_request_quotes"
      setValueToLocalStore AUTO_SELECTING "false"
      setValueToLocalStore FINDING_QUOTES_POLLING "false"
      setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
      when (getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") do
        void $ pure $ firebaseLogEvent "ny_user_auto_confirm"
      let
        currentTime = (convertUTCtoISC (getCurrentUTC "") "HH:mm:ss")

        findingQuotesTime = convertUTCtoISC (getValueToLocalNativeStore FINDING_QUOTES_START_TIME) "HH:mm:ss"
      if withinTimeRange findingQuotesTime currentTime "22:00:00" || withinTimeRange findingQuotesTime currentTime "05:00:00" then do
        void $ lift $ lift $ showToast (getString STR.PLEASE_FIND_REVISED_FARE_ESTIMATE)
        void $ pure $ firebaseLogEvent "ny_user_new_estimate_after_night_charges_applicable"
        updateLocalStage FindEstimateAndSearch
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { currentStage = FindEstimateAndSearch, searchAfterEstimate = false } })
      else do
        void $ pure $ setValueToLocalStore FINDING_QUOTES_START_TIME (getCurrentUTC "LazyCheck")
        response <- lift $ lift $ Remote.selectEstimate (Remote.makeEstimateSelectReq (flowWithoutOffers WithoutOffers) (if state.props.customerTip.enableTips && state.props.customerTip.isTipSelected && state.props.customerTip.tipForDriver > 0 then Just state.props.customerTip.tipForDriver else Nothing) state.data.otherSelectedEstimates (state.data.config.isAdvancedBookingEnabled) state.data.deliveryDetailsInfo) (state.props.estimateId)
        case response of
          Right res -> do
            void $ pure $ setValueToLocalStore LOCAL_STAGE (show FindingQuotes)
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { currentStage = FindingQuotes, searchExpire = (getSearchExpiryTime true) } })
            let
              tipViewData = if state.props.customerTip.isTipSelected then state.props.tipViewProps { stage = TIP_ADDED_TO_SEARCH } else HomeScreenData.initData.props.tipViewProps
            logInfo "retry_finding_quotes" ("selectedEstimate Current Stage: " <> (show state.props.currentStage) <> " LOCAL_STAGE: " <> (getValueToLocalStore LOCAL_STAGE) <> "Estimate Id :" <> state.props.estimateId)
            void $ pure $ setTipViewData (TipViewData { stage: tipViewData.stage, activeIndex: tipViewData.activeIndex, isVisible: tipViewData.isVisible })
            void $ pure $ JB.startLottieProcess JB.lottieAnimationConfig {rawJson = (HU.getAssetsBaseUrl FunctionCall) <> "lottie/progress_loader_line.json", lottieId = (getNewIDWithTag "lottieLoaderAnimProgress"), minProgress = 0.0, scaleType="CENTER_CROP"}
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { customerTip = if homeScreen.props.customerTip.isTipSelected then homeScreen.props.customerTip else HomeScreenData.initData.props.customerTip { enableTips = homeScreen.props.customerTip.enableTips }, tipViewProps = tipViewData, findingQuotesProgress = 0.0 } })
            homeScreenFlow
          Left err -> do
            void $ pure $ firebaseLogEvent "ny_user_estimate_expired"
            updateLocalStage FindEstimateAndSearch
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { currentStage = FindEstimateAndSearch, searchAfterEstimate = true } })
            let
              errResp = err.response

              codeMessage = decodeError errResp.errorMessage "errorCode"
            if (err.code == 400 && codeMessage == "SEARCH_REQUEST_EXPIRED") then do
              void $ lift $ lift $ showToast (getString STR.ESTIMATES_EXPIRY_ERROR)
            else do
              void $ lift $ lift $ showToast (getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { currentStage = SearchLocationModel } })
            currentFlowStatus false

    EDIT_DESTINATION_SOFT state -> do
      let destAddress = SearchReqLocation { gps : LatLong { lat : state.props.destinationLat , lon : state.props.destinationLong } , address : (LocationAddress state.data.destinationAddress)}
      resp <- lift $ lift $ HelpersAPI.callApi $ Remote.makeEditLocationRequest state.data.driverInfoCardState.rideId Nothing (Just destAddress)
      case resp of
        Right (EditLocationRes editDestinationSoftResp) -> do
          if (editDestinationSoftResp.bookingUpdateRequestId == Nothing) then do
            void $ lift $ lift $ showToast (getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{destination = state.data.driverInfoCardState.destination, destinationAddress = state.data.driverInfoCardState.destinationAddress}, props{destinationLat = state.data.driverInfoCardState.destinationLat, destinationLong = state.data.driverInfoCardState.destinationLng}})
            setValueToLocalStore TRACKING_DRIVER "False"
            void $ lift $ lift $ toggleLoader true
            checkRideStatus true false
            else do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ConfirmingEditDestinationLoc, bookingUpdateRequestId = editDestinationSoftResp.bookingUpdateRequestId}})
        Left (err) -> do
          void $ lift $ lift $ showToast (decodeError err.response.errorMessage "errorMessage")
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{destination = state.data.driverInfoCardState.destination, destinationAddress = state.data.driverInfoCardState.destinationAddress}, props{destinationLat = state.data.driverInfoCardState.destinationLat, destinationLong = state.data.driverInfoCardState.destinationLng}})
          setValueToLocalStore TRACKING_DRIVER "False"
          void $ lift $ lift $ toggleLoader true
          checkRideStatus true false
      homeScreenFlow

    EDIT_DEST_BACKPRESSED ->  do
      (GlobalState globalState) <- getState
      let state = globalState.homeScreen
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{destination = state.data.driverInfoCardState.destination, destinationAddress = state.data.driverInfoCardState.destinationAddress}, props{destinationLat = state.data.driverInfoCardState.destinationLat, destinationLong = state.data.driverInfoCardState.destinationLng}})
      setValueToLocalStore FINDING_EDIT_LOC_RESULTS "false"
      setValueToLocalStore TRACKING_DRIVER "False"
      void $ lift $ lift $ toggleLoader true
      checkRideStatus true false
      homeScreenFlow

    EDIT_LOCATION_SELECTED item addToRecents -> do
      void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      (GlobalState newState) <- getState
      let
        state = newState.homeScreen
      (GetPlaceNameResp destinationDetailResp) <- getPlaceNameResp (item.title <> ", " <> item.subTitle) state.props.destinationPlaceId state.props.destinationLat state.props.destinationLong (if state.props.isSource == Just true then dummyLocationListItemState else item)
      let
        (PlaceName destinationDetailResponse) = (fromMaybe HomeScreenData.dummyLocationName (destinationDetailResp !! 0))

        (LatLong destinationLocation) = (destinationDetailResponse.location)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { destinationLat = destinationLocation.lat, destinationLong = destinationLocation.lon } })
      (GlobalState updatedState) <- getState
      let
        bothLocationChangedState = updatedState.homeScreen { props { hotSpot { selectedSpot = Nothing, centroidPoint = Nothing } } }

      (ServiceabilityRes destServiceabilityResp) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq bothLocationChangedState.props.destinationLat bothLocationChangedState.props.destinationLong) DESTINATION
      let
        destServiceable = destServiceabilityResp.serviceable
      when (addToRecents)
        $ do
            addLocationToRecents item bothLocationChangedState true destServiceabilityResp.serviceable
            fetchAndModifyLocationLists bothLocationChangedState.data.savedLocations
      (GlobalState globalState) <- getState
      let
        updateScreenState = globalState.homeScreen

        recentList =
          updateLocListWithDistance
            updateScreenState.data.recentSearchs.predictionArray
            updateScreenState.props.sourceLat
            updateScreenState.props.sourceLong
            true
            state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist
      if ((not destServiceable) && (updateScreenState.props.destinationLat /= 0.0 && updateScreenState.props.destinationLat /= -0.1) && (updateScreenState.props.destinationLong /= 0.0 && bothLocationChangedState.props.destinationLong /= -0.1)) then do
        if (getValueToLocalStore LOCAL_STAGE == "HomeScreen") then do
          _ <- void $ lift $ lift $ showToast (getString STR.LOCATION_UNSERVICEABLE)
          pure unit
        else
          pure unit
        modifyScreenState $ HomeScreenStateType (\homeScreen -> updateScreenState { props { isDestServiceable = false, isRideServiceable = false, isSource = Just false, isSrcServiceable = true }, data { recentSearchs { predictionArray = recentList } } })
        homeScreenFlow
      else
        modifyScreenState
          $ HomeScreenStateType
              ( \homeScreen ->
                  updateScreenState
                    { props
                      { isRideServiceable = true
                      , isSrcServiceable = true
                      , isDestServiceable = true
                      }
                    , data
                      { recentSearchs
                        { predictionArray =
                          recentList
                        }
                      }
                    }
              )

      editDestinationFlow
    EDIT_LOCATION_DEST_SELECTED -> editDestinationFlow
    LOCATION_SELECTED item addToRecents -> do
      void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS) -- TODO : Handlde Loader in IOS Side
      void $ lift $ lift $ toggleLoader true
      (GlobalState newState) <- getState
      updateCurrentLocation ""
      let
        state = newState.homeScreen
        searchWithoutPlaceName = any (_ == state.props.rideSearchProps.sourceSelectType) [ ST.MAP, ST.FAVOURITE, ST.RETRY_SEARCH, ST.SUGGESTION ] && state.props.isSource == Just true
      case searchWithoutPlaceName of
        true -> pure unit
        false -> case state.props.isSource of
          Just true -> do
            (GetPlaceNameResp sourceDetailResp) <- getPlaceNameResp (item.title <> ", " <> item.subTitle) state.props.sourcePlaceId state.props.sourceLat state.props.sourceLong (if state.props.isSource == Just false then dummyLocationListItemState else item)
            let
              (PlaceName sourceDetailResponse) = (fromMaybe HomeScreenData.dummyLocationName (sourceDetailResp !! 0))

              (LatLong sourceLocation) = sourceDetailResponse.location
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { sourceLat = sourceLocation.lat, sourceLong = sourceLocation.lon } })
          Just false -> do
            (GetPlaceNameResp destinationDetailResp) <- getPlaceNameResp (item.title <> ", " <> item.subTitle) state.props.destinationPlaceId state.props.destinationLat state.props.destinationLong (if state.props.isSource == Just true then dummyLocationListItemState else item)
            let
              (PlaceName destinationDetailResponse) = (fromMaybe HomeScreenData.dummyLocationName (destinationDetailResp !! 0))

              (LatLong destinationLocation) = (destinationDetailResponse.location)
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { destinationLat = destinationLocation.lat, destinationLong = destinationLocation.lon } })
          _ -> pure unit
      updateSourceLocation ""
      (GlobalState updatedState) <- getState
      let
        bothLocationChangedState = updatedState.homeScreen { props { hotSpot { selectedSpot = Nothing, centroidPoint = Nothing } } }
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq bothLocationChangedState.props.sourceLat bothLocationChangedState.props.sourceLong) ORIGIN
      let
        srcServiceable = sourceServiceabilityResp.serviceable
      let
        (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
      let
        pickUpPoints =
          if null srcSpecialLocation.gatesInfo then
            filterHotSpots bothLocationChangedState sourceServiceabilityResp.hotSpotInfo bothLocationChangedState.props.sourceLat bothLocationChangedState.props.sourceLong
          else
            mapSpecialZoneGates srcSpecialLocation.gatesInfo
      (ServiceabilityRes destServiceabilityResp) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq bothLocationChangedState.props.destinationLat bothLocationChangedState.props.destinationLong) DESTINATION
      let
        (SpecialLocation desSpecialZone) = fromMaybe HomeScreenData.specialLocation (destServiceabilityResp.specialLocation)
        destServiceable = destServiceabilityResp.serviceable
        dropPoints =
          if null desSpecialZone.gatesInfo then
            filterHotSpots bothLocationChangedState destServiceabilityResp.hotSpotInfo bothLocationChangedState.props.destinationLat bothLocationChangedState.props.destinationLong
          else
            mapSpecialZoneGates desSpecialZone.gatesInfo
      let
        pickUpLoc = if length pickUpPoints > 0 then (if state.props.defaultPickUpPoint == "" then fetchDefaultPickupPoint pickUpPoints state.props.sourceLat state.props.sourceLong else state.props.defaultPickUpPoint) else (fromMaybe HomeScreenData.dummyLocation (state.data.nearByPickUpPoints !! 0)).place
      setUserCity CUSTOMER_LOCATION $ show (getCityNameFromCode sourceServiceabilityResp.city)
      let
        geoJson = transformGeoJsonFeature srcSpecialLocation.geoJson srcSpecialLocation.gatesInfo

        isHotSpot = null srcSpecialLocation.gatesInfo && not (null pickUpPoints)
      modifyScreenState
        $ HomeScreenStateType
            ( \homeScreen ->
                bothLocationChangedState
                  { data
                    { polygonCoordinates = geoJson
                    , nearByPickUpPoints = pickUpPoints
                    }
                  , props
                    { defaultPickUpPoint = (fromMaybe HomeScreenData.dummyLocation (pickUpPoints !! 0)).place
                    , city = getCityNameFromCode sourceServiceabilityResp.city
                    , isSpecialZone = (srcSpecialLocation.geoJson) /= Nothing
                    , confirmLocationCategory = if length pickUpPoints > 0 then (getZoneType srcSpecialLocation.category) else NOZONE
                    , findingQuotesProgress = 0.0
                    , locateOnMapProps
                      { sourceLocationName = Just srcSpecialLocation.locationName
                      , sourceGates = Just $ pickUpPoints
                      }
                    , hotSpot
                      { centroidPoint =
                        if isHotSpot then
                          Just { lat: bothLocationChangedState.props.sourceLat, lng: bothLocationChangedState.props.sourceLong }
                        else
                          Nothing
                      }
                    , showShimmer = false
                    }
                  }
            )
      when (addToRecents)
        $ do
            addLocationToRecents item bothLocationChangedState sourceServiceabilityResp.serviceable destServiceabilityResp.serviceable
            fetchAndModifyLocationLists bothLocationChangedState.data.savedLocations
      (GlobalState globalState) <- getState
      let
        updateScreenState = globalState.homeScreen

        recentList =
          updateLocListWithDistance
            updateScreenState.data.recentSearchs.predictionArray
            updateScreenState.props.sourceLat
            updateScreenState.props.sourceLong
            true
            state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist
      if (not srcServiceable && (updateScreenState.props.sourceLat /= -0.1 && updateScreenState.props.sourceLong /= -0.1) && (updateScreenState.props.sourceLat /= 0.0 && updateScreenState.props.sourceLong /= 0.0)) then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> updateScreenState { props { isSrcServiceable = false, isRideServiceable = false, isSource = Just true }, data { recentSearchs { predictionArray = recentList } } })
        homeScreenFlow
      else if ((not destServiceable) && (updateScreenState.props.destinationLat /= 0.0 && updateScreenState.props.destinationLat /= -0.1) && (updateScreenState.props.destinationLong /= 0.0 && bothLocationChangedState.props.destinationLong /= -0.1)) then do
        if (getValueToLocalStore LOCAL_STAGE == "HomeScreen") then do
          _ <- void $ lift $ lift $ showToast (getString STR.LOCATION_UNSERVICEABLE)
          pure unit
        else
          pure unit
        modifyScreenState $ HomeScreenStateType (\homeScreen -> updateScreenState { props { isDestServiceable = false, isRideServiceable = false, isSource = Just false, isSrcServiceable = true }, data { recentSearchs { predictionArray = recentList } } })
        homeScreenFlow
      else
        modifyScreenState
          $ HomeScreenStateType
              ( \homeScreen ->
                  updateScreenState
                    { props
                      { isRideServiceable = true
                      , isSrcServiceable = true
                      , isDestServiceable = true
                      , hasEstimateBackpoint = false
                      , customerTip = HomeScreenData.initData.props.customerTip
                      , tipViewProps = HomeScreenData.initData.props.tipViewProps
                      }
                    , data
                      { recentSearchs
                        { predictionArray =
                          recentList
                        }
                      }
                    }
              )
      rideSearchFlow "NORMAL_FLOW"
    SEARCH_LOCATION input state cacheInput -> do
      let
        config = getCityConfig state.data.config.cityConfig (getValueToLocalStore CUSTOMER_LOCATION)
        cityConfig =
          case state.props.isSource of
            Just true -> config { geoCodeConfig { strictBounds = false } }
            _ -> config
        event = case state.props.isSource of
          Just true -> "ny_user_auto_complete_api_trigger_src"
          Just false -> "ny_user_auto_complete_api_trigger_dst"
          Nothing -> ""
      void $ lift $ lift $ liftFlow $ logEvent logField_ event
      case DHM.lookup (DS.toLower input) state.props.rideSearchProps.cachedPredictions of
        Just locationList' -> do
          logInfo "auto_complete_cached_predictions" input
          modifyScreenState $ HomeScreenStateType (\homeScreen -> state { data { locationList = locationList' }, props { searchLocationModelProps { isAutoComplete = true, showLoader = false } , firstTimeAmbulanceSearch = false } })
        Nothing -> do
          logInfo "auto_complete_search_predictions" input
          (SearchLocationResp searchLocationResp) <- Remote.searchLocationBT (Remote.makeSearchLocationReq input state.props.sourceLat state.props.sourceLong (EHC.getMapsLanguageFormat $ getLanguageLocale languageKey) "" (if state.props.firstTimeAmbulanceSearch then state.data.config.ambulanceConfig else cityConfig.geoCodeConfig) state.props.rideSearchProps.autoCompleteType state.props.rideSearchProps.sessionId  (if state.props.firstTimeAmbulanceSearch then state.props.searchType else Nothing))
          let 
            sortedByDistanceList = sortPredictionByDistance searchLocationResp.predictions

            predictionList = getLocationList sortedByDistanceList

            listToBeUpdated =
              if state.props.isSource == Just true then
                state.data.recentSearchs.predictionArray
              else
                state.data.destinationSuggestions

            recentLists =
              updateLocListWithDistance
                listToBeUpdated
                state.props.sourceLat
                state.props.sourceLong
                true
                state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist

            filteredRecentsList = filterRecentSearches recentLists predictionList

            filteredPredictionList = differenceOfLocationLists predictionList filteredRecentsList

            filteredLocationList =
              map
                ( \item -> do
                    let
                      savedLocation = getPrediction item state.data.savedLocations

                      locIsPresentInSavedLoc = checkPrediction item state.data.savedLocations
                    if not locIsPresentInSavedLoc then
                      item
                        { lat = savedLocation.lat
                        , lon = savedLocation.lon
                        , locationItemType = Just SAVED_LOCATION
                        , postfixImageUrl = fetchImage FF_ASSET "ny_ic_fav_red"
                        }
                    else
                      item
                        { lat = item.lat
                        , lon = item.lon
                        , locationItemType = item.locationItemType
                        , postfixImageUrl = fetchImage FF_ASSET "ny_ic_fav"
                        }
                )
                (filteredRecentsList <> filteredPredictionList)
            cachedPredictions = if cacheInput
              then state.props.rideSearchProps.cachedPredictions
              else DHM.insert (DS.toLower input) filteredLocationList state.props.rideSearchProps.cachedPredictions
          modifyScreenState
            $ HomeScreenStateType
                ( \homeScreen ->
                    state
                      { data { locationList = filteredLocationList }
                      , props
                        { searchLocationModelProps
                          { isAutoComplete = true
                          , showLoader = false
                          }
                        , rideSearchProps { cachedPredictions = cachedPredictions }
                        , firstTimeAmbulanceSearch = false
                        }
                      }
                )
      homeScreenFlow

    CONFIRM_FARE state -> do
      void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS) -- TODO : Handlde Loader in IOS Side
      void $ lift $ lift $ toggleLoader true
      case state.props.bookingUpdateRequestId of
        Just id -> do
          resp <- lift $ lift $ HelpersAPI.callApi $ Remote.makeEditLocResultConfirmReq id
          case resp of
            Right (APISuccessResp resp) -> do
              void $ void $ lift $ lift $ showToast $  "Please wait while we confirm with your driver"
              lift $ lift $ liftFlow $ JB.showInAppNotification $ JB.inAppNotificationPayload{title = "Update Request sent to Driver", message = "Please wait for driver to accept your request.", channelId = "EditDest", showLoader = false, durationInMilliSeconds = 15000}
            Left (err) -> do
              void $ lift $ lift $ showToast (getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
        Nothing -> void $ lift $ lift $ showToast (getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{destination = state.data.driverInfoCardState.destination, destinationAddress = state.data.driverInfoCardState.destinationAddress}, props{destinationLat = state.data.driverInfoCardState.destinationLat, destinationLong = state.data.driverInfoCardState.destinationLng}})
      setValueToLocalStore TRACKING_DRIVER "False"
      void $ lift $ lift $ toggleLoader true
      checkRideStatus true false
      homeScreenFlow

    SELECT_ESTIMATE_AND_QUOTES state -> do
          let selectedEstimateOrQuote = state.data.selectedEstimatesObject
          setValueToLocalStore AUTO_SELECTING "false"
          setValueToLocalStore FINDING_QUOTES_POLLING "false"
          setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
          setValueToLocalStore FARE_ESTIMATE_DATA selectedEstimateOrQuote.price
          setValueToLocalStore SELECTED_VARIANT selectedEstimateOrQuote.vehicleVariant

          -- Logs
          liftFlowBT $ logEvent logField_ "ny_user_request_quotes"
          liftFlowBT $ logEventWithMultipleParams logField_ "ny_rider_request_quote"
            $ [ { key: "Request Type", value: unsafeToForeign if (getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then "Auto Assign" else "Manual Assign" }
              , { key: "Estimate Fare (₹)", value: unsafeToForeign (state.data.suggestedAmount + state.data.rateCard.additionalFare) }
              , { key: "Estimated Ride Distance", value: unsafeToForeign state.data.rideDistance }
              , { key: "Night Ride", value: unsafeToForeign state.data.rateCard.isNightShift }
            ]
          if (getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_auto_confirm" else pure unit

          case selectedEstimateOrQuote.searchResultType of
            ChooseVehicle.ESTIMATES -> do
              let valid = timeValidity (getCurrentUTC "") selectedEstimateOrQuote.validTill
              if valid then do
                void $ Remote.selectEstimateBT (Remote.makeEstimateSelectReq (flowWithoutOffers WithoutOffers) (if state.props.customerTip.enableTips && state.props.customerTip.isTipSelected && state.props.customerTip.tipForDriver > 0 then Just state.props.customerTip.tipForDriver else Nothing) state.data.otherSelectedEstimates (state.data.config.isAdvancedBookingEnabled) state.data.deliveryDetailsInfo) selectedEstimateOrQuote.id
                void $ pure $ setValueToLocalStore FINDING_QUOTES_START_TIME (getCurrentUTC "LazyCheck")
                setValueToLocalStore LOCAL_STAGE $ show FindingQuotes
                logStatus "finding_quotes" ("estimateId : " <> selectedEstimateOrQuote.id)
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props { currentStage = FindingQuotes
                                                                                          , estimateId = selectedEstimateOrQuote.id
                                                                                          , isPopUp = NoPopUp
                                                                                          , searchExpire = (getSearchExpiryTime true) }})
              else do
                void $ lift $ lift $ showToast (getString STR.ESTIMATES_EXPIRY_ERROR_AND_FETCH_AGAIN)
                findEstimates state
            ChooseVehicle.QUOTES _ -> do
              void $ pure $ enableMyLocation false
              updateLocalStage ConfirmingRide
              response  <- lift $ lift $ Remote.rideConfirm selectedEstimateOrQuote.id
              case response of
                Right (ConfirmRes resp) -> do
                  let bookingId = resp.bookingId
                  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{ currentStage = ConfirmingRide
                                                                                          , bookingId = bookingId
                                                                                          , isPopUp = NoPopUp }
                                                                                    , data { fareProductType = if state.data.fareProductType /= FPT.RENTAL && state.data.fareProductType /= FPT.INTER_CITY then FPT.ONE_WAY_SPECIAL_ZONE else state.data.fareProductType} })
                Left err  -> do
                  if not (err.code == 400 && (decodeError err.response.errorMessage "errorCode") == "QUOTE_EXPIRED") then
                    void $ lift $ lift $ showToast (getString STR.ERROR_OCCURED_TRY_AGAIN)
                  else
                    pure unit
                  void $ setValueToLocalStore AUTO_SELECTING "false"
                  updateLocalStage QuoteList
                  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{  currentStage = QuoteList
                                                                                            , selectedQuote = Nothing
                                                                                            , expiredQuotes = snoc state.props.expiredQuotes selectedEstimateOrQuote.id }
                                                                                    , data { quoteListModelState = [] } })
          homeScreenFlow
    SELECT_ESTIMATE state -> do
        logStatus "setting_price" ""
        void $ pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
        updateLocalStage SettingPrice
        let sourceSpecialTagIcon = zoneLabelIcon state.props.zoneType.sourceTag
            destSpecialTagIcon = zoneLabelIcon state.props.zoneType.destinationTag
            srcMarker = (normalRoute "").srcMarker
            destMarker = (normalRoute "").destMarker
            isRoundTrip = state.props.searchLocationModelProps.tripType == ROUND_TRIP
        case state.props.routeEndPoints of
          Just points -> do
            push <- lift $ lift $ liftFlow $ getPushFn Nothing "HomeScreen"
            let callback = runFn2 EHC.getMarkerCallback push MarkerLabelOnClick
                sourceAddress = if state.props.isSpecialZone && not (DS.null state.props.defaultPickUpPoint)
                                  then state.props.defaultPickUpPoint
                                  else state.data.source
                sourceMarkerConfig = JB.defaultMarkerConfig{ markerId = srcMarker, pointerIcon = srcMarker, shortTitle = (runFn3 splitString sourceAddress "," 2), primaryText = sourceAddress, secondaryText = fromMaybe "" state.props.locateOnMapProps.sourceLocationName, labelImage = defaultMarkerImageConfig{image = sourceSpecialTagIcon}, position{ lat = if isRoundTrip then state.props.sourceLat else points.source.lat, lng = if isRoundTrip then state.props.sourceLong else points.source.lng }, labelActionImage = defaultMarkerImageConfig{image = "ny_ic_chevron_right_black_2", height = markerArrowSize, width = markerArrowSize}, markerCallback = callback, labelMaxWidth = estimateLabelMaxWidth, labelMaxLines = 2, labelTextSize = 11}
                destMarkerConfig = JB.defaultMarkerConfig{ markerId = destMarker, pointerIcon = destMarker, shortTitle = (runFn3 splitString points.destination.place "," 2), primaryText = points.destination.place, labelImage = defaultMarkerImageConfig{image = destSpecialTagIcon}, position{ lat = points.destination.lat, lng = points.destination.lng }, labelActionImage = defaultMarkerImageConfig{image = "ny_ic_chevron_right_black_2", height = markerArrowSize, width = markerArrowSize}, markerCallback = callback, labelMaxWidth = estimateLabelMaxWidth, labelMaxLines = 2, labelTextSize = 11, anchorV = 1.0}
            lift $ lift $ liftFlow $ updateMarker sourceMarkerConfig
            lift $ lift $ liftFlow $ updateMarker destMarkerConfig
          _ -> pure unit

        homeScreenFlow
    GET_SELECT_LIST state -> do
      when (isLocalStageOn QuoteList)
        $ do
            updateFlowStatus SEARCH_CANCELLED
      homeScreenFlow
    CONFIRM_RIDE state -> do
      pure $ enableMyLocation false
      let
        selectedQuote = getSelectedQuote state

        currentStage = getCurrentStage state
      logStatus "confirming_quote" selectedQuote
      case selectedQuote of
        Just quote -> do
          updateLocalStage currentStage
          handleRideConfirmation quote currentStage state
        Nothing -> homeScreenFlow
      where
      getSelectedQuote :: HomeScreenState -> Maybe String
      getSelectedQuote state =
        if state.data.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE then
          state.data.specialZoneSelectedQuote
        else if state.data.fareProductType == FPT.INTER_CITY then
          state.data.selectedQuoteId
        else
          state.props.selectedQuote

      getCurrentStage :: HomeScreenState -> Stage
      getCurrentStage state = if any (_ == state.data.fareProductType) [ FPT.INTER_CITY, FPT.RENTAL ] then ConfirmingQuotes else ConfirmingRide

      handleRideConfirmation :: String -> Stage -> HomeScreenState -> FlowBT String Unit
      handleRideConfirmation quote currentStage state = do
        response <- lift $ lift $ Remote.rideConfirm quote
        case response of
          Right (ConfirmRes resp) -> do
            let
              bookingId = resp.bookingId
            if currentStage == ConfirmingQuotes then
              handleConfirmingQuotes bookingId state
            else
              handleConfirmingRide bookingId currentStage
          Left err -> do
            if ((decodeError err.response.errorMessage "errorCode") == "INVALID_REQUEST" && (decodeError err.response.errorMessage "errorMessage") == "ACTIVE_BOOKING_PRESENT") then do
              void $ lift $ lift $ showToast "Active Booking Present"
              updateLocalStage HomeScreen
              updateUserInfoToState state
              homeScreenFlow
            else if not (err.code == 400 && (decodeError err.response.errorMessage "errorCode") == "QUOTE_EXPIRED") then do
              void $ lift $ lift $ showToast (getString STR.ERROR_OCCURED_TRY_AGAIN)
              let _ = runFn2 EHC.updatePushInIdMap "EstimatePolling" true
              rideSearchRequestFlow state
            else
              pure unit
            setValueToLocalStore AUTO_SELECTING "false"
            updateLocalStage QuoteList
            modifyScreenState
              $ HomeScreenStateType
                  ( \homeScreen ->
                      homeScreen
                        { props
                          { currentStage = QuoteList
                          , selectedQuote = Nothing
                          }
                        , data
                          { quoteListModelState = []
                          }
                        }
                  )
            homeScreenFlow 
      handleConfirmingQuotes :: String -> HomeScreenState -> FlowBT String Unit
      handleConfirmingQuotes bookingId state = do
        let
          diffInSeconds = EHC.compareUTCDate (if DS.null state.data.startTimeUTC then (getCurrentUTC "") else state.data.startTimeUTC) (getCurrentUTC "")

          isNow = diffInSeconds < 60 * 30
        if isNow then
          enterRentalRideSearchFlow bookingId
        else do
          modifyScreenState $ RideScheduledScreenStateType (\rideScheduledScreen -> rideScheduledScreen{ data { bookingId = bookingId } })
          updateScheduledRides true true
          -- setValueToLocalStore BOOKING_TIME_LIST $ encodeBookingTimeList $ Arr.sortWith (_.rideStartTime) $ (decodeBookingTimeList FunctionCall) <> [ { bookingId: bookingId, rideStartTime: state.data.startTimeUTC, estimatedDuration: state.data.maxEstimatedDuration } ]
          rideScheduledFlow

      handleConfirmingRide :: String -> Stage -> FlowBT String Unit
      handleConfirmingRide bookingId currentStage = do
        modifyScreenState
          $ HomeScreenStateType
              ( \homeScreen ->
                  homeScreen
                    { props
                      { currentStage = currentStage
                      , bookingId = bookingId
                      , isPopUp = NoPopUp
                      }
                    }
              )
        homeScreenFlow
    ONGOING_RIDE state -> do
      setValueToLocalStore TRACKING_ENABLED "True"
      setValueToLocalStore TRACKING_DRIVER "False"
      setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_DRIVER_ARRIVAL"
      let
        srcLat = state.data.driverInfoCardState.sourceLat

        srcLon = state.data.driverInfoCardState.sourceLng

        dstLat = state.data.driverInfoCardState.destinationLat

        dstLon = state.data.driverInfoCardState.destinationLng
      updateLocalStage state.props.currentStage
      if spy "ONGOING_RIDEONGOING_RIDE CURRENT" state.props.currentStage == RideCompleted then do
        let
          sourceSpecialTagIcon = zoneLabelIcon state.props.zoneType.sourceTag

          destSpecialTagIcon = zoneLabelIcon state.props.zoneType.destinationTag

          markers = normalRoute ""

          srcMarkerConfig = defaultMarkerConfig { markerId = markers.srcMarker, pointerIcon = markers.srcMarker }

          destMarkerConfig = defaultMarkerConfig { markerId = markers.destMarker, pointerIcon = markers.destMarker }
        void $ Remote.drawMapRoute srcLat srcLon dstLat dstLon srcMarkerConfig destMarkerConfig "DRIVER_LOCATION_UPDATE" Nothing "pickup" (specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon true getPolylineAnimationConfig)
        homeScreenFlow
      else if state.props.currentStage == HomeScreen then do
        void $ pure $ removeAllPolylines ""
        void $ pure $ spy "INSIDE ELSE IF OF ONGOING" state.props.currentStage
        void $ updateLocalStage HomeScreen
        updateUserInfoToState state
        homeScreenFlow
      else do
        lift $ lift $ triggerRideStatusEvent "DRIVER_ASSIGNMENT" Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
        let voipConfig = getCustomerVoipConfig $ DS.toLower $ getValueToLocalStore CUSTOMER_LOCATION
        if (voipConfig.customer.enableVoipFeature) then do 
          void $ pure $ JB.initSignedCall state.data.driverInfoCardState.rideId false
        else pure unit
        homeScreenFlow
    CANCEL_RIDE_REQUEST state cancelType -> do
      let
        cancelReasonCode = if cancelType == NORMAL_RIDE_CANCEL then state.props.cancelReasonCode else "Cancelling Rentals Quotes Search"

        cancelDescription = if cancelType == NORMAL_RIDE_CANCEL then state.props.cancelDescription else "Rental reallocation"
      response <- lift $ lift $ Remote.cancelRide (Remote.makeCancelRequest cancelDescription cancelReasonCode ) (state.props.bookingId)
      case response of
        Right _ -> do
          void $ pure $ currentPosition ""
          void $ updateLocalStage HomeScreen
          liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_rider_cancellation"
            $ [ { key: "Reason code", value: unsafeToForeign cancelReasonCode }
              , { key: "Additional info", value: unsafeToForeign cancelDescription }
              , { key: "Pickup", value: unsafeToForeign state.data.driverInfoCardState.source }
              , { key: "Estimated Ride Distance", value: unsafeToForeign state.data.rideDistance }
              , { key: "Night Ride", value: unsafeToForeign state.data.rateCard.isNightShift }
              , { key: "BookingId", value: unsafeToForeign state.props.bookingId }
              ]
          updateScheduledRides true true
          -- setValueToLocalStore BOOKING_TIME_LIST $ encodeBookingTimeList $ filter (\item -> item.bookingId /= state.props.bookingId) $ decodeBookingTimeList FunctionCall
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { autoScroll = false, isCancelRide = false, currentStage = HomeScreen, rideRequestFlow = false, isSearchLocation = NoView } })
          lift $ lift $ triggerRideStatusEvent "CANCELLED_PRODUCT" Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
          void $ pure $ clearTimerWithId <$> state.props.waitingTimeTimerIds
          void $ pure $ JB.destroySignedCall unit
          liftFlowBT $ logEvent logField_ "ny_user_ride_cancelled_by_user"
          liftFlowBT $ logEvent logField_ $ "ny_user_cancellation_reason: " <> state.props.cancelReasonCode
          removeChatService ""
          updateUserInfoToState state
        Left err -> do
          void $ void $ lift $ lift $ showToast $  getString STR.UNABLE_TO_CANCEL_RIDE
      when (HU.isParentView FunctionCall) $ pure $ HU.terminateApp state.props.currentStage true
      homeScreenFlow
    FCM_NOTIFICATION notification notificationBody state -> fcmHandler notification state notificationBody
    LOGOUT -> do
      (APISuccessResp resp) <- Remote.logOutBT LogOutReq
      removeChatService ""
      void $ pure $ deleteValueFromLocalStore REGISTERATION_TOKEN
      void $ pure $ deleteValueFromLocalStore REGISTRATION_APPROVED
      void $ pure $ deleteValueFromLocalStore CUSTOMER_ID
      void $ pure $ deleteValueFromLocalStore CONTACTS
      void $ pure $ deleteValueFromLocalStore USER_EMAIL
      void $ pure $ deleteValueFromLocalStore CUSTOMER_FIRST_RIDE
      void $ pure $ deleteValueFromLocalStore BOOKING_TIME_LIST
      void $ pure $ deleteValueFromLocalStore DISABILITY_NAME
      deleteValueFromLocalStore INTERCITY_BUS_PHONE_NUMBER_PERMISSION
      void $ pure $ factoryResetApp ""
      void $ pure $ clearCache ""
      void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_logout"
      void $ pure $ (setText (getNewIDWithTag "EnterMobileNumberEditText") "")
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> EnterMobileNumberScreenData.initData)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
      enterMobileNumberScreenFlow -- Removed choose langauge screen
    REFRESH_HOME_SCREEN -> homeScreenFlow
    CONFIRM_EDITED_PICKUP state -> do
      let srcAddress = SearchReqLocation { gps : state.props.editedPickUpLocation.gps , address : (LocationAddress state.props.editedPickUpLocation.address)}
      (res :: (Either ErrorResponse EditLocationRes)) <- lift $ lift $ HelpersAPI.callApi $ Remote.makeEditLocationRequest state.data.driverInfoCardState.rideId (Just srcAddress) Nothing
      case res of
        Right _  -> pure unit
        Left err -> do
          void $ void $ lift $ lift $ showToast $  getString STR.DRIVER_ALMOST_AT_PICKUP
          pure unit
      updateLocalStage RideAccepted
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = RideAccepted, locateOnMap = false}})
      setValueToLocalStore TRACKING_DRIVER "False"
      checkRideStatus true false
      homeScreenFlow
    UPDATE_CHAT -> do
      removeChatService ""
      homeScreenFlow
    RELOAD saveToCurrLocs -> do
      (GlobalState state) <- getState
      void $ liftFlowBT $ setMapPadding 0 0 0 0
      if state.homeScreen.props.currentStage == SearchLocationModel || state.homeScreen.props.currentStage == EditingDestinationLoc then do
        if (saveToCurrLocs && state.homeScreen.props.storeCurrentLocs) then addLocToCurrLoc state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong state.homeScreen.data.source else pure unit
        void $ pure $ toggleBtnLoader "" false
        homeScreenFlow
      else do
        if state.homeScreen.props.sourceLat /= 0.0 && state.homeScreen.props.sourceLong /= 0.0 then do
          (ServiceabilityRes sourceServiceabilityResp) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong) ORIGIN
          let
            (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
          let
            pickUpPoints =
              if null srcSpecialLocation.gatesInfo then
                filterHotSpots state.homeScreen sourceServiceabilityResp.hotSpotInfo state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong
              else
                mapSpecialZoneGates srcSpecialLocation.gatesInfo
          if (sourceServiceabilityResp.serviceable) then do
            let
              cityName = getCityNameFromCode sourceServiceabilityResp.city

              geoJson = transformGeoJsonFeature srcSpecialLocation.geoJson srcSpecialLocation.gatesInfo
            (GlobalState currentState) <- getState
            fullAddress <- getPlaceName currentState.homeScreen.props.sourceLat currentState.homeScreen.props.sourceLong HomeScreenData.dummyLocation false
            case fullAddress of
              Just (PlaceName address) -> do
                modifyScreenState
                  $ HomeScreenStateType
                      ( \homeScreen ->
                          homeScreen
                            { data
                              { source = address.formattedAddress
                              , sourceAddress = encodeAddress address.formattedAddress [] Nothing currentState.homeScreen.props.sourceLat currentState.homeScreen.props.sourceLong
                              }
                            , props
                              { isSrcServiceable = true
                              }
                            }
                      )
              Nothing -> pure unit
            void $ pure $ setCleverTapUserProp [ { key: "Customer Location", value: unsafeToForeign $ show cityName } ]
            setUserCity CUSTOMER_LOCATION $ show cityName
            modifyScreenState
              $ HomeScreenStateType
                  ( \homeScreen ->
                      homeScreen
                        { data
                          { polygonCoordinates = geoJson
                          , nearByPickUpPoints = pickUpPoints
                          }
                        , props
                          { isSrcServiceable = true
                          , showlocUnserviceablePopUp = false
                          , city = cityName
                          , locateOnMapProps { sourceLocationName = Just srcSpecialLocation.locationName }
                          , showShimmer = false
                          }
                        }
                  )
            if (saveToCurrLocs && state.homeScreen.props.storeCurrentLocs) then
              addLocToCurrLoc state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong state.homeScreen.data.source
            else
              pure unit
          else do
            let
              isWhitelisted = any (_ == getValueFromWindow (show MOBILE_NUMBER)) (getNumbersToWhiteList "")
            void $ pure $ firebaseLogEvent "ny_loc_unserviceable"
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { isSrcServiceable = isWhitelisted, showlocUnserviceablePopUp = (not isWhitelisted), showShimmer = false } })
        else do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { isSrcServiceable = true, showlocUnserviceablePopUp = false, showShimmer = false } })
      homeScreenFlow
    RETRY -> homeScreenFlow
    EDIT_LOCATION_FLOW finalState -> do
      pure $ removeAllPolylines ""
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq finalState.data.driverInfoCardState.initialPickupLat finalState.data.driverInfoCardState.initialPickupLon) ORIGIN
      let isServiceable = sourceServiceabilityResp.serviceable
          (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
          pickUpPoints = mapSpecialZoneGates srcSpecialLocation.gatesInfo
          geoJson = transformGeoJsonFeature srcSpecialLocation.geoJson srcSpecialLocation.gatesInfo
          gateAddress = (fromMaybe HomeScreenData.dummyLocation ((filter( \ (item) -> (item.place == finalState.props.defaultPickUpPoint)) pickUpPoints) !! 0))
      liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = false, lat = finalState.data.driverInfoCardState.initialPickupLat, lon = finalState.data.driverInfoCardState.initialPickupLon , geoJson = geoJson, points = pickUpPoints, zoomLevel = zoomLevel, labelId = getNewIDWithTag "LocateOnMapPin", editPickUpThreshold = finalState.data.config.mapConfig.locateOnMapConfig.editPickUpThreshold, editPickupLocation = true, circleConfig = editPickupCircleConfig }
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{nearByPickUpPoints = pickUpPoints, polygonCoordinates = geoJson, source = finalState.data.driverInfoCardState.source },props{currentStage = EditPickUpLocation,rideRequestFlow = true, locateOnMapLocation{sourceLat = finalState.props.sourceLat, sourceLng = finalState.props.sourceLong, source = finalState.data.source, sourceAddress = finalState.data.sourceAddress}}})
      void $ pure $ updateLocalStage EditPickUpLocation
      homeScreenFlow
    REALLOCATE_RIDE state -> do
      if DS.null state.props.estimateId then
        currentFlowStatus false
      else
        homeScreenFlow
    CHECK_SERVICEABILITY updatedState lat long -> do
      let
        isWhitelisted = any (_ == getValueFromWindow (show MOBILE_NUMBER)) (getNumbersToWhiteList "")
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq lat long) ORIGIN
      let
        (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation sourceServiceabilityResp.specialLocation
      let
        sourceLat = if sourceServiceabilityResp.serviceable then lat else updatedState.props.sourceLat

        sourceLong = if sourceServiceabilityResp.serviceable then long else updatedState.props.sourceLong

        cityName = if sourceServiceabilityResp.serviceable then getCityNameFromCode sourceServiceabilityResp.city else updatedState.props.city

        srcServiceability = isWhitelisted || sourceServiceabilityResp.serviceable
      sourcePlaceName <- getPlaceName sourceLat sourceLong HomeScreenData.dummyLocation false
      setUserCity CUSTOMER_LOCATION (show cityName)
      void $ pure $ firebaseLogEvent $ "ny_loc_unserviceable_" <> show (not sourceServiceabilityResp.serviceable)
      when (updatedState.props.currentStage == ConfirmingLocation) $ do
        checkForSpecialZoneAndHotSpots updatedState (ServiceabilityRes sourceServiceabilityResp) lat long
      modifyScreenState
        $ HomeScreenStateType
            ( \homeScreen ->
                homeScreen
                  { props
                    { locateOnMapLocation
                      { sourceLat = sourceLat
                      , sourceLng = sourceLong
                      , source =
                        case sourcePlaceName of
                          Just (PlaceName placeDetails) -> placeDetails.formattedAddress
                          Nothing -> getString STR.CURRENT_LOCATION
                      }
                    , sourceLat = sourceLat
                    , sourceLong = sourceLong
                    , isSrcServiceable = srcServiceability
                    , showlocUnserviceablePopUp = not srcServiceability
                    , city = cityName
                    , showShimmer = false
                    }
                  }
            )
      homeScreenFlow
    HOME_SCREEN -> do
      (GlobalState state) <- getState
      when (isLocalStageOn FindingQuotes)
        $ do
            cancelEstimate state.homeScreen.props.estimateId
      let markerName = getCurrentLocationMarker $ getValueToLocalStore VERSION_NAME
          markerConfig = defaultMarkerConfig{ markerId = markerName, pointerIcon = markerName }
      void $ pure $ removeAllPolylines ""
      void $ lift $ lift $ liftFlow $ showMarker markerConfig 9.9 9.9 160 0.5 0.9 (getNewIDWithTag "CustomerHomeScreen")
      void $ pure $ currentPosition ""
      void $ updateLocalStage HomeScreen
      updateUserInfoToState state.homeScreen
      homeScreenFlow
    CHECK_CURRENT_STATUS -> do
      (GlobalState state) <- getState
      when (isLocalStageOn FindingQuotes)
        $ do
            cancelEstimate state.homeScreen.props.estimateId
      let markerName = getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)
          markerConfig = defaultMarkerConfig{ markerId = markerName, pointerIcon = markerName }
      void $ pure $ removeAllPolylines ""
      void $ lift $ lift $ liftFlow $ showMarker markerConfig 9.9 9.9 160 0.5 0.9 (getNewIDWithTag "CustomerHomeScreen")
      void $ pure $ currentPosition ""
      if state.homeScreen.props.estimateId /= "" then cancelEstimate state.homeScreen.props.estimateId else pure unit
      void $ updateLocalStage HomeScreen
      updateUserInfoToState state.homeScreen
      currentFlowStatus false
    UPDATE_LOCATION_NAME state lat lon -> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq lat lon) ORIGIN
      let
        srcServiceable = sourceServiceabilityResp.serviceable

        (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)

        cityName = getCityNameFromCode sourceServiceabilityResp.city

        pickUpPoints = mapSpecialZoneGates srcSpecialLocation.gatesInfo

        gateAddress =
          if DS.null state.props.defaultPickUpPoint then
            HomeScreenData.dummyLocation
          else
            fromMaybe HomeScreenData.dummyLocation (Arr.find (\pickupPoint -> pickupPoint.place == state.props.defaultPickUpPoint) pickUpPoints)
      setUserCity CUSTOMER_LOCATION $ show cityName
      checkForSpecialZoneAndHotSpots state (ServiceabilityRes sourceServiceabilityResp) lat lon
      let
        cachedLat = (if state.props.isSource == Just true then state.props.locateOnMapLocation.sourceLat else state.props.locateOnMapLocation.destinationLat)

        cachedLon = (if state.props.isSource == Just true then state.props.locateOnMapLocation.sourceLng else state.props.locateOnMapLocation.destinationLng)

        cachedLocation = (if state.props.isSource == Just true then state.props.locateOnMapLocation.source else state.props.locateOnMapLocation.destination)

        distanceBetweenLatLong = getDistanceBwCordinates lat lon cachedLat cachedLon

        isMoreThan20Meters = distanceBetweenLatLong > (state.data.config.mapConfig.locateOnMapConfig.apiTriggerRadius / 1000.0)
      modifyScreenState
        $ HomeScreenStateType
            ( \homeScreen ->
                homeScreen
                  { props
                    { city = if state.props.isSource == Just true then cityName else AnyCity
                    , sourcePlaceId = if state.props.isSource == Just true then Nothing else homeScreen.props.sourcePlaceId
                    , destinationPlaceId = if state.props.isSource == Just false then Nothing else homeScreen.props.destinationPlaceId
                    , destinationLat = if state.props.isSource == Just false && state.props.currentStage /= ConfirmingLocation then lat else state.props.destinationLat
                    , destinationLong = if state.props.isSource == Just false && state.props.currentStage /= ConfirmingLocation then lon else state.props.destinationLong
                    , sourceLat = if state.props.isSource == Just true then lat else state.props.sourceLat
                    , sourceLong = if state.props.isSource == Just true then lon else state.props.sourceLong
                    , confirmLocationCategory = getZoneType srcSpecialLocation.category
                    }
                  }
            )
      (GlobalState globalState) <- getState
      let
        state = globalState.homeScreen
      if isMoreThan20Meters || cachedLocation == "" || (state.props.isSource == Just true && isJust gateAddress.address) then do
        fullAddress <- getPlaceName lat lon gateAddress true
        case fullAddress of
          Just (PlaceName placeDetails) -> do
            let
              currentLocationItem = getCurrentLocationItem placeDetails state lat lon
            void $ liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_placename_api_lom_onDrag" [{key: "FareProductType" , value: (unsafeToForeign (show state.data.fareProductType))}]
            modifyScreenState
              $ HomeScreenStateType
                  ( \homeScreen ->
                      homeScreen
                        { data
                          { destination = if state.props.isSource == Just false && state.props.currentStage /= ConfirmingLocation then placeDetails.formattedAddress else homeScreen.data.destination
                          , selectedLocationListItem = currentLocationItem
                          , source = if state.props.isSource == Just true then placeDetails.formattedAddress else homeScreen.data.source
                          , sourceAddress =
                            case state.props.isSource, (state.props.currentStage /= ConfirmingLocation) of
                              Just true, true -> encodeAddress placeDetails.formattedAddress placeDetails.addressComponents Nothing lat lon
                              _, _ -> encodeAddress homeScreen.data.source [] state.props.sourcePlaceId lat lon
                          , destinationAddress =
                            case state.props.isSource, (state.props.currentStage /= ConfirmingLocation) of
                              Just false, true -> encodeAddress placeDetails.formattedAddress placeDetails.addressComponents Nothing lat lon
                              _, _ -> encodeAddress homeScreen.data.destination [] state.props.destinationPlaceId lat lon
                          }
                        }
                  )
          Nothing -> void $ void $ lift $ lift $ showToast $  getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
      else do
        void $ liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_placename_cache_lom_onDrag" [{key: "FareProductType" , value: (unsafeToForeign (show state.data.fareProductType))}]
        modifyScreenState
          $ HomeScreenStateType
              ( \homeScreen ->
                  homeScreen
                    { data
                      { destination = if state.props.isSource == Just false && state.props.currentStage /= ConfirmingLocation then state.props.locateOnMapLocation.destination else homeScreen.data.destination
                      , source = if state.props.isSource == Just true then state.props.locateOnMapLocation.source else homeScreen.data.source
                      , sourceAddress =
                        case state.props.isSource, (state.props.currentStage /= ConfirmingLocation) of
                          Just true, true -> state.props.locateOnMapLocation.sourceAddress
                          _, _ -> state.data.sourceAddress
                      , destinationAddress =
                        case state.props.isSource, (state.props.currentStage /= ConfirmingLocation) of
                          Just false, true -> state.props.locateOnMapLocation.destinationAddress
                          _, _ -> state.data.destinationAddress
                      }
                    }
              )
      let
        _ = spy "UPDATE_LOCATION_NAME" "UPDATE_LOCATION_NAME"
      homeScreenFlow
    UPDATE_PICKUP_NAME state lat lon -> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq lat lon) ORIGIN
      let
        srcServiceable = sourceServiceabilityResp.serviceable

        (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)

        pickUpPoints = mapSpecialZoneGates srcSpecialLocation.gatesInfo

        cityName = getCityNameFromCode sourceServiceabilityResp.city

        gateAddress =
          if DS.null state.props.defaultPickUpPoint then
            HomeScreenData.dummyLocation
          else
            fromMaybe HomeScreenData.dummyLocation (Arr.find (\pickupPoint -> pickupPoint.place == state.props.defaultPickUpPoint) pickUpPoints)
      setUserCity CUSTOMER_LOCATION $ show cityName
      checkForSpecialZoneAndHotSpots state (ServiceabilityRes sourceServiceabilityResp) lat lon
      let
        distanceBetweenLatLong = getDistanceBwCordinates lat lon state.props.locateOnMapLocation.sourceLat state.props.locateOnMapLocation.sourceLng

        isMoreThan20Meters = distanceBetweenLatLong > (state.data.config.mapConfig.locateOnMapConfig.apiTriggerRadius / 1000.0)
      modifyScreenState $ HomeScreenStateType ( \homeScreen -> updateLatLon lat lon srcSpecialLocation cityName homeScreen)
      if isMoreThan20Meters || isJust gateAddress.address || DS.null state.props.locateOnMapLocation.source || state.data.fareProductType == FPT.DELIVERY then do
        fullAddress <- getPlaceName lat lon gateAddress true
        case fullAddress of
          Just (PlaceName address) -> do
            void $ liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_placename_api_cpu_onDrag" [{key: "isSource", value: (unsafeToForeign (show state.props.isSource))},
                                                                                                         {key: "FareProductType" , value: (unsafeToForeign (show state.data.fareProductType))}]
            modifyScreenState $ HomeScreenStateType( \homeScreen -> updateAddress state address lat lon homeScreen)
          Nothing -> void $ void $ lift $ lift $ showToast $  getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
      else do
        void $ liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_placename_cache_cpu_onDrag" [{key: "isSource", value: (unsafeToForeign (show state.props.isSource))},
                                                                                                       {key: "FareProductType" , value: (unsafeToForeign (show state.data.fareProductType))}  ]
        modifyScreenState
          $ HomeScreenStateType
              ( \homeScreen ->
                  homeScreen
                    { data
                      { source = state.props.locateOnMapLocation.source
                      , sourceAddress = state.props.locateOnMapLocation.sourceAddress
                      }
                    , props
                      { editedPickUpLocation {address = state.props.locateOnMapLocation.sourceAddress }
                      }
                    }
              )
      let
        _ = spy "UPDATE_PICKUP_LOCATION_NAME" "UPDATE_PICKUP_LOCATION_NAME"
      homeScreenFlow
      where
        updateLatLon lat lon srcSpecialLocation cityName homeScreen = do
          homeScreen
            { props
              { sourceLat = if state.props.isSource == Just true then lat else state.props.sourceLat
              , sourceLong = if state.props.isSource == Just true then  lon else state.props.sourceLong
              , destinationLat = if state.props.isSource == Just false && state.data.fareProductType == FPT.DELIVERY then lat else state.props.destinationLat
              , destinationLong = if state.props.isSource == Just false && state.data.fareProductType == FPT.DELIVERY then lon else state.props.destinationLong
              , editedPickUpLocation {gps = LatLong {lat : lat
                                              , lon : lon
                                              }
                            }
              , confirmLocationCategory = getZoneType srcSpecialLocation.category
              , city = cityName
              }
            }
        updateAddress state address lat lon homeScreen =
          homeScreen
            { data
              { source =  if state.props.isSource == Just true then address.formattedAddress else state.data.source
              , sourceAddress = if state.props.isSource == Just true then encodeAddress address.formattedAddress address.addressComponents Nothing lat lon else state.data.sourceAddress
              , destination = if state.props.isSource == Just false && state.data.fareProductType == FPT.DELIVERY then address.formattedAddress else state.data.destination 
              , destinationAddress = if state.props.isSource == Just false && state.data.fareProductType == FPT.DELIVERY then encodeAddress address.formattedAddress address.addressComponents Nothing lat lon else state.data.destinationAddress
              }
            , props
              { editedPickUpLocation {address = encodeAddress address.formattedAddress address.addressComponents Nothing lat lon }
              }
            }

    GO_TO_FAVOURITES_ -> do
      void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_addresses"
      savedLocationFlow HomeScreenFlow
    OPEN_GOOGLE_MAPS state -> do
      void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_ride_track_gmaps"
      (GetDriverLocationResp resp) <- Remote.getDriverLocationBT (state.data.driverInfoCardState.rideId)
      let
        sourceLat = (resp ^. _lat)

        sourceLng = (resp ^. _lon)

        destLat = if state.props.currentStage == RideAccepted then state.data.driverInfoCardState.sourceLat else state.data.driverInfoCardState.destinationLat

        destLng = if state.props.currentStage == RideAccepted then state.data.driverInfoCardState.sourceLng else state.data.driverInfoCardState.destinationLng
      void $ pure $ openNavigation destLat destLng "DRIVE"
      homeScreenFlow
    IN_APP_TRACK_STATUS state -> do
      case state.props.currentStage of
        RideAccepted -> do
          void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_pickup_track_inapp"
          pure unit
        RideStarted -> do
          void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_ride_track_inapp"
          pure unit
        _ -> pure unit
      if (spy "driver current Stage " isLocalStageOn RideAccepted) || (spy "driver current Stage " isLocalStageOn RideStarted) then do
        setValueToLocalStore TRACKING_DRIVER "False"
        if not state.props.isInApp then do
          setValueToLocalStore TRACKING_ENABLED "False"
          homeScreenFlow
        else do
          setValueToLocalStore TRACKING_ENABLED "True"
          homeScreenFlow
      else
        homeScreenFlow
    UPDATE_SAVED_LOCATION -> do
      (SavedLocationsListRes savedLocationResp) <- FlowCache.updateAndFetchSavedLocations false
      updateSourceLocation ""
      updateCurrentLocation ""
      fetchAndModifyLocationLists $ AddNewAddress.getSavedLocations savedLocationResp.list
      homeScreenFlow
    GO_TO_INVOICE_ updatedState -> do
      let
        prevRideState = updatedState.data.rideRatingState
      let
        finalAmount = show prevRideState.finalAmount
      modifyScreenState $ InvoiceScreenStateType (\invoiceScreen -> invoiceScreen { props { fromHomeScreen = true }, data { totalAmount = ((getCurrency appConfig) <> " " <> finalAmount), date = prevRideState.dateDDMMYY, tripCharges = ((getCurrency appConfig) <> " " <> finalAmount), selectedItem { date = prevRideState.dateDDMMYY, bookingId = prevRideState.bookingId, rideStartTime = prevRideState.rideStartTime, rideEndTime = prevRideState.rideEndTime, rideId = prevRideState.rideId, shortRideId = prevRideState.shortRideId, vehicleNumber = prevRideState.vehicleNumber, time = prevRideState.rideStartTime, source = prevRideState.source, destination = prevRideState.destination, driverName = prevRideState.driverName, totalAmount = ((getCurrency appConfig) <> " " <> finalAmount) }, config = updatedState.data.config } })
      invoiceScreenFlow
    CHECK_FOR_DUPLICATE_SAVED_LOCATION state -> do
      let
        recents =
          map
            ( \item -> item { postfixImageVisibility = false, postfixImageUrl = "" }
            )
            (differenceOfLocationLists (state.data.recentSearchs.predictionArray) state.data.savedLocations)
      case state.data.selectedLocationListItem of
        Nothing -> do
          modifyScreenState
            $ AddNewAddressScreenStateType
                ( \addNewAddressScreen ->
                    addNewAddressScreen
                      { props
                        { showSavePlaceView = false
                        , fromHome = state.props.currentStage == HomeScreen
                        , fromScreen = Screen.getScreen Screen.HOME_SCREEN
                        , editLocation = false
                        , editSavedLocation = false
                        , isLocateOnMap = false
                        , isBtnActive = false
                        , isSearchedLocationServiceable = true
                        , tagExists = false
                        , placeNameExists = false
                        }
                      , data
                        { addressSavedAs = ""
                        , placeName = ""
                        , savedLocations = state.data.savedLocations
                        , locationList = recents
                        , recentSearchs { predictionArray = recents }
                        , selectedTag = state.props.tagType
                        , savedTags = AddNewAddress.getSavedTagsFromHome state.data.savedLocations
                        , address = ""
                        , activeIndex =
                          case state.props.tagType of
                            Just tag -> case tag of
                              HOME_TAG -> Just 0
                              WORK_TAG -> Just 1
                              _ -> Just 2
                            Nothing -> Nothing
                        }
                      }
                )
          addNewAddressScreenFlow
        Just selectedLocationListItem -> do
          case selectedLocationListItem.locationItemType of
            Just RECENTS -> getDistanceDiff state (fromMaybe 0.0 selectedLocationListItem.lat) (fromMaybe 0.0 selectedLocationListItem.lon)
            Nothing -> getDistanceDiff state (fromMaybe 0.0 selectedLocationListItem.lat) (fromMaybe 0.0 selectedLocationListItem.lon)
            _ -> do
              (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (selectedLocationListItem.title <> ", " <> selectedLocationListItem.subTitle) selectedLocationListItem.placeId (fromMaybe 0.0 selectedLocationListItem.lat) (fromMaybe 0.0 selectedLocationListItem.lon) selectedLocationListItem
              let
                (PlaceName placeName) = (fromMaybe HomeScreenData.dummyLocationName (placeNameResp !! 0))
              let
                (LatLong placeLatLong) = (placeName.location)
              (ServiceabilityRes serviceabilityRes) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq (placeLatLong.lat) (placeLatLong.lon)) DESTINATION
              case (serviceabilityRes.serviceable) of
                false -> do
                  void $ lift $ lift $ showToast ("Location Unserviceable")
                  homeScreenFlow
                _ -> modifyScreenState $ HomeScreenStateType (\homeScreen -> state { data { selectedLocationListItem = Just selectedLocationListItem { lat = Just (placeLatLong.lat), lon = Just (placeLatLong.lon) } } })
              getDistanceDiff state { data { saveFavouriteCard { selectedItem { lat = Just (placeLatLong.lat), lon = Just (placeLatLong.lon) } }, selectedLocationListItem = Just selectedLocationListItem { lat = Just (placeLatLong.lat), lon = Just (placeLatLong.lon) } } } (placeLatLong.lat) (placeLatLong.lon)
    GO_TO_CALL_EMERGENCY_CONTACT state -> do
      (UserSosRes res) <- Remote.userSosBT (Remote.makeUserSosReq (Remote.createUserSosFlow "EmergencyContact" state.props.emergencyHelpModelState.currentlySelectedContact.phoneNo) state.data.driverInfoCardState.rideId false false Nothing Nothing)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> state { props { emergencyHelpModelState { sosId = res.sosId } } })
      homeScreenFlow
    GO_TO_CALL_POLICE state -> do
      (UserSosRes res) <- Remote.userSosBT (Remote.makeUserSosReq (Remote.createUserSosFlow "Police" "") state.data.driverInfoCardState.rideId false false Nothing Nothing)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> state { props { emergencyHelpModelState { sosId = res.sosId } } })
      homeScreenFlow
    GO_TO_CALL_SUPPORT state -> do
      (UserSosRes res) <- Remote.userSosBT (Remote.makeUserSosReq (Remote.createUserSosFlow "CustomerCare" "") state.data.driverInfoCardState.rideId false false Nothing Nothing)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> state { props { emergencyHelpModelState { sosId = res.sosId } } })
      homeScreenFlow
    GO_TO_SOS_STATUS state -> do
      res <- Remote.userSosStatusBT state.props.emergencyHelpModelState.sosId (Remote.makeSosStatus state.props.emergencyHelpModelState.sosStatus "")
      homeScreenFlow
    GO_TO_FETCH_CONTACTS state -> do
      (GetEmergContactsResp res) <- Remote.getEmergencyContactsBT GetEmergContactsReq
      let
        contacts =
          map
            ( \(ContactDetails item) ->
                { number: item.mobileNumber
                , name: item.name
                , isSelected: true
                , priority: fromMaybe 1 item.priority
                , enableForFollowing: fromMaybe false item.enableForFollowing
                , enableForShareRide: fromMaybe false item.enableForShareRide
                , shareTripWithEmergencyContactOption : EmergencyContactsScreenData.getRideOptionFromKeyEM $ fromMaybe API.NEVER_SHARE item.shareTripWithEmergencyContactOption
                , onRide: fromMaybe false item.onRide
                , contactPersonId : item.contactPersonId
                , isFollowing : Nothing
                , notifiedViaFCM : item.notifiedViaFCM
                }
            )
            res.defaultEmergencyNumbers
      let
        newContacts = transformContactList contacts
      modifyScreenState $ HomeScreenStateType (\homeScreen -> state { props { emergencyHelpModelState { emergencyContactData = newContacts } } })
      homeScreenFlow
    SAVE_FAVOURITE state -> do
      let
        tag = case (toLower state.data.saveFavouriteCard.tag) of
          "work" -> "Work"
          "home" -> "Home"
          _ -> state.data.saveFavouriteCard.tag
      void $ setValueToLocalStore RELOAD_SAVED_LOCATION "true"
      case state.data.saveFavouriteCard.selectedItem.lat, state.data.saveFavouriteCard.selectedItem.lon of
        Nothing, Nothing -> fetchLatAndLong state tag
        _, _ -> do
          resp <- Remote.addSavedLocationBT (encodeAddressDescription state.data.saveFavouriteCard.address tag state.data.saveFavouriteCard.selectedItem.placeId state.data.saveFavouriteCard.selectedItem.lat state.data.saveFavouriteCard.selectedItem.lon [])
          pure unit
      void $ lift $ lift $ showToast (getString STR.FAVOURITE_ADDED_SUCCESSFULLY)
      (SavedLocationsListRes savedLocationResp) <- FlowCache.updateAndFetchSavedLocations true
      let
        updatedLocationList = getUpdatedLocationList state.data.locationList state.data.saveFavouriteCard.selectedItem.placeId
      let
        updatedRecents = getUpdatedLocationList state.data.recentSearchs.predictionArray state.data.saveFavouriteCard.selectedItem.placeId
      modifyScreenState $ HomeScreenStateType (\homeScreen -> state { data { locationList = updatedLocationList, recentSearchs { predictionArray = updatedRecents }, savedLocations = (AddNewAddress.getSavedLocations savedLocationResp.list) } })
      when (HU.isParentView FunctionCall) $ pure $ HU.emitTerminateApp Nothing true
      homeScreenFlow
    GO_TO_REFERRAL referralType -> do
      let
        referralCode = getValueToLocalStore CUSTOMER_REFERRAL_CODE
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { referralType = referralType, referralCode = if any (_ == referralCode) [ "__failed", "" ] then "" else referralCode })
      referralScreenFlow
    ON_CALL state callType exophoneNumber -> do
      (APISuccessResp res) <- Remote.onCallBT (Remote.makeOnCallReq state.data.driverInfoCardState.rideId (show callType) exophoneNumber)
      homeScreenFlow
    TRIGGER_PERMISSION_FLOW flowType -> do
      modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen { stage = flowType })
      permissionScreenFlow
    GO_TO_TICKET_BOOKING_FLOW state -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { currentStage = DescriptionStage, previousStage = DescriptionStage } })
      placeListFlow
    REPEAT_RIDE_FLOW_HOME state -> do
      updateRepeatRideDetails state
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq state.sourceLat state.sourceLong) ORIGIN
      let
        cityName = getCityNameFromCode sourceServiceabilityResp.city

        (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)

        geoJson = transformGeoJsonFeature srcSpecialLocation.geoJson srcSpecialLocation.gatesInfo

        pickUpPoints = mapSpecialZoneGates srcSpecialLocation.gatesInfo
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { city = cityName, locateOnMapProps { sourceLocationName = Just srcSpecialLocation.locationName, sourceGeoJson = Just geoJson, sourceGates = Just pickUpPoints }, repeatRideServiceTierName = state.serviceTierNameV2 } })
      setUserCity CUSTOMER_LOCATION $ show cityName
      when (state.isSpecialZone)
        $ do
            modifyScreenState
              $ HomeScreenStateType
                  ( \homeScreen ->
                      homeScreen
                        { props
                          { rideSearchProps { sourceSelectType = ST.REPEAT_RIDE } }
                        , data
                          { polygonCoordinates = geoJson
                          , nearByPickUpPoints = pickUpPoints
                          }
                        }
                  )
      rideSearchFlow "REPEAT_RIDE_FLOW"
    GO_TO_RIDE_SEARCH_FLOW -> pure unit
    -- _ <- lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreen")
    -- rideSearchFlowV2 --
    -- rideSearchFlow "NORMAL_FLOW"
    CONFIRM_RENTAL_RIDE -> do
      (GlobalState state) <- getState
      let
        bookingId = state.rentalScreen.data.bookingId
      enterRentalRideSearchFlow bookingId
    EXIT_TO_TICKETING _ -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData { props { navigateToHome = true } })
      modifyScreenState $ TicketingScreenStateType (\_ -> PlaceListData.initData { props { hideMyTickets = false } })
      placeListFlow
    GO_TO_METRO_BOOKING state -> do
      modifyScreenState $ MetroTicketBookingScreenStateType (\_ -> MetroTicketBookingScreenData.initData {props { busClicked = state.props.busClicked, showShimmer = false , ticketServiceType = state.props.ticketServiceType }})
      metroTicketBookingFlow
    GO_TO_RENTALS_FLOW state -> do
      latestScheduledRides <- FlowCache.fetchAndUpdateScheduledRides true
      modifyScreenState
        $ RentalScreenStateType
            ( \_ ->
                RentalScreenData.initData
                  { data
                    { pickUpLoc
                      { address = if DS.null state.data.source then getString STR.CURRENT_LOCATION else state.data.source
                      , city = state.props.city
                      , lat = Just state.props.currentLocation.lat
                      , lon = Just state.props.currentLocation.lng
                      , placeId = Nothing
                      }
                    , latestScheduledRides = latestScheduledRides
                    }
                  }
            )
      rentalScreenFlow
    GO_TO_SCHEDULED_RIDES bookingId-> do
      modifyScreenState $ RideSummaryScreenStateType (\rideSummaryScreen -> RideSummaryScreenData.initData{data{fromScreen = (Screen.getScreen Screen.HOME_SCREEN),bookingId = bookingId}})
      rideSummaryScreenFlow
    GO_TO_SEARCH_LOCATION_SCREEN_FOR_ROUTE_SEARCH state source -> do
     let 
       currentCity = getValueToLocalStore CUSTOMER_LOCATION
       searchLocationState = currentState.searchLocationScreen
     (AutoCompleteResp routeStopresponse) <- Remote.busAutoCompleteBT (show state.props.ticketServiceType) currentCity (show state.props.sourceLat <> "," <> show state.props.sourceLong) Nothing "10" Nothing
     if null searchLocationState.data.routeSearchedList then do
        modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData{ props { actionType = BusSearchSelectionAction, canSelectFromFav = false, focussedTextField = Just SearchLocPickup , routeSearch = true , isAutoComplete = false}, data { ticketServiceType = BUS , srcLoc = Nothing, destLoc = Nothing, routeSearchedList = routeStopresponse.routes , stopsSearchedList = routeStopresponse.stops , updatedRouteSearchedList = routeStopresponse.routes , updatedStopsSearchedList = routeStopresponse.stops, rideType = slsState.data.rideType } })
     else do
        modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData{ props { actionType = BusSearchSelectionAction, canSelectFromFav = false, focussedTextField = Just SearchLocPickup , routeSearch = true , isAutoComplete = false}, data {ticketServiceType = BUS , srcLoc = Nothing, destLoc = Nothing , routeSearchedList = searchLocationState.data.updatedRouteSearchedList , stopsSearchedList = searchLocationState.data.updatedStopsSearchedList ,updatedRouteSearchedList = searchLocationState.data.updatedRouteSearchedList , updatedStopsSearchedList = searchLocationState.data.updatedStopsSearchedList, rideType = slsState.data.rideType} })
     searchLocationFlow
    GO_TO_NAMMASAFETY state triggerSos showtestDrill -> do
      updateSafetyScreenState state SafetyScreenData.defaultTimerValue showtestDrill triggerSos
      case (triggerSos || showtestDrill) of
        true -> do
          let
            isRideCompleted = state.props.currentStage == RideCompleted
          modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { props { reportPastRide = isRideCompleted }, data { lastRideDetails = if isRideCompleted then Arr.head $ myRideListTransformer true [ state.data.ratingViewState.rideBookingRes ] state.data.config Nothing else Nothing } })
          activateSafetyScreenFlow
        false -> nammaSafetyFlow
    GO_TO_DRIVER_PROFILES state -> do
      modifyScreenState $ DriverProfileScreenCommonStateType ( \driverProfileScreen -> driverProfileScreen { props { rideId = state.data.driverInfoCardState.rideId } } )
      driverProfileScreenFlow
    GO_TO_SAFETY_SETTING_SCREEN -> do
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { props { isOffUs = true } })
      safetySettingsFlow
    SAFETY_SUPPORT state isSafe -> do
      res <- lift $ lift $ Remote.sendSafetySupport $ Remote.makeAskSupportRequest state.props.bookingId isSafe $ "User need help - Ride on different route"
      case res of
        Right resp -> do
          void $ pure $ setValueToLocalNativeStore SAFETY_ALERT_TYPE "false"
          if isSafe then do
              void $ void $ lift $ lift $ showToast $  getString STR.GLAD_TO_KNOW_YOU_ARE_SAFE
              pure unit
          else do
            updateSafetyScreenState state SafetyScreenData.defaultTimerValue false true
            activateSafetyScreenFlow
        Left err -> do
          void $ void $ lift $ lift $ showToast $  getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
          pure unit
      homeScreenFlow
    GO_TO_SHARE_RIDE state -> do
      contacts <- getFormattedContacts
      let
        appName = fromMaybe state.data.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
      (res :: (Either ErrorResponse EmergencyContactsTrackingRes)) <- lift $ lift $ HelpersAPI.callApi $ EmergencyContactsTrackingReq state.data.driverInfoCardState.rideId

      let contactsWithstatus = case res of
            Left _ -> contacts
            Right res -> do
                map (\contactItem -> let
                      (EmergencyContactsTrackingRes statusDetails) = res
                      contactStatus = Arr.find (\(CurrentTrackingDetails item) -> isJust contactItem.contactPersonId && contactItem.contactPersonId == Just item.personId ) statusDetails.details
                      in case contactStatus of
                        Just (CurrentTrackingDetails details) -> do
                          let isWithinTimeLimit = runFn2 JB.differenceBetweenTwoUTCInMinutes (getCurrentUTC "") details.updateTime < state.data.config.safety.followingInterval
                          contactItem { isFollowing = Just isWithinTimeLimit }
                        Nothing -> contactItem { isFollowing = Nothing }
                ) contacts
      modifyScreenState $ HomeScreenStateType (\homeScreen -> state { data { contactList = Just contactsWithstatus }, props { showShareRide = true } })
      homeScreenFlow
    GO_TO_NOTIFY_RIDE_SHARE state -> do
      let
        req = ShareRideReq { emergencyContactNumbers: map (\item -> item.number) $ filter (\item -> item.isSelected) $ fromMaybe [] state.data.contactList }
      void $ lift $ lift $ Remote.shareRide req
      void $ void $ lift $ lift $ showToast $  getString STR.RIDE_SHARED_WITH_SELECTED_CONTACTS
      void $ pure $ cleverTapCustomEvent "ny_user_auto_share_ride"
      pure $ toggleBtnLoader "" false
      modifyScreenState $ HomeScreenStateType (\homeScreen -> state { props { showShareRide = false , chatcallbackInitiated = false}, data { contactList = Nothing } })
      homeScreenFlow
    EXIT_TO_FOLLOW_RIDE -> updateFollower false false Nothing
    GO_TO_MY_METRO_TICKETS homeScreenState -> do
      modifyScreenState $ MetroMyTicketsScreenStateType (\state -> state { data {userBlocked = homeScreenState.props.userBlocked }, props { entryPoint = ST.HomeScreenToMetroMyTickets} })
      metroMyTicketsFlow
    GO_TO_SAFETY_EDUCATION -> do
      let
        videoList = RC.safetyBannerVideoConfigData (DS.toLower $ getValueToLocalStore CUSTOMER_LOCATION) $ fetchLanguage $ getLanguageLocale languageKey
      modifyScreenState $ NammaSafetyScreenStateType (\safetyScreen -> safetyScreen { data { videoList = videoList }, props { showVideoView = true, educationViewIndex = Just 0, fromBannerLink = true } })
      safetyEducationFlow
    REPEAT_SEARCH state -> do
      cancelEstimate state.props.estimateId
      let
        topProvider = filter (\quotes -> quotes.providerType == ONUS && quotes.serviceTierName /= Just "Book Any") state.data.specialZoneQuoteList
        finalEsitmate = if null topProvider then filter (\quotes -> quotes.providerType == OFFUS && quotes.serviceTierName /= Just "Book Any") state.data.specialZoneQuoteList else topProvider
        mbSpecialZoneQuoteList = fromMaybe ChooseVehicle.config (head finalEsitmate)
        valid = timeValidity (getCurrentUTC "") mbSpecialZoneQuoteList.validTill

      if (valid) then do
        updateLocalStage SettingPrice
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state { props { currentStage = SettingPrice } })
        homeScreenFlow
      else do
        findEstimates state
    GOTO_CONFIRMING_LOCATION_STAGE finalState -> do
      liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { lat = finalState.props.sourceLat, lon = finalState.props.sourceLong, geoJson = finalState.data.polygonCoordinates, points = finalState.data.nearByPickUpPoints, labelId = getNewIDWithTag "LocateOnMapPin", zoomLevel = zoomLevel, locationName = fromMaybe "" finalState.props.locateOnMapProps.sourceLocationName, specialZoneMarkerConfig { labelImage = zoneLabelIcon finalState.props.confirmLocationCategory, labelMaxWidth = locateOnMapLabelMaxWidth} }
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { currentStage = ConfirmingLocation, rideRequestFlow = true, locateOnMapLocation { sourceLat = finalState.props.sourceLat, sourceLng = finalState.props.sourceLong, source = finalState.data.source, sourceAddress = finalState.data.sourceAddress }, locateOnMapProps { cameraAnimatedToSource = false }, isSource = Just true } })
      void $ pure $ updateLocalStage ConfirmingLocation
      void $ lift $ lift $ toggleLoader false
      homeScreenFlow
    UPDATE_REFERRAL_CODE referralCode -> do
      referralAppliedStatus <- applyReferralCode referralCode
      case referralAppliedStatus of
        REFERRAL_APPLIED -> do
          void $ pure $ hideKeyboardOnNavigation true
          _ <- UI.successScreen "" ""
          modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { showThanks = true, referralComponentProps { stage = ST.APPLIED_POPUP } })
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { isReferred = true, referralComponentProps { stage = NO_REFERRAL_STAGE }, referral { referralStatus = NO_REFERRAL, showAddReferralPopup = false } } })
        REFERRAL_INVALID -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { referralComponentProps { isInvalidCode = true } } })
        _ -> pure unit
      homeScreenFlow
    GO_TO_RIDE_RELATED_ISSUES state -> do
      let
        rideId = state.data.driverInfoCardState.rideId

        language = fetchLanguage $ getLanguageLocale languageKey
      void $ lift $ lift $ toggleLoader true
      mbCategory <- fetchParticularIssueCategory "RIDE_RELATED"
      case mbCategory of
        Just (Category category) -> do
          (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language category.issueCategoryId "" rideId ""
          let
            getOptionsRes' = mapWithIndex (\index (Option optionObj) -> optionObj { option = optionObj.option }) getOptionsRes.options

            messages' = mapWithIndex (\index (Message currMessage) -> makeChatComponent' (reportIssueMessageTransformer currMessage.message) currMessage.messageTitle currMessage.messageAction currMessage.label "Bot" (getCurrentUTC "") "Text" (500 * (index + 1))) getOptionsRes.messages

            chats' =
              map
                ( \(Message currMessage) ->
                    Chat
                      { chatId: currMessage.id
                      , chatType: "IssueMessage"
                      , timestamp: (getCurrentUTC "")
                      }
                )
                getOptionsRes.messages
          void $ lift $ lift $ toggleLoader false
          modifyScreenState $ ReportIssueChatScreenStateType (\_ -> ReportIssueChatScreenData.initData { data { entryPoint = ReportIssueChatScreenData.HomeScreenEntry, chats = chats', tripId = Just rideId, selectedCategory = { categoryName : getString STR.REPORT_AN_ISSUE, categoryId : category.issueCategoryId, categoryImageUrl : Nothing, categoryAction : Nothing, isRideRequired : false, maxAllowedRideAge : Nothing, categoryType : "Category", allowedRideStatuses : Nothing}, options = getOptionsRes', chatConfig { messages = messages' }, selectedRide = Nothing } })
          flowRouter IssueReportChatScreenFlow
        Nothing -> do
          void $ lift $ lift $ toggleLoader false
          void $ void $ lift $ lift $ showToast $  getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
          homeScreenFlow
    GOTO_PICKUP_INSTRUCTIONS state lat lon gateName locationName -> do
      let pickupInstructions = RC.pickupInstructions locationName gateName $ fetchLanguage $ getLanguageLocale languageKey
      if (null pickupInstructions) then do
        void $ pure $ openNavigation lat lon "WALK"
        homeScreenFlow
      else do
        modifyScreenState $ PickupInstructionsScreenStateType $ \pickupInstructionsScreen -> pickupInstructionsScreen { data { pickupLat = lat, pickupLong = lon, pickupInstructions = pickupInstructions } }
        pickupInstructionsScreenFlow
    PARCEL (GO_TO_PARCEL_INSTRUCTIONS updatedState) -> do
      if getValueToLocalNativeStore PARCEL_INSTRUCTIONS_VISITED /= "true" then do
        modifyScreenState $ ParcelDeliveryScreenStateType (\_ -> ParcelDeliveryScreenData.initData { data { currentStage = ST.DELIVERY_INSTRUCTIONS}})
        parcelDeliveryFlow
      else do
        void $ pure $ updateLocalStage SearchLocationModel
        modifyScreenState $ ParcelDeliveryScreenStateType (\_ -> ParcelDeliveryScreenData.initData)
        modifyScreenState $ HomeScreenStateType (\_ -> updatedState {
          props { homeScreenPrimaryButtonLottie = true, isSource = Just true, currentStage = SearchLocationModel, isSearchLocation = SearchLocation, searchLocationModelProps{crossBtnSrcVisibility = true},  rideSearchProps{ sessionId = generateSessionId unit } }
        , data {fareProductType = FPT.DELIVERY, source="", locationList = updatedState.data.recentSearchs.predictionArray}
        })
        homeScreenFlow
    PARCEL (GET_DELIVERY_IMAGE updatedState) -> do
      res <- lift $ lift $ Remote.getDeliveryImage updatedState.data.driverInfoCardState.rideId
      case res of
        Right (API.GetDeliveryImageResponse resp) -> do
          let isNotValidImage = resp == "" || DS.contains (DS.Pattern "error") resp || DS.length resp < 100
          if isNotValidImage then do
            modifyScreenState $ HomeScreenStateType (\homeScreen -> updatedState { data { deliveryImage = Nothing }})
            void $ void $ lift $ lift $ showToast $  "Image Not Uploaded, please try again"
          else do
            modifyScreenState $ HomeScreenStateType (\homeScreen -> updatedState { data { deliveryImage = Just resp }, props { showDeliveryImageAndOtpModal = true, loadingDeliveryImage = false} })
        Left _ -> do
          void $ void $ lift $ lift $ showToast $  "Image Not Uploaded, please try again"
      homeScreenFlow
    PARCEL (GO_TO_DELIVERY_DETAILS updatedState) -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> updatedState)
      modifyScreenState $ ParcelDeliveryScreenStateType (\parcelDeliveryScreen -> parcelDeliveryScreen { props { isEditModal = false}, data { tipForDriver = if updatedState.data.config.tipsEnabled && updatedState.props.customerTip.tipForDriver /= 0 then Just updatedState.props.customerTip.tipForDriver else Nothing, parcelQuoteList = updatedState.data.selectedEstimatesObject, sourceAddress = updatedState.data.sourceAddress, destinationAddress = updatedState.data.destinationAddress, route = updatedState.data.route, sourceLat = updatedState.props.sourceLat, sourceLong = updatedState.props.sourceLong, destinationLat = updatedState.props.destinationLat, destinationLong = updatedState.props.destinationLong, rateCard = updatedState.data.rateCard}})
      parcelDeliveryFlow
    GO_TO_BUS_TICKET_BOOKING_SCREEN state -> do
      modifyScreenState $ BusTicketBookingScreenStateType (\_ -> BusTicketBookingScreenData.initData { props {srcLat =  state.props.sourceLat , srcLong = state.props.sourceLong}, data {ticketServiceType = BUS}})
      busTicketBookingFlow
    _ -> homeScreenFlow

findEstimates :: HomeScreenState -> FlowBT String Unit
findEstimates updatedState = do
  logField_ <- lift $ lift $ getLogFields
  if updatedState.data.source == getString STR.CURRENT_LOCATION then do
    fullAddress <- getPlaceName updatedState.props.sourceLat updatedState.props.sourceLong HomeScreenData.dummyLocation true
    case fullAddress of
      Just (PlaceName address) -> do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> updatedState { data { source = address.formattedAddress, sourceAddress = encodeAddress address.formattedAddress [] Nothing updatedState.props.sourceLat updatedState.props.sourceLong } })
      Nothing -> void $ void $ lift $ lift $ showToast $  getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
  else
    pure unit
  (GlobalState globalState) <- getState
  let
    state = globalState.homeScreen
  liftFlowBT $ logEventWithTwoParams logField_ "ny_user_source_and_destination" "ny_user_enter_source" (take 99 (state.data.source)) "ny_user_enter_destination" (take 99 (state.data.destination))
  liftFlowBT $ logEventWithTwoParams logField_ "ny_user_sourceLat_sourceLong" "ny_user_source_lat" (show state.props.sourceLat) "ny_ic_source_long" (show state.props.sourceLong)
  liftFlowBT $ logEventWithTwoParams logField_ "ny_user_destinationLat_destinationLong" "ny_user_destination_lat" (show state.props.destinationLat) "ny_ic_destination_long" (show state.props.destinationLong)
  (ServiceabilityRes sourceServiceabilityResp) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq state.props.sourceLat state.props.sourceLong) ORIGIN
  if (not sourceServiceabilityResp.serviceable) then do
    updateLocalStage SearchLocationModel
    setUserCity CUSTOMER_LOCATION $ show (getCityNameFromCode sourceServiceabilityResp.city)
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { showIntercityUnserviceablePopUp = true, currentStage = SearchLocationModel, rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = false, isSource = Just true, isRideServiceable = false, city = getCityNameFromCode sourceServiceabilityResp.city } })
    homeScreenFlow
  else
    pure unit
  (ServiceabilityRes sourceServiceabilityRespDest) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq state.props.destinationLat state.props.destinationLong) DESTINATION
  let
    isIntercity = updatedState.data.currentCityConfig.enableIntercity && any (_ == (Just "*")) [ sourceServiceabilityResp.currentCity, sourceServiceabilityRespDest.currentCity ] || (isJust sourceServiceabilityResp.currentCity && isJust sourceServiceabilityRespDest.currentCity && sourceServiceabilityResp.currentCity /= sourceServiceabilityRespDest.currentCity)
    schedulingEnabled = updatedState.data.currentCityConfig.enableScheduling
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { fareProductType = if isIntercity then FPT.INTER_CITY else homeScreen.data.fareProductType} })
  when (state.data.startTimeUTC /= "" && not isIntercity) do
    modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData { props { showNormalRideNotSchedulablePopUp = true } })
    updateLocalStage HomeScreen
    homeScreenFlow
  if (isIntercity && schedulingEnabled) then do
    void $ lift $ lift $ toggleLoader true
    routeObj <- getRouteInfo state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong
    void $ lift $ lift $ toggleLoader false
    let
      currentUTC =  (getCurrentUTC "")
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {
      props {
        currentStage= GoToTripSelect
      , isSearchLocation = SelectTripType
      , searchLocationModelProps {
          totalRideDistance = routeObj.estDistance
        , totalRideDuration = routeObj.estDuration
        , tripType = CTA.ONE_WAY_TRIP
        }
      , isTripSchedulable = false
      }
      , data {
        tripEstDuration = routeObj.estDuration
      , tripTypeDataConfig = HomeScreenData.tripTypeDataConfig{
                tripPickupData = Just HomeScreenData.dummyTripTypeData{
                tripDateReadableString = convertUTCtoISC currentUTC "D MMM, h:mm A" }
              }
            }
      }
    )

    updateLocalStage GoToTripSelect
    homeScreenFlow
  else rideSearchRequestFlow state

rideSearchRequestFlow :: HomeScreenState -> FlowBT String Unit
rideSearchRequestFlow updatedState = do
  void $ liftFlowBT $ setMapPadding 0 0 0 0
  logField_ <- lift $ lift $ getLogFields

  (GlobalState globalState) <- getState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "HomeScreen"
  let
    state = globalState.homeScreen
    currentTime = (convertUTCtoISC (getCurrentUTC "") "h:mm:ss A")

    currentDate = getCurrentDate ""

    markers = normalRoute ""

    isRoundTrip = state.props.searchLocationModelProps.tripType == ROUND_TRIP

    sourceAddress =
      if state.props.isSpecialZone && not (DS.null state.props.defaultPickUpPoint) then
        state.props.defaultPickUpPoint
      else
        state.data.source

    callback = runFn2 EHC.getMarkerCallback push MarkerLabelOnClick

    srcMarkerConfig = defaultMarkerConfig{ markerId = markers.srcMarker, pointerIcon = markers.srcMarker, shortTitle = (runFn3 splitString sourceAddress "," 2), primaryText = sourceAddress, secondaryText = fromMaybe "" state.props.locateOnMapProps.sourceLocationName, labelImage = defaultMarkerImageConfig{image = zoneLabelIcon state.props.zoneType.sourceTag}, labelActionImage = defaultMarkerImageConfig{image = "ny_ic_chevron_right_black_2", height = markerArrowSize, width = markerArrowSize}, labelMaxWidth = estimateLabelMaxWidth, markerCallback = callback, labelMaxLines = 2, labelTextSize = 11}

    destMarkerConfig = defaultMarkerConfig{ markerId = markers.destMarker, pointerIcon = markers.destMarker, shortTitle = (runFn3 splitString state.data.destination "," 2), primaryText = state.data.destination, labelImage = defaultMarkerImageConfig{image = zoneLabelIcon state.props.zoneType.destinationTag}, labelActionImage = defaultMarkerImageConfig{image = "ny_ic_chevron_right_black_2", height = markerArrowSize, width = markerArrowSize}, labelMaxWidth = estimateLabelMaxWidth, markerCallback = callback, labelMaxLines = 2, labelTextSize = 11,  anchorV = 1.0}
  void $ pure
    $ setCleverTapUserProp
        [ { key: "Latest Search From", value: unsafeToForeign ("lat: " <> (show updatedState.props.sourceLat) <> " long: " <> (show updatedState.props.sourceLong)) }
        , { key: "Latest Search", value: (unsafeToForeign $ currentDate <> " " <> currentTime) }
        ]

  let
    startTimeUTC = if (state.data.fareProductType == FPT.INTER_CITY && state.data.startTimeUTC /= "") then state.data.startTimeUTC else (getCurrentUTC "")
    isRoundTrip = state.props.searchLocationModelProps.tripType == ROUND_TRIP
    returnTimeUTC = if isRoundTrip then (Just state.data.returnTimeUTC) else Nothing
    isScheduledRideSearch = startTimeUTC > (getCurrentUTC "")
    currentTime = convertUTCtoISC (getCurrentUTC "")  "hh:mm A"
  let searchReq = if (state.data.fareProductType == FPT.INTER_CITY) then Remote.makeRoundTripReq state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong state.data.sourceAddress state.data.destinationAddress startTimeUTC returnTimeUTC isRoundTrip
                  else Remote.makeRideSearchReq state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong state.data.sourceAddress state.data.destinationAddress startTimeUTC state.props.rideSearchProps.sourceManuallyMoved state.props.rideSearchProps.destManuallyMoved state.props.rideSearchProps.sessionId state.props.isSpecialZone state.data.fareProductType
  (SearchRes rideSearchRes) <- Remote.rideSearchBT $ searchReq
  void $ pure $ setValueToLocalStore STARTED_ESTIMATE_SEARCH "FALSE"
  routeResponse <- Remote.drawMapRoute state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong srcMarkerConfig destMarkerConfig "NORMAL" rideSearchRes.routeInfo "pickup" (specialLocationConfig "" "" false getPolylineAnimationConfig)
  when isRoundTrip $
    void $ lift $ lift $ liftFlow $ showMarker srcMarkerConfig 9.9 9.9 160 0.5 0.9 (getNewIDWithTag "CustomerHomeScreen")
  logStatus "ride_search" rideSearchRes
  void $ pure $ deleteValueFromLocalStore TIP_VIEW_DATA
  when (state.data.fareProductType == FPT.INTER_CITY) (do liftFlowBT $ logEventWithMultipleParams logField_ "user_intercity_ride_search"
                                                                $ [ { key: "Pickup", value: unsafeToForeign state.data.source }
                                                                  , { key: "Destination", value: unsafeToForeign state.data.destination }
                                                                  , { key: "ScheduledRideSearch", value: unsafeToForeign isScheduledRideSearch}
                                                                  , { key: "RoundTrip", value: unsafeToForeign isRoundTrip }
                                                                  , { key: "Ride Search Time", value : unsafeToForeign currentTime}
                                                                  ] )
  case rideSearchRes.routeInfo of
    Just (Route response) -> do
      let
        distance = if response.distance < 1000 then toStringJSON (response.distance) <> " m" else parseFloat (INT.toNumber (response.distance) / 1000.0) 2 <> " km"

        maxEstimatedDuration = max (response.duration / 60) $ (response.distance / 1000) * 3
        duration = if (state.data.fareProductType == FPT.INTER_CITY && state.props.searchLocationModelProps.totalRideDuration /= 0) then (HU.formatDuration state.props.searchLocationModelProps.totalRideDuration) else ((show (response.duration / 60)) <> " min")

        Snapped points = response.points

        rideStartTime =if (state.data.fareProductType == FPT.INTER_CITY && state.data.startTimeUTC /= "") then state.data.startTimeUTC else (getCurrentUTC "")

        rideEndTime = if ( state.data.returnTimeUTC == "") then (EHC.getUTCAfterNSeconds rideStartTime (maxEstimatedDuration*60)) else  state.data.returnTimeUTC

        maybeInvalidBookingDetails = invalidBookingTime startTimeUTC $ Just maxEstimatedDuration
      checkForScheduled rideStartTime rideEndTime 1800
      case head points, last points of
        Just (LatLong source), Just (LatLong dest) -> do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { maxEstimatedDuration = maxEstimatedDuration }, props { routeEndPoints = Just ({ source: { lat: source.lat, lng: source.lon, place: state.data.source, address: Nothing, city: Nothing, isSpecialPickUp: Just false }, destination: { lat: dest.lat, lng: dest.lon, place: state.data.destination, address: Nothing, city: Nothing, isSpecialPickUp: Just false } })} })
        _, _ -> pure unit
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { rideDistance = distance, rideDuration = duration, source = state.data.source, sourceAddress = state.data.sourceAddress }})
      let
        distanceBtwCurrentAndSource = getDistanceBwCordinates state.props.sourceLat state.props.sourceLong state.props.currentLocation.lat state.props.currentLocation.lng

        isDistMoreThanThreshold = state.props.currentLocation.lat /= 0.0 && state.props.currentLocation.lng /= 0.0 && distanceBtwCurrentAndSource > state.data.config.mapConfig.locateOnMapConfig.pickUpToSourceThreshold
      -- Commenting the below condition as it is not required now
      -- if ((MU.getMerchant FunctionCall) /= MU.YATRI && response.distance >= 50000) then do
      --   updateLocalStage DistanceOutsideLimits
      --   modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = DistanceOutsideLimits ,rideRequestFlow = true, isSearchLocation = SearchLocation, findingQuotesProgress = 0.0, isShorterTrip = false}})
      --   homeScreenFlow
      if ((response.distance < 500 || (isDistMoreThanThreshold && state.data.fareProductType /= FPT.DELIVERY)) && Arr.all (_ == false) [ isLocalStageOn PickUpFarFromCurrentLocation, isLocalStageOn ShortDistance ]) then do
        let
          currentStage = if (isDistMoreThanThreshold && state.data.fareProductType /= FPT.DELIVERY) then PickUpFarFromCurrentLocation else ShortDistance
        updateLocalStage currentStage
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { maxEstimatedDuration = maxEstimatedDuration }, props { currentStage = currentStage, rideRequestFlow = true, isSearchLocation = SearchLocation, distance = response.distance, isShorterTrip = response.distance < 500, findingQuotesProgress = 0.0 } })
        homeScreenFlow
      else
        pure unit
      pure unit
    Nothing -> pure unit
  void $ liftFlowBT
    $ setFlowStatusData
        ( FlowStatusData
            { source: { lat: state.props.sourceLat, lng: state.props.sourceLong, place: state.data.source, address: state.props.locateOnMapProps.sourceLocationName, city: getCityCodeFromCity state.props.city, isSpecialPickUp: Just false }
            , destination: { lat: state.props.destinationLat, lng: state.props.destinationLong, place: state.data.destination, address: Nothing, city: Nothing, isSpecialPickUp: Just false }
            , sourceAddress: state.data.sourceAddress
            , destinationAddress: state.data.destinationAddress
            , sourceLabelIcon: Just $ zoneLabelIcon state.props.zoneType.sourceTag
            , destLabelIcon: Just $ zoneLabelIcon state.props.zoneType.destinationTag
            , sourceGeoJson: if DS.null state.data.polygonCoordinates then Nothing else Just $ state.data.polygonCoordinates
            , sourceGates: if null state.data.nearByPickUpPoints then Nothing else Just $ state.data.nearByPickUpPoints
            }
        )
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { searchId = rideSearchRes.searchId, currentStage = FindingEstimate, rideRequestFlow = true, selectedQuote = Nothing, isSearchLocation = SearchLocation, sourcePlaceId = Nothing, destinationPlaceId = Nothing, findingQuotesProgress = 0.0 }, data { nearByDrivers = Nothing, route = if state.data.fareProductType == FPT.DELIVERY then rideSearchRes.routeInfo else homeScreen.data.route } })
  updateLocalStage FindingEstimate
  homeScreenFlow

checkForScheduled :: String -> String -> Int -> FlowBT String Unit
checkForScheduled  rideStartTime rideEndTime rideScheduledBufferTime = do
        resp <- FlowCache.fetchAndUpdateScheduledRides false
        {overLapping,overLappedBooking} <- FlowCache.overlappingRides rideStartTime (Just rideEndTime) rideScheduledBufferTime
        if overLapping then do
            void $ updateLocalStage HomeScreen
            modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData{props{showScheduledRideExistsPopUp = true},data{latestScheduledRides = resp,overLappingBooking = overLappedBooking}})
            homeScreenFlow
            void $ lift $ lift $ toggleLoader false
        else pure unit

getRouteInfo::  Number -> Number -> Number -> Number ->  FlowBT String {estDistance :: Int , estDuration :: Int}
getRouteInfo srcLat srcLong destLat destLng = do
  (GetRouteResp routeResponse) <- Remote.getRouteBT "intercity" (Remote.makeGetRouteReq srcLat srcLong destLat destLng Nothing)
  case (head routeResponse) of
    Just (Route route) -> do
          pure $ { estDistance : route.distance , estDuration : route.duration }
    Nothing -> pure $ {estDistance : 0, estDuration :0}

updateFollower :: Boolean -> Boolean -> Maybe String -> FlowBT String Unit
updateFollower callFollowersApi callInitUi eventType = do
  (GlobalState allState) <- getState
  automaticallySharedFollowers <- getFollowers allState
  let
    followers = nubByEq (\a b -> a.bookingId == b.bookingId) $ Arr.union automaticallySharedFollowers (fromMaybe [] allState.homeScreen.data.manuallySharedFollowers)
    noOfFollowers = Arr.length followers
  setValueToLocalStore TRACKING_DRIVER "False"
  setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
  let currentUserOnRide = elem allState.homeScreen.props.currentStage [ RideAccepted, RideStarted, ChatWithDriver ]
      checkUsersLocalStage = elem (getValueToLocalStore LOCAL_STAGE) [ "RideAccepted", "RideStarted", "ChatWithDriver" ]
  when (currentUserOnRide || checkUsersLocalStage) $ removeChatService ""
  void $ pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
  void $ liftFlowBT $ runEffectFn1 EHC.updateIdMap "FollowsRide"
  let
    currentFollowerName = getMockFollowerName ""

    currentFollower =
      if isJust eventType then
        Arr.find (\follower -> DS.contains (Pattern currentFollowerName) (fromMaybe "" follower.name)) followers
      else
        Nothing
  modifyScreenState
    $ FollowRideScreenStateType
        ( \followRideScreen ->
            followRideScreen
              { data
                { followers = followers
                , currentFollower =
                  case noOfFollowers == 1, isJust currentFollower of
                    true, _ -> Arr.head followers
                    _, true -> currentFollower
                    _, _ -> followRideScreen.data.currentFollower
                , sosStatus =
                  if eventType == Just "SOS_MOCK_DRILL" then
                    Just CTA.MockPending
                  else
                    Nothing
                , currentStage =
                  if isJust eventType && isNothing currentFollower then
                    MockFollowRide
                  else if noOfFollowers > 1 && isNothing currentFollower then
                    PersonList
                  else
                    FollowingRide
                }
              , props
                { city = allState.homeScreen.props.city
                , currentUserOnRide = currentUserOnRide
                , isMock = elem eventType [ Just "SOS_MOCK_DRILL", Just "SOS_MOCK_DRILL_NOTIFY" ]
                }
              }
        )
  when (eventType == Just "SOS_MOCK_DRILL") $ liftFlowBT $ deleteDismisedMockDrills currentFollower
  followRideScreenFlow callInitUi
  where
  getFollowers allState =
    if callFollowersApi then do
      resp <- lift $ lift $ Remote.getFollowRide ""
      case resp of
        Right (FollowRideRes response) -> pure $ map (\(Followers follower) -> transformFollower follower) response
        Left err -> pure $ fromMaybe [] allState.homeScreen.data.followers
    else do
      pure $ fromMaybe [] allState.homeScreen.data.followers
  transformFollower follower = {name : follower.name, bookingId : follower.bookingId, mobileNumber : follower.mobileNumber, priority : follower.priority, isManualFollower : false, personId : follower.personId}

followRideScreenFlow :: Boolean -> FlowBT String Unit
followRideScreenFlow callInitUI = do
  hideLoaderFlow
  when callInitUI $ lift $ lift $ initUI
  flow <- UI.followRideScreen
  case flow of
    RESTART_TRACKING -> do
      void $ liftFlowBT $ runEffectFn1 EHC.updateIdMap "FollowsRide"
      followRideScreenFlow false
    GO_TO_HS_FROM_FOLLOW_RIDE state removeManualFollower -> do
      void $ liftFlowBT $ runEffectFn1 EHC.updateIdMap "FollowsRide"
      pure $ removeAllPolylines ""
      removeChatService ""
      void $ liftFlowBT $ runEffectFn1 clearMap ""
      (GlobalState gs) <- getState
      let
        currentMapFragment =
          if os == "IOS" then
            "CustomerHomeScreenMap"
          else if gs.homeScreen.props.currentStage == HomeScreen then
            "CustomerHomeScreenMap"
          else
            "CustomerHomeScreen"
      void $ liftFlowBT $ reallocateMapFragment (getNewIDWithTag currentMapFragment)
      modifyScreenState $ FollowRideScreenStateType (\_ -> FollowRideScreenData.initData)
      case state.data.currentFollower, removeManualFollower of
        Just currfollower, true -> do
          let currentManualFollowers = fromMaybe [] gs.homeScreen.data.manuallySharedFollowers
              updatedManualFollowers = if currfollower.isManualFollower then Arr.filter (\follower -> follower.bookingId /= currfollower.bookingId ) currentManualFollowers else currentManualFollowers
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { manuallySharedFollowers = if null updatedManualFollowers then Nothing else Just updatedManualFollowers } })
        _,_ -> pure unit
      currentFlowStatus false
    OPEN_GOOGLE_MAPS_FOLLOW_RIDE state -> do
      case state.data.driverInfoCardState of
        Nothing -> followRideScreenFlow false
        Just ride -> do
          (GetDriverLocationResp resp) <- Remote.getDriverLocationBT ride.rideId
          let
            sourceLat = (resp ^. _lat)

            sourceLng = (resp ^. _lon)

            destLat = ride.destinationLat

            destLng = ride.destinationLng
          void $ pure $ openNavigation sourceLat sourceLng "DIRECTION"
          followRideScreenFlow false
    GO_TO_DRIVER_PROFILE_FROM_FOLLOWRIDE state -> do
      modifyScreenState $ DriverProfileScreenCommonStateType ( \driverProfileScreen -> driverProfileScreen { props { rideId = fromMaybe "" $ (\a -> a.rideId) <$> state.data.driverInfoCardState } } )
      driverProfileScreenFlow

getDistanceDiff :: HomeScreenState -> Number -> Number -> FlowBT String Unit
getDistanceDiff state lat lon = do
  distanceInfo <- getDistanceInfo (state.data.savedLocations) "" (lat) (lon) (fromMaybe "" state.data.saveFavouriteCard.selectedItem.placeId)
  case distanceInfo.locExistsAs of
    "" -> modifyScreenState $ HomeScreenStateType (\homeScreen -> state { props { isSaveFavourite = true } })
    _ -> do
      void $ lift $ lift $ showToast (getString STR.ALREADY_EXISTS)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> state { data { saveFavouriteCard { selectedItem = locationListStateObj } } })
  homeScreenFlow

fetchLatAndLong :: HomeScreenState -> String -> FlowBT String Unit
fetchLatAndLong state tag = case state.data.saveFavouriteCard.selectedItem.placeId of
  Just placeID -> do
    (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (state.data.saveFavouriteCard.selectedItem.title <> ", " <> state.data.saveFavouriteCard.selectedItem.subTitle) (Just placeID) (fromMaybe 0.0 state.data.saveFavouriteCard.selectedItem.lat) (fromMaybe 0.0 state.data.saveFavouriteCard.selectedItem.lon) state.data.saveFavouriteCard.selectedItem
    let
      (PlaceName placeName) = (fromMaybe HomeScreenData.dummyLocationName (placeNameResp !! 0))
    let
      (LatLong placeLatLong) = (placeName.location)
    resp <- Remote.addSavedLocationBT (encodeAddressDescription state.data.saveFavouriteCard.address tag state.data.saveFavouriteCard.selectedItem.placeId (Just placeLatLong.lat) (Just placeLatLong.lon) placeName.addressComponents)
    void $ FlowCache.updateAndFetchSavedLocations true
    pure unit
  Nothing -> pure unit


editDestinationFlow :: FlowBT String Unit
editDestinationFlow = do
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ConfirmingEditDestinationLoc}})
  (GlobalState homeScreenModifiedState) <- getState
  void $ lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenEditDest")
  let homeScreenState = homeScreenModifiedState.homeScreen
  void $ pure $ hideKeyboardOnNavigation true
  void $ pure $ removeAllPolylines ""
  let srcLat = homeScreenState.props.sourceLat
  let srcLon = homeScreenState.props.sourceLong
  let dstLat = homeScreenState.props.destinationLat
  let dstLon = homeScreenState.props.destinationLong
  let primaryText = homeScreenState.data.destination
      markers = normalRoute ""
      srcMarkerConfig = defaultMarkerConfig{ pointerIcon = markers.srcMarker }
      destMarkerConfig = defaultMarkerConfig{ pointerIcon = markers.destMarker, primaryText = primaryText, anchorU = 0.5, anchorV = 1.0}
      routeConfig = JB.mkRouteConfig (Remote.walkCoordinate srcLat srcLon dstLat dstLon) srcMarkerConfig destMarkerConfig Nothing "NORMAL_ROUTE" "DOT" false JB.DEFAULT (JB.mapRouteConfig{vehicleSizeTagIcon = HU.getVehicleSize unit, polylineAnimationConfig = getPolylineAnimationConfig})
      destAddress = SearchReqLocation { gps : LatLong { lat : homeScreenState.props.destinationLat , lon : homeScreenState.props.destinationLong } , address : (LocationAddress homeScreenState.data.destinationAddress)}
  liftFlowBT $ drawRoute [routeConfig] (getNewIDWithTag "CustomerHomeScreenEditDest")
  resp <- lift $ lift $ HelpersAPI.callApi $ Remote.makeEditLocationRequest homeScreenState.data.driverInfoCardState.rideId Nothing (Just destAddress)
  case resp of
    Right (EditLocationRes editDestinationSoftResp) -> do
      if (editDestinationSoftResp.bookingUpdateRequestId == Nothing) then do
        void $ lift $ lift $ showToast (getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
        callCheckRideStatus homeScreenState
        else do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ConfirmingEditDestinationLoc, bookingUpdateRequestId = editDestinationSoftResp.bookingUpdateRequestId}})
    Left (err) -> do
      void $ lift $ lift $ showToast (decodeError err.response.errorMessage "errorMessage")
      callCheckRideStatus homeScreenState
  homeScreenFlow
  where
    callCheckRideStatus homeScreenState = do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{destination = homeScreenState.data.driverInfoCardState.destination, destinationAddress = homeScreenState.data.driverInfoCardState.destinationAddress}, props{destinationLat = homeScreenState.data.driverInfoCardState.destinationLat, destinationLong = homeScreenState.data.driverInfoCardState.destinationLng}})
      setValueToLocalStore TRACKING_DRIVER "False"
      void $ lift $ lift $ toggleLoader true
      checkRideStatus true false


rideSearchFlow :: String -> FlowBT String Unit
rideSearchFlow flowType = do
  void $ pure $ hideKeyboardOnNavigation true
  logField_ <- lift $ lift $ getLogFields
  (GlobalState homeScreenModifiedState) <- getState
  void $ liftFlowBT $ setMapPadding 0 0 0 0
  let
    finalState = homeScreenModifiedState.homeScreen -- bothLocationChangedState{props{isSrcServiceable =homeScreenModifiedState.homeScreen.props.isSrcServiceable, isDestServiceable = homeScreenModifiedState.homeScreen.props.isDestServiceable, isRideServiceable = homeScreenModifiedState.homeScreen.props.isRideServiceable }}
  if (finalState.props.sourceLat /= 0.0 && finalState.props.sourceLong /= 0.0) && (finalState.props.destinationLat /= 0.0 && finalState.props.destinationLong /= 0.0) && (finalState.data.source /= "") && (finalState.data.destination /= "") then do
    let
      searchWithoutConfirmPickup = any (_ == finalState.props.rideSearchProps.sourceSelectType) [ ST.MAP, ST.RETRY_SEARCH, ST.REPEAT_RIDE ] || (finalState.props.rideSearchProps.sourceSelectType == ST.FAVOURITE && not finalState.props.isSpecialZone)
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { flowWithoutOffers = flowWithoutOffers WithoutOffers } })
    case searchWithoutConfirmPickup of
      false -> do
        pure $ removeAllPolylines ""
        liftFlowBT $ setMapPadding 0 0 0 0
        if finalState.data.source == (getString STR.CURRENT_LOCATION) then void $ pure $ currentPosition "" else pure unit
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { currentStage = GoToConfirmLocation } })
        void $ pure $ updateLocalStage GoToConfirmLocation
      true -> do
        let
          currentTime = (convertUTCtoISC (getCurrentUTC "") "h:mm:ss A")
          currentDate = getCurrentDate ""
        void $ pure
          $ setCleverTapUserProp
              [ { key: "Latest Search From", value: unsafeToForeign ("lat: " <> (show finalState.props.sourceLat) <> " long: " <> (show finalState.props.sourceLong)) }
              , { key: "Latest Search", value: unsafeToForeign (currentDate <> " " <> currentTime) }
              ]
        let
          startTimeUTC = if (finalState.data.fareProductType == FPT.INTER_CITY && finalState.data.startTimeUTC /= "") then finalState.data.startTimeUTC else (getCurrentUTC "")
        (SearchRes rideSearchRes) <- Remote.rideSearchBT (Remote.makeRideSearchReq finalState.props.sourceLat finalState.props.sourceLong finalState.props.destinationLat finalState.props.destinationLong finalState.data.sourceAddress finalState.data.destinationAddress startTimeUTC finalState.props.rideSearchProps.sourceManuallyMoved finalState.props.rideSearchProps.destManuallyMoved finalState.props.rideSearchProps.sessionId finalState.props.isSpecialZone finalState.data.fareProductType)
        void $ pure $ setValueToLocalStore STARTED_ESTIMATE_SEARCH "FALSE"
        void $ pure $ deleteValueFromLocalStore TIP_VIEW_DATA
        void $ liftFlowBT
          $ setFlowStatusData
              ( FlowStatusData
                  { source:
                      { lat: finalState.props.sourceLat
                      , lng: finalState.props.sourceLong
                      , place: finalState.data.source
                      , address: Nothing
                      , city: getCityCodeFromCity finalState.props.city
                      , isSpecialPickUp: Just false
                      }
                  , destination:
                      { lat: finalState.props.destinationLat
                      , lng: finalState.props.destinationLong
                      , place: finalState.data.destination
                      , address: Nothing
                      , city: Nothing
                      , isSpecialPickUp: Just false
                      }
                  , sourceAddress: finalState.data.sourceAddress
                  , destinationAddress: finalState.data.destinationAddress
                  , sourceLabelIcon: Just $ zoneLabelIcon finalState.props.zoneType.sourceTag
                  , destLabelIcon: Just $ zoneLabelIcon finalState.props.zoneType.destinationTag
                  , sourceGeoJson: finalState.props.locateOnMapProps.sourceGeoJson
                  , sourceGates: finalState.props.locateOnMapProps.sourceGates
                  }
              )
        case finalState.props.currentStage of
          TryAgain -> do
            when (finalState.props.customerTip.enableTips)
              $ do
                  cancelEstimate finalState.props.estimateId
            void $ pure $ updateLocalStage TryAgain
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { searchId = rideSearchRes.searchId, currentStage = TryAgain, rideRequestFlow = true }, data { nearByDrivers = Nothing } })
          _ -> do
            push <- lift $ lift $ liftFlow $ getPushFn Nothing "HomeScreen"
            let
              sourceSpecialTagIcon = zoneLabelIcon finalState.props.zoneType.sourceTag

              destSpecialTagIcon = zoneLabelIcon finalState.props.zoneType.destinationTag

              markers = normalRoute ""

              callback = runFn2 EHC.getMarkerCallback push MarkerLabelOnClick

              srcMarkerConfig = defaultMarkerConfig{ markerId = markers.srcMarker, pointerIcon = markers.srcMarker, shortTitle = (runFn3 splitString finalState.data.source "," 2), primaryText = finalState.data.source, labelActionImage = defaultMarkerImageConfig{image = "ny_ic_chevron_right_black_2", height = markerArrowSize, width = markerArrowSize}, labelMaxWidth = estimateLabelMaxWidth, markerCallback = callback, labelMaxLines = 2, labelTextSize = 11}

              destMarkerConfig = defaultMarkerConfig{ markerId = markers.destMarker, pointerIcon = markers.destMarker, shortTitle = (runFn3 splitString finalState.data.destination "," 2), primaryText = finalState.data.destination, labelActionImage = defaultMarkerImageConfig{image = "ny_ic_chevron_right_black_2", height = markerArrowSize, width = markerArrowSize}, labelMaxWidth = estimateLabelMaxWidth, markerCallback = callback, labelMaxLines = 2, labelTextSize = 11}
            routeResponse <- Remote.drawMapRoute finalState.props.sourceLat finalState.props.sourceLong finalState.props.destinationLat finalState.props.destinationLong srcMarkerConfig destMarkerConfig "NORMAL" rideSearchRes.routeInfo "pickup" (specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon false getPolylineAnimationConfig)
            case rideSearchRes.routeInfo of
              Just (Route response) -> do
                let
                  distance = if response.distance < 1000 then toStringJSON (response.distance) <> " m" else parseFloat (INT.toNumber (response.distance) / 1000.0) 2 <> " km"

                  duration = (show (response.duration / 60)) <> " min"

                  Snapped points = response.points
                case head points, last points of
                  Just (LatLong source), Just (LatLong dest) -> do
                    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { routeEndPoints = Just ({ source: { lat: source.lat, lng: source.lon, place: finalState.data.source, address: Nothing, city: Nothing, isSpecialPickUp: Just false }, destination: { lat: dest.lat, lng: dest.lon, place: finalState.data.destination, address: Nothing, city: Nothing, isSpecialPickUp: Just false } }) } })
                  _, _ -> pure unit
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { rideDistance = distance, rideDuration = duration, source = finalState.data.source, sourceAddress = finalState.data.sourceAddress } })
                let
                  distanceBtwCurrentAndSource = getDistanceBwCordinates finalState.props.sourceLat finalState.props.sourceLong finalState.props.currentLocation.lat finalState.props.currentLocation.lng

                  isDistMoreThanThreshold = finalState.props.currentLocation.lat /= 0.0 && finalState.props.currentLocation.lng /= 0.0 && (distanceBtwCurrentAndSource > finalState.data.config.mapConfig.locateOnMapConfig.pickUpToSourceThreshold) && flowType == "NORMAL_FLOW"

                  cityConfig = getCityConfig finalState.data.config.cityConfig (getValueToLocalStore CUSTOMER_LOCATION)
                if (cityConfig.geoCodeConfig.strictBounds && response.distance >= cityConfig.geoCodeConfig.radius) then do
                  void $ pure $ updateLocalStage DistanceOutsideLimits
                  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { currentStage = DistanceOutsideLimits, rideRequestFlow = true, isSearchLocation = SearchLocation } })
                else if ((response.distance < 500 || (isDistMoreThanThreshold && finalState.data.fareProductType /= FPT.DELIVERY)) && Arr.all (_ == false) [ isLocalStageOn PickUpFarFromCurrentLocation, isLocalStageOn ShortDistance ] && flowType /= "REPEAT_RIDE_FLOW" ) then do
                  let
                    currentStage = if (isDistMoreThanThreshold && finalState.data.fareProductType /= FPT.DELIVERY) then PickUpFarFromCurrentLocation else ShortDistance
                  void $ pure $ updateLocalStage currentStage
                  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { currentStage = currentStage, rideRequestFlow = true, isSearchLocation = SearchLocation, isShorterTrip = response.distance < 500, distance = response.distance, findingQuotesProgress = 0.0 } })
                else do
                  if flowType == "REPEAT_RIDE_FLOW" then liftFlowBT $ logEventWithParams logField_ "ny_user_repeat_ride_flow" "searchId" rideSearchRes.searchId else pure unit
                  void $ pure $ updateLocalStage FindingEstimate
                  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { isRepeatRide = (flowType == "REPEAT_RIDE_FLOW"), searchId = rideSearchRes.searchId, currentStage = FindingEstimate, rideRequestFlow = true, isSearchLocation = SearchLocation, sourcePlaceId = Nothing, destinationPlaceId = Nothing, isShorterTrip = false }, data { source = finalState.data.source, sourceAddress = finalState.data.sourceAddress, nearByDrivers = Nothing } })
                void $ lift $ lift $ toggleLoader false
              Nothing -> pure unit
    homeScreenFlow
  else do
    let
      updatedLocationList = updateLocListWithDistance finalState.data.destinationSuggestions finalState.props.sourceLat finalState.props.sourceLong true finalState.data.config.suggestedTripsAndLocationConfig.locationWithinXDist
      focusOnSource = finalState.props.sourceLat == 0.0 || finalState.props.sourceLong == 0.0
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { locationList = updatedLocationList }, props { isSource = Just focusOnSource, isRideServiceable = true, isSrcServiceable = true, isDestServiceable = true, currentStage = SearchLocationModel } })
    homeScreenFlow


getfeedbackReqs :: RiderRideCompletedScreenState -> String -> FeedbackReq
getfeedbackReqs state audio = (Remote.makeFeedBackReqs (state.ratingCard.rating) (state.rideRatingState.rideId) (state.ratingCard.feedbackText) (Just state.ratingCard.favDriver) audio state.ratingViewState.nightSafety state.ratingViewState.wasOfferedAssistance)

dummyAddressGeometry :: AddressGeometry
dummyAddressGeometry =
  AddressGeometry
    { geometry:
        Geometry
          { location:
              LocationS
                { lat: 0.0
                , lng: 0.0
                }
          }
    }

getFinalAmount :: RideBookingRes -> Int
getFinalAmount (RideBookingRes resp) =
  let
    rideList = resp.rideList

    (RideAPIEntity ride) = (fromMaybe dummyRideAPIEntity (rideList !! 0))
  in
    INT.round $ fromMaybe 0.0 $ fromString (show (fromMaybe 0 ride.computedPrice))

tripDetailsScreenFlow :: FlowBT String Unit
tripDetailsScreenFlow = do
  (GlobalState globalState) <- getState
  logField_ <- lift $ lift $ getLogFields
  expiryTime <- pure $ getExpiryTime globalState.tripDetailsScreen.data.selectedItem.rideEndTimeUTC isForLostAndFound
  categories <- if length globalState.tripDetailsScreen.data.categories > 0
    then pure globalState.tripDetailsScreen.data.categories
    else fetchCategories
  modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen { data {categories = categories}, props { canConnectWithDriver = (expiryTime <= 86400) } }) -- expiryTime < 24hrs or 86400 seconds
  flow <- UI.tripDetailsScreen
  case flow of
    GO_TO_HELPSCREEN -> flowRouter HelpAndSupportScreenFlow
    GO_TO_RIDES -> myRidesScreenFlow
    GO_TO_REPORT_ISSUE_CHAT_SCREEN -> flowRouter IssueReportChatScreenFlow
    GO_TO_RIDE_COMPLETED_SCREEN -> riderRideCompletedScreenFlow
    GO_TO_INVOICE updatedState -> do
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_invoice_clicked"
        $ [ { key: "Pickup", value: unsafeToForeign updatedState.data.selectedItem.source }
          , { key: "Destination", value: unsafeToForeign updatedState.data.selectedItem.destination }
          , { key: "Fare", value: unsafeToForeign updatedState.data.selectedItem.totalAmount }
          , { key: "Status", value: unsafeToForeign updatedState.data.selectedItem.status }
          , { key: "Ride completion timestamp", value: unsafeToForeign updatedState.data.selectedItem.rideEndTime }
          , { key: "Rating", value: (unsafeToForeign $ updatedState.data.selectedItem.rating) }
          ]
      modifyScreenState $ InvoiceScreenStateType (\invoiceScreen -> invoiceScreen { props { fromHomeScreen = false }, data { totalAmount = updatedState.data.totalAmount, date = updatedState.data.date, tripCharges = updatedState.data.totalAmount, selectedItem = updatedState.data.selectedItem, rideType = updatedState.data.selectedItem.rideType} })
      invoiceScreenFlow
    GO_TO_HOME state -> do
      if state.props.fromMyRides == Home then do
        updateLocalStage HomeScreen
      else
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { settingSideBar { opened = SettingSideBarController.CLOSED } }})
      homeScreenFlow
    CONNECT_WITH_DRIVER updatedState -> do
      void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      resp <- Remote.callDriverBT updatedState.data.selectedItem.rideId
      void $ lift $ lift $ toggleLoader false
      config <- getAppConfigFlowBT appConfig
      void $ lift $ lift $ showToast (getString STR.REQUEST_RECEIVED_WE_WILL_CALL_YOU_BACK_SOON)
      void $ Remote.sendIssueBT (Remote.makeSendIssueReq  (Just config.appData.supportMail) (Just updatedState.data.selectedItem.rideId) "LOSTANDFOUND" "LOST AND FOUND" $ Just false)
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {props{fromMyRides = updatedState.props.fromMyRides}})
      tripDetailsScreenFlow
    GET_CATEGORIES_LIST updatedState -> do
      let language = fetchLanguage $ getLanguageLocale languageKey
      (GetCategoriesRes response) <- Remote.getCategoriesBT language
      let categories' = map (\(Category catObj) ->{ categoryName : if (language == "en") then capitalize catObj.category else catObj.category , categoryId : catObj.issueCategoryId, categoryAction : Just catObj.label, categoryImageUrl : Just catObj.logoUrl, isRideRequired : catObj.isRideRequired, maxAllowedRideAge : catObj.maxAllowedRideAge, categoryType : catObj.categoryType, allowedRideStatuses : catObj.allowedRideStatuses}) response.categories
      modifyScreenState $ TripDetailsScreenStateType (\helpAndSupportScreen -> updatedState { data {categories = categories' }, props { fromMyRides = updatedState.props.fromMyRides} } )
      tripDetailsScreenFlow
    GO_TO_ISSUE_CHAT_SCREEN updatedState selectedCategory -> do
      let language = fetchLanguage $ getLanguageLocale languageKey
      currentIssueList <- if (fromMaybe "" selectedCategory.categoryAction) == "RIDE_RELATED"
                              then do
                                (FetchIssueListResp issueListResponse) <- Remote.fetchIssueListBT language
                                let issues = getApiIssueList issueListResponse.issues
                                pure $ getUpdatedIssueList ["OPEN", "PENDING", "RESOLVED", "REOPENED"] issues
                              else pure []
      (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language selectedCategory.categoryId "" updatedState.data.selectedItem.rideId ""
      let options' = mapWithIndex (\index (Option optionObj) -> optionObj{ option = (show (index + 1)) <> ". " <> (reportIssueMessageTransformer optionObj.option)}) getOptionsRes.options
          messages' = mapWithIndex (\index (Message currMessage) -> makeChatComponent' (reportIssueMessageTransformer currMessage.message) currMessage.messageTitle currMessage.messageAction currMessage.label "Bot" (getCurrentUTC "") "Text" (500 * (index + 1)))getOptionsRes.messages
          chats' = map (\(Message currMessage) -> Chat {
                      chatId : currMessage.id,
                      chatType : "IssueMessage",
                      timestamp : getCurrentUTC ""
                    }) getOptionsRes.messages
          categoryName = getString STR.REPORT_AN_ISSUE
          selectedCategory' = selectedCategory  {categoryName = categoryName}
          merchantExoPhone' = if updatedState.data.selectedItem.merchantExoPhone == "" then Nothing else Just updatedState.data.selectedItem.merchantExoPhone
      modifyScreenState $ ReportIssueChatScreenStateType (\ reportIssueChatState -> reportIssueChatState {
        data {
          merchantExoPhone = merchantExoPhone'
        , selectedRide = Just updatedState.data.selectedItem
        , tripId = Just updatedState.data.selectedItem.rideId
        , entryPoint = ReportIssueChatScreenData.TripDetailsScreenEntry
        , chats = chats'
        , selectedCategory = selectedCategory'
        , options = options'
        , chatConfig = ReportIssueChatScreenData.initData.data.chatConfig{messages = messages'} }})
      flowRouter IssueReportChatScreenFlow
  where
    fetchCategories :: FlowBT String (Array CategoryListType)
    fetchCategories = do
      let language = fetchLanguage $ getLanguageLocale languageKey
      (GetCategoriesRes response) <- Remote.getCategoriesBT language
      pure $
        map
          (\(Category catObj) ->
            { categoryName : if (language == "en") then capitalize catObj.category else catObj.category
            , categoryId : catObj.issueCategoryId
            , categoryAction : Just catObj.label
            , categoryImageUrl : Just catObj.logoUrl
            , isRideRequired : catObj.isRideRequired
            , maxAllowedRideAge : catObj.maxAllowedRideAge
            , categoryType: catObj.categoryType
            , allowedRideStatuses: catObj.allowedRideStatuses
            }
          ) response.categories

invoiceScreenFlow :: FlowBT String Unit
invoiceScreenFlow = do
  flow <- UI.invoiceScreen
  (GlobalState newState) <- getState
  case flow of
    InvoiceScreenOutput.GoBack -> do
      tripDetailsScreenFlow
    InvoiceScreenOutput.GoToHome -> homeScreenFlow
  pure unit

contactUsScreenFlow :: FlowBT String Unit
contactUsScreenFlow = do
  flow <- UI.contactUsScreen
  case flow of
    GO_TO_HOME_FROM_CONTACT state -> do
      void $ Remote.sendIssueBT (Remote.makeSendIssueReq (Just state.data.email) Nothing state.data.description state.data.subject $ Just false)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { settingSideBar { opened = SettingSideBarController.CLOSED } } })
      homeScreenFlow
  pure unit

flowRouter :: FlowState -> FlowBT String Unit
flowRouter flowState = case flowState of
  HelpAndSupportScreenFlow -> do
    nextFlow <- UI.helpAndSupportScreen
    flowRouter nextFlow
  IssueReportChatScreenFlow -> do
    nextFlow <- UI.reportIssueChatScreen
    flowRouter nextFlow
  RideSelectionScreenFlow -> do
    nextFlow <- UI.rideSelection
    flowRouter nextFlow
  SelectFaqScreenFlow -> do
    nextFlow <- UI.selectFaqScreen
    flowRouter nextFlow
  FaqScreenFlow -> do
    nextFlow <- UI.faqScreen
    flowRouter nextFlow
  HomeScreenFlow -> homeScreenFlow
  RiderRideCompleted -> riderRideCompletedScreenFlow
  RiderRideEndScreen -> riderRideCompletedScreenFlow
  ActivateSafetyScreenFlow -> activateSafetyScreenFlow
  TripDetailsScreenFlow -> tripDetailsScreenFlow
  ContactUsScreenFlow -> contactUsScreenFlow
  MyRidesScreenFlow -> myRidesScreenFlow
  GoToFavouritesScreenFlow -> savedLocationFlow FaqScreenFlow
  ChangeLanguageScreenFlow -> selectLanguageScreenFlow FaqScreenFlow

myRidesScreenFlow :: FlowBT String Unit
myRidesScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  (GlobalState globalState) <- getState
  modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> myRidesScreen { data { isSrcServiceable = globalState.homeScreen.props.isSrcServiceable }})
  flow <- UI.myRidesScreen
  case flow of
    REFRESH state -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> state { props { fromNavBar = state.props.fromNavBar } })
      myRidesScreenFlow
    TRIP_DETAILS state -> do
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_my_rides_view_details"
        $ [ { key: "Pickup", value: unsafeToForeign state.data.selectedItem.source }
          , { key: "Destination", value: unsafeToForeign state.data.selectedItem.destination }
          , { key: "Fare", value: unsafeToForeign state.data.selectedItem.totalAmount }
          , { key: "Status", value: unsafeToForeign state.data.selectedItem.status }
          , { key: if state.data.selectedItem.status == "CANCELLED" then "Time" else "Ride completion timestamp"
            , value: unsafeToForeign $ if state.data.selectedItem.status == "CANCELLED" then state.data.selectedItem.time else state.data.selectedItem.rideEndTime
            }
          , { key: "Rating", value: (unsafeToForeign $ state.data.selectedItem.rating) }
          ]
      modifyScreenState $ TripDetailsScreenStateType (\tripDetails -> tripDetails { data { vehicleVariant = state.data.selectedItem.vehicleVariant }, props { fromMyRides = MyRides } })
      tripDetailsScreenFlow
    LOADER_OUTPUT state -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> state { data { offsetValue = state.data.offsetValue + 8 }, props { fromNavBar = state.props.fromNavBar } })
      myRidesScreenFlow
    BOOK_RIDE -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { settingSideBar { opened = SettingSideBarController.CLOSED } } })
      homeScreenFlow
    GO_TO_NAV_BAR -> homeScreenFlow
    GO_TO_HELP_SCREEN -> flowRouter HelpAndSupportScreenFlow
    REPEAT_RIDE_FLOW state -> do
      let
        trip = getTripFromRideHistory state
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { rideHistoryTrip = Just trip, settingSideBar { opened = SettingSideBarController.CLOSED } } })
      homeScreenFlow
    GO_TO_RIDE_SCHEDULED_SCREEN state -> do
      let
        selectedCard = state.data.selectedItem
      modifyScreenState $ RideSummaryScreenStateType (\rideSummaryScreen -> RideSummaryScreenData.initData{data{fromScreen = (Screen.getScreen Screen.MY_RIDES_SCREEN),bookingId = Just selectedCard.bookingId}})
      rideSummaryScreenFlow

    MY_RIDES_GO_TO_HOME_SCREEN _ -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
      updateLocalStage HomeScreen
      homeScreenFlow
    NOTIFICATION_HANDLER notification notificationBody -> do
      (GlobalState globalState) <- getState
      fcmHandler notification globalState.homeScreen notificationBody

selectLanguageScreenFlow :: FlowState -> FlowBT String Unit
selectLanguageScreenFlow goBackState = do
  logField_ <- lift $ lift $ getLogFields
  flow <- UI.selectLanguageScreen
  case flow of
    UPDATE_LANGUAGE state -> do
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_lang_selected"
        $ [ { key: "Previous language", value: unsafeToForeign $ getLanguageLocale languageKey }
          , { key: "New language", value: unsafeToForeign state.props.selectedLanguage }
          ]
      void $ pure $ setLanguageLocale state.props.selectedLanguage
      void $ lift $ lift $ liftFlow $ logEventWithParams logField_ "ny_user_lang_selec" "language" (state.props.selectedLanguage)
      let
        langVal = case (state.props.selectedLanguage) of
          "HI_IN" -> "HINDI"
          "EN_US" -> "ENGLISH"
          "KN_IN" -> "KANNADA"
          "BN_IN" -> "BENGALI"
          "ML_IN" -> "MALAYALAM"
          _ -> state.data.config.defaultLanguage
      void $ pure $ setCleverTapUserProp [ { key: "Preferred Language", value: unsafeToForeign langVal } ]
      resp <- lift $ lift $ Remote.updateProfile (Remote.mkUpdateProfileRequest FunctionCall)
      modifyScreenState $ SelectLanguageScreenStateType (\selectLanguageScreen -> SelectLanguageScreenData.initData)
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen { data { categories = [] } })
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen { data { categories = [] }, props { needIssueListApiCall = true } })
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { famousDestinations = [] }})
      homeScreenFlow
    GO_BACK_SCREEN ->
      case goBackState of
        FaqScreenFlow -> flowRouter FaqScreenFlow
        HomeScreenFlow -> homeScreenFlow
        _ -> homeScreenFlow

emergencyScreenFlow :: FlowBT String Unit
emergencyScreenFlow = do
  flow <- UI.emergencyContactsScreen
  let newHomeScreenStage homescreenState = if homescreenState.props.currentStage == ChatWithDriver then homescreenState.props.stageBeforeChatScreen else homescreenState.props.currentStage
  case flow of
    UPDATE_DEFAULT_CONTACTS state -> do
      void $ Remote.emergencyContactsBT $ Remote.postContactsReq state.data.selectedContacts
      if state.props.showInfoPopUp then
          void $ lift $ lift $ showToast (getString STR.CONTACT_REMOVED_SUCCESSFULLY)
        else
          void $ lift $ lift $ showToast $ getString STR.TRUSTED_CONTACS_ADDED_SUCCESSFULLY
      modifyScreenState $ EmergencyContactsScreenStateType (\_ -> state { data { emergencyContactsList = state.data.selectedContacts }, props { showInfoPopUp = false, saveEmergencyContacts = true, getDefaultContacts = true } })
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { data { emergencyContactsList = state.data.selectedContacts }, props { setupStage = ST.SetDefaultEmergencyContacts, showShimmer = true } })
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { safetySettings = Nothing, currentStage = newHomeScreenStage homeScreen, chatcallbackInitiated = false}, data{contactList = Nothing } })
      modifyScreenState $ DataFetchScreenStateType (\ dataFetchScreen -> dataFetchScreen { data { emergencyContactsList = state.data.selectedContacts } })
      nammaSafetyFlow
    POST_CONTACTS_SAFETY state shouldGoToSafetyScreen -> do
      void $ Remote.emergencyContactsBT $ Remote.postContactsReq state.data.selectedContacts
      when (not shouldGoToSafetyScreen)
        $ if state.props.showInfoPopUp then
            void $ lift $ lift $ showToast (getString STR.CONTACT_REMOVED_SUCCESSFULLY)
          else
            void $ lift $ lift $ showToast $ getString STR.TRUSTED_CONTACS_ADDED_SUCCESSFULLY
      modifyScreenState $ EmergencyContactsScreenStateType (\_ -> state { data { emergencyContactsList = state.data.selectedContacts }, props { showInfoPopUp = false, saveEmergencyContacts = false, getDefaultContacts = true } })
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { data { emergencyContactsList = state.data.selectedContacts }, props { setupStage = ST.SetDefaultEmergencyContacts, showShimmer = true } })
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { safetySettings = Nothing, currentStage = newHomeScreenStage homeScreen, chatcallbackInitiated = false}, data{contactList = Nothing } })
      modifyScreenState $ DataFetchScreenStateType (\ dataFetchScreen -> dataFetchScreen { data { emergencyContactsList = state.data.selectedContacts } })
      emergencyScreenFlow --dataFetchScreenFlow  (DataExplainWithFetchSD.stageData $ TrustedContacts []) 0
    POST_CONTACTS state shouldGoToSafetyScreen -> updateEmergencyContacts state shouldGoToSafetyScreen
    GET_CONTACTS state -> do
      (GetEmergContactsResp res) <- Remote.getEmergencyContactsBT GetEmergContactsReq
      let
        contacts =
          getDefaultPriorityList
            $ map
                ( \(ContactDetails item) ->
                    { number: item.mobileNumber
                    , name: item.name
                    , isSelected: true
                    , enableForFollowing: fromMaybe false item.enableForFollowing
                    , enableForShareRide: fromMaybe false item.enableForShareRide
                    , shareTripWithEmergencyContactOption: EmergencyContactsScreenData.getRideOptionFromKeyEM $ fromMaybe API.NEVER_SHARE item.shareTripWithEmergencyContactOption
                    , onRide: fromMaybe false item.onRide
                    , priority: fromMaybe 1 item.priority
                    , contactPersonId : item.contactPersonId
                    , isFollowing : Nothing
                    , notifiedViaFCM : item.notifiedViaFCM
                    }
                )
                res.defaultEmergencyNumbers
      modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactsScreen -> state { data { emergencyContactsList = contacts } })
      emergencyScreenFlow
    REFRESH_EMERGECY_CONTACTS_SCREEN state -> do
      modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactsScreen -> state)
      emergencyScreenFlow
    GO_TO_SELECT_CONTACT state -> do
      selectContactsFlow (\contacts -> do
          let validSelectedContacts =
                ( mapWithIndex
                    ( \index contact ->
                        if ((DS.length contact.number) > 10 && (DS.length contact.number) <= 12 && ((DS.take 1 contact.number) == "0" || (DS.take 2 contact.number) == "91")) then
                          contact { number = DS.drop ((DS.length contact.number) - 10) contact.number, priority = boolToInt $ index /= 0 }
                        else
                          contact { priority = boolToInt $ index /= 0 }
                    )
                    contacts
                )
          void $ pure $ hideKeyboardOnNavigation true
          let updatedState = state { data { editedText = "", selectedContacts = getDefaultPriorityList validSelectedContacts }, props { showContactList = false } }
          updateEmergencyContacts updatedState false
          pure unit
        ) state.data.selectedContacts 3
      emergencyScreenFlow
  where
    updateEmergencyContacts :: ST.EmergencyContactsScreenState -> Boolean -> FlowBT String Unit
    updateEmergencyContacts state shouldGoToSafetyScreen = do
      let newHomeScreenStage homescreenState = if homescreenState.props.currentStage == ChatWithDriver then homescreenState.props.stageBeforeChatScreen else homescreenState.props.currentStage
      void $ Remote.emergencyContactsBT $ Remote.postContactsReq state.data.selectedContacts
      when (not shouldGoToSafetyScreen)
        $ if state.props.showInfoPopUp then do
            void $ lift $ lift $ showToast (getString STR.CONTACT_REMOVED_SUCCESSFULLY)
          else
            void $ lift $ lift $ showToast $  getString STR.TRUSTED_CONTACS_ADDED_SUCCESSFULLY
      let contactsCount = length state.data.selectedContacts
      if contactsCount < 1
        then
          modifyScreenState $ EmergencyContactsScreenStateType (\_ -> EmergencyContactsScreenData.initData{props { fromNewSafetyFlow= true, saveEmergencyContacts = true } })
        else do
          modifyScreenState $ EmergencyContactsScreenStateType (\_ -> state { data { emergencyContactsList = state.data.selectedContacts }, props { showInfoPopUp = false } })
          modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { data { emergencyContactsList = state.data.selectedContacts }, props { setupStage = ST.SetDefaultEmergencyContacts, showShimmer = true } })
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { safetySettings = Nothing, currentStage = newHomeScreenStage homeScreen, chatcallbackInitiated = false}, data{contactList = Nothing } })
          modifyScreenState $ DataFetchScreenStateType (\ dataFetchScreen -> dataFetchScreen { data { emergencyContactsList = state.data.selectedContacts } })
      (GlobalState globalState) <- getState
      case globalState.nammaSafetyScreen.data.hasCompletedSafetySetup, shouldGoToSafetyScreen, state.props.fromSosFlow, state.props.fromNewSafetyFlow of
        _, _, true, _ -> activateSafetyScreenFlow
        true, true, _, _ -> safetySettingsFlow
        _, _, _, _ -> emergencyScreenFlow

aboutUsScreenFlow :: FlowBT String Unit
aboutUsScreenFlow = do
  flow <- UI.aboutUsScreen
  case flow of
    GO_TO_HOME_FROM_ABOUT -> homeScreenFlow

driverProfileScreenFlow :: FlowBT String Unit
driverProfileScreenFlow = do
  flow <- UI.driverProfileScreen
  case flow of
    _ -> homeScreenFlow

permissionScreenFlow :: FlowBT String Unit
permissionScreenFlow = do
  void $ pure $ hideKeyboardOnNavigation true
  flow <- UI.permissionScreen
  permissionConditionA <- lift $ lift $ liftFlow $ isLocationPermissionEnabled unit
  permissionConditionB <- lift $ lift $ liftFlow $ isLocationEnabled unit
  internetCondition <- lift $ lift $ liftFlow $ isInternetAvailable unit
  case flow of
    REFRESH_INTERNET -> do
      if internetCondition then
        currentFlowStatus false
      else do
        modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen { stage = INTERNET_ACTION })
        permissionScreenFlow
    TURN_ON_GPS ->
      if not internetCondition then do
        modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen { stage = INTERNET_ACTION })
        permissionScreenFlow
      else do
        setValueToLocalStore PERMISSION_POPUP_TIRGGERED "true"
        currentFlowStatus false
    TURN_ON_INTERNET -> case (getValueToLocalStore USER_NAME == "__failed") of
      true -> pure unit
      _ ->
        if os == "IOS" && not permissionConditionB then do
          modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen { stage = LOCATION_DENIED })
          permissionScreenFlow
        else if not (permissionConditionA && permissionConditionB) then do
          modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen { stage = LOCATION_DISABLED })
          permissionScreenFlow
        else do
          case getGlobalPayload Constants.globalPayload of
            Just payload -> do
              liftFlowBT $ terminateUI $ Just "PermissionScreen"
              baseAppFlow payload false
            Nothing -> permissionScreenFlow

myProfileScreenFlow :: FlowBT String Unit
myProfileScreenFlow = do
  disabilityListT <- updateDisabilityList "My_Profile_Screen"
  modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState -> myProfileScreenState { data { disabilityOptions { disabilityOptionList = disabilityListT }, editedDisabilityOptions { disabilityOptionList = disabilityListT } } })
  flow <- UI.myProfileScreen
  case flow of
    UPDATE_USER_PROFILE state -> do
      void $ pure $ toggleBtnLoader "" false
      void $ pure $ spy "profile_updated_state" state
      let
        stringName = seperateByWhiteSpaces (state.data.editedName)

        name = split (Pattern " ") stringName

        nameLength = length name

        gender = getGenderValue state.data.editedGender

        email = if state.data.editedEmailId == state.data.emailId || (state.data.editedEmailId == Just "") then Nothing else state.data.editedEmailId

        disability = case state.data.editedDisabilityOptions.selectedDisability of
          Just disability ->
            if (state.data.editedDisabilityOptions.activeIndex == 1) then
              Just (Remote.mkDisabilityData disability (fromMaybe "" state.data.editedDisabilityOptions.otherDisabilityReason))
            else
              Nothing
          _ -> Nothing

        hasDisability = if state.props.changeAccessibility then (Just (isJust disability)) else Nothing
      resp <-
        if nameLength > 2 then
          lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (name !! 1) (name !! (nameLength - 1)) (email) gender hasDisability disability)
        else if nameLength == 2 then
          lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (Just "") (name !! 1) (email) gender hasDisability disability)
        else if nameLength == 1 then
          lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (Just "") (Just "") (email) gender hasDisability disability)
        else
          lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (Just "") (Just "") (Just "") (email) gender hasDisability disability)
      case resp of
        Right response -> do
          setValueToLocalStore USER_NAME stringName
          void $ pure $ setCleverTapUserProp [{key : "Name", value : unsafeToForeign stringName}]
          let
            tag = case disability of
              Just (Disability value) -> value.tag
              Nothing -> ""
          modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen { data { disability = Just { id: "", tag: tag, description: "" } } })
          case gender of
            Just gender -> do
              void $ pure $ setCleverTapUserProp [{key : "gender", value : unsafeToForeign gender}]
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { settingSideBar { gender = Just gender } }, props { isBanner = false } })
            _ -> pure unit
          case email of
            Just email -> do
              void $ pure $ setCleverTapUserProp [{key : "email", value : unsafeToForeign email}]
              setValueToLocalStore USER_EMAIL email
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { settingSideBar { email = Just email } } })
            _ -> pure unit
          modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState -> MyProfileScreenData.initData)
          myProfileScreenFlow
        Left err -> do
          let
            errResponse = err.response
          let
            codeMessage = decodeError errResponse.errorMessage "errorCode"
          case codeMessage of
            "PERSON_EMAIL_ALREADY_EXISTS" -> do
              pure $ setText (getNewIDWithTag "EmailEditText") ""
              modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState -> myProfileScreenState { props { isEmailValid = false, updateProfile = true }, data { emailErrorMessage = Just EMAIL_EXISTS, name = state.data.name, editedName = state.data.editedName, emailId = state.data.emailId, gender = state.data.gender, editedGender = state.data.editedGender } })
            _ -> void $ lift $ lift $ showToast (getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
          myProfileScreenFlow
      myProfileScreenFlow
    GO_TO_HOME_ -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { settingSideBar { opened = SettingSideBarController.CLOSED } } })
      homeScreenFlow

favouriteDriverTripFlow :: FlowBT String Unit
favouriteDriverTripFlow = do
  act <- UI.favouriteDriverTrips
  case act of
    GO_TO_FAVOURITE_DRIVER_PROFILE state -> do
      modifyScreenState $ DriverProfileScreenCommonStateType ( \driverProfileScreen -> driverProfileScreen { props { driverId = fromMaybe "" state.data.driverId } } )
      driverProfileScreenFlow
    GO_BACK_TO_SAVED_LOCATION state -> do
      void $ lift $ lift $ toggleLoader true
      resp <- lift $ lift $ Remote.removeFavouriteDriver (fromMaybe "" state.data.driverId)
      void $ lift $ lift $ delay $ Milliseconds 2000.0
      void $ lift $ lift $ toggleLoader false
      case resp of
        Right resp -> do
          savedLocationFlow HomeScreenFlow
        Left _ -> do
          void $ void $ lift $ lift $ showToast $  getString STR.FAILED_TO_REMOVE_DRIVER
          savedLocationFlow HomeScreenFlow
  pure unit

savedLocationFlow :: FlowState -> FlowBT String Unit
savedLocationFlow goBackState = do
  void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)
  flow <- UI.savedLocationScreen
  (SavedLocationsListRes savedLocationResp) <- FlowCache.updateAndFetchSavedLocationsBT SavedLocationReq false
  case flow of
    GO_TO_FAV_DRIVER_PROFILE id -> do
      modifyScreenState $ DriverProfileScreenCommonStateType ( \driverProfileScreen -> driverProfileScreen { props { driverId = id } } )
      driverProfileScreenFlow
    GOTO_FAVOURITEDRIVERS_LIST state -> do
      modifyScreenState $  FavouriteDriverTripsStateType (\favouriteDriverListScreen -> favouriteDriverListScreen { data { driverNumber = state.data.driverNo, driverName = state.data.driverName, driverId = state.data.driverId}})
      favouriteDriverTripFlow

    ADD_NEW_LOCATION state -> do
      (GlobalState newState) <- getState
      resp <- lift $ lift $ getRecentSearches newState.addNewAddressScreen
      let
        currentGeoHash = runFn3 encodeGeohash (fromMaybe 0.0 $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LAT) (fromMaybe 0.0 $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LON) newState.homeScreen.data.config.suggestedTripsAndLocationConfig.geohashPrecision

        suggestionsMap = getSuggestionsMapFromLocal FunctionCall

        suggestionsObject = (fromMaybe dummySuggestionsObject (getSuggestedRidesAndLocations currentGeoHash suggestionsMap newState.homeScreen.data.config.suggestedTripsAndLocationConfig.geohashLimitForMap))

        suggestedDestinationsArr = (differenceOfLocationLists suggestionsObject.destinationSuggestions (AddNewAddress.getSavedLocations savedLocationResp.list))
      let
        recents =
          map
            (\item -> item { postfixImageUrl = "", postfixImageVisibility = false })
            suggestedDestinationsArr
      modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen { data { savedLocations = getSavedLocationForAddNewAddressScreen state.data.savedLocations, locationList = recents, placeName = "", address = "", addressSavedAs = "", recentSearchs { predictionArray = recents }, savedTags = (AddNewAddress.getSavedTags savedLocationResp.list) }, props { showSavePlaceView = false, editLocation = false, isLocationServiceable = true, isSearchedLocationServiceable = true, isLocateOnMap = false, fromHome = false, fromScreen = Screen.getScreen Screen.SAVED_LOCATION_SCREEN } })
      case (AddNewAddress.validTag (AddNewAddress.getSavedTags savedLocationResp.list) "HOME" ""), (AddNewAddress.validTag (AddNewAddress.getSavedTags savedLocationResp.list) "WORK" "") of
        false, false -> modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen { data { activeIndex = (Just 2), selectedTag = (Just OTHER_TAG) }, props { editSavedLocation = false } })
        _, _ -> modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen { data { activeIndex = Nothing, selectedTag = Nothing }, props { editSavedLocation = false } })
      addNewAddressScreenFlow
    DELETE_LOCATION tagName -> do
      resp <- Remote.deleteSavedLocationBT (DeleteSavedLocationReq (trim tagName))
      void $ FlowCache.updateAndFetchSavedLocations true
      void $ lift $ lift $ showToast (getString STR.FAVOURITE_REMOVED_SUCCESSFULLY)
      setValueToLocalStore RELOAD_SAVED_LOCATION "true"
      savedLocationFlow HomeScreenFlow
    EDIT_LOCATION cardState -> do
      (ServiceabilityRes serviceabilityRes) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq (fromMaybe 0.0 cardState.lat) (fromMaybe 0.0 cardState.lon)) ORIGIN
      let
        savedLocs = AddNewAddress.getSavedLocations savedLocationResp.list
      updateSavedLocations savedLocs
      modifyScreenState
        $ AddNewAddressScreenStateType
            ( \addNewAddressScreen ->
                addNewAddressScreen
                  { props
                    { tagExists = false
                    , showSavePlaceView = true
                    , editLocation = true
                    , editSavedLocation = true
                    , isBtnActive = false
                    , isLocateOnMap = false
                    , isLocationServiceable = (serviceabilityRes.serviceable)
                    , fromHome = false
                    , fromScreen = Screen.getScreen Screen.SAVED_LOCATION_SCREEN
                    }
                  , data
                    { existsAs = ""
                    , selectedTag = getCardType (fromMaybe "" (cardState.cardType))
                    , placeName = cardState.tagName
                    , savedLocations = savedLocs
                    , address = cardState.savedLocation
                    , addressSavedAs = cardState.tagName
                    , selectedItem
                      { title = (fromMaybe "" ((split (Pattern ",") (cardState.savedLocation)) !! 0))
                      , description = cardState.savedLocation
                      , lat = cardState.lat
                      , lon = cardState.lon
                      , placeId = cardState.placeId
                      , subTitle = (drop ((fromMaybe 0 (indexOf (Pattern ",") (cardState.savedLocation))) + 2) (cardState.savedLocation))
                      }
                    , savedTags = (AddNewAddress.getSavedTags savedLocationResp.list)
                    , lat = fromMaybe 0.0 cardState.lat
                    , lon = fromMaybe 0.0 cardState.lon
                    , latSelectedFromMap = fromMaybe 0.0 cardState.lat
                    , lonSelectedFromMap = fromMaybe 0.0 cardState.lon
                    , locSelectedFromMap = ""
                    , activeIndex =
                      case (getCardType (fromMaybe "" (cardState.cardType))) of
                        Just card -> case card of
                          HOME_TAG -> Just 0
                          WORK_TAG -> Just 1
                          OTHER_TAG -> Just 2
                        Nothing -> Nothing
                    }
                  }
            )
      addNewAddressScreenFlow
    GO_BACK_FROM_SAVED_LOCATION -> do
      void $ lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreen")
      case goBackState of
        FaqScreenFlow -> flowRouter FaqScreenFlow
        HomeScreenFlow -> homeScreenFlow
        _ -> homeScreenFlow
  pure unit

addNewAddressScreenFlow :: FlowBT String Unit
addNewAddressScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  flow <- UI.addNewAddressScreen
  case flow of
    SEARCH_ADDRESS input state -> do
      (GlobalState newState) <- getState
      let sourceLat =  fromMaybe newState.homeScreen.props.sourceLat $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LAT
          sourceLong =  fromMaybe newState.homeScreen.props.sourceLong $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LON
      (SearchLocationResp searchLocationResp) <- Remote.searchLocationBT (Remote.makeSearchLocationReq input sourceLat sourceLong (EHC.getMapsLanguageFormat (getLanguageLocale languageKey)) "" defaultCityConfig.geoCodeConfig Nothing "" Nothing)
      let
        sortedByDistanceList = sortPredictionByDistance searchLocationResp.predictions

        predictionList = AddNewAddress.getLocationList sortedByDistanceList

        recentLists = state.data.recentSearchs.predictionArray

        filteredRecentsList = filterRecentSearches recentLists predictionList

        filteredPredictionList = differenceOfLocationLists predictionList filteredRecentsList
      modifyScreenState
        $ AddNewAddressScreenStateType
            ( \addNewAddressScreen ->
                state
                  { data
                    { locationList =
                      map
                        ( \item ->
                            item
                              { postfixImageVisibility = (not (checkPrediction item state.data.savedLocations))
                              , postfixImageUrl = if (checkPrediction item state.data.savedLocations) then "" else fetchImage FF_ASSET "ny_ic_fav_red"
                              , isClickable = (checkPrediction item state.data.savedLocations)
                              , alpha = if (checkPrediction item state.data.savedLocations) then 1.0 else 0.5
                              }
                        )
                        (filteredPredictionList <> filteredRecentsList)
                    }
                  }
            )
      addNewAddressScreenFlow
    ADD_LOCATION state -> do
      if (state.props.editSavedLocation) then do
        void $ Remote.deleteSavedLocationBT (DeleteSavedLocationReq (trim state.data.placeName))
        pure unit
      else
        pure unit
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_favourite_added"
        $ [ { key: "Address", value: unsafeToForeign state.data.address }
          , { key: "Tag", value: unsafeToForeign state.data.selectedTag }
          ]
      (GetPlaceNameResp sourcePlace) <- getPlaceNameResp (state.data.selectedItem.title <> ", " <> state.data.selectedItem.subTitle) state.data.selectedItem.placeId (fromMaybe 0.0 state.data.selectedItem.lat) (fromMaybe 0.0 state.data.selectedItem.lon) state.data.selectedItem
      let
        source = state.data.selectedItem.description

        (PlaceName sourceAddressGeometry) = (fromMaybe HomeScreenData.dummyLocationName (sourcePlace !! 0))

        (LatLong sourceLocation) = (sourceAddressGeometry.location)

        lat = sourceLocation.lat

        lng = sourceLocation.lon

        newstate =
          state
            { data
              { lat = lat
              , lon = lng
              , selectedItem
                { description = source
                , lat = Just lat
                , lon = Just lng
                }
              , addressComponents = sourceAddressGeometry.addressComponents
              }
            }
      resp <- Remote.addSavedLocationBT (AddNewAddress.encodeAddressDescription newstate)
      void $ FlowCache.updateAndFetchSavedLocations true
      if state.props.editSavedLocation then
        void $ lift $ lift $ showToast (getString STR.FAVOURITE_UPDATED_SUCCESSFULLY)
      else
        void $ lift $ lift $ showToast (getString STR.FAVOURITE_ADDED_SUCCESSFULLY)
      setValueToLocalStore RELOAD_SAVED_LOCATION "true"
      void $ lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      let
        homeScreen = Screen.getScreen Screen.HOME_SCREEN

        searchLocationScreen = Screen.getScreen Screen.SEARCH_LOCATION_SCREEN
      if state.props.fromHome || state.props.fromScreen == homeScreen || state.props.fromScreen == searchLocationScreen then do
        (GlobalState globalState) <- getState
        (SavedLocationsListRes savedLocationResp) <- FlowCache.updateAndFetchSavedLocations false
        let
          updatedLocationList = getUpdatedLocationList globalState.homeScreen.data.locationList state.data.selectedItem.placeId
        modifyScreenState
          $ HomeScreenStateType
              ( \homeScreen ->
                  homeScreen
                    { data
                      { settingSideBar { opened = SettingSideBarController.CLOSED }
                      , locationList = updatedLocationList
                      , savedLocations = (AddNewAddress.getSavedLocations savedLocationResp.list)
                      }
                    }
              )
        when (not state.props.fromHome)
          $ do
              void $ lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag if os == "IOS" then "CustomerHomeScreenMap" else "CustomerHomeScreen")
        case state.props.fromScreen of
          homeScreen -> do
            void $ lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreen")
            homeScreenFlow
          searchLocationScreen -> do
            searchLocationFlow
          _ -> do
            void $ lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreen")
            homeScreenFlow
      else
        savedLocationFlow HomeScreenFlow
    UPDATE_LOCATION_NAME_ADDRESS state lat lon -> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq lat lon) ORIGIN
      let
        isServiceable = sourceServiceabilityResp.serviceable

        (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)

        pickUpPoints = mapSpecialZoneGates srcSpecialLocation.gatesInfo

        geoJson = transformGeoJsonFeature srcSpecialLocation.geoJson srcSpecialLocation.gatesInfo

        gateAddress =
          if DS.null state.props.defaultPickUpPoint then
            HomeScreenData.dummyLocation
          else
            fromMaybe HomeScreenData.dummyLocation (Arr.find (\pickupPoint -> pickupPoint.place == state.props.defaultPickUpPoint) pickUpPoints)
      if not (DS.null geoJson) && not (null pickUpPoints) && (geoJson /= state.data.polygonCoordinates || pickUpPoints /= state.data.nearByPickUpPoints) then do
        modifyScreenState
          $ AddNewAddressScreenStateType
              ( \addNewAddressScreen ->
                  addNewAddressScreen
                    { data
                      { polygonCoordinates = geoJson
                      , nearByPickUpPoints = pickUpPoints
                      }
                    , props
                      { isSpecialZone = not (DS.null geoJson)
                      , isServiceable = isServiceable
                      }
                    }
              )
        void $ pure $ removeAllPolylines ""
        liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { lat = lat, lon = lon, geoJson = geoJson, points = pickUpPoints, labelId = getNewIDWithTag "AddAddressPin", locationName = srcSpecialLocation.locationName }
        addNewAddressScreenFlow
      else do
        fullAddress <- getPlaceName lat lon gateAddress true
        case fullAddress of
          Just (PlaceName address) -> do
            modifyScreenState
              $ AddNewAddressScreenStateType
                  ( \addNewAddressScreen ->
                      addNewAddressScreen
                        { data
                          { locSelectedFromMap = address.formattedAddress
                          , latSelectedFromMap = lat
                          , lonSelectedFromMap = lon
                          }
                        , props { isServiceable = isServiceable }
                        }
                  )
          Nothing -> void $ void $ lift $ lift $ showToast $  getString STR.SOMETHING_WENT_WRONG_TRY_AGAIN_LATER
        addNewAddressScreenFlow
    GO_TO_FAVOURITES -> do
      void $ lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreen")
      savedLocationFlow HomeScreenFlow
    CHECK_LOCATION_SERVICEABILITY state locItemType -> do
      void $ pure $ spy "Inside CHECK_LOCATION_SERVICEABILITY" state
      let
        item = state.data.selectedItem
      if item.locationItemType /= Just RECENTS then do
        (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (item.title <> ", " <> item.subTitle) item.placeId (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon) item
        let
          (PlaceName placeName) = (fromMaybe HomeScreenData.dummyLocationName (placeNameResp !! 0))
        let
          (LatLong placeLatLong) = (placeName.location)
        (ServiceabilityRes serviceabilityRes) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq placeLatLong.lat placeLatLong.lon) ORIGIN
        case (serviceabilityRes.serviceable), (state.props.editLocation) of
          true, isEditLocation ->
            modifyScreenState
              $ AddNewAddressScreenStateType
                  ( \addNewAddressScreen ->
                      addNewAddressScreen
                        { data
                          { address = item.description
                          , selectedItem = item
                          , selectedTag =
                            if isEditLocation then
                              addNewAddressScreen.data.selectedTag
                            else
                              Nothing
                          , addressSavedAs =
                            case isEditLocation of
                              true ->
                                if (toLower state.data.placeName /= "home" && toLower state.data.placeName /= "work") then
                                  state.data.addressSavedAs
                                else
                                  state.data.placeName
                              _ -> addNewAddressScreen.data.addressSavedAs
                          }
                        , props
                          { isSearchedLocationServiceable = true
                          , showSavePlaceView = true
                          , tagExists = false
                          , isLocateOnMap = false
                          , isBtnActive = isEditLocation
                          }
                        }
                  )
          _, _ -> do
            pure $ setText (getNewIDWithTag "SavedLocationEditText") item.description
            void $ pure $ hideKeyboardOnNavigation true
            modifyScreenState
              $ AddNewAddressScreenStateType
                  ( \addNewAddressScreen ->
                      addNewAddressScreen
                        { props
                          { isSearchedLocationServiceable = false
                          , isLocateOnMap = false
                          , showSavePlaceView = false
                          }
                        , data
                          { recentSearchs { predictionArray = state.data.recentSearchs.predictionArray }
                          , address = item.description
                          }
                        }
                  )
            addNewAddressScreenFlow
        updateDistanceInfo state (Just placeLatLong.lat) (Just placeLatLong.lon)
      else do
        let
          recentItem = (fromMaybe dummyLocationListItemState ((filter (\(recent) -> (recent.placeId) == (item.placeId)) (state.data.recentSearchs.predictionArray)) !! 0))
        modifyScreenState
          $ AddNewAddressScreenStateType
              ( \addNewAddressScreen ->
                  addNewAddressScreen
                    { data
                      { address = item.description
                      , selectedItem = item
                      , selectedTag =
                        if state.props.editLocation then
                          addNewAddressScreen.data.selectedTag
                        else
                          Nothing
                      , addressSavedAs =
                        case state.props.editLocation of
                          true ->
                            if (toLower state.data.placeName /= "home" && toLower state.data.placeName /= "work") then
                              state.data.addressSavedAs
                            else
                              state.data.placeName
                          _ -> addNewAddressScreen.data.addressSavedAs
                      }
                    , props
                      { isSearchedLocationServiceable = true
                      , showSavePlaceView = true
                      , tagExists = false
                      , isLocateOnMap = false
                      , isBtnActive = state.props.editLocation
                      }
                    }
              )
        updateDistanceInfo state recentItem.lat recentItem.lon
    GO_TO_HOME_SCREEN_FLOW -> do
      void $ lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreen")
      homeScreenFlow
    GO_TO_SEARCH_LOC_SCREEN -> do
      void $ lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "SearchLocationScreenMap")
      searchLocationFlow
  pure unit

referralScreenFlow :: FlowBT String Unit
referralScreenFlow = do
  modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { referralCode = getValueToLocalStore CUSTOMER_REFERRAL_CODE })
  flow <- UI.referralScreen
  case flow of
    UPDATE_REFERRAL referralCode -> do
      referralAppliedStatus <- applyReferralCode referralCode
      case referralAppliedStatus of
        REFERRAL_APPLIED -> do
          modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { showThanks = true, referralComponentProps { stage = ST.APPLIED_POPUP } })
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { isReferred = true } })
        REFERRAL_INVALID -> modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { isInvalidCode = true, referralComponentProps { isInvalidCode = true } })
        _ -> pure unit
      referralScreenFlow
    BACK_TO_HOME -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> ReferralScreen.initData)
      void $ lift $ lift $ liftFlow $ adjustViewWithKeyboard "true"
      void $ pure $ hideKeyboardOnNavigation true
      homeScreenFlow

isForLostAndFound :: Boolean
isForLostAndFound = true

checkAndUpdateSavedLocations :: HomeScreenState -> FlowBT String Unit
checkAndUpdateSavedLocations state = do
  when ((getValueToLocalStore RELOAD_SAVED_LOCATION == "true") || state.props.currentStage == HomeScreen)
    $ do
        (SavedLocationsListRes savedLocationResp) <- FlowCache.updateAndFetchSavedLocations false
        fetchAndModifyLocationLists $ AddNewAddress.getSavedLocations savedLocationResp.list
        pure unit
  pure unit

addLocationToRecents :: LocationListItemState -> HomeScreenState -> Boolean -> Boolean -> FlowBT String Unit
addLocationToRecents item state srcServiceable destServiceable = do
  (GlobalState currentState) <- getState
  let
    serviceable = if (state.props.isSource == Just false) then destServiceable else srcServiceable

    lat = if (state.props.isSource == Just false) then state.props.destinationLat else state.props.sourceLat

    lon = if (state.props.isSource == Just false) then state.props.destinationLong else state.props.sourceLong

    latLong = case item.locationItemType of
      Just PREDICTION -> { latitude: lat, longitude: lon }
      _ -> { latitude: (fromMaybe 0.0 item.lat), longitude: (fromMaybe 0.0 item.lon) }
  saveToRecents item latLong.latitude latLong.longitude serviceable
  when (state.props.isSource == Just false)
    $ do
        setSuggestionsMapInLocal item currentState.homeScreen.props.sourceLat currentState.homeScreen.props.sourceLong latLong.latitude latLong.longitude serviceable state.data.config
  pure unit

saveToRecents :: LocationListItemState -> Number -> Number -> Boolean -> FlowBT String Unit
saveToRecents item lat lon serviceability = do
  (GlobalState currentState) <- getState
  recentPredictionsObject <- lift $ lift $ getObjFromLocal currentState.homeScreen
  when (serviceability && lat /= 0.0 && lon /= 0.0 && isJust item.lat && isJust item.lon)
    $ do
        modifyScreenState $ GlobalPropsType (\globalProps -> globalProps { recentSearches = addToRecentSearches item { lat = Just lat, lon = Just lon, locationScore = Just 0.0 } recentPredictionsObject.predictionArray })
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { recentSearchs { predictionArray = addToRecentSearches item { lat = Just lat, lon = Just lon, locationScore = Just 0.0 } recentPredictionsObject.predictionArray } } })
        (GlobalState modifiedState) <- getState
        void $ pure $ saveObject "RECENT_SEARCHES" modifiedState.homeScreen.data.recentSearchs
        pure unit

setSuggestionsMapInLocal :: LocationListItemState -> Number -> Number -> Number -> Number -> Boolean -> AppConfig -> FlowBT String Unit
setSuggestionsMapInLocal item srcLat srcLon lat lon serviceability config = do
  when (serviceability && lat /= 0.0 && lon /= 0.0)
    $ do
        let
          currentSourceGeohash = runFn3 encodeGeohash srcLat srcLon config.suggestedTripsAndLocationConfig.geohashPrecision

          destinationWithLatLong = item { lat = Just lat, lon = Just lon }

          currentMap = getSuggestionsMapFromLocal FunctionCall

          updatedMap = addOrUpdateSuggestedDestination currentSourceGeohash destinationWithLatLong currentMap config.suggestedTripsAndLocationConfig
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { suggestionsData { suggestionsMap = updatedMap } } })
        void $ pure $ setSuggestionsMap updatedMap
        pure unit

fetchAndModifyLocationLists :: Array (LocationListItemState) -> FlowBT String Unit
fetchAndModifyLocationLists savedLocationResp = do
  (GlobalState currentState) <- getState
  let
    state = currentState.homeScreen

    suggestionsConfig = state.data.config.suggestedTripsAndLocationConfig
  recentPredictionsObject <- lift $ lift $ getObjFromLocal currentState.homeScreen
  let
    { savedLocationsWithOtherTag
    , recentlySearchedLocations
    , suggestionsMap
    , trips
    , suggestedDestinations
    } = getHelperLists savedLocationResp recentPredictionsObject currentState.homeScreen state.props.sourceLat state.props.sourceLong
  updateSavedLocations savedLocationResp
  modifyScreenState $ GlobalPropsType (\globalProps -> globalProps { cachedSearches = suggestedDestinations, recentSearches = recentlySearchedLocations, savedLocations = savedLocationResp })
  modifyScreenState $ SearchLocationScreenStateType (\slsState -> slsState { data { locationList = suggestedDestinations } })
  modifyScreenState
    $ HomeScreenStateType
        ( \homeScreen ->
            homeScreen
              { data
                { savedLocations = savedLocationResp
                , recentSearchs { predictionArray = recentlySearchedLocations }
                , locationList = suggestedDestinations
                , destinationSuggestions = suggestedDestinations
                , suggestionsData { suggestionsMap = suggestionsMap }
                , tripSuggestions = removeDuplicateTrips trips suggestionsConfig.destinationGeohashPrecision
                }
              }
        )

addLocToCurrLoc :: Number -> Number -> String -> FlowBT String Unit
addLocToCurrLoc lat lon name = do
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { previousCurrentLocations { pastCurrentLocations = addToPrevCurrLoc { lat: lat, lon: lon, placeName: name } homeScreen.data.previousCurrentLocations.pastCurrentLocations } } })
  (GlobalState modifiedState) <- getState
  void $ pure $ saveCurrentLocations "PREVIOUS_CURRENT_LOCATION" modifiedState.homeScreen.data.previousCurrentLocations
  pure unit

getDistanceInfo :: Array LocationListItemState -> String -> Number -> Number -> String -> FlowBT String { tagExists :: Boolean, locExistsAs :: String }
getDistanceInfo savedLocations excludeLocation lat lon placeId = do
  distArr <- pure $ ((AddNewAddress.calculateDistance savedLocations excludeLocation lat lon))
  rslt <- pure $ ((AddNewAddress.isValidLocation savedLocations excludeLocation placeId))
  let
    placeIdExists = (fromMaybe { locationName: "", distanceDiff: 1.0 } ((rslt) !! 0))

    minDist = ((fromMaybe { locationName: "", distanceDiff: 1.0 } ((distArr) !! 0)))

    locExistsAs = case placeIdExists.locationName /= "", minDist.distanceDiff <= 0.020 of
      true, _ -> placeIdExists.locationName
      false, true -> minDist.locationName
      _, _ -> ""

    tagExists = ((length rslt) > 0 || minDist.distanceDiff <= 0.020)
  pure $ { tagExists, locExistsAs }

updateDistanceInfo :: AddNewAddressScreenState -> Maybe Number -> Maybe Number -> FlowBT String Unit
updateDistanceInfo state lat lon = do
  distanceInfo <- getDistanceInfo state.data.savedLocations (if state.props.editLocation then state.data.placeName else "") (fromMaybe 0.0 lat) (fromMaybe 0.0 lon) (fromMaybe "" state.data.selectedItem.placeId)
  modifyScreenState
    $ AddNewAddressScreenStateType
        ( \addNewAddressScreen ->
            addNewAddressScreen
              { props
                { tagExists = distanceInfo.tagExists
                , isLocateOnMap = false
                , showSavePlaceView = true
                , isBtnActive = false
                }
              , data
                { selectedTag = state.data.selectedTag
                , activeIndex = state.data.activeIndex
                , existsAs = distanceInfo.locExistsAs
                }
              }
        )
  addNewAddressScreenFlow

dummyLocationListItemState :: LocationListItemState
dummyLocationListItemState = locationListStateObj { locationItemType = Just PREDICTION }

removeChatService :: String -> FlowBT String Unit -- TODO:: Create a chat service and remove this
removeChatService _ = do
  let
    state = HomeScreenData.initData.data
  void $ lift $ lift $ liftFlow $ stopChatListenerService
  void $ pure $ setValueToLocalNativeStore READ_MESSAGES "0"
  modifyScreenState
    $ HomeScreenStateType
        ( \homeScreen ->
            homeScreen
              { props { sendMessageActive = false, chatcallbackInitiated = false, unReadMessages = false, openChatScreen = false, showChatNotification = false, canSendSuggestion = true, isChatNotificationDismissed = false, isNotificationExpanded = false, removeNotification = true, enableChatWidget = false }
              , data { messages = [], messagesSize = "-1", chatSuggestionsList = [], messageToBeSent = "", lastMessage = state.lastMessage, waitTimeInfo = false, lastSentMessage = state.lastSentMessage, lastReceivedMessage = state.lastReceivedMessage }
              }
        )

  modifyScreenState $ FollowRideScreenStateType (\followRideScreenState -> followRideScreenState { props { chatCallbackInitiated = false } })

setFlowStatusData :: Encode FlowStatusData => FlowStatusData -> Effect Unit
setFlowStatusData object = void $ pure $ setValueToLocalStore FLOW_STATUS_DATA (encodeJSON object)

updateFlowStatus :: NotifyFlowEventType -> FlowBT String Unit
updateFlowStatus eventType = do
  (FlowStatusRes flowStatus) <- Remote.flowStatusBT "LazyCheck"
  case flowStatus.currentStatus of
    RIDE_ASSIGNED _ -> do
      checkRideStatus true false
      homeScreenFlow
    _ -> do
      res <- lift $ lift $ Remote.notifyFlowEvent (Remote.makeNotifyFlowEventReq (show eventType))
      hideLoaderFlow
      case res of
        Right _ -> homeScreenFlow
        Left err -> do
          let
            errResp = err.response

            codeMessage = decodeError errResp.errorMessage "errorCode"
          when (err.code == 400 && codeMessage == "ACTIVE_BOOKING_EXISTS")
            $ do
                currentFlowStatus false

getTicketBookings :: Array TicketBookingItem -> Array TicketBookingItem -> Array TicketBookingItem -> TicketBookings
getTicketBookings bookedRes pendingRes cancelledRes =
  { pendingBooking: pendingRes
  , booked: bookedRes
  , cancelled: cancelledRes
  }

cancelEstimate :: String -> FlowBT String Unit
cancelEstimate bookingId = do
  logField_ <- lift $ lift $ getLogFields
  res <- lift $ lift $ Remote.cancelEstimate bookingId
  if bookingId == "" then do
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { currentStage = HomeScreen, autoScroll = false } })
  else do
    case res of
      Right res -> do
        -- TODO : to be removed after new bundle is 100% available (replace with pure unit)
        let
          (APISuccessResp resp) = res
        case resp.result of
          "Success" -> do
            if (getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then do
              void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_cancel_waiting_for_driver_assign"
              pure unit
            else do
              void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_cancel_waiting_for_quotes"
              pure unit
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { isSearchCancelled = true } })
          _ -> do
            void $ liftFlowBT $ logEvent logField_ "ny_fs_cancel_estimate_booking_exists_right"
            currentFlowStatus false
      Left err -> do
        let
          errResp = err.response

          codeMessage = decodeError errResp.errorMessage "errorCode"
        if (err.code == 400 && codeMessage == "ACTIVE_BOOKING_EXISTS") then do
          void $ liftFlowBT $ logEvent logField_ "ny_fs_cancel_estimate_booking_exists_left"
          currentFlowStatus false
        else do
          void $ void $ lift $ lift $ showToast $  getString STR.CANCELLATION_UNSUCCESSFULL_PLEASE_TRY_AGAIN
          void $ liftFlowBT $ logEvent logField_ "ny_fs_cancel_estimate_failed_left"
          currentFlowStatus false

getGenderValue :: Maybe Gender.Gender -> Maybe String
getGenderValue gender = case gender of
  Just value -> case value of
    Gender.MALE -> Just "MALE"
    Gender.FEMALE -> Just "FEMALE"
    Gender.OTHER -> Just "OTHER"
    _ -> Just "PREFER_NOT_TO_SAY"
  Nothing -> Nothing

getPlaceCoordinates :: String -> FlowBT String { latitude :: Number, longitude :: Number }
getPlaceCoordinates address =
  let
    { latitude, longitude } = runFn1 getLatLonFromAddress address
  in
    pure { latitude, longitude }

isAddressFieldsNothing :: Address -> Boolean
isAddressFieldsNothing address = all isNothing [ address.area, address.state, address.country, address.building, address.door, address.street, address.city, address.areaCode, address.ward, address.placeId ]

getPlaceName :: Number -> Number -> Location -> Boolean -> FlowBT String (Maybe PlaceName)
getPlaceName lat long location getApiResponse = do
  case location.address of
    Just address -> do
      let
        addressComponent = mkAddressComponent location "sublocality"
      pure $ Just $ mkPlaceName lat long address (Just addressComponent)
    Nothing -> do
      let
        address = runFn2 getLocationNameV2 lat long
      config <- getAppConfigFlowBT appConfig
      logField_ <- lift $ lift $ getLogFields
      if address /= "NO_LOCATION_FOUND" && config.geoCoder.enableLLtoAddress then do
        liftFlowBT $ logEvent logField_ "ny_geocode_ll_address_found"
        pure $ Just $ mkPlaceName lat long address Nothing
      else do
        if getApiResponse then do
          (GetPlaceNameResp locationName) <- Remote.placeNameBT (Remote.makePlaceNameReq lat long $ EHC.getMapsLanguageFormat $ getLanguageLocale languageKey)
          liftFlowBT $ logEvent logField_ "ny_geocode_ll_address_fallback"
          pure $ locationName !! 0
        else do
          pure Nothing
  where
  mkPlaceName :: Number -> Number -> String -> Maybe AddressComponents -> PlaceName
  mkPlaceName lat long address addressComponent =
    PlaceName
      { formattedAddress: address
      , location: LatLong { lat: lat, lon: long }
      , plusCode: Nothing
      , addressComponents: [] <> catMaybes [ addressComponent ]
      , placeId: Nothing
      }

  mkAddressComponent :: Location -> String -> AddressComponents
  mkAddressComponent location addressType =
    AddressComponents
      { longName: location.place
      , shortName: location.place
      , types: [ addressType ]
      }

dummyLocationData :: LocationData
dummyLocationData =
  LocationData
    { lat: 0.0
    , lon: 0.0
    , name: Nothing
    }

checkAndUpdateLocations :: FlowBT String Unit
checkAndUpdateLocations = do
  let
    mBPayload = getGlobalPayload Constants.globalPayload
  maybe
    (pure unit)
    ( \(GlobalPayload payload) -> do
        void $ pure $ spy "inside right" payload
        let
          (Payload innerPayload) = payload.payload
        case isNothing innerPayload.search_type of
          true -> pure unit
          false -> do
            let
              searchType = fromMaybe "normal_search" $ innerPayload.search_type
            if searchType /= "normal_search" then do
              let
                (LocationData source) = fromMaybe dummyLocationData innerPayload.source
              let
                (LocationData destination) = fromMaybe dummyLocationData innerPayload.destination
              modifyScreenState
                $ HomeScreenStateType
                    ( \homescreen ->
                        homescreen
                          { data
                            { source = (fromMaybe "" source.name)
                            , destination = (fromMaybe "" destination.name)
                            , sourceAddress = encodeAddress (fromMaybe "" source.name) [] Nothing source.lat source.lon
                            , destinationAddress = encodeAddress (fromMaybe "" destination.name) [] Nothing destination.lat destination.lon
                            }
                          , props
                            { sourceLat = source.lat
                            , sourceLong = source.lon
                            , destinationLat = destination.lat
                            , destinationLong = destination.lon
                            , isSource = Just false
                            , isSearchLocation = SearchLocation
                            }
                          }
                    )
            else
              pure unit
    )
    mBPayload

rideCompletedDetails :: RideBookingRes -> Array ClevertapEventParams
rideCompletedDetails (RideBookingRes resp) = do
  let
    (RideBookingAPIDetails bookingDetails) = resp.bookingDetails

    (RideBookingDetails contents) = bookingDetails.contents

    (RideAPIEntity ride) = fromMaybe dummyRideAPIEntity (resp.rideList !! 0)

    differenceOfDistance = fromMaybe 0 contents.estimatedDistance - (fromMaybe 0 ride.chargeableRideDistance)

    finalAmount = getFinalAmount (RideBookingRes resp)

    timeVal = (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "HH:mm:ss")

    nightChargesVal = (withinTimeRange "22:00:00" "5:00:00" timeVal)

    actualTollCharge = maybe dummyPrice (\obj -> obj ^. _amountWithCurrency) $ Arr.find (\entity -> entity ^. _description == "TOLL_CHARGES") (resp.fareBreakup)
  [ { key: "Estimate ride distance (km)", value: unsafeToForeign (fromMaybe 0 contents.estimatedDistance / 1000) }
  , { key: "Actual ride distance (km)", value: unsafeToForeign ((fromMaybe 0 ride.chargeableRideDistance) / 1000) }
  , { key: "Difference between estimated and actual ride distance (km)", value: unsafeToForeign (differenceOfDistance / 1000) }
  , { key: "Total Estimated fare (₹)", value: unsafeToForeign (resp.estimatedFare) }
  , { key: "Total Actual fare (₹)", value: unsafeToForeign (finalAmount) }
  , { key: "Difference between estimated and actual fares (₹)", value: unsafeToForeign (resp.estimatedFare - finalAmount) }
  , { key: "Driver pickup charges (₹)", value: unsafeToForeign "10" }
  , { key: "Night ride", value: unsafeToForeign nightChargesVal }
  , { key: "Actual Toll Charges", value: unsafeToForeign actualTollCharge.amount }
  , { key: "Has Toll", value: unsafeToForeign (maybe false (\charge -> charge.amount /= 0.0) (Just actualTollCharge)) }
  ]

personStatsData :: PersonStatsRes -> GetProfileRes -> Array ClevertapEventParams
personStatsData (PersonStatsRes resp) (GetProfileRes response) =
  [ { key: "First ride taken", value: unsafeToForeign if response.hasTakenRide then "true" else "false" }
  , { key: "Common App Use Case", value: unsafeToForeign resp.commonAppUseCase }
  , { key: "Emergency Contacts Num", value: unsafeToForeign resp.emergencyContactsNum }
  , { key: "Favourite Locations Num", value: unsafeToForeign resp.favoriteLocationsNum }
  , { key: "Frequency Category", value: unsafeToForeign resp.frequencyCategory }
  , { key: "Is Blocked", value: unsafeToForeign resp.isBlocked }
  , { key: "Is Churned User", value: unsafeToForeign resp.isChurnedUser }
  , { key: "Is WhatsApp Opt-In Status", value: unsafeToForeign resp.isWhatsAppOptInStatus }
  , { key: "total_rider_trips", value: unsafeToForeign resp.lifetimeRides }
  , { key: "Last Ride Taken", value: unsafeToForeign (fromMaybe "" resp.lastRideTaken) }
  , { key: "Latest Search", value: unsafeToForeign (fromMaybe "" resp.latestSearch) }
  , { key: "Off Peak Rides Rate", value: unsafeToForeign (fromMaybe 0.0 resp.offPeakRidesRate) }
  , { key: "Overall Cancellation Rate", value: unsafeToForeign (fromMaybe 0.0 resp.overalCancellationRate) }
  , { key: "Sign-up Date", value: unsafeToForeign resp.signupDate }
  , { key: "Status", value: unsafeToForeign (fromMaybe "" resp.status) }
  , { key: "User Cancellation Rate", value: unsafeToForeign (fromMaybe 0.0 resp.userCancellationRate) }
  , { key: "User Category", value: unsafeToForeign resp.userCategory }
  , { key: "Weekday Evening Peak Rides Rate", value: unsafeToForeign (fromMaybe 0.0 resp.weekdayEveningPeakRidesRate) }
  , { key: "Weekday Morning Peak Rides Rate", value: unsafeToForeign (fromMaybe 0.0 resp.weekdayMorningPeakRidesRate) }
  , { key: "Weekday Rides Rate", value: unsafeToForeign (fromMaybe 0.0 resp.weekdayRidesRate) }
  , { key: "Weekend Peak Ride Rate", value: unsafeToForeign (fromMaybe 0.0 resp.weekendPeakRideRate) }
  , { key: "Weekend Rides Rate", value: unsafeToForeign (fromMaybe 0.0 resp.weekendRidesRate) }
  ]

updateSourceLocation :: String -> FlowBT String Unit
updateSourceLocation _ = do
  (GlobalState currentState) <- getState
  let
    disabled = case currentState.homeScreen.data.disability of
      Just val -> Just val.tag
      Nothing -> Just ""
  when (disabled == Just "BLIND_LOW_VISION")
    $ do
        fullAddress <- getPlaceName currentState.homeScreen.props.sourceLat currentState.homeScreen.props.sourceLong HomeScreenData.dummyLocation true
        case fullAddress of
          Just (PlaceName address) -> do
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { source = address.formattedAddress, sourceAddress = encodeAddress address.formattedAddress [] Nothing currentState.homeScreen.props.sourceLat currentState.homeScreen.props.sourceLong } })
          Nothing -> void $ void $ lift $ lift $ showToast $  getString STR.SOMETHING_WENT_WRONG_TRY_AGAIN_LATER
        pure unit
  pure unit

updateCurrentLocation :: String -> FlowBT String Unit
updateCurrentLocation _ = do
  (GlobalState currentState) <- getState
  currentAddress <- getPlaceName currentState.homeScreen.props.currentLocation.lat currentState.homeScreen.props.currentLocation.lng HomeScreenData.dummyLocation false
  case currentAddress of
    Just (PlaceName address) -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { currentLocation { place = address.formattedAddress } } })
    Nothing -> pure unit
  pure unit

updateUserInfoToState :: HomeScreenState -> FlowBT String Unit
updateUserInfoToState state =
  modifyScreenState
    $ HomeScreenStateType
        ( \homeScreen ->
            HomeScreenData.initData
              { data
                { disability = state.data.disability
                , settingSideBar
                  { gender = state.data.settingSideBar.gender
                  , email = state.data.settingSideBar.email
                  , hasCompletedSafetySetup = state.data.settingSideBar.hasCompletedSafetySetup
                  }
                , destinationSuggestions = state.data.destinationSuggestions
                , tripSuggestions = state.data.tripSuggestions
                , followers = state.data.followers
                , famousDestinations = state.data.famousDestinations
                , manuallySharedFollowers = state.data.manuallySharedFollowers
                }
              , props
                { isBanner = state.props.isBanner
                , sosBannerType = state.props.sosBannerType
                , followsRide = state.props.followsRide
                , isSafetyCenterDisabled = state.props.isSafetyCenterDisabled
                , city = state.props.city
                , showShimmer = false
                }
              }
        )

placeListFlow :: FlowBT String Unit
placeListFlow = do
  (GlobalState currentState) <- getState
  void $ pure $ spy "ZOO TICKET PLACE LIST CALLED" currentState
  (GlobalState state) <- getState
  uiAction <- lift $ lift $ runScreen $ PlaceListS.screen state.ticketingScreen
  case uiAction of
    PlaceListC.ExitToHomeScreen updatedState -> do
      modifyScreenState $ TicketingScreenStateType (\_ -> updatedState)
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> homeScreenFlow)
    PlaceListC.ExitToMyTicketsScreen updatedState -> do
      modifyScreenState $ TicketingScreenStateType (\_ -> updatedState)
      (GetAllBookingsRes bookedRes) <- Remote.getAllBookingsBT Booked
      (GetAllBookingsRes pendingRes) <- Remote.getAllBookingsBT Pending
      (GetAllBookingsRes cancelledRes) <- Remote.getAllBookingsBT Cancelled
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData { props { navigateToHome = false, currentStage = ViewTicketStage, previousStage = ViewTicketStage, ticketBookingList = getTicketBookings (buildBookingDetails bookedRes) (buildBookingDetails pendingRes) (buildBookingDetails cancelledRes) } })
      (App.BackT $ App.BackPoint <$> pure unit) >>= (\_ -> ticketListFlow)
    PlaceListC.BookTickets updatedState selectedPlace -> do
      modifyScreenState $ TicketingScreenStateType (\_ -> updatedState)
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { navigateToHome = false, currentStage = DescriptionStage, previousStage = DescriptionStage }, data { totalAmount = 0, placeInfo = Just selectedPlace } })
      (App.BackT $ App.BackPoint <$> pure unit) >>= (\_ -> placeDetailsFlow)
    _ -> App.BackT $ pure App.GoBack

placeDetailsFlow :: FlowBT String Unit
placeDetailsFlow = do
  (GlobalState currentState) <- getState
  void $ pure $ spy "ZOO TICKET PLACE DETAILS CALLED" currentState
  liftFlowBT $ hideLoader ""
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen
  { data
      { dateOfVisit = if DS.null ticketBookingScreen.data.dateOfVisit
                      then (if isTodayBookingAllowed ticketBookingScreen
                            then (getNextDateV2 "")
                            else (getNextDate "yyyy-mm-dd"))
                      else ticketBookingScreen.data.dateOfVisit
      }
  , props
      {
        selectedOperationalDay = ticketBookingScreen.props.selectedOperationalDay
      }
  })
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ PlaceDetailsS.screen state.ticketBookingScreen
  case action of
    PlaceDetailsC.GoToHomeScreen state -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData)
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> goToHomeScreenWithHybridCheck state)
    PlaceDetailsC.GoToTicketPayment state -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState -> state)
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> ticketPaymentFlow state.data)
    PlaceDetailsC.GoToOpenGoogleMaps state lat2 long2 -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState -> state)
      (App.BackT $ App.BackPoint <$> pure unit) >>= (\_ -> openGoogleMaps lat2 long2)
    PlaceDetailsC.BookTickets state -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData)
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> goToHomeScreenWithHybridCheck state)
    PlaceDetailsC.GoToTicketBook updatedState selectedDateString-> do
      modifyScreenState $ TicketBookingScreenStateType (\_ -> updatedState)
      (App.BackT $ App.BackPoint <$> pure unit) >>= (\_ -> placeDetailsFlow)
  where
  isTodayBookingAllowed :: ST.TicketBookingScreenState -> Boolean
  isTodayBookingAllowed state =
        case state.data.placeInfo of
              Just (TicketPlaceResp placeInfo) -> fromMaybe true placeInfo.allowSameDayBooking
              Nothing -> true

  openGoogleMaps lat long = do
    void $ pure $ openNavigation lat long "DRIVE"
    placeDetailsFlow

  goToHomeScreenWithHybridCheck state =
    if state.props.navigateToHome 
      then do
        when (HU.isParentView FunctionCall) $ pure $ HU.emitTerminateApp Nothing true
        homeScreenFlow
      else placeListFlow

ticketStatusFlow :: FlowBT String Unit
ticketStatusFlow = do
  (GlobalState currentState) <- getState
  void $ pure $ spy "ZOO TICKET STATUS CALLED" currentState
  liftFlowBT $ hideLoader ""
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { data { dateOfVisit = (getNextDateV2 "") } })
  flow <- UI.ticketStatusScreen
  case flow of
    GO_TO_HOME_SCREEN_FROM_TICKET_BOOKING state -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { focussedBottomIcon = MOBILITY } })
      goToHomeScreenWithHybridCheck state
    REFRESH_PAYMENT_STATUS state -> do
      (GetTicketStatusResp ticketStatus) <- Remote.getTicketStatusBT state.props.selectedBookingId
      updatePaymentStatusData ticketStatus state.props.selectedBookingId
      setValueToLocalStore PAYMENT_STATUS_POOLING "false"
      ticketStatusFlow
    GO_TO_TICKET_LIST state -> do
      (GetAllBookingsRes bookedRes) <- Remote.getAllBookingsBT Booked
      (GetAllBookingsRes pendingRes) <- Remote.getAllBookingsBT Pending
      (GetAllBookingsRes cancelledRes) <- Remote.getAllBookingsBT Cancelled
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData { props { navigateToHome = false, currentStage = ViewTicketStage, previousStage = ViewTicketStage, ticketBookingList = getTicketBookings (buildBookingDetails bookedRes) (buildBookingDetails pendingRes) (buildBookingDetails cancelledRes)} })
      ticketListFlow
    GET_BOOKING_INFO_SCREEN state bookingStatus -> do
      (TicketBookingDetails resp) <- Remote.getTicketBookingDetailsBT state.props.selectedBookingId
      if bookingStatus == Pending then do
        modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { currentStage = BookingConfirmationStage } })
        setValueToLocalStore PAYMENT_STATUS_POOLING "true"
        fillBookingDetails (TicketBookingDetails resp) state.props.selectedBookingId "Pending"
        ticketStatusFlow
      else do
        let
          ticketBookingDetails = (ticketDetailsTransformer (TicketBookingDetails resp))
        let
          dummyListItem = TicketBookingScreenData.dummyServiceDetails
        modifyScreenState $ TicketInfoScreenStateType (\ticketInfoScreen -> ticketInfoScreen { data { selectedBookingInfo = ticketBookingDetails }, props { activeListItem = fromMaybe dummyListItem (ticketBookingDetails.services !! 0), rightButtonDisable = (length ticketBookingDetails.services < 2) } })
        zooTicketInfoFlow
    _ -> ticketStatusFlow
  where
    goToHomeScreenWithHybridCheck state =
      if state.props.navigateToHome 
        then do
          when (HU.isParentView FunctionCall) $ pure $ HU.emitTerminateApp Nothing true
          homeScreenFlow
        else placeListFlow

metroTicketBookingFlow :: FlowBT String Unit
metroTicketBookingFlow = do
  (GlobalState currentState) <- getState
  config <- getAppConfigFlowBT appConfig
  metroStationsList <-  lift $ lift $ getMetroStationsObjFromLocal ""
  let
    -- metroStationsList = []
    currentCity = getValueToLocalStore CUSTOMER_LOCATION

    stationsForCurrentCity = (findMetroStationsForCity currentCity metroStationsList)
  parsedMetroStation <- case stationsForCurrentCity of
    Just stations -> do
      let diffSec = runFn2 differenceBetweenTwoUTCInMinutes (getCurrentUTC "") stations.lastUpdatedAt
          metroTicketBookingScreen = currentState.metroTicketBookingScreen
          (FRFSConfigAPIRes metroBookingConfigResp) = metroTicketBookingScreen.data.metroBookingConfigResp
          metroStationValidTill = metroBookingConfigResp.metroStationTtl
      if diffSec > metroStationValidTill
        then fetchMetroStations currentCity metroStationsList metroTicketBookingScreen.props.isEmptyRoute (show metroTicketBookingScreen.props.ticketServiceType) currentState.homeScreen.props.sourceLat currentState.homeScreen.props.sourceLong currentState.metroTicketBookingScreen.data.srcCode currentState.metroTicketBookingScreen.data.destCode
        else pure $ parseMetroStations stations.stations
    Nothing -> fetchMetroStations currentCity metroStationsList currentState.metroTicketBookingScreen.props.isEmptyRoute (show currentState.metroTicketBookingScreen.props.ticketServiceType) currentState.homeScreen.props.sourceLat currentState.homeScreen.props.sourceLong currentState.metroTicketBookingScreen.data.srcCode currentState.metroTicketBookingScreen.data.destCode
  flow <- UI.metroTicketBookingScreen
  case flow of
    GO_TO_HOME_SCREEN_FROM_METRO_TICKET state -> homeScreenFlow
    GO_TO_METRO_STATION_SEARCH srcdest state -> do
      (GetMetroStationResponse getMetroStationResp) <- Remote.getMetroStationBT (show state.props.ticketServiceType) currentCity state.props.isEmptyRoute "" (show currentState.homeScreen.props.sourceLat <> "," <> show currentState.homeScreen.props.sourceLong)
      let
        searchLocationState = currentState.searchLocationScreen
        parsedStations_ = parseMetroStations getMetroStationResp
        textFieldFocus = case srcdest of
          Src -> Just SearchLocPickup
          Dest -> Just SearchLocDrop
        srcLocation = if state.data.srcCode == "" then Nothing else Just $ SearchLocationScreenData.dummyLocationInfo { metroInfo = Just { stationName : state.data.srcLoc, stationCode : state.data.srcCode }, address = state.data.srcLoc, stationCode = state.data.srcCode }
        destLocation = if state.data.destCode == "" then Nothing else Just $ SearchLocationScreenData.dummyLocationInfo { metroInfo = Just { stationName : state.data.destLoc, stationCode : state.data.destCode }, address = state.data.destLoc, stationCode = state.data.destCode }
      if null searchLocationState.data.metroStations then
        modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData{ props { actionType = if state.props.ticketServiceType == BUS then BusStationSelectionAction else MetroStationSelectionAction, canSelectFromFav = false, focussedTextField = textFieldFocus }, data { srcLoc = srcLocation, destLoc = destLocation,fromScreen = getScreen Screen.METRO_TICKET_BOOKING_SCREEN, metroStations = parsedStations_, updatedMetroStations = parsedStations_, rideType = slsState.data.rideType } })
      else
        modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData{ props { actionType = if state.props.ticketServiceType == BUS then BusStationSelectionAction else MetroStationSelectionAction, canSelectFromFav = false, focussedTextField = textFieldFocus }, data { srcLoc = srcLocation, destLoc = destLocation, fromScreen = getScreen Screen.METRO_TICKET_BOOKING_SCREEN, metroStations = searchLocationState.data.metroStations, updatedMetroStations = searchLocationState.data.metroStations, rideType = slsState.data.rideType } })
      searchLocationFlow
    -- GO_TO_ROUTE_SEARCH_METRO_SCREEN state ->  do 
    --  let
    --   currentCity = getValueToLocalStore CUSTOMER_LOCATION
    --  (GetBusRoutesResponse busRoutesResp) <-  Remote.getBusRoutesBT currentCity (state.data.srcCode) state.data.destCode
    --  modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state { data {routeList =  busRoutesResp , ticketCount = state.data.ticketCount }, props { isButtonActive = true, ticketServiceType =  BUS, routeList = not state.props.routeList , showRouteOptions = true} })
    --  metroTicketBookingFlow
    METRO_FARE_AND_PAYMENT state -> do
      modifyScreenState $ BusTicketBookingScreenStateType (\_ -> BusTicketBookingScreenData.initData)
      if state.props.currentStage == MetroTicketSelection || state.props.currentStage == BusTicketSelection then do
        if state.data.srcCode == state.data.destCode then do
          void $ lift $ lift $ showToast "Source and destination cannot be same."
          void $ pure $ toggleBtnLoader "" false
          modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state { props { currentStage  = if state.props.ticketServiceType == BUS then ST.BusTicketSelection else  ST.MetroTicketSelection } })
          metroTicketBookingFlow
        else do
          (FrfsSearchResp searchMetroResp) <- Remote.frfsSearchBT (show state.props.ticketServiceType) $ Remote.makeSearchMetroReq state.data.srcCode state.data.destCode state.data.ticketCount $ (DS.null state.props.routeName) ? Nothing $ Just state.props.routeName
          modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state { data { searchId = searchMetroResp.searchId }, props { currentStage = GetMetroQuote } })
      else if state.props.currentStage == ConfirmMetroQuote then do
        -- metroBookingStatus <- lift $ lift $ Remote.confirmMetroQuote state.data.quoteId
        metroBookingStatus <- lift $ lift $ Remote.confirmMetroQuoteV2 state.data.quoteId $ API.FRFSQuoteConfirmReq {discounts: fromMaybe [] state.data.applyDiscounts}
        updateMetroBookingQuoteInfo metroBookingStatus
      else
        pure unit
      metroTicketBookingFlow
    GO_TO_MY_METRO_TICKET_SCREEN -> do
      modifyScreenState $ MetroMyTicketsScreenStateType (\metroMyTicketsScreen -> metroMyTicketsScreen { props { entryPoint = ST.MetroTicketBookingToMetroMyTickets } })
      metroMyTicketsFlow
    GO_TO_METRO_ROUTE_MAP -> do
      let
        currentCity = getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION
      modifyScreenState $ MetroTicketDetailsScreenStateType (\_ -> MetroTicketDetailsScreenData.initData)
      modifyScreenState $ MetroTicketDetailsScreenStateType (\slsState -> slsState { data { city = currentCity }, props { stage = ST.MetroMapStage, previousScreenStage = ST.MetroTicketSelectionStage } })
      metroTicketDetailsFlow
    REFRESH_METRO_TICKET_SCREEN state -> do
      modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state { props { currentStage = ConfirmMetroQuote } })
      modifyScreenState $ MetroTicketStatusScreenStateType (\metroTicketStatusScreen -> metroTicketStatusScreen { data { quoteId = state.data.quoteId } })
      metroTicketBookingFlow
    GO_TO_HOME_FROM_METRO_TICKET -> homeScreenFlow
    GO_TO_METRO_PAYMENT_PAGE orderResp bookingId state -> do
      modifyScreenState $ MetroTicketBookingScreenStateType (\ticketBookingState -> ticketBookingState { props { currentStage = ConfirmMetroQuote } })
      modifyScreenState $ MetroTicketStatusScreenStateType (\ticketStatusState -> ticketStatusState { props { entryPoint = if state.props.ticketServiceType == BUS then ST.BusTicketToMetroTicketStatus else ticketStatusState.props.entryPoint } })
      metroTicketPaymentFlow orderResp bookingId
    GO_TO_SEARCH_SCREEN state -> do 
      modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData { props { actionType = BusSearchSelectionAction, canSelectFromFav = false, focussedTextField = Just SearchLocPickup , routeSearch = true , isAutoComplete = false , srcLat = state.props.srcLat , srcLong = state.props.srcLong }, data {fromScreen =(Screen.getScreen Screen.BUS_TICKET_BOOKING_SCREEN),ticketServiceType = BUS , srcLoc = Nothing, destLoc = Nothing, rideType = slsState.data.rideType} })
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> searchLocationFlow)
    EDIT_TICKET_BOOKING_STOPS state -> do
      modifyScreenState $ SearchLocationScreenStateType (\slsState -> slsState{ props { actionType = BusStopSelectionAction
                                                                                      , canSelectFromFav = false
                                                                                      , focussedTextField = Just SearchLocPickup 
                                                                                      , routeSearch = false 
                                                                                      , isAutoComplete = false }
                                                                              , data  { fromScreen = (Screen.getScreen Screen.BUS_TICKET_BOOKING_SCREEN)
                                                                                      , srcLoc = Nothing
                                                                                      , destLoc = Nothing
                                                                                      , updatedStopsSearchedList = slsState.data.stopsSearchedList } })
      searchLocationFlow  
    GO_TO_AADHAAR_VERIFICATION_SCREEN state offerType -> do
      (GetProfileRes profileResponse) <- Remote.getProfileBT ""
      if (profileResponse.aadhaarVerified == Just true) then do
        let appliedDiscountItem = Just $ [ API.FRFSDiscountReq
                { code: offerType
                , quantity: 1
                } ]
        metroBookingStatus <- lift $ lift $ Remote.confirmMetroQuoteV2 state.data.quoteId $ API.FRFSQuoteConfirmReq {discounts: fromMaybe [] appliedDiscountItem}
        updateMetroBookingQuoteInfo metroBookingStatus
        modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state { data {applyDiscounts = appliedDiscountItem}, props { currentStage = GetMetroQuote} })
        metroTicketBookingFlow
      else do
        modifyScreenState $ AadhaarVerificationScreenType (\_ -> AadhaarVerificationScreenData.initData)
        aadhaarVerificationFlow offerType
  where
  fetchMetroStations currentCity metroStationsList routeCode ticketServiceType srcLat srcLong srcCode destCode= do
    (GetMetroStationResponse getMetroStationResp) <- Remote.getMetroStationBT ticketServiceType currentCity routeCode "" (show srcLat <> "," <> show srcLong)
    void $ pure $ saveObject "METRO_STATIONS" (getMetroStations currentCity getMetroStationResp metroStationsList)
    (GetBusRoutesResponse busRoutesResp) <- if ticketServiceType == "BUS" 
                      then Remote.getBusRoutesBT currentCity srcCode destCode
                      else pure $ GetBusRoutesResponse []
    let
      parsedStations_ = parseMetroStations getMetroStationResp
    modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData { data {metroStations = parsedStations_, updatedMetroStations = parsedStations_, rideType = slsState.data.rideType } })
    modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state { props { srcLat = srcLat , srcLong = srcLong , currentStage = BusTicketSelection } , data {routeList =  busRoutesResp } })
    pure parsedStations_

  getMetroStations :: String -> Array FRFSStationAPI -> Array MetroStations -> Array ST.MetroStations
  getMetroStations city metroStationsResp metroStationArr =
    let
      currentCity = getCityFromString city

      filteredStationArr = filter (\station -> station.city /= currentCity) metroStationArr

      currentCityStations =
        [ { city: currentCity
          , stations: metroStationsResp
          , lastUpdatedAt: getCurrentUTC ""
          }
        ]
    in
      filteredStationArr <> currentCityStations

  findMetroStationsForCity :: String -> Array MetroStations -> Maybe MetroStations
  findMetroStationsForCity city stations =
    let
      currentCity = getCityFromString city
    in
      if null stations then
        Nothing
      else
        Arr.find (\station -> station.city == currentCity) stations

  parseMetroStations :: Array FRFSStationAPI -> Array Station
  parseMetroStations stations =
    map
      ( \(FRFSStationAPI item) ->
          { stationName: item.name
          , stationCode: item.code
          }
      )
      stations

metroTicketPaymentFlow :: CreateOrderRes -> String -> FlowBT String Unit
metroTicketPaymentFlow (CreateOrderRes orderResp) bookingId = do
  liftFlowBT $ initiatePaymentPage
  lift $ lift $ doAff $ makeAff \cb -> runEffectFn1 checkPPInitiateStatus (cb <<< Right) $> nonCanceler
  let sdkPayload = maybe (encodeJSON orderResp.sdk_payload) (addLanguageToPayload (getPaymentPageLangKey (getLanguageLocale languageKey))) orderResp.sdk_payload_json
  void $ paymentPageUI sdkPayload
  void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)
  void $ lift $ lift $ toggleLoader true
  setValueToLocalStore METRO_PAYMENT_STATUS_POOLING "true"
  modifyScreenState $ MetroTicketStatusScreenStateType (\metroTicketStatusScreen -> metroTicketStatusScreen { data { bookingId = bookingId } })
  resp <- lift $ lift $ Remote.getMetroStatus bookingId
  case resp of
    Right (GetMetroBookingStatusResp getMetroStatusResp) -> do
      let (FRFSTicketBookingStatusAPIRes metroTicketStatusResp2) = getMetroStatusResp
      void $ pure $ toggleBtnLoader "" false
      void $ lift $ lift $ toggleLoader false
      case metroTicketStatusResp2.payment of
        Just (FRFSBookingPaymentAPI paymentInfo) -> do
          if paymentInfo.status == "NEW" then
            metroTicketBookingFlow
          else do
            modifyScreenState $ MetroTicketDetailsScreenStateType (\metroTicketDetailsState -> metroTicketDetailsTransformer getMetroStatusResp metroTicketDetailsState)
            modifyScreenState $ MetroTicketStatusScreenStateType (\metroTicketStatusScreen -> metroTicketStatusTransformer getMetroStatusResp metroTicketStatusScreen)
            metroTicketStatusFlow
        Nothing -> defaultFlow
    Left _ -> defaultFlow
  where
    defaultFlow :: FlowBT String Unit
    defaultFlow = do
      modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state { props { currentStage  = if state.props.ticketServiceType == BUS then ST.BusTicketSelection else  ST.MetroTicketSelection} })
      metroTicketBookingFlow

ticketListFlow :: FlowBT String Unit
ticketListFlow = do
  (GlobalState currentState) <- getState
  void $ pure $ spy "ZOO TICKET TICKET LIST CALLED" currentState
  liftFlowBT $ hideLoader ""
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { data { dateOfVisit = (getNextDateV2 "") } })
  flow <- UI.ticketListScreen
  case flow of
    GO_TO_TICKET_PAYMENT state -> ticketPaymentFlow state.data
    GO_TO_OPEN_GOOGLE_MAPS_FROM_ZOO_FLOW dstLat1 dstLon2 -> do
      void $ pure $ openNavigation dstLat1 dstLon2 "DRIVE"
      ticketListFlow
    GET_BOOKING_INFO_SCREEN state bookingStatus -> do
      (TicketBookingDetails resp) <- Remote.getTicketBookingDetailsBT state.props.selectedBookingId
      if bookingStatus == Pending then do
        modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { currentStage = BookingConfirmationStage } })
        setValueToLocalStore PAYMENT_STATUS_POOLING "true"
        fillBookingDetails (TicketBookingDetails resp) state.props.selectedBookingId "Pending"
        ticketStatusFlow
      else do
        let
          ticketBookingDetails = (ticketDetailsTransformer (TicketBookingDetails resp))
        let
          dummyListItem = TicketBookingScreenData.dummyServiceDetails
        modifyScreenState $ TicketInfoScreenStateType (\ticketInfoScreen -> ticketInfoScreen { data { selectedBookingInfo = ticketBookingDetails }, props { activeListItem = fromMaybe dummyListItem (ticketBookingDetails.services !! 0), rightButtonDisable = (length ticketBookingDetails.services < 2) } })
        zooTicketInfoFlow
    GO_TO_HOME_SCREEN_FROM_TICKET_BOOKING state -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { focussedBottomIcon = MOBILITY } })
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData)
      if state.props.navigateToHome 
        then do
          when (HU.isParentView FunctionCall) $ pure $ HU.emitTerminateApp Nothing true
          homeScreenFlow
        else placeListFlow
    RESET_SCREEN_STATE -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData)
      ticketListFlow
    REFRESH_PAYMENT_STATUS state -> do
      (GetTicketStatusResp ticketStatus) <- Remote.getTicketStatusBT state.props.selectedBookingId
      updatePaymentStatusData ticketStatus state.props.selectedBookingId
      setValueToLocalStore PAYMENT_STATUS_POOLING "false"
      ticketListFlow
    _ -> ticketListFlow

-- zooTicketBookingFlow :: FlowBT String Unit -- NOTE :: KEEPING IT FOR REFERENCE
-- zooTicketBookingFlow = do
--   (GlobalState currentState) <- getState
--   void $ pure $ spy "ZOO TICKET BOOKING FLOW CALLED" currentState
--   liftFlowBT $ hideLoader
--   modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen{data{dateOfVisit = (getNextDateV2 "")}})
--   flow <- UI.ticketBookingScreen
--   case flow of
--     GO_TO_TICKET_PAYMENT state -> ticketPaymentFlow state.data
--     GO_TO_OPEN_GOOGLE_MAPS_FROM_ZOO_FLOW dstLat1 dstLon2  -> do
--       void $ pure $ openNavigation dstLat1 dstLon2 "DRIVE"
--       zooTicketBookingFlow
--     GET_BOOKING_INFO_SCREEN state bookingStatus -> do
--       (TicketBookingDetails resp) <- Remote.getTicketBookingDetailsBT state.props.selectedBookingId
--       if bookingStatus == Pending
--         then do
--           modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { currentStage = BookingConfirmationStage } })
--           setValueToLocalStore PAYMENT_STATUS_POOLING "true"
--           fillBookingDetails (TicketBookingDetails resp) state.props.selectedBookingId "Pending"
--           zooTicketBookingFlow
--         else do
--           let ticketBookingDetails = (ticketDetailsTransformer (TicketBookingDetails resp))
--           let dummyListItem = TicketBookingScreenData.dummyServiceDetails
--           modifyScreenState $ TicketInfoScreenStateType (\ticketInfoScreen ->  ticketInfoScreen{data{selectedBookingInfo = ticketBookingDetails}, props {activeListItem = fromMaybe dummyListItem (ticketBookingDetails.services !! 0), rightButtonDisable = (length ticketBookingDetails.services < 2)}})
--           zooTicketInfoFlow
--     GO_TO_HOME_SCREEN_FROM_TICKET_BOOKING state -> do
--       modifyScreenState $ TicketBookingScreenStateType (\_ ->  TicketBookingScreenData.initData)
--       if state.props.navigateToHome then homeScreenFlow else placeListFlow
--     RESET_SCREEN_STATE -> do
--       modifyScreenState $ TicketBookingScreenStateType (\_ ->  TicketBookingScreenData.initData)
--       zooTicketBookingFlow
--     REFRESH_PAYMENT_STATUS state -> do
--       (GetTicketStatusResp ticketStatus) <- Remote.getTicketStatusBT state.props.selectedBookingId
--       updatePaymentStatusData ticketStatus state.props.selectedBookingId
--       setValueToLocalStore PAYMENT_STATUS_POOLING "false"
--       zooTicketBookingFlow
--     _ -> zooTicketBookingFlow
ticketPaymentFlow :: TicketBookingScreenData -> FlowBT String Unit
ticketPaymentFlow screenData = do
  liftFlowBT $ initiatePaymentPage
  let
    ticketPlaceID = maybe "" (\(TicketPlaceResp ticketPlaceResp) -> ticketPlaceResp.id) screenData.placeInfo
  (CreateOrderRes orderResp) <- Remote.bookTicketsBT (Remote.mkBookingTicketReq screenData) ticketPlaceID
  let shortOrderID = orderResp.order_id
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { data { shortOrderId = shortOrderID }, props { selectedBookingId = shortOrderID } })
  lift $ lift $ doAff $ makeAff \cb -> runEffectFn1 checkPPInitiateStatus (cb <<< Right) $> nonCanceler
  let sdkPayload = maybe (encodeJSON orderResp.sdk_payload) (addLanguageToPayload (getPaymentPageLangKey (getLanguageLocale languageKey))) orderResp.sdk_payload_json
  void $ paymentPageUI sdkPayload
  void $ lift $ lift $ toggleLoader true
  void $ pure $ toggleBtnLoader "" false
  (GetTicketStatusResp ticketStatus) <- Remote.getTicketStatusBT shortOrderID
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { currentStage = BookingConfirmationStage } })
  updatePaymentStatusData ticketStatus shortOrderID
  void $ lift $ lift $ toggleLoader false
  ticketStatusFlow

updatePaymentStatusData :: String -> String -> FlowBT String Unit
updatePaymentStatusData ticketStatus shortOrderID = case ticketStatus of
  "Booked" -> do
    infoRes <- Remote.getTicketBookingDetailsBT shortOrderID
    fillBookingDetails infoRes shortOrderID ticketStatus
  "Pending" -> do
    void $ void $ lift $ lift $ showToast $  "Fetching the status"
    infoRes <- Remote.getTicketBookingDetailsBT shortOrderID
    setValueToLocalStore PAYMENT_STATUS_POOLING "true"
    fillBookingDetails infoRes shortOrderID ticketStatus
  "Failed" -> do
    modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { paymentStatus = PP.Failed } })
  _ -> do
    void $ void $ lift $ lift $ showToast $  getString STR.SOMETHING_WENT_WRONG_TRY_AGAIN_LATER
    modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { currentStage = ticketBookingScreen.props.previousStage } }) -- temporary fix - will remove once 500 INTERNAL_SERVER_ERROR is solved.
    pure unit

zooTicketInfoFlow :: FlowBT String Unit
zooTicketInfoFlow = do
  logField_ <- lift $ lift $ getLogFields
  flow <- UI.ticketInfoScreen
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState -> ticketBookingScreenState { props { currentStage = MyTicketsStage } })
  case flow of
    GO_TO_HOME_SCREEN_FROM_TICKET_INFO -> currentFlowStatus false
    _ -> pure unit

fillBookingDetails :: TicketBookingDetails -> String -> String -> FlowBT String Unit
fillBookingDetails (TicketBookingDetails resp) shortOrderID ticketStatus = do
  let
    serv = resp.services !! 0
  modifyScreenState
    $ TicketBookingScreenStateType
        ( \ticketBookingScreen ->
            ticketBookingScreen
              { props
                { paymentStatus = if ticketStatus == "Booked" then PP.Success else PP.Pending
                }
              , data
                { zooName = resp.ticketPlaceName
                , keyValArray =
                  [ { key: "Date", val: convertUTCtoISC resp.visitDate "Do MMM YYYY" }
                  , { key: "Booking For", val: "" }
                  , { key: "Total Paid", val: ("₹" <> show resp.amount) }
                  , { key: "Booking ID", val: resp.ticketShortId }
                  , { key: "Transaction ID", val: shortOrderID }
                  ]
                    <> case serv of
                        Nothing -> []
                        Just (TicketBookingServiceDetails serviceDetails) ->
                          if isJust serviceDetails.expiryDate then
                            [ { key: "Valid until"
                              , val:
                                  (maybe (convertUTCtoISC (fromMaybe "" serviceDetails.expiryDate) "hh:mm A") (\sl -> fromMaybe "" (convertUTCToISTAnd12HourFormat sl)) serviceDetails.slot)
                                    <> ", "
                                    <> (convertUTCtoISC (fromMaybe "" serviceDetails.expiryDate) "Do MMM YYYY")
                              }
                            ]
                          else
                            []
                , bookedForArray = (map (\(TicketBookingServiceDetails item) -> getTicketBookingForName item) resp.services)
                }
              }
        )
  where
  getTicketBookingForName ticket = ticket.ticketServiceName <> (DS.joinWith "" $ (map (\(TicketBookingCategoryDetails cat) -> if cat.name /= "all" then " ( " <> cat.name <> " ) " else "") ticket.categories))

getCurrentLocationItem :: LocationDetails -> HomeScreenState -> Number -> Number -> Maybe LocationListItemState
getCurrentLocationItem placeDetails state lat lon =
  let
    latLon = getCoordinates state
  in
    Just
      ( locationListStateObj
          { prefixImageUrl = fetchImage FF_ASSET "ny_ic_recent_search"
          , postfixImageUrl = fetchImage FF_ASSET "ny_ic_fav"
          , title = getTitle placeDetails
          , subTitle = getSubTitle placeDetails
          , placeId = placeDetails.placeId
          , lat = Just latLon.lat
          , lon = Just latLon.lon
          , description = placeDetails.formattedAddress
          , tagType = Just $ show LOC_LIST
          , locationItemType = Just SUGGESTED_DESTINATIONS
          , fullAddress = encodeAddress placeDetails.formattedAddress placeDetails.addressComponents Nothing latLon.lat latLon.lon
          }
      )
  where
  getTitle :: LocationDetails -> String
  getTitle placeDetails = (fromMaybe "" ((split (Pattern ",") (placeDetails.formattedAddress)) !! 0))

  getSubTitle :: LocationDetails -> String
  getSubTitle placeDetails =
    let
      formattedAddress = placeDetails.formattedAddress

      index = fromMaybe 0 (indexOf (Pattern ",") formattedAddress)
    in
      drop (index + 2) formattedAddress

  getCoordinates :: HomeScreenState -> { lat :: Number, lon :: Number }
  getCoordinates state =
    { lat:
        if state.props.currentStage /= ConfirmingLocation then
          lat
        else if state.props.isSource == Just true then
          state.props.sourceLat
        else
          state.props.destinationLat
    , lon:
        if state.props.currentStage /= ConfirmingLocation then
          lon
        else if state.props.isSource == Just true then
          state.props.sourceLong
        else
          state.props.destinationLong
    }

rideScheduledFlow :: FlowBT String Unit
rideScheduledFlow = do
  (GlobalState currentState) <- getState
  config <- getAppConfigFlowBT appConfig
  modifyScreenState $ RideScheduledScreenStateType (\rideScheduledScreen -> rideScheduledScreen{ data { config = config} })
  action <- lift $ lift $ runScreen $ UI.rideScheduledScreen currentState.rideScheduledScreen
  case action of
    RideScheduledScreenOutput.CancelRentalRide state -> do
      resp <- lift $ lift $ Remote.cancelRide (Remote.makeCancelRequest state.props.cancelDescription state.props.cancelReasonCode) (state.data.bookingId)
      case resp of
        Right resp -> do
          updateScheduledRides true true
          homeScreenFlow
        Left _ -> do
          void $ lift $ lift $ showToast "Failed To Cancel Ride"
          rideScheduledFlow
    RideScheduledScreenOutput.GoToHomeScreen state -> do
      updateLocalStage HomeScreen
      when (state.data.fromScreen /= Screen.getScreen Screen.RIDE_SCHEDULED_SCREEN) $ modifyScreenState $ HomeScreenStateType (\_ -> HomeScreenData.initData { props { showShimmer = true } })
      modifyScreenState $ RideScheduledScreenStateType (\_ -> RideScheduledScreenData.initData)
      (GlobalState state') <- getState
      updateUserInfoToState state'.homeScreen
      homeScreenFlow
    RideScheduledScreenOutput.GoToSearchLocationScreen updatedState -> do
      modifyScreenState $ RideScheduledScreenStateType (\_ -> updatedState)
      modifyScreenState
        $ SearchLocationScreenStateType
            ( \slsState ->
                SearchLocationScreenData.initData
                  { data { fromScreen = (Screen.getScreen Screen.RIDE_SCHEDULED_SCREEN), srcLoc = Just updatedState.data.source, destLoc = updatedState.data.destination, rideType = slsState.data.rideType }
                  , props { focussedTextField = Just SearchLocDrop, actionType = ST.AddingStopAction }
                  }
            )
      searchLocationFlow
    RideScheduledScreenOutput.GoToMyRidesScreen state -> do
      modifyScreenState $ RideScheduledScreenStateType (\_ -> RideScheduledScreenData.initData)
      myRidesScreenFlow

    RideScheduledScreenOutput.NotificationListenerSO notificationType notificationBody-> do
      (GlobalState globalState) <- getState
      fcmHandler notificationType globalState.homeScreen notificationBody
      homeScreenFlow
    _ -> pure unit
  where
  getStringFromMaybeAddress :: Maybe BookingLocationAPIEntity -> String
  getStringFromMaybeAddress maybeLocation = maybe "" (\location -> decodeAddress (Booking location)) maybeLocation

metroTicketDetailsFlow :: FlowBT String Unit
metroTicketDetailsFlow = do
  logField_ <- lift $ lift $ getLogFields
  flow <- UI.metroTicketDetailsScreen
  case flow of
    BACK_TO_SEARCH_METRO_LOCATION -> metroTicketBookingFlow
    GO_BACK_TO_HOME_SCREEN -> homeScreenFlow
    GO_TO_MY_METRO_TICKETS_FLOW -> metroMyTicketsFlow
    SOFT_CANCEL_BOOKING state -> do
      (APISuccessResp result) <- Remote.metroBookingSoftCancelBT state.data.bookingId
      modifyScreenState $ MetroTicketDetailsScreenStateType (\state -> state { props { stage = MetroSoftCancelStatusStage, showLoader = true } })
      metroTicketDetailsFlow
    HARD_CANCEL_BOOKING state -> do
      (APISuccessResp result) <- Remote.metroBookingHardCancelBT state.data.bookingId
      modifyScreenState $ MetroTicketDetailsScreenStateType (\state -> state { props { stage = MetroHardCancelStatusStage, showLoader = true, isBookingCancellable = Nothing } })
      metroTicketDetailsFlow
    GO_TO_BUS_TICKET_BOOKING_SCREEN_FROM_METRO_TICKET_DETAILS_SCREEN -> do
      modifyScreenState $ MetroTicketDetailsScreenStateType (\_ -> MetroTicketDetailsScreenData.initData)
      busTicketBookingFlow
    GO_TO_BUS_TRACKING -> busTrackingScreenFlow
    _ -> metroTicketDetailsFlow

metroMyTicketsFlow :: FlowBT String Unit
metroMyTicketsFlow = do
  logField_ <- lift $ lift $ getLogFields
  flow <- UI.metroMyTicketsScreen
  case flow of
    GO_TO_METRO_TICKET_DETAILS_FLOW bookingId -> do
      (GetMetroBookingStatusResp resp) <- Remote.getMetroStatusBT bookingId
      let
        (FRFSTicketBookingStatusAPIRes metroTicketBookingStatus) = resp
        tickets = metroTicketBookingStatus.tickets
        busConfigs = RC.getBusFlowConfigs $ getValueToLocalStore CUSTOMER_LOCATION
      if (metroTicketBookingStatus.status == "CONFIRMED") then do
        let goToTracking = busConfigs.showPostBookingTracking && maybe false (\(API.FRFSTicketAPI i) -> i.status /= "EXPIRED") (tickets !! 0)
        if metroTicketBookingStatus.vehicleType == "BUS" && goToTracking then  do
          let route = spy "route" $ (fromMaybe [] metroTicketBookingStatus.routeStations) !! 0
              _ = spy "metroTicketBookingStatus" metroTicketBookingStatus
          let stationList = fromMaybe [] (getStationsFromBusRoute <$> route)
          let routeDetails = case route of 
                      Just (FRFSRouteAPI r) -> {code : r.code, shortName : r.shortName}
                      Nothing -> {code : "", shortName : ""}
 
          let source = maybe Nothing (\(FRFSStationAPI s) -> Just {stationName : s.name,stationCode :s.code}) (stationList !! 0)
          let dest = maybe Nothing (\(FRFSStationAPI s) -> Just {stationName : s.name,stationCode :s.code}) (stationList !! (length stationList -1))
          modifyScreenState $ BusTrackingScreenStateType (\busScreen -> BusTrackingScreenData.initData { data { sourceStation = source, destinationStation = dest, busRouteCode = routeDetails.code, bookingId = bookingId, routeShortName = routeDetails.shortName}, props{ showRouteDetailsTab = false } })
          busTrackingScreenFlow
        else do
          modifyScreenState
            $ MetroTicketDetailsScreenStateType
                ( \metroTicketDetailsState ->
                    let
                      transformedState = metroTicketDetailsTransformer resp metroTicketDetailsState
                    in
                      transformedState { props { previousScreenStage = ST.MetroMyTicketsStage } }
                )
          metroTicketDetailsFlow
      else if (metroTicketBookingStatus.status == "CANCELLED") then do
        modifyScreenState
          $ MetroTicketDetailsScreenStateType
              ( \metroTicketDetailsState ->
                  let
                    transformedState = metroTicketDetailsTransformer resp metroTicketDetailsState
                  in
                    transformedState { props { previousScreenStage = ST.MetroMyTicketsStage, stage = MetroHardCancelStatusStage } }
              )
        metroTicketDetailsFlow
      else if (any (_ == metroTicketBookingStatus.status) [ "PAYMENT_PENDING", "FAILED" ]) then do
        modifyScreenState
          $ MetroTicketStatusScreenStateType
              ( \metroTicketStatusScreen ->
                  let
                    transformedState = metroTicketStatusTransformer resp metroTicketStatusScreen
                  in
                    transformedState { props { entryPoint = MyMetroTicketsToMetroTicketStatus } }
              )
        setValueToLocalStore METRO_PAYMENT_STATUS_POOLING "false"
        metroTicketStatusFlow
      else
        metroMyTicketsFlow
    GO_TO_METRO_TICKET_STAUS_FLOW bookingStatusResp -> do
      modifyScreenState $ MetroTicketStatusScreenStateType (\metroTicketStatusScreen -> metroTicketStatusTransformer bookingStatusResp metroTicketStatusScreen)
      metroTicketStatusFlow
    GO_HOME_FROM_METRO_MY_TICKETS -> homeScreenFlow
    GO_METRO_BOOKING_FROM_METRO_MY_TICKETS -> do
      modifyScreenState
          $ MetroTicketBookingScreenStateType
              ( \state ->
                  state
                    { props { currentStage = ST.ConfirmMetroQuote }
                    }
              )
      metroTicketBookingFlow
    GO_BUS_BOOKING_FROM_METRO_MY_TICKETS -> busTicketBookingFlow
    _ -> metroMyTicketsFlow

viewTicketDetialsFlow :: Maybe String -> FlowBT String Unit
viewTicketDetialsFlow mbBookingId = do
  case mbBookingId of
    Just bookingId -> do
      response <- lift $ lift $ Remote.getMetroBookingStatus bookingId
      case response of
        (Right (GetMetroBookingStatusResp resp)) -> do
          let  (FRFSTicketBookingStatusAPIRes metroTicketBookingStatus) = resp
          if (metroTicketBookingStatus.status == "CONFIRMED") then do
            modifyScreenState
              $ MetroTicketDetailsScreenStateType
                  ( \metroTicketDetailsState ->
                      let
                        transformedState = metroTicketDetailsTransformer resp metroTicketDetailsState
                      in
                        transformedState { props { previousScreenStage = ST.MetroMyTicketsStage } }
                  )
            metroTicketDetailsFlow
          else if (metroTicketBookingStatus.status == "CANCELLED") then do
            modifyScreenState
              $ MetroTicketDetailsScreenStateType
                  ( \metroTicketDetailsState ->
                      let
                        transformedState = metroTicketDetailsTransformer resp metroTicketDetailsState
                      in
                        transformedState { props { previousScreenStage = ST.MetroMyTicketsStage, stage = MetroHardCancelStatusStage } }
                  )
            metroTicketDetailsFlow
          else if (any (_ == metroTicketBookingStatus.status) [ "PAYMENT_PENDING", "FAILED" ]) then do
            modifyScreenState
              $ MetroTicketStatusScreenStateType
                  ( \metroTicketStatusScreen ->
                      let
                        transformedState = metroTicketStatusTransformer resp metroTicketStatusScreen
                      in
                        transformedState { props { entryPoint = MyMetroTicketsToMetroTicketStatus } }
                  )
            setValueToLocalStore METRO_PAYMENT_STATUS_POOLING "false"
            metroTicketStatusFlow
          else
            metroMyTicketsFlow
        _ -> metroMyTicketsFlow
    Nothing -> homeScreenFlow


metroTicketStatusFlow :: FlowBT String Unit
metroTicketStatusFlow = do
  (GlobalState currentState) <- getState
  flow <- UI.metroTicketStatusScreen
  case flow of
    GO_TO_METRO_TICKET_DETAILS state resp -> do
      modifyScreenState $ MetroTicketDetailsScreenStateType (\metroTicketDetailsState -> metroTicketDetailsTransformer resp MetroTicketDetailsScreenData.initData)
      modifyScreenState $ MetroTicketDetailsScreenStateType (\slsState -> slsState { props { previousScreenStage = ST.MetroTicketStatusStage } })
      metroTicketDetailsFlow
    REFRESH_STATUS_AC state -> do
      void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      (GetMetroBookingStatusResp metroStatusResp) <- Remote.getMetroStatusBT state.data.bookingId
      let
        (FRFSTicketBookingStatusAPIRes metroTicketStatusResp) = metroStatusResp

        paymentOrder = metroTicketStatusResp.payment >>= (\(FRFSBookingPaymentAPI payment') -> payment'.paymentOrder)

        shortOrderID = case paymentOrder of
          Just (CreateOrderRes orderResp) -> orderResp.order_id
          Nothing -> ""
      void $ lift $ lift $ toggleLoader false
      modifyScreenState $ MetroTicketDetailsScreenStateType (\metroTicketDetailsState -> metroTicketDetailsTransformer metroStatusResp metroTicketDetailsState)
      modifyScreenState $ MetroTicketStatusScreenStateType (\metroTicketStatusScreen -> metroTicketStatusTransformer metroStatusResp metroTicketStatusScreen)
      metroTicketStatusFlow
    GO_TO_TRY_AGAIN_PAYMENT state -> do
      let 
        (FRFSTicketBookingStatusAPIRes resp) = state.data.resp
        ticketServiceType = if resp.vehicleType == "BUS" then BUS else API.METRO
        routeCode = case (resp.routeStations :: Maybe (Array FRFSRouteAPI)) of
                  Nothing -> ""
                  Just routes -> case head routes of
                                  Just (FRFSRouteAPI route) -> route.code
                                  Nothing -> ""
        sourceName = case (resp.stations :: Array FRFSStationAPI) of
                          routes -> case head routes of
                                          Just (FRFSStationAPI route) -> route.name
                                          Nothing -> ""
                          _ -> ""
        destinationName = case (resp.stations :: Array FRFSStationAPI) of
                          routes -> case last routes of
                                          Just (FRFSStationAPI route) -> route.name
                                          Nothing -> ""
                          _ -> ""
        srcCode = case (resp.stations :: Array FRFSStationAPI) of
                          routes -> case head routes of
                                          Just (FRFSStationAPI route) -> route.code
                                          Nothing -> ""
                          _ -> ""
        destCode = case (resp.stations :: Array FRFSStationAPI) of
                          routes -> case last routes of
                                          Just (FRFSStationAPI route) -> route.code
                                          Nothing -> ""
                          _ -> ""
      modifyScreenState $ MetroTicketBookingScreenStateType (\mtbstate -> mtbstate { props { ticketServiceType = ticketServiceType , currentStage  = if ticketServiceType == BUS then ST.BusTicketSelection else  ST.MetroTicketSelection , routeName = routeCode , srcLat = currentState.homeScreen.props.sourceLat , srcLong = currentState.homeScreen.props.sourceLong , isButtonActive = true}, data {ticketCount = 1 ,srcLoc = sourceName , destLoc = destinationName,srcCode = srcCode, destCode = destCode} })
      metroTicketBookingFlow
    GO_TO_HOME_SCREEN_FROM_METRO_TICKET_STATUS_SCREEN -> homeScreenFlow
    GO_TO_METRO_TICKETS_SCREEN_FROM_METRO_TICKET_STATUS_SCREEN -> metroMyTicketsFlow
    GO_TO_BUS_TICKET_BOOKING_SCREEN_FROM_METRO_TICKET_STATUS_SCREEN -> do
      setValueToLocalStore METRO_PAYMENT_STATUS_POOLING "false"
      modifyScreenState $ BusTicketBookingScreenStateType (\_ -> BusTicketBookingScreenData.initData { data {ticketServiceType = BUS}})
      busTicketBookingFlow

searchLocationFlow :: FlowBT String Unit
searchLocationFlow = do
  (GlobalState globalState) <- getState
  _ <- pure $ spy "debug route searchLocationFlow state" globalState.searchLocationScreen
  action <- lift $ lift $ runScreen $ UI.searchLocationScreen globalState.searchLocationScreen globalState.globalProps
  _ <- pure $ spy "debug route searchLocationFlow action" action
  case action of
    SearchLocationController.AddStop state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> searchLocationFlow)
    SearchLocationController.UpdateLocName state lat lon -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      handleUpdateLocNameFlow state lat lon
    SearchLocationController.Reload state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      when (state.props.searchLocStage == LocateOnMapStage) do
        let
          { currentLat, currentLng } = { currentLat: fromMaybe 0.0 state.data.currentLoc.lat, currentLng: fromMaybe 0.0 state.data.currentLoc.lon }

          focussedField = maybe Nothing (\currField -> if currField == SearchLocPickup then (state.data.srcLoc) else (state.data.destLoc)) (state.props.focussedTextField)
        fullAddress <- getPlaceName currentLat currentLng HomeScreenData.dummyLocation true
        let
          address = maybe "" (\(PlaceName address) -> address.formattedAddress) fullAddress
        modifyScreenState $ SearchLocationScreenStateType (\_ -> state { data { latLonOnMap = fromMaybe (maybe SearchLocationScreenData.dummyLocationInfo (\srcLoc -> srcLoc { address = address }) state.data.srcLoc) focussedField } })
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> searchLocationFlow)
    SearchLocationController.GoToMetroRouteMap state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      let
        currentCity = getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION
      modifyScreenState $ MetroTicketDetailsScreenStateType (\_ -> MetroTicketDetailsScreenData.initData)
      modifyScreenState $ MetroTicketDetailsScreenStateType (\slsState -> slsState { data { city = currentCity }, props { stage = ST.MetroMapStage, previousScreenStage = ST.SearchMetroLocationStage } })
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> metroTicketDetailsFlow)
    SearchLocationController.NoOutput state -> modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
    SearchLocationController.SearchPlace searchString state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> searchPlaceFlow searchString state)
    SearchLocationController.SaveFavLoc state savedLoc -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> checkRedundantFavLocFlow state savedLoc)
    SearchLocationController.ConfirmAndSaveFav state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> confirmAndSaveLocFlow state)
    SearchLocationController.PredictionClicked prediction state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      predictionClickedFlow prediction state
    SearchLocationController.GoToRouteBusSearch state input -> do
     let 
       currentCity = getValueToLocalStore CUSTOMER_LOCATION
     (AutoCompleteResp routeStopresponse) <- Remote.busAutoCompleteBT (show state.data.ticketServiceType) currentCity (show globalState.homeScreen.props.sourceLat <> "," <> show globalState.homeScreen.props.sourceLong) (Just input) "10" Nothing
     if null routeStopresponse.routes && null routeStopresponse.stops then do
      modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData{ props { actionType = BusSearchSelectionAction, canSelectFromFav = false, focussedTextField = Just SearchLocPickup , routeSearch = true , isAutoComplete = false}, data { srcLoc = Nothing, destLoc = Nothing, routeSearchedList = routeStopresponse.routes , stopsSearchedList = routeStopresponse.stops , updatedRouteSearchedList = routeStopresponse.routes , updatedStopsSearchedList = routeStopresponse.stops, rideType = slsState.data.rideType } })
      (App.BackT $ App.BackPoint <$> pure unit) >>= (\_ -> searchPlaceFlow input state)    
     else do
        let rideType =
              if null routeStopresponse.routes
              then STOP
              else if null routeStopresponse.stops
              then ROUTES
              else state.data.rideType
        modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData{ props { actionType = BusSearchSelectionAction, canSelectFromFav = false, focussedTextField = Just SearchLocPickup , routeSearch = true , isAutoComplete = false}, data { rideType = rideType ,srcLoc = Nothing, destLoc = Nothing, routeSearchedList = routeStopresponse.routes , stopsSearchedList = routeStopresponse.stops , updatedRouteSearchedList = routeStopresponse.routes , updatedStopsSearchedList = routeStopresponse.stops } })
     searchLocationFlow
    SearchLocationController.AddFavLoc state tag -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      addFavLocFlow state tag
    SearchLocationController.HomeScreen state -> do
      (GlobalState globalState) <- getState
      void $ (setValueToLocalStore TRACKING_DRIVER) "False"
      when (globalState.homeScreen.props.currentStage == RideAccepted || globalState.homeScreen.props.currentStage == RideStarted)
        $ do
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { route = Nothing } })
      when (globalState.homeScreen.props.currentStage == HomeScreen)
        $ do
            modifyScreenState $ HomeScreenStateType (\_ -> HomeScreenData.initData)
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> homeScreenFlow )
    SearchLocationController.RentalsScreen state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      modifyScreenState $ RentalScreenStateType (\rentalScreenState -> rentalScreenState { data { currentStage = RENTAL_SELECT_PACKAGE }, props { showPrimaryButton = true } })
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> rentalScreenFlow )
    SearchLocationController.LocSelectedOnMap state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      locSelectedOnMapFlow state
    SearchLocationController.MetroTicketBookingScreen state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> metroTicketBookingFlow )
    SearchLocationController.RideScheduledScreen state ->  (App.BackT $ App.NoBack <$> pure unit) >>= (\_ ->  rideScheduledFlow)
    SearchLocationController.CurrentFlowStatus -> (App.BackT $ App.NoBack <$> pure unit) >>= (\_ ->  currentFlowStatus false)
    SearchLocationController.SelectedQuote state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      modifyScreenState $ RentalScreenStateType (\rentalScreenState -> rentalScreenState { data { selectedQuote = state.data.selectedQuote, currentStage = RENTAL_CONFIRMATION }, props { showPrimaryButton = true } })
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ ->  rentalScreenFlow)
    SearchLocationController.NotificationListenerSO notificationType notificationBody-> do
      (GlobalState globalState) <- getState
      fcmHandler notificationType globalState.homeScreen notificationBody
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ ->  homeScreenFlow)
    SearchLocationController.BusTicketBookingScreen state -> do
      modifyScreenState $ BusTicketBookingScreenStateType (\bookingState -> bookingState{ data{ ticketServiceType = state.data.ticketServiceType } })
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ ->  busTicketBookingFlow)
    SearchLocationController.BusRouteStopSearchScreen state -> do
      modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData{ props{ actionType = BusSearchSelectionAction, focussedTextField = Just SearchLocPickup, canSelectFromFav = false, routeSearch = true }, data{ rideType = slsState.data.rideType } })
      pure $ setText (getNewIDWithTag (show SearchLocPickup)) $ ""
      searchLocationFlow
    SearchLocationController.GO_TO_BUS_SEARCH state -> do
      modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData { props { actionType = BusSearchSelectionAction, canSelectFromFav = false, focussedTextField = Just SearchLocPickup , routeSearch = true , isAutoComplete = false , srcLat = state.props.srcLat , srcLong = state.props.srcLong }, data {fromScreen =(Screen.getScreen Screen.BUS_TICKET_BOOKING_SCREEN),ticketServiceType = BUS , srcLoc = Nothing, destLoc = Nothing, rideType = slsState.data.rideType} })
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> searchLocationFlow)
    _ -> pure unit
  where
  locSelectedOnMapFlow :: SearchLocationScreenState -> FlowBT String Unit
  locSelectedOnMapFlow state = do
    modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
    let
      latNum = fromMaybe 0.0 state.data.latLonOnMap.lat

      lonNum = fromMaybe 0.0 state.data.latLonOnMap.lon

      focussedField = fromMaybe SearchLocPickup state.props.focussedTextField
    { pickUpPoints, locServiceable, city, geoJson, specialLocCategory } <- getServiceability latNum lonNum $ fromMaybe SearchLocPickup state.props.focussedTextField
    if locServiceable then do
      case state.props.searchLocStage of
        ConfirmLocationStage -> do
          case state.props.actionType of
            SearchLocationAction -> do
              updateLocationFromMap state { props { focussedTextField = Just SearchLocPickup } }
              enterRideSearchFLow
            AddingStopAction -> do
              updateRentalsData focussedField geoJson state
              addStopFlow state
            _ -> pure unit
        LocateOnMapStage -> do
          case state.props.actionType of
            SearchLocationAction -> do
              updateLocationFromMap state
              (GlobalState globalState) <- getState
              let
                updatedSlsState = globalState.searchLocationScreen
              if state.props.areBothLocMandatory then do
                checkForBothLocs state state.data.srcLoc state.data.destLoc
              else do
                void $ pure $ showKeyboard $ getNewIDWithTag (show SearchLocDrop)
                modifyScreenState
                  $ SearchLocationScreenStateType
                      ( \slsState ->
                          slsState
                            { props { searchLocStage = PredictionsStage, focussedTextField = Just SearchLocDrop }
                            , data { latLonOnMap = SearchLocationScreenData.dummyLocationInfo, specialZoneCoordinates = "", confirmLocCategory = NOZONE, nearByGates = [] }
                            }
                      ) -- restoring to previous state
                (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> searchLocationFlow)
            AddingStopAction -> do
              updateRentalsData focussedField geoJson state -- depending on fromScreen change the screen addStopFlow
              addStopFlow state
            _ -> pure unit
        _ -> pure unit
    else
      modifyScreenState
        $ SearchLocationScreenStateType
            ( \slsState ->
                slsState
                  { props { locUnserviceable = true, searchLocStage = PredictionsStage }
                  , data
                    { latLonOnMap = SearchLocationScreenData.dummyLocationInfo
                    , srcLoc = if focussedField == SearchLocPickup then Nothing else slsState.data.srcLoc
                    , destLoc = if focussedField == SearchLocDrop then Nothing else slsState.data.destLoc
                    }
                  }
            )
    searchLocationFlow

  addStopFlow :: SearchLocationScreenState -> FlowBT String Unit
  addStopFlow state = do
    if state.data.fromScreen == (Screen.getScreen Screen.HOME_SCREEN) then do
      (GlobalState globalState) <- getState
      let
        bookingId = globalState.homeScreen.props.bookingId

        isEdit = globalState.homeScreen.data.driverInfoCardState.destination /= ""

        destLoc = fromMaybe SearchLocationScreenData.dummyLocationInfo state.data.destLoc

        stopLocation = encodeAddress destLoc.address [] Nothing (fromMaybe 0.0 (destLoc.lat)) (fromMaybe 0.0 (destLoc.lon))
      let
        req = Remote.makeStopReq (fromMaybe 0.0 (destLoc.lat)) (fromMaybe 0.0 (destLoc.lon)) stopLocation
      response <- lift $ lift $ Remote.addOrEditStop bookingId req isEdit
      void $ (setValueToLocalStore TRACKING_DRIVER) "False"
      case response of
        Right _ -> do
          let
            driverInfoCard = if isLocalStageOn RideStarted then globalState.homeScreen.data.driverInfoCardState { destinationLat = fromMaybe globalState.homeScreen.data.driverInfoCardState.destinationLat destLoc.lat, destinationLng = fromMaybe globalState.homeScreen.data.driverInfoCardState.destinationLng destLoc.lon, destinationAddress = stopLocation, destination = destLoc.address} else globalState.homeScreen.data.driverInfoCardState
          modifyScreenState
            $ HomeScreenStateType
                ( \homeScreen ->
                    homeScreen
                      { data { route = Nothing, driverInfoCardState = driverInfoCard }
                      , props { stopLoc = Just { lat: fromMaybe 0.0 destLoc.lat, lng: fromMaybe 0.0 destLoc.lon, stopLocAddress: destLoc.address } }
                      }
                ) --, driverInfoCardState {destinationLat = fromMaybe homeScreen.data.driverInfoCardState.destinationLat destLoc.lat, destinationLng = fromMaybe homeScreen.data.driverInfoCardState.destinationLng destLoc.lon}}})
          pure unit
        Left err -> do
          void $ void $ lift $ lift $ showToast $  "Error While " <> (if isEdit then "Editing" else "Adding") <> " Stop"
          pure unit
      homeScreenFlow
    else if (state.data.fromScreen == (Screen.getScreen Screen.RIDE_SCHEDULED_SCREEN)) then do
      (GlobalState globalState) <- getState
      let
        bookingId = globalState.rideScheduledScreen.data.bookingId

        isEdit = isJust globalState.rideScheduledScreen.data.destination

        destLoc = fromMaybe SearchLocationScreenData.dummyLocationInfo state.data.destLoc

        stopLocation = encodeAddress destLoc.address [] Nothing (fromMaybe 0.0 (destLoc.lat)) (fromMaybe 0.0 (destLoc.lon))
      let
        req = Remote.makeStopReq (fromMaybe 0.0 (destLoc.lat)) (fromMaybe 0.0 (destLoc.lon)) stopLocation
      response <- lift $ lift $ Remote.addOrEditStop bookingId req isEdit
      case response of
        Right _ -> do
          modifyScreenState $ RideScheduledScreenStateType (\rideScheduledScreen -> rideScheduledScreen { data { destination = state.data.destLoc } })
          pure unit
        Left err -> do
          void $ void $ lift $ lift $ showToast $  "Error While " <> (if isEdit then "Editing" else "Adding") <> " Stop"
          pure unit
      rideScheduledFlow
    else
      pure unit

  updateRentalsData :: SearchLocationTextField -> String -> SearchLocationScreenState -> FlowBT String Unit
  updateRentalsData focussedField geoJson state = do
    -- if focussedField == SearchLocPickup && geoJson /= "" then do  -- to enable special pickup zone flow
    --   modifyScreenState $
    --     SearchLocationScreenStateType (\slsState -> slsState {props{searchLocStage = PredictionsStage, focussedTextField = Nothing ,locUnserviceable = false, isSpecialZone = true  }
    --                                                           , data {latLonOnMap = SearchLocationScreenData.dummyLocationInfo, specialZoneCoordinates = "", confirmLocCategory = NOZONE, nearByGates = []}}) -- restoring to previous state
    -- else do

    if state.data.fromScreen == (Screen.getScreen Screen.RENTAL_SCREEN) then do
      modifyScreenState $ RentalScreenStateType (\rentalScreen -> rentalScreen { data { pickUpLoc = fromMaybe SearchLocationScreenData.dummyLocationInfo state.data.srcLoc, dropLoc = state.data.destLoc } })
      rentalScreenFlow
    else
      pure unit

  -- rentalScreenFlow
  updateLocationFromMap :: SearchLocationScreenState -> FlowBT String Unit
  updateLocationFromMap state = do
    (GlobalState globalState) <- getState
    let
      focussedField = fromMaybe SearchLocPickup globalState.searchLocationScreen.props.focussedTextField
    if focussedField == SearchLocPickup then do
      modifyScreenState
        $ SearchLocationScreenStateType
            (\slsState -> slsState { data { srcLoc = Just state.data.latLonOnMap }, props { pickUpSelectedOnMap = true } })
    else
      modifyScreenState
        $ SearchLocationScreenStateType
            (\slsState -> slsState { data { destLoc = Just state.data.latLonOnMap } })
    pure unit

  handleUpdateLocNameFlow :: SearchLocationScreenState -> String -> String -> FlowBT String Unit
  handleUpdateLocNameFlow state lat lon = do
    modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
    let
      latNum = fromMaybe 0.0 (fromString lat)

      lonNum = fromMaybe 0.0 (fromString lon)

      focussedField = fromMaybe SearchLocPickup state.props.focussedTextField
    { pickUpPoints, locServiceable, city, geoJson, specialLocCategory } <- getServiceability latNum lonNum $ fromMaybe SearchLocPickup state.props.focussedTextField
    let
      cityName = getCityNameFromCode city

      isSpecialZone =
        (geoJson /= "") && (geoJson /= state.data.specialZoneCoordinates)
          && pickUpPoints
          /= state.data.nearByGates

      locOnMap = state.data.latLonOnMap

      updatedState = { lat: fromString lat, lon: fromString lon, placeId: locOnMap.placeId, address: locOnMap.address, addressComponents: locOnMap.addressComponents, city: cityName, metroInfo: Nothing , busStopInfo : Nothing, stationCode: "" }
    modifyScreenState
      $ SearchLocationScreenStateType
          (\slsState -> slsState { data { latLonOnMap = updatedState, confirmLocCategory = getZoneType specialLocCategory, srcLoc = if focussedField == SearchLocPickup then Just updatedState else state.data.srcLoc, destLoc = if focussedField == SearchLocDrop then Just updatedState else state.data.destLoc } })
    if isSpecialZone then
      specialLocFlow geoJson pickUpPoints specialLocCategory latNum lonNum
    else
      updateLocDetailsFlow state latNum lonNum pickUpPoints cityName

  updateLocDetailsFlow :: SearchLocationScreenState -> Number -> Number -> Array Location -> City -> FlowBT String Unit
  updateLocDetailsFlow state lat lon pickUpPoints cityName = do
    let
      latLonOnMap = state.data.latLonOnMap
    let
      { mapLat, mapLon } = { mapLat: fromMaybe 0.0 latLonOnMap.lat, mapLon: fromMaybe 0.0 latLonOnMap.lon }

      distanceBwLatLon = getDistanceBwCordinates lat lon mapLat mapLon

      isDistMoreThanThreshold = distanceBwLatLon > (state.appConfig.mapConfig.locateOnMapConfig.apiTriggerRadius / 1000.0)

      pickUpPoint = filter (\item -> (item.place == state.data.defaultGate)) pickUpPoints

      gateAddress = fromMaybe HomeScreenData.dummyLocation (head pickUpPoint)

      focussedField = fromMaybe SearchLocPickup state.props.focussedTextField
    when (isDistMoreThanThreshold) do
      fullAddress <- getPlaceName lat lon gateAddress true
      case fullAddress of
        Just (PlaceName address) -> do
          let
            updatedAddress = { address: address.formattedAddress, lat: Just lat, lon: Just lon, placeId: Nothing, city: cityName, addressComponents: encodeAddress address.formattedAddress [] Nothing lat lon, metroInfo: Nothing , busStopInfo : Nothing, stationCode: "" }
          modifyScreenState
            $ SearchLocationScreenStateType
                (\slsState -> slsState { data { latLonOnMap = updatedAddress, confirmLocCategory = NOZONE, srcLoc = if focussedField == SearchLocPickup then Just updatedAddress else state.data.srcLoc, destLoc = if focussedField == SearchLocDrop then Just updatedAddress else state.data.destLoc } })
        Nothing -> void $ void $ lift $ lift $ showToast $  getString STR.SOMETHING_WENT_WRONG_TRY_AGAIN_LATER
    (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> searchLocationFlow)

  specialLocFlow :: String -> Array Location -> String -> Number -> Number -> FlowBT String Unit
  specialLocFlow geoJson pickUpPoints category lat lon = do
    modifyScreenState
      $ SearchLocationScreenStateType
          ( \searchLocScreen ->
              searchLocScreen
                { data
                  { specialZoneCoordinates = geoJson
                  , nearByGates = pickUpPoints
                  , confirmLocCategory = getZoneType category
                  }
                }
          )
    void $ pure $ removeAllPolylines ""
    liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { lat = lat, lon = lon, geoJson = geoJson, points = pickUpPoints }
    (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> searchLocationFlow)

  searchPlaceFlow :: String -> SearchLocationScreenState -> FlowBT String Unit
  searchPlaceFlow searchString state = do
    modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
    (GlobalState globalState) <- getState
    savedLoc <- fetchGlobalSavedLocations
    let
      { currentLat, currentLng } = if elem state.props.actionType [BusSearchSelectionAction , NoBusRouteSelectionAction] then { currentLat: globalState.homeScreen.props.sourceLat, currentLng: globalState.homeScreen.props.sourceLong } else { currentLat: fromMaybe 0.0 state.data.currentLoc.lat, currentLng: fromMaybe 0.0 state.data.currentLoc.lon }
      _ = spy "actionType" state.props.actionType
      { lat, lng } = maybe { lat: currentLat, lng: currentLng } (\loc -> { lat: fromMaybe 0.0 loc.lat, lng: fromMaybe 0.0 loc.lon }) $ maybe Nothing (\currField -> if currField == SearchLocPickup then (state.data.srcLoc) else (state.data.destLoc)) $ state.props.focussedTextField
      config = getCityConfig state.appConfig.cityConfig (getValueToLocalStore CUSTOMER_LOCATION)
      cityConfig = case state.props.focussedTextField of
        Just SearchLocPickup -> config { geoCodeConfig { strictBounds = false } }
        _ -> config
    (SearchLocationResp searchLocationResp) <- Remote.searchLocationBT (Remote.makeSearchLocationReq searchString lat lng (EHC.getMapsLanguageFormat $ getLanguageLocale languageKey) "" cityConfig.geoCodeConfig Nothing "" Nothing)
    let
      sortedByDistanceList = sortPredictionByDistance searchLocationResp.predictions

      predictionList = getLocationList sortedByDistanceList

      sortedRecentsList = updateLocListWithDistance globalState.globalProps.recentSearches lat lng true state.appConfig.suggestedTripsAndLocationConfig.locationWithinXDist

      filteredRecentsList = filterRecentSearches sortedRecentsList predictionList

      filteredPredictionList = differenceOfLocationLists predictionList filteredRecentsList

      updatedLocList =
        map
          ( \item -> do
              let
                savedLocation = getPrediction item savedLoc

                locInSavedLoc = checkPrediction item savedLoc
              if (not locInSavedLoc) then
                item { lat = savedLocation.lat, lon = savedLocation.lon, locationItemType = Just SAVED_LOCATION, postfixImageUrl = fetchImage FF_ASSET "ny_ic_fav_red" }
              else
                item { lat = item.lat, lon = item.lon, locationItemType = item.locationItemType, postfixImageUrl = fetchImage FF_ASSET "ny_ic_fav" }
          )
          (filteredRecentsList <> filteredPredictionList)
    if state.props.actionType == BusSearchSelectionAction then do
       modifyScreenState
          $ SearchLocationScreenStateType
              ( \searchLocationScreen ->
                  searchLocationScreen
                    { data { locationList = spy "updatedLocList" updatedLocList , routeSearchedList = [] , updatedRouteSearchedList = [] , stopsSearchedList = [] , updatedStopsSearchedList = [], ticketServiceType = BUS , srcLoc = Nothing, destLoc = Nothing }
                    , props { showLoader = false , actionType = NoBusRouteSelectionAction , canSelectFromFav = false , routeSearch = true , isAutoComplete = false }
                    }
              )
    else do
        modifyScreenState
          $ SearchLocationScreenStateType
              ( \searchLocationScreen ->
                  searchLocationScreen
                    { data { locationList = updatedLocList}
                    , props { showLoader = false }
                    }
              )
    searchLocationFlow

  checkRedundantFavLocFlow :: SearchLocationScreenState -> Array LocationListItemState -> FlowBT String Unit
  checkRedundantFavLocFlow state savedLoc = do
    modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
    let
      selectedItem = state.data.saveFavouriteCard.selectedItem
    case selectedItem.locationItemType of
      Just RECENTS -> getDistDiff savedLoc (fromMaybe 0.0 selectedItem.lat) (fromMaybe 0.0 selectedItem.lon) (fromMaybe "" selectedItem.placeId)
      Nothing -> getDistDiff savedLoc (fromMaybe 0.0 selectedItem.lat) (fromMaybe 0.0 selectedItem.lon) (fromMaybe "" selectedItem.placeId)
      _ -> do
        (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (selectedItem.title <> "," <> selectedItem.subTitle) selectedItem.placeId (fromMaybe 0.0 selectedItem.lat) (fromMaybe 0.0 selectedItem.lon) selectedItem
        let
          (PlaceName placeName) = maybe SearchLocationScreenData.dummyLocationName identity $ head placeNameResp

          (LatLong placeLatLong) = placeName.location
        (ServiceabilityRes serviceabilityRes) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq placeLatLong.lat placeLatLong.lon) DESTINATION
        case serviceabilityRes.serviceable of
          false -> do
            void $ void $ lift $ lift $ showToast $  getString STR.LOCATION_UNSERVICEABLE
            searchLocationFlow
          _ -> modifyScreenState $ SearchLocationScreenStateType (\_ -> state { data { saveFavouriteCard { selectedItem { lat = Just placeLatLong.lat, lon = Just placeLatLong.lon } } } })
        getDistDiff savedLoc placeLatLong.lat placeLatLong.lon (fromMaybe "" selectedItem.placeId)
    pure unit

  confirmAndSaveLocFlow :: SearchLocationScreenState -> FlowBT String Unit
  confirmAndSaveLocFlow state = do
    modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
    let
      saveFavouriteCard = state.data.saveFavouriteCard

      selectedItem = saveFavouriteCard.selectedItem

      tag = case (toLower saveFavouriteCard.tag) of
        "work" -> "Work"
        "home" -> "Home"
        _ -> saveFavouriteCard.tag
    void $ setValueToLocalStore RELOAD_SAVED_LOCATION "true"
    { lat, long, addressComponents } <- case selectedItem.lat, selectedItem.lon of
      Nothing, Nothing -> fetchLatLong selectedItem tag
      _, _ -> pure $ { lat: selectedItem.lat, long: selectedItem.lon, addressComponents: [] }
    when (isJust lat && isJust long)
      $ do
          resp <- Remote.addSavedLocationBT (encodeAddressDescription saveFavouriteCard.address tag selectedItem.placeId lat long addressComponents)
          void $ void $ lift $ lift $ showToast $  getString STR.FAVOURITE_ADDED_SUCCESSFULLY
    savedLocResp <- lift $ lift $ Remote.getSavedLocationList ""
    case savedLocResp of
      Right (SavedLocationsListRes savedLocs) -> do
        let
          updatedLocList = getUpdatedLocationList state.data.locationList selectedItem.placeId

          savedLocList = AddNewAddress.savedLocTransformer savedLocs.list
        modifyScreenState $ SearchLocationScreenStateType (\searchLocScreenState -> searchLocScreenState { data { locationList = updatedLocList } })
        updateSavedLocations savedLocList
        searchLocationFlow
      Left (err) -> searchLocationFlow

predictionClickedFlow :: LocationListItemState -> SearchLocationScreenState -> FlowBT String Unit
predictionClickedFlow prediction state = do
  (GlobalState currentState) <- getState
  if state.props.actionType == AddingStopAction then do
    void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS) -- TODO : Handlde Loader in IOS Side
    void $ lift $ lift $ toggleLoader true
    let
      { lat, lon, placeId } = { lat: fromMaybe 0.0 prediction.lat, lon: fromMaybe 0.0 prediction.lon, placeId: prediction.placeId }
    (GetPlaceNameResp resp) <- getPlaceNameResp (prediction.title <> ", " <> prediction.subTitle) placeId lat lon prediction
    let
      (PlaceName placeName) = maybe SearchLocationScreenData.dummyLocationName identity $ head resp

      (LatLong placeLatLong) = placeName.location

      { placeLat, placeLon } = { placeLat: placeLatLong.lat, placeLon: placeLatLong.lon }
    maybe
      (pure unit)
      (\currTextField -> onPredictionClicked placeLat placeLon currTextField prediction)
      state.props.focussedTextField
    void $ lift $ lift $ toggleLoader false
    (App.BackT $ App.NoBack <$> pure unit) >>= (\_ ->  searchLocationFlow)
  -- else if state.props.actionType == BusRouteSelectionAction then do
  --       modifyScreenState $ BusTrackingScreenStateType (\busScreen -> busScreen { data { sourceStation = Nothing, destinationStation = Nothing, busRouteCode = state.props.routeSelected} })
  --       busTrackingScreenFlow
  else if state.props.actionType == BusSearchSelectionAction || state.props.actionType == BusRouteSelectionAction then do
          if state.data.searchRideType == API.BUS_ROUTE then do
              let 
                currentCity = getValueToLocalStore CUSTOMER_LOCATION
                busRouteSelected = (spy "routeSelected" state.props.routeSelected)
                busRouteName = state.props.routeName
                _ = spy "searchRideType = " state.data.searchRideType
                -- srcLocation = Just $ SearchLocationScreenData.dummyLocationInfo { busStopInfo = Just { stationName : state.props.stopNameSelected, stationCode : state.props.stopCodeSelected }, address =  state.props.stopNameSelected, stationCode = state.props.stopCodeSelected }
                -- destLocation = Just $ SearchLocationScreenData.dummyLocationInfo { busStopInfo = Just { stationName : state.props.stopNameSelected, stationCode : "" }, address = "", stationCode = "" }
              (GetMetroStationResponse getBusStopResp) <- Remote.getMetroStationBT (show state.data.ticketServiceType) currentCity state.props.routeSelected "" (show currentState.homeScreen.props.sourceLat <> "," <> show currentState.homeScreen.props.sourceLong)
              pure $ setText (getNewIDWithTag (show SearchLocPickup)) ""
              -- if null state.data.routeSearchedList || null state.data.stopsSearchedList then do
              modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData{ props { actionType = BusStopSelectionAction ,canSelectFromFav = false, focussedTextField = Just SearchLocPickup ,routeName = busRouteName , routeSelected = busRouteSelected,srcLat =  state.props.srcLat , srcLong = state.props.srcLong,isAutoComplete = false }, data { fromScreen =(Screen.getScreen Screen.BUS_ROUTE_STOPS_SEARCH_SCREEN) , srcLoc = Nothing, destLoc = Nothing, stopsSearchedList = getBusStopResp , updatedStopsSearchedList = getBusStopResp, rideType = slsState.data.rideType } })
              -- searchLocationFlow
              -- modifyScreenState $ SearchLocationScreenStateType (\_ -> SearchLocationScreenData.initData)
              -- modifyScreenState $ SearchLocationScreenStateType (\slsState -> slsState { props { actionType = BusRouteSelectionAction, canSelectFromFav = false, focussedTextField = Just SearchLocPickup , routeSearch = true , isAutoComplete = false, routeSelected = busRouteSelected , srcLat =  state.props.srcLat , srcLong = state.props.srcLong}, data { ticketServiceType = BUS , srcLoc = Nothing, destLoc = Nothing,  stopsSearchedList = getBusStopResp , updatedStopsSearchedList = getBusStopResp} })
              -- else do
              --     modifyScreenState $ SearchLocationScreenStateType (\_ -> SearchLocationScreenData.initData)
              --     modifyScreenState $ SearchLocationScreenStateType (\slsState -> slsState { props { actionType = BusRouteSelectionAction, canSelectFromFav = false, focussedTextField = Just SearchLocPickup , routeSearch = true , isAutoComplete = false , routeSelected = busRouteSelected}, data {ticketServiceType = BUS , srcLoc = Nothing, destLoc = Nothing , stopsSearchedList = state.data.updatedStopsSearchedList , updatedStopsSearchedList = state.data.updatedStopsSearchedList } })
              -- searchLocationFlow
              let busConfigs = RC.getBusFlowConfigs $ getValueToLocalStore CUSTOMER_LOCATION
              let rideType = if state.data.rideType == ROUTES then Nothing else Just STOP
              if busConfigs.showBusTracking then do
                modifyScreenState $ BusTrackingScreenStateType (\busScreen -> BusTrackingScreenData.initData { data { rideType = rideType, busRouteCode = state.props.routeSelected, routeShortName = prediction.title} , props {srcLat = currentState.homeScreen.props.sourceLat , srcLon = currentState.homeScreen.props.sourceLong } })
                busTrackingScreenFlow
              else if rideType == Just STOP then metroTicketBookingFlow
              else searchLocationFlow

          else do
                let 
                  currentCity = getValueToLocalStore CUSTOMER_LOCATION
                  busRouteSelected = state.props.routeSelected
                  destLocation = Just $ SearchLocationScreenData.dummyLocationInfo { busStopInfo = Just { stationName : state.props.stopNameSelected, stationCode : state.props.stopCodeSelected }, address =  state.props.stopNameSelected, stationCode = state.props.stopCodeSelected }
                  srcLocation = Just $ SearchLocationScreenData.dummyLocationInfo { busStopInfo = Just { stationName : "", stationCode : "" }, address = "", stationCode = "" }
                  autoCompleteBusStop = state.props.autoCompleteBusStop || state.props.actionType == BusSearchSelectionAction
                (GetMetroStationResponse getBusStopResp) <- if autoCompleteBusStop then  Remote.getMetroStationBT (show state.data.ticketServiceType) currentCity "" state.props.stopCodeSelected (show currentState.homeScreen.props.sourceLat <> "," <> show currentState.homeScreen.props.sourceLong) else pure $ GetMetroStationResponse []
                let leastDistanceStationName = getStationWithLeastDistance (GetMetroStationResponse getBusStopResp)
                --------------
                modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData{ props { actionType = BusStopSelectionAction ,  canSelectFromFav = false, focussedTextField = Just SearchLocPickup , routeSelected = busRouteSelected, autoCompleteBusStop = autoCompleteBusStop,srcLat =  state.props.srcLat , srcLong = state.props.srcLong }, data { fromScreen =(Screen.getScreen Screen.BUS_ROUTE_STOPS_SEARCH_SCREEN) , srcLoc = srcLocation, destLoc = destLocation, stopsSearchedList = if autoCompleteBusStop then  leastDistanceStationName else state.data.updatedStopsSearchedList , updatedStopsSearchedList = if autoCompleteBusStop then leastDistanceStationName else state.data.updatedStopsSearchedList, rideType = STOP } })
                pure $ setText (getNewIDWithTag (show SearchLocPickup)) ""
                searchLocationFlow
                -----------------  use below code to skip manual pickup selection in case of stops -------------------------------
                -- let newState = SearchLocationScreenData.initData{ props { actionType = BusStopSelectionAction ,  canSelectFromFav = false, focussedTextField = Just SearchLocPickup , routeSelected = busRouteSelected, autoCompleteBusStop = autoCompleteBusStop,srcLat =  state.props.srcLat , srcLong = state.props.srcLong }, data { fromScreen =(Screen.getScreen Screen.BUS_ROUTE_STOPS_SEARCH_SCREEN) , srcLoc = srcLocation, destLoc = destLocation, stopsSearchedList = if autoCompleteBusStop then  leastDistanceStationName else state.data.updatedStopsSearchedList , updatedStopsSearchedList = if autoCompleteBusStop then leastDistanceStationName else state.data.updatedStopsSearchedList, rideType = STOP } }
                -- let srcLoc =
                --       case newState.data.updatedStopsSearchedList !! 0 of
                --         Just (FRFSStationAPI stop) -> 
                --           Just SearchLocationScreenData.dummyLocationInfo
                --             { address = stop.name
                --             , busStopInfo = Just { stationName : stop.name, stationCode : stop.code }
                --             , stationCode = stop.code }
                --         Nothing -> Nothing
                -- modifyScreenState $ SearchLocationScreenStateType (\slsState -> newState{ data { srcLoc = srcLoc } })
                -- pure $ setText (getNewIDWithTag (show SearchLocPickup)) ""
                -- (GlobalState globalState) <- getState
                -- _ <- pure $ spy "debug route globalState.searchLocationScreen after modify" globalState.searchLocationScreen
                -- predictionClickedFlow prediction globalState.searchLocationScreen
                -------------------------------------------------------------------------------------------------------------------
  else if state.props.actionType == NoBusRouteSelectionAction then do
    let { lat, lon, placeId } = { lat: fromMaybe 0.0 prediction.lat, lon: fromMaybe 0.0 prediction.lon, placeId: prediction.placeId }
    (GetPlaceNameResp resp) <- getPlaceNameResp (prediction.title <> ", " <> prediction.subTitle) placeId lat lon prediction
    let (PlaceName placeName) = maybe SearchLocationScreenData.dummyLocationName identity $ head resp
        (LatLong placeLatLong) = placeName.location
        { placeLat, placeLon } = { placeLat: placeLatLong.lat, placeLon: placeLatLong.lon }
        currentCity = getValueToLocalStore CUSTOMER_LOCATION
        
    (AutoCompleteResp busStopResponse) <- Remote.busAutoCompleteBT (show state.data.ticketServiceType) currentCity (show placeLat <> "," <> show placeLon) (Nothing) "10" Nothing
    pure $ setText (getNewIDWithTag (show SearchLocPickup)) ""
    if (null busStopResponse.stops) then do
      void $ pure $ hideKeyboardOnNavigation true
      modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData{ props { actionType = BusRouteSelectionAction, canSelectFromFav = false, focussedTextField = Just SearchLocPickup , routeSearch = true , isAutoComplete = false,srcLat =  state.props.srcLat , srcLong = state.props.srcLong }, data { ticketServiceType = BUS , destLoc = Nothing,  stopsSearchedList = busStopResponse.stops , updatedStopsSearchedList = busStopResponse.stops, rideType = slsState.data.rideType} })
    else do
      modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData{ props { actionType = BusRouteSelectionAction, canSelectFromFav = false, focussedTextField = Just SearchLocPickup , routeSearch = true , isAutoComplete = false,autoCompleteBusStop = true ,srcLat =  state.props.srcLat , srcLong = state.props.srcLong}, data { ticketServiceType = BUS , destLoc = Nothing,  stopsSearchedList = busStopResponse.stops , updatedStopsSearchedList = busStopResponse.stops, rideType = slsState.data.rideType} })
    searchLocationFlow 
  else if state.props.actionType == MetroStationSelectionAction || state.props.actionType == BusStationSelectionAction || state.props.actionType == BusStopSelectionAction then do
    if (spy "value1 :" (isJust state.data.srcLoc)) && (spy "value2 :" (isJust state.data.destLoc)) then do
      -- TicketBookingScreenStateType
      let
        currentCity = getValueToLocalStore CUSTOMER_LOCATION

        src = maybe "" (\(loc) -> loc.address) state.data.srcLoc

        dest = maybe "" (\(loc) -> loc.address) state.data.destLoc

        srcCode = spy "srcCode" (maybe "" (\(loc) -> loc.stationCode) state.data.srcLoc)

        destCode = spy "destCode" (maybe "" (\(loc) -> loc.stationCode) state.data.destLoc)

        -- srcStation = maybe Nothing (\(loc) -> loc.busStopInfo) state.data.srcLoc
        -- destStation = maybe Nothing (\(loc) -> loc.busStopInfo) state.data.destLoc
        busRouteSelected = state.props.routeSelected
        busRouteName = state.props.routeName
        ticketServiceType = if state.props.actionType == BusStopSelectionAction then API.BUS else API.METRO
      if state.data.rideType == STOP then do
        (App.BackT $ App.BackPoint <$> pure unit) >>= (\_ -> do
          modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state { data { ticketCount = 1 
                                                                                        , srcLoc = src
                                                                                        , destLoc = dest
                                                                                        , srcCode = srcCode
                                                                                        , destCode = destCode 
                                                                                        , routeList = []}
                                                                                , props { currentStage = BusTicketSelection 
                                                                                        , isButtonActive = true
                                                                                        , ticketServiceType =  ticketServiceType
                                                                                        , srcLat = currentState.homeScreen.props.sourceLat 
                                                                                        , srcLong = currentState.homeScreen.props.sourceLong} })
          modifyScreenState $ SelectBusRouteScreenType (\state -> state{ data{ srcLoc = src, destLoc = dest } })
          selectBusRouteScreenFlow srcCode destCode
        )
      else do
        (GetBusRoutesResponse busRoutesResp) <- if ticketServiceType == API.BUS && state.props.actionType == BusStopSelectionAction && state.props.routeSelected == ""
                          then Remote.getBusRoutesBT currentCity srcCode destCode
                          else pure $ GetBusRoutesResponse []
        modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state { data { ticketCount = 1 , srcLoc = src, destLoc = dest, srcCode = srcCode, destCode = destCode , routeList = busRoutesResp}, props {currentStage = BusTicketSelection , isButtonActive = true, ticketServiceType =  ticketServiceType ,routeName = busRouteName, isEmptyRoute = busRouteSelected , srcLat = currentState.homeScreen.props.sourceLat , srcLong = currentState.homeScreen.props.sourceLong} })
        (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> metroTicketBookingFlow)
    else do
      if state.props.focussedTextField == Just SearchLocPickup then
        void $ pure $ showKeyboard (getNewIDWithTag (show SearchLocPickup))
      else do
        void $ pure $ showKeyboard (getNewIDWithTag (show SearchLocDrop))
      modifyScreenState $ SearchLocationScreenStateType (\state -> state { data { updatedMetroStations = state.data.metroStations , updatedStopsSearchedList = state.data.updatedStopsSearchedList } })
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ ->  searchLocationFlow)
  else do
    void $ lift $ lift $ toggleLoader false
    (App.BackT $ App.NoBack <$> pure unit) >>= (\_ ->  searchLocationFlow)
  where
  onPredictionClicked :: Number -> Number -> SearchLocationTextField -> LocationListItemState -> FlowBT String Unit
  onPredictionClicked placeLat placeLon currTextField prediction = do
    { pickUpPoints, locServiceable, city, geoJson, specialLocCategory } <- getServiceability placeLat placeLon currTextField
    let
      focussedField = show currTextField
    if locServiceable then do
      let
        { sourceLoc, destinationLoc, updatedState } = mkSrcAndDestLoc placeLat placeLon state currTextField prediction city
      liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { lat = placeLat, lon = placeLon, geoJson = geoJson, points = pickUpPoints }
      modifyScreenState
        $ SearchLocationScreenStateType
            ( \slsScreen ->
                slsScreen
                  { props { locUnserviceable = false }
                  , data { srcLoc = sourceLoc, destLoc = destinationLoc, latLonOnMap = updatedState, confirmLocCategory = getZoneType specialLocCategory }
                  }
            )
      void $ lift $ lift $ toggleLoader false
      updateCachedLocation prediction placeLat placeLon state locServiceable
      if state.props.areBothLocMandatory then do
        checkForBothLocs state sourceLoc destinationLoc
      else do
        let
          defaultPP = fetchDefaultPickupPoint pickUpPoints placeLat placeLon
        modifyScreenState
          $ SearchLocationScreenStateType
              ( \slsState ->
                  slsState
                    { props { searchLocStage = ConfirmLocationStage }
                    , data { latLonOnMap = updatedState, confirmLocCategory = getZoneType specialLocCategory, nearByGates = pickUpPoints, defaultGate = defaultPP }
                    }
              )
        liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = false, lat = placeLat, lon = placeLon, geoJson = geoJson, points = pickUpPoints, zoomLevel = zoomLevel }
        (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> searchLocationFlow)
    else do
      modifyScreenState $ SearchLocationScreenStateType (\state -> state { props { searchLocStage = PredictionsStage, locUnserviceable = true } })
      void $ lift $ lift $ toggleLoader false
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> searchLocationFlow)
  
  getStationWithLeastDistance :: GetMetroStationResponse -> Array FRFSStationAPI
  getStationWithLeastDistance (GetMetroStationResponse stations) = 
    let
      stationsWithDistance = filter (\(FRFSStationAPI s) -> 
          case s.distance of
            Just _ -> true
            Nothing -> false
        ) stations
      --sortBy (comparing (fromMaybe 0 <<< \(FRFSStationAPI s) -> s.distance)) stationsWithDistance
    in
      sortBy (comparing (fromMaybe 0 <<< \(FRFSStationAPI s) -> s.distance)) stationsWithDistance
      -- case head sortedStations of
      --   Just (FRFSStationAPI station) -> Just station.name
      --   Nothing -> Nothing
  mkSrcAndDestLoc :: Number -> Number -> SearchLocationScreenState -> SearchLocationTextField -> LocationListItemState -> Maybe String -> { sourceLoc :: Maybe LocationInfo, destinationLoc :: Maybe LocationInfo, updatedState :: LocationInfo }
  mkSrcAndDestLoc placeLat placeLon state currTextField prediction city =
    let
      updatedState = { lat: Just placeLat, lon: Just placeLon, city: (getCityNameFromCode city), addressComponents: encodeAddress prediction.description [] Nothing placeLat placeLon, placeId: prediction.placeId, address: prediction.description, metroInfo: Nothing , busStopInfo : Nothing , stationCode: "" }

      sourceLoc = if currTextField == SearchLocPickup then Just updatedState else state.data.srcLoc

      destinationLoc = if currTextField == SearchLocPickup then state.data.destLoc else Just updatedState
    in
      { sourceLoc, destinationLoc, updatedState }

  updateCachedLocation :: LocationListItemState -> Number -> Number -> SearchLocationScreenState -> Boolean -> FlowBT String Unit
  updateCachedLocation prediction placeLat placeLon state locServiceable = do
    saveToRecents prediction placeLat placeLon locServiceable
    let
      { currLat, currLon } = { currLat: fromMaybe 0.0 state.data.currentLoc.lat, currLon: fromMaybe 0.0 state.data.currentLoc.lon }

      { srcLat, srcLon } = maybe ({ srcLat: currLat, srcLon: currLon }) (\loc -> { srcLat: fromMaybe currLat loc.lat, srcLon: fromMaybe currLon loc.lon }) state.data.srcLoc
    when (state.props.focussedTextField == Just SearchLocDrop)
      $ do
          setSuggestionsMapInLocal prediction srcLat srcLon placeLat placeLon locServiceable state.appConfig
    pure unit

addFavLocFlow :: SearchLocationScreenState -> String -> FlowBT String Unit
addFavLocFlow state tag = do
  modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
  savedLoc <- fetchGlobalSavedLocations
  (GlobalState globalState) <- getState
  let
    recents = globalState.globalProps.recentSearches
  modifyScreenState
    $ AddNewAddressScreenStateType
        ( \addNewAddressScreen ->
            addNewAddressScreen
              { props
                { showSavePlaceView = false
                , fromScreen = Screen.getScreen Screen.SEARCH_LOCATION_SCREEN
                , editLocation = false
                , editSavedLocation = false
                , isLocateOnMap = false
                , isBtnActive = false
                , isSearchedLocationServiceable = true
                , tagExists = false
                , placeNameExists = false
                }
              , data
                { addressSavedAs = ""
                , placeName = ""
                , savedLocations = savedLoc
                , locationList = recents
                , recentSearchs { predictionArray = recents }
                , selectedTag = getCardType tag
                , savedTags = getExistingTags savedLoc
                , address = ""
                , activeIndex =
                  case tag of
                    "HOME_TAG" -> Just 0
                    "WORK_TAG" -> Just 1
                    _ -> Just 2
                }
              }
        )
  (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> addNewAddressScreenFlow)

checkForBothLocs :: SearchLocationScreenState -> Maybe LocationInfo -> Maybe LocationInfo -> FlowBT String Unit
checkForBothLocs state sourceLoc destinationLoc =
  if isJust sourceLoc && isJust destinationLoc then do
    if (not state.props.pickUpSelectedOnMap) then do
      let
        lat = maybe 0.0 (\loc -> fromMaybe 0.0 loc.lat) sourceLoc

        lon = maybe 0.0 (\loc -> fromMaybe 0.0 loc.lon) sourceLoc
      { pickUpPoints, locServiceable, city, geoJson, specialLocCategory } <- getServiceability lat lon SearchLocPickup
      let
        defaultPP = fetchDefaultPickupPoint pickUpPoints lat lon
      modifyScreenState
        $ SearchLocationScreenStateType
            ( \slsState ->
                slsState
                  { props { searchLocStage = ConfirmLocationStage, focussedTextField = Just SearchLocPickup }
                  , data { latLonOnMap = fromMaybe SearchLocationScreenData.dummyLocationInfo sourceLoc, confirmLocCategory = getZoneType specialLocCategory, nearByGates = pickUpPoints, defaultGate = defaultPP }
                  }
            )
      liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = false, lat = lat, lon = lon, geoJson = geoJson, points = pickUpPoints, zoomLevel = zoomLevel }
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> searchLocationFlow)
    else
      enterRideSearchFLow
  else do
    void $ pure $ showKeyboard $ getNewIDWithTag (show SearchLocDrop)
    modifyScreenState
      $ SearchLocationScreenStateType
          ( \slsState ->
              slsState
                { props { focussedTextField = Just SearchLocDrop, searchLocStage = PredictionsStage }
                , data { latLonOnMap = SearchLocationScreenData.dummyLocationInfo }
                }
          ) -- restoring to previous statez
    (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> searchLocationFlow)

getSrcAndDestLoc :: SearchLocationScreenState -> { srcLat :: Number, srcLon :: Number, srcAddressComponents :: Address, srcAddress :: String, srcCity :: City, destLat :: Number, destLon :: Number, destAddressComponents :: Address, destAddress :: String, destCity :: City }
getSrcAndDestLoc state =
  let
    defaultLoc = { lat: 0.0, lon: 0.0, addressComponents: dummyAddress, address: "", city: AnyCity }

    extractLoc = maybe defaultLoc (\loc -> { lat: fromMaybe 0.0 loc.lat, lon: fromMaybe 0.0 loc.lon, addressComponents: loc.addressComponents, address: loc.address, city: loc.city })

    { lat: srcLat, lon: srcLon, addressComponents: srcAddressComponents, address: srcAddress, city: srcCity } = extractLoc state.data.srcLoc

    { lat: destLat, lon: destLon, addressComponents: destAddressComponents, address: destAddress, city: destCity } = extractLoc state.data.destLoc
  in
    { srcLat, srcLon, srcAddressComponents, srcAddress, srcCity, destLat, destLon, destAddressComponents, destAddress, destCity }

getServiceability :: Number -> Number -> SearchLocationTextField -> FlowBT String SearchLocationController.ServiceabilityResponse
getServiceability placeLat placeLon currTextField = do
  let
    serviceabilityType = if currTextField == SearchLocPickup then ORIGIN else DESTINATION
  (ServiceabilityRes serviceabilityRes) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq placeLat placeLon) serviceabilityType
  let
    city = serviceabilityRes.city

    locServiceable = serviceabilityRes.serviceable

    (SpecialLocation specialLoc) = fromMaybe HomeScreenData.specialLocation (serviceabilityRes.specialLocation)

    geoJson = transformGeoJsonFeature specialLoc.geoJson specialLoc.gatesInfo

    specialLocCategory = specialLoc.category

    pickUpPoints = mapSpecialZoneGates specialLoc.gatesInfo
  pure $ { pickUpPoints, locServiceable, city, geoJson, specialLocCategory }

fetchLatLong :: LocationListItemState -> String -> FlowBT String { lat :: Maybe Number, long :: Maybe Number, addressComponents :: Array AddressComponents }
fetchLatLong selectedItem tag = do
  case selectedItem.placeId of
    Just placeId -> do
      (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (selectedItem.title <> "," <> selectedItem.subTitle) (Just placeId) (fromMaybe 0.0 selectedItem.lat) (fromMaybe 0.0 selectedItem.lon) selectedItem
      let
        (PlaceName placeName) = maybe SearchLocationScreenData.dummyLocationName identity $ head placeNameResp

        (LatLong placeLatLong) = placeName.location
      pure { lat: Just placeLatLong.lat, long: Just placeLatLong.lon, addressComponents: placeName.addressComponents }
    Nothing -> pure { lat: Nothing, long: Nothing, addressComponents: [] }

getDistDiff :: Array LocationListItemState -> Number -> Number -> String -> FlowBT String Unit
getDistDiff savedLoc lat lon placeId = do
  let
    distanceInfo = getDistInfo savedLoc "" lat lon placeId
  case distanceInfo.locExistsAs of
    "" -> modifyScreenState $ SearchLocationScreenStateType (\searchLocScreenState -> searchLocScreenState { props { showSaveFavCard = true } })
    _ -> do
      void $ lift $ lift $ showToast (getString STR.ALREADY_EXISTS)
      modifyScreenState $ SearchLocationScreenStateType (\searchLocScreenState -> searchLocScreenState { data { saveFavouriteCard { selectedItem = locationListStateObj } } })
  searchLocationFlow

updateSavedLocations :: Array LocationListItemState -> FlowBT String Unit
updateSavedLocations savedLocs = do
  modifyScreenState
    $ GlobalPropsType
        (\globalProps -> globalProps { savedLocations = savedLocs })

fetchGlobalSavedLocations :: FlowBT String (Array LocationListItemState)
fetchGlobalSavedLocations = do
  (GlobalState globalState) <- getState
  pure $ (globalState.globalProps.savedLocations)

activateSafetyScreenFlow :: FlowBT String Unit
activateSafetyScreenFlow = do
  flow <- UI.activateSafetyScreen
  case flow of
    ActivateSafetyScreen.GoBack state -> do
      if state.props.fromScreen == Just RideCompletedScreen then riderRideCompletedScreenFlow
      else if state.props.isFromSafetyCenter
        then do
          modifyScreenState $ NammaSafetyScreenStateType (\safetyScreen -> safetyScreen { props { isFromSafetyCenter = false } })
          dataFetchScreenFlow (DataExplainWithFetchSD.stageData $ SafetyDrill []) 0
        else homeScreenFlow
    ActivateSafetyScreen.GoToEmergencyContactScreen state -> do
      modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactScreen -> emergencyContactScreen { props { fromSosFlow = true, appName = state.props.appName }, data { emergencyContactsList = state.data.emergencyContactsList } })
      emergencyScreenFlow
    ActivateSafetyScreen.CreateSos state flow -> do
      let
        rideId = case state.data.lastRideDetails of
          Nothing -> state.data.rideId
          Just ride -> ride.rideId

        flowType = show flow
      (GlobalState gState) <- getState
      if state.props.showTestDrill then do
        void $ lift $ lift $ Remote.createMockSos (gState.homeScreen.props.currentStage == RideStarted) false
        void $ pure $ cleverTapCustomEvent "ny_user_test_drill"
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { sosBannerType = Nothing } })
        pure unit
      else do
        (UserSosRes res) <- Remote.userSosBT $ Remote.makeUserSosReq (Remote.createUserSosFlow flowType "") rideId state.props.reportPastRide (flow == SafetyFlow) state.data.currentLatLon (Just state.props.reportPastRide)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {chatcallbackInitiated = false} , data { contactList = Nothing } })
        if flow == SafetyFlow then do
          modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { data { sosId = res.sosId } })
          void $ pure $ cleverTapCustomEventWithParams "ny_user_sos_activated" "current_time" (getCurrentUTC "")
        else
          void $ pure $ cleverTapCustomEvent "ny_user_call_police_activated"
        setValueToLocalStore IS_SOS_ACTIVE "true"
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { props { shouldCallAutomatically = true }, data { sosType = Just flow } })
      if flow /= SafetyFlow then activateSafetyScreenFlow else sosActiveFlow state state.props.isFromSafetyCenter
    ActivateSafetyScreen.GoToSosScreen state -> sosActiveFlow state false
    ActivateSafetyScreen.GoToEducationScreen state -> safetyEducationFlow
    ActivateSafetyScreen.GoToIssueScreen state -> do
      let
        language = fetchLanguage $ getLanguageLocale languageKey
      (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language "f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0" "" state.data.rideId ""
      let
        getOptionsRes' = mapWithIndex (\index (Option optionObj) -> optionObj { option = (show (index + 1)) <> ". " <> (reportIssueMessageTransformer optionObj.option) }) getOptionsRes.options

        messages' = mapWithIndex (\index (Message currMessage) -> makeChatComponent' (reportIssueMessageTransformer currMessage.message) currMessage.messageTitle currMessage.messageAction currMessage.label "Bot" (getCurrentUTC "") "Text" (500 * (index + 1))) getOptionsRes.messages

        chats' =
          map
            ( \(Message currMessage) ->
                Chat
                  { chatId: currMessage.id
                  , chatType: "IssueMessage"
                  , timestamp: (getCurrentUTC "")
                  }
            )
            getOptionsRes.messages
      void $ pure $ cleverTapCustomEvent "ny_user_report_safety_issue_activated"
      modifyScreenState $ ReportIssueChatScreenStateType (\_ -> ReportIssueChatScreenData.initData { data { entryPoint = ReportIssueChatScreenData.SafetyScreen, chats = chats', tripId = Just state.data.rideId, selectedCategory = { categoryName : "Safety Related Issue", categoryId : "f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0", categoryImageUrl : Nothing, categoryAction : Nothing, isRideRequired : false, maxAllowedRideAge : Nothing, categoryType : "Category", allowedRideStatuses : Nothing} , options = getOptionsRes', chatConfig { messages = messages' }, selectedRide = Nothing } })
      flowRouter IssueReportChatScreenFlow
    ActivateSafetyScreen.NotifyMockDrill state -> do
      _ <- lift $ lift $ Remote.createMockSos (not $ DS.null state.data.rideId) true
      activateSafetyScreenFlow
    ActivateSafetyScreen.GoToDataFetchScreen state -> do
      if state.data.autoCallDefaultContact
        then do
          modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactScreen -> emergencyContactScreen { data{ emergencyContactsList = state.data.emergencyContactsList }, props { showDropDown = false, fromNewSafetyFlow= true, saveEmergencyContacts = true, getDefaultContacts = length state.data.emergencyContactsList > 1 } })
          emergencyScreenFlow
        else do
          modifyScreenState $ DataFetchScreenStateType (\ dataFetchScreen -> dataFetchScreen { data { emergencyContactsList = state.data.emergencyContactsList } })
          dataFetchScreenFlow (DataExplainWithFetchSD.stageData $ EmergencyActions []) 1

safetySettingsFlow :: FlowBT String Unit
safetySettingsFlow = do
  flow <- UI.safetySettingsScreen
  case flow of
    SafetySettingsScreen.GoBack updatedState -> homeScreenFlow
    SafetySettingsScreen.PostEmergencySettings state -> do
      updateEmergencySettings state
      safetySettingsFlow
    SafetySettingsScreen.GoToEmergencyContactScreen updatedState -> do
      modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactScreen -> emergencyContactScreen { props { fromSosFlow = false, appName = updatedState.props.appName }, data { emergencyContactsList = updatedState.data.emergencyContactsList } })
      emergencyScreenFlow
    SafetySettingsScreen.GoToEducationScreen updatedState -> safetyEducationFlow
    SafetySettingsScreen.GoToSetupScreen updatedState -> setupSafetySettingsFlow
    SafetySettingsScreen.GoToActivateSosScreen state -> activateSafetyScreenFlow
    SafetySettingsScreen.PostContacts state -> do
      void $ Remote.emergencyContactsBT (Remote.postContactsReq $ getDefaultPriorityList state.data.emergencyContactsList)
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { data { emergencyContactsList = state.data.emergencyContactsList } })
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {chatcallbackInitiated = false} , data { contactList = Nothing } })
      safetySettingsFlow

setupSafetySettingsFlow :: FlowBT String Unit
setupSafetySettingsFlow = do
  flow <- UI.setupSafetySettingsScreen
  logField_ <- lift $ lift $ getLogFields
  case flow of
    SetupSafetySettingsScreen.GoBack state -> safetySettingsFlow
    SetupSafetySettingsScreen.PostContacts state -> do
      void $ Remote.emergencyContactsBT (Remote.postContactsReq $ getDefaultPriorityList state.data.emergencyContactsList)
      if state.props.showInfoPopUp then do
          void $ lift $ lift $ showToast (getString STR.CONTACT_REMOVED_SUCCESSFULLY)
      else
        void $ lift $ lift $ showToast $  getString STR.TRUSTED_CONTACS_ADDED_SUCCESSFULLY
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> state { props { showInfoPopUp = false } })
      setupSafetySettingsFlow
    SetupSafetySettingsScreen.Refresh state -> pure unit
    SetupSafetySettingsScreen.PostEmergencySettings state -> do
      updateEmergencySettings state
      void $ liftFlowBT $ logEvent logField_ "finish_safety_setup_tapped"
      safetySettingsFlow
    SetupSafetySettingsScreen.GoToEmergencyContactScreen state -> do
      modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactScreen -> emergencyContactScreen { props { fromSosFlow = false, appName = state.props.appName }, data { emergencyContactsList = state.data.emergencyContactsList } })
      emergencyScreenFlow

sosActiveFlow :: NammaSafetyScreenState -> Boolean -> FlowBT String Unit
sosActiveFlow state isFromSafetyCenter = do
  flow <- UI.sosActiveScreen
  case flow of
    SosActiveScreen.UpdateAsSafe state -> do
      let
        sosId = if state.props.showTestDrill then "mock-sos" else state.data.sosId
      void $ lift $ lift $ Remote.markRideAsSafe sosId state.props.showTestDrill state.props.reportPastRide
      when (not $ DS.null state.data.rideId)
        $ do
            void $ pure $ cleverTapCustomEventWithParams "ny_user_sos_marked_safe" "current_time" (getCurrentUTC "")
            pure unit
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { data { sosId = "" } })
      setValueToLocalStore IS_SOS_ACTIVE "false"
      if isFromSafetyCenter then do
        modifyScreenState $ NammaSafetyScreenStateType (\safetyScreen -> safetyScreen { props { isFromSafetyCenter = false } })
        nammaSafetyFlow
      else if state.props.fromScreen == Just RideCompletedScreen then riderRideCompletedScreenFlow else homeScreenFlow
    SosActiveScreen.UpdateAction state comment -> do
      res <- Remote.userSosStatusBT state.data.sosId (Remote.makeSosStatus "Pending" comment)
      sosActiveFlow state isFromSafetyCenter
    SosActiveScreen.GoToEducationScreen state -> safetyEducationFlow
    SosActiveScreen.GoBack state -> if state.props.fromScreen == Just RideCompletedScreen then riderRideCompletedScreenFlow else homeScreenFlow
    _ -> sosActiveFlow state isFromSafetyCenter
  pure unit

safetyEducationFlow :: FlowBT String Unit
safetyEducationFlow = do
  let
    videoList = RC.safetyVideoConfigData (DS.toLower $ getValueToLocalStore CUSTOMER_LOCATION) $ fetchLanguage $ getLanguageLocale languageKey
  modifyScreenState $ NammaSafetyScreenStateType (\safetyScreen -> safetyScreen { data { videoList = if null safetyScreen.data.videoList then videoList else safetyScreen.data.videoList } })
  void $ pure $ cleverTapCustomEvent "ny_user_safety_learn_more_clicked"
  flow <- UI.safetyEducationScreen
  case flow of
    SafetyEducationScreen.Refresh _ -> safetyEducationFlow
    SafetyEducationScreen.GoToHomeScreen _ -> homeScreenFlow
    _ -> safetyEducationFlow
  pure unit

updateEmergencySettings :: ST.NammaSafetyScreenState -> FlowBT String Unit
updateEmergencySettings state = do
  let
    req =
      UpdateEmergencySettingsReq
        { shareEmergencyContacts: Just state.data.shareToEmergencyContacts
        , shareTripWithEmergencyContactOption: Just state.data.shareTripWithEmergencyContactOption
        , nightSafetyChecks: Just state.data.nightSafetyChecks
        , hasCompletedSafetySetup: Just true
        , autoCallDefaultContact: Nothing
        , enableOtpLessRide: Nothing
        , enablePostRideSafetyCheck: Nothing
        , enableUnexpectedEventsCheck: Nothing
        , hasCompletedMockSafetyDrill: Just state.data.hasCompletedMockSafetyDrill
        , informPoliceSos: Nothing
        , notifySafetyTeamForSafetyCheckFailure: Nothing
        , notifySosWithEmergencyContacts: Nothing
        , shakeToActivate: Nothing
        , shareTripWithEmergencyContacts: Nothing
        }

    wasSetupAlreadyDone = state.data.hasCompletedSafetySetup
  void $ lift $ lift $ Remote.updateEmergencySettings req
  if not wasSetupAlreadyDone then do
    void $ lift $ lift $ showToast $  getString STR.NAMMA_SAFETY_IS_SET_UP
    void $ Remote.emergencyContactsBT $ Remote.postContactsReq $ map (\item -> item { enableForFollowing = true }) state.data.emergencyContactsList
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { safetySettings = Nothing, chatcallbackInitiated = false}, data{contactList = Nothing } })
  else
    pure unit
  modifyScreenState
    $ HomeScreenStateType
        ( \homeScreen ->
            homeScreen
              { props
                { sosBannerType =
                  if not state.data.hasCompletedMockSafetyDrill then
                    Just ST.MOCK_DRILL_BANNER
                  else
                    Nothing
                }
              , data { settingSideBar { hasCompletedSafetySetup = true } }
              }
        )

checkForSpecialZoneAndHotSpots :: HomeScreenState -> ServiceabilityRes -> Number -> Number -> FlowBT String Unit
checkForSpecialZoneAndHotSpots state (ServiceabilityRes serviceabilityResp) lat lon = do
  let
    (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation serviceabilityResp.specialLocation

    pickUpPoints = mapSpecialZoneGates srcSpecialLocation.gatesInfo

    geoJson = transformGeoJsonFeature srcSpecialLocation.geoJson srcSpecialLocation.gatesInfo

    zoneType = getZoneType srcSpecialLocation.category

    canUpdateHotSpots = maybe true (\point -> (getDistanceBwCordinates lat lon point.lat point.lng) * 1000.0 > state.data.config.mapConfig.locateOnMapConfig.hotSpotConfig.updateHotSpotOutSideRange) state.props.hotSpot.centroidPoint

    locationName = srcSpecialLocation.locationName
  if not (DS.null geoJson) && not (null pickUpPoints) then do
    if (geoJson /= state.data.polygonCoordinates || pickUpPoints /= state.data.nearByPickUpPoints) then do
      modifyScreenState
        $ HomeScreenStateType
            ( \homeScreen ->
                homeScreen
                  { data
                    { polygonCoordinates = geoJson
                    , nearByPickUpPoints = pickUpPoints
                    }
                  , props
                    { city = getCityNameFromCode serviceabilityResp.city
                    , defaultPickUpPoint = (fromMaybe HomeScreenData.dummyLocation (pickUpPoints !! 0)).place
                    , isSpecialZone = not (DS.null geoJson)
                    , confirmLocationCategory = zoneType
                    , hotSpot { centroidPoint = Nothing }
                    , locateOnMapProps { sourceLocationName = Just locationName }
                    }
                  }
            )
      if isLocalStageOn EditPickUpLocation && os == "IOS" then void $ pure $ removeAllPolygons "" else void $ pure $ removeAllPolylines ""
      liftFlowBT
        $ runEffectFn1 locateOnMap
            locateOnMapConfig
              { lat = if isLocalStageOn EditPickUpLocation then state.data.driverInfoCardState.initialPickupLat else lat
              , lon = if isLocalStageOn EditPickUpLocation then state.data.driverInfoCardState.initialPickupLon else lon
              , geoJson = geoJson
              , points = pickUpPoints
              , zoomLevel = zoomLevel
              , labelId = getNewIDWithTag "LocateOnMapPin"
              , locationName = locationName
              , specialZoneMarkerConfig { labelImage = zoneLabelIcon zoneType }
              , editPickUpThreshold = state.data.config.mapConfig.locateOnMapConfig.editPickUpThreshold
              , editPickupLocation = isLocalStageOn EditPickUpLocation
              , circleConfig = editPickupCircleConfig
              }
      homeScreenFlow
    else
      pure unit
  else if not (null serviceabilityResp.hotSpotInfo) && canUpdateHotSpots && state.data.config.mapConfig.locateOnMapConfig.hotSpotConfig.enableHotSpot then do
    let
      points = filterHotSpots state serviceabilityResp.hotSpotInfo lat lon
    if (state.data.nearByPickUpPoints /= points && not (null points)) then do
      if isLocalStageOn EditPickUpLocation && os == "IOS" then void $ pure $ removeAllPolygons "" else void $ pure $ removeAllPolylines ""
      modifyScreenState
        $ HomeScreenStateType
            ( \homeScreen ->
                homeScreen
                  { data
                    { polygonCoordinates = ""
                    , nearByPickUpPoints = points
                    }
                  , props
                    { isSpecialZone = false
                    , defaultPickUpPoint = (fromMaybe HomeScreenData.dummyLocation (points !! 0)).place
                    , confirmLocationCategory = zoneType
                    , hotSpot { centroidPoint = Just { lat: lat, lng: lon } }
                    }
                  }
            )
      liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { lat = if isLocalStageOn EditPickUpLocation then state.data.driverInfoCardState.initialPickupLat else 0.0, lon = if isLocalStageOn EditPickUpLocation then state.data.driverInfoCardState.initialPickupLon else 0.0, points = points, zoomLevel = zoomLevel, labelId = getNewIDWithTag "LocateOnMapPin", editPickUpThreshold = state.data.config.mapConfig.locateOnMapConfig.editPickUpThreshold, editPickupLocation = isLocalStageOn EditPickUpLocation, circleConfig = editPickupCircleConfig}
    else
      pure unit
  else
    pure unit
firstRideCompletedEvent :: String -> FlowBT String Unit
firstRideCompletedEvent str = do
  logField_ <- lift $ lift $ getLogFields
  (GlobalState globalState) <- getState
  let
    appName = fromMaybe "" $ runFn3 getAnyFromWindow "appName" Nothing Just
    eventPrefix = case appName of
      "Mana Yatri" -> "my_"
      "Yatri" -> "y_"
      "Namma Yatri" -> "ny_"
      _ -> fromMaybe "" $ (DS.split (DS.Pattern " ") appName) !! 0

    firstRideEventCheck = getValueToLocalStore CUSTOMER_FIRST_RIDE
  if firstRideEventCheck == "false" then do
    let
      clientId = getValueToLocalStore CUSTOMER_CLIENT_ID
    rideBookingListResponse <- lift $ lift $ HelpersAPI.callApi $ Remote.makeRideBookingListWithStatus "2" "0" "COMPLETED" (Just clientId)
    case rideBookingListResponse of
      Right (RideBookingListRes listResp) -> do
        let
          arraySize = Arr.length listResp.list
        if (arraySize == 1) then do
          void $ liftFlowBT $ logEvent logField_ $ eventPrefix <> "user_first_ride_completed"
          liftFlowBT $ logEventWithMultipleParams logField_ (eventPrefix <> "user_first_ride_completed_with_props") $  rideCompletedEventParams globalState.homeScreen
          setValueToLocalStore CUSTOMER_FIRST_RIDE "true"
        else if arraySize > 1 then do
          setValueToLocalStore CUSTOMER_FIRST_RIDE "true"
        else
          setValueToLocalStore CUSTOMER_FIRST_RIDE "false"
      Left (err) -> pure unit
  else
    pure unit

fetchParticularIssueCategory :: String -> FlowBT String (Maybe Category)
fetchParticularIssueCategory categoryLabel = do
  let language = fetchLanguage $ getLanguageLocale languageKey
  (GetCategoriesRes response) <- Remote.getCategoriesBT language
  let selfServeCategories = filter (\(Category category) -> category.categoryType == "Category") response.categories
  pure $ Arr.find (\(Category item) -> item.label == categoryLabel) selfServeCategories

rentalScreenFlow :: FlowBT String Unit
rentalScreenFlow = do
  (GlobalState currentState) <- getState
  latestScheduledRides <- FlowCache.fetchAndUpdateScheduledRides true
  modifyScreenState $ RentalScreenStateType (\rentalScreen -> rentalScreen { data { latestScheduledRides = latestScheduledRides}})
  action <- lift $ lift $ runScreen $ UI.rentalScreen currentState.rentalScreen
  case action of
    RentalScreenController.DoRentalSearch state -> findRentalEstimates state

    RentalScreenController.GoToHomeScreen state maybeInvalidBookingDetail -> do
      -- when (isJust maybeInvalidBookingDetail) do
      --   modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{invalidBookingId = maybe Nothing (\invalidBookingDetail -> Just invalidBookingDetail.bookingId) maybeInvalidBookingDetail}, props {showScheduledRideExistsPopUp = true}})
      --   modifyScreenState $ RentalScreenStateType (\_ -> RentalScreenData.initData)
      modifyScreenState $ RentalScreenStateType (\rentalScreen -> rentalScreen { data { latestScheduledRides = Nothing}})
      homeScreenFlow
    RentalScreenController.SearchLocationForRentals updatedState locToBeUpdated -> do
      let
        locToBeUpdated' = case locToBeUpdated of
          "PickUpLoc" -> SearchLocPickup
          "FirstStop" -> SearchLocDrop
          _ -> SearchLocPickup
      modifyScreenState
        $ RentalScreenStateType (\rentalScreen -> updatedState)
      (GlobalState globalState) <- getState
      modifyScreenState
        $ SearchLocationScreenStateType
            ( \slsState ->
                SearchLocationScreenData.initData
                  { data { locationList = globalState.globalProps.cachedSearches, fromScreen = (Screen.getScreen Screen.RENTAL_SCREEN), srcLoc = Just updatedState.data.pickUpLoc, destLoc = updatedState.data.dropLoc, rideType = slsState.data.rideType }
                  , props { focussedTextField = Just locToBeUpdated' }
                  }
            )
      searchLocationFlow
    RentalScreenController.UpdateQuoteList updatedState -> do
      modifyScreenState $ RentalScreenStateType (\_ -> updatedState)
      rentalScreenFlow
    RentalScreenController.GoToRideScheduledScreen updatedState -> do
      rideScheduledFlow
    RentalScreenController.GoToSelectVariant updatedState -> do
      modifyScreenState $ RentalScreenStateType (\_ -> updatedState)
      searchLocationFlow
    RentalScreenController.OnRentalRideConfirm updatedState -> do
      let
        quoteConfig = updatedState.data.selectedQuote

        selectedQuote =
          ( fromMaybe
                { quoteDetails: ChooseVehicle.config
                , index: 0
                , activeIndex: 0
                , fareDetails: { plannedPerKmRate: 0, baseFare: 0, includedKmPerHr: 0, perExtraKmRate: 0, perExtraMinRate: 0, perHourCharge: 0, nightShiftCharge: 0, tollCharges: Nothing, deadKmFare: Nothing }
                }
                quoteConfig
            )
            .quoteDetails
      setValueToLocalStore SELECTED_VARIANT selectedQuote.vehicleVariant
      response <- lift $ lift $ Remote.rideConfirm (selectedQuote.id)
      case response of
        Right (ConfirmRes resp) -> do
          modifyScreenState $ RentalScreenStateType (\_ -> updatedState { data { bookingId = resp.bookingId } })
          let
            dropLoc = fromMaybe SearchLocationScreenData.dummyLocationInfo updatedState.data.dropLoc
          let
            diffInSeconds = EHC.compareUTCDate (if updatedState.data.startTimeUTC == "" then (getCurrentUTC "") else updatedState.data.startTimeUTC) (getCurrentUTC "")

            isNow = diffInSeconds < 60 * 30

            pickUpLocLat = fromMaybe 0.0 updatedState.data.pickUpLoc.lat

            pickUpLocLon = fromMaybe 0.0 updatedState.data.pickUpLoc.lon

            dropLocLat = fromMaybe 0.0 dropLoc.lat

            dropLocLon = fromMaybe 0.0 dropLoc.lon
          srcFullAddress <- getPlaceName pickUpLocLat pickUpLocLon HomeScreenData.dummyLocation true
          destFullAddress <- getPlaceName dropLocLat dropLocLon HomeScreenData.dummyLocation true
          let
            source = maybe ("") (\(PlaceName address) -> address.formattedAddress) srcFullAddress

            dest = if isJust updatedState.data.dropLoc then maybe ("") (\(PlaceName address) -> address.formattedAddress) destFullAddress else ""
          if isNow then do
            void $ liftFlowBT
              $ setFlowStatusData
                  ( FlowStatusData
                      { source: { lat: pickUpLocLat, lng: pickUpLocLon, place: source, address: Nothing, city: getCityCodeFromCity updatedState.data.pickUpLoc.city, isSpecialPickUp: Nothing }
                      , destination: { lat: dropLocLat, lng: dropLocLon, place: dest, address: Nothing, city: Nothing, isSpecialPickUp: Nothing }
                      , sourceAddress: encodeAddress source [] Nothing pickUpLocLat pickUpLocLon
                      , destinationAddress: encodeAddress dest [] Nothing dropLocLat dropLocLon
                      , sourceLabelIcon: Nothing
                      , destLabelIcon: Nothing
                      , sourceGeoJson: Nothing
                      , sourceGates: Nothing
                      }
                  )
            modifyScreenState
              $ HomeScreenStateType
                  ( \homeScreen ->
                      homeScreen
                        { props
                          { sourceLat = pickUpLocLat
                          , sourceLong = pickUpLocLon
                          , destinationLat = if (isJust updatedState.data.dropLoc) then dropLocLat else 0.0
                          , destinationLong = if (isJust updatedState.data.dropLoc) then dropLocLon else 0.0
                          , rideRequestFlow = true
                          , bookingId = resp.bookingId
                          , city = updatedState.data.pickUpLoc.city
                          }
                        , data
                          { source = source
                          , destination = dest
                          , sourceAddress = encodeAddress source [] Nothing pickUpLocLat pickUpLocLon
                          , destinationAddress = encodeAddress dest [] Nothing dropLocLat dropLocLon
                          }
                        }
                  )
            setValueToLocalStore CONFIRM_QUOTES_POLLING "false"
            enterRentalRideSearchFlow resp.bookingId
          else do
            updateScheduledRides true true
            modifyScreenState $ RideScheduledScreenStateType (\rideScheduledScreen -> rideScheduledScreen{ data { bookingId = resp.bookingId } })
            modifyScreenState $ RentalScreenStateType (\rentalScreen -> rentalScreen {data{latestScheduledRides = Nothing}})
            rideScheduledFlow
        Left err -> do
          let _ = spy "inside (decodeError err.response.errorMessage errorCode)" (decodeError err.response.errorMessage "errorCode")
          if ((decodeError err.response.errorMessage "errorCode") == "INVALID_REQUEST" && DS.contains (Pattern "Quote expired") (decodeError err.response.errorMessage "errorMessage")) then do
            void $ lift $ lift $ showToast (getString STR.QUOTES_EXPIRY_ERROR_AND_FETCH_AGAIN)
            findRentalEstimates updatedState
            rentalScreenFlow
          else do
            void $ lift $ lift $ showToast "A Ride is already scheduled. Please Choose another time."
            homeScreenFlow
    RentalScreenController.GoToSelectPackage updatedState -> do
      modifyScreenState $ RentalScreenStateType (\_ -> updatedState)
      rentalScreenFlow
    _ -> pure unit


findRentalEstimates :: RentalScreenState -> FlowBT String Unit
findRentalEstimates state = do
      let
        dropLoc = fromMaybe SearchLocationScreenData.dummyLocationInfo state.data.dropLoc
        pickupLoc = state.data.pickUpLoc
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.locServiceabilityBT (Remote.makeServiceabilityReq (fromMaybe 0.0 pickupLoc.lat) (fromMaybe 0.0 pickupLoc.lon)) ORIGIN
      if false && state.data.pickUpLoc.address == (getString STR.CURRENT_LOCATION) then do
        let
          placeLat = fromMaybe 0.0 state.data.pickUpLoc.lat

          placeLon = fromMaybe 0.0 state.data.pickUpLoc.lon

          currTextField = SearchLocPickup
        { pickUpPoints, locServiceable, city, geoJson, specialLocCategory } <- getServiceability placeLat placeLon currTextField
        let
          focussedField = show currTextField
        if locServiceable then do
          liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { lat = placeLat, lon = placeLon, geoJson = geoJson, points = pickUpPoints }
          modifyScreenState
            $ SearchLocationScreenStateType
                ( \slsScreen ->
                    slsScreen
                      { props { locUnserviceable = false, searchLocStage = ConfirmLocationStage }
                      , data { srcLoc = Just state.data.pickUpLoc, latLonOnMap = state.data.pickUpLoc, confirmLocCategory = getZoneType specialLocCategory }
                      }
                )
          searchLocationFlow
        else do
          modifyScreenState $ SearchLocationScreenStateType (\state -> state { props { searchLocStage = PredictionsStage, locUnserviceable = true } })
          void $ lift $ lift $ toggleLoader false
          searchLocationFlow
      else do
        void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS) -- TODO : Handlde Loader in IOS Side
        void $ lift $ lift $ toggleLoader true
        srcFullAddress <- getPlaceName (fromMaybe 0.0 state.data.pickUpLoc.lat) (fromMaybe 0.0 state.data.pickUpLoc.lon) HomeScreenData.dummyLocation true
        destFullAddress <- getPlaceName (fromMaybe 0.0 dropLoc.lat) (fromMaybe 0.0 dropLoc.lon) HomeScreenData.dummyLocation true
        let
          currentTime = runFn2 EHC.getUTCAfterNSecondsImpl (EHC.getCurrentUTC "") 60 -- TODO-codex :: Delay, need to check if this is the correct way

          isTimeAheadOfCurrent = unsafePerformEffect $ runEffectFn2 compareDate (state.data.startTimeUTC) currentTime

          newState = if state.data.startTimeUTC == "" || not isTimeAheadOfCurrent then state { data { startTimeUTC = currentTime } } else state

          address = maybe "" (\(PlaceName address) -> address.formattedAddress) srcFullAddress

          destAddress = maybe "" (\(PlaceName address) -> address.formattedAddress) destFullAddress

          rideDate = convertUTCtoISC newState.data.startTimeUTC "D" <> " " <> convertUTCtoISC newState.data.startTimeUTC "MMMM" <> " " <> convertUTCtoISC newState.data.startTimeUTC "YYYY"

          rideTime = convertUTCtoISC newState.data.startTimeUTC "HH" <> ":" <> convertUTCtoISC newState.data.startTimeUTC "mm"

          srcMarkerConfig = defaultMarkerConfig { pointerIcon = "ny_ic_auto_map" }

          destMarkerConfig = defaultMarkerConfig { pointerIcon = "src_marker" }

          estimatedReturnUTC = EHC.getUTCAfterNSeconds newState.data.startTimeUTC (state.data.rentalBookingData.baseDuration * 60 * 60)

        checkForScheduled newState.data.startTimeUTC estimatedReturnUTC 1800
        (SearchRes rideSearchRes) <- Remote.rideSearchBT (Remote.mkRentalSearchReq (fromMaybe 0.0 newState.data.pickUpLoc.lat) (fromMaybe 0.0 newState.data.pickUpLoc.lon) (fromMaybe 0.0 dropLoc.lat) (fromMaybe 0.0 dropLoc.lon) (encodeAddress address [] Nothing (fromMaybe 0.0 state.data.pickUpLoc.lat) (fromMaybe 0.0 state.data.pickUpLoc.lon)) (encodeAddress destAddress [] Nothing (fromMaybe 0.0 dropLoc.lat) (fromMaybe 0.0 dropLoc.lon)) newState.data.startTimeUTC (newState.data.rentalBookingData.baseDistance * 1000) (newState.data.rentalBookingData.baseDuration * 60 * 60))
        modifyScreenState $ RentalScreenStateType (\rentalScreen -> state { data { searchId = rideSearchRes.searchId } })
        modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData { data { rideType = slsState.data.rideType, srcLoc = Just newState.data.pickUpLoc { address = address }, destLoc = state.data.dropLoc, route = rideSearchRes.routeInfo, rideDetails { searchId = rideSearchRes.searchId, rideDistance = state.data.rentalBookingData.baseDistance, rideDuration = state.data.rentalBookingData.baseDuration, rideScheduledDate = rideDate, rideScheduledTime = rideTime, rideScheduledTimeUTC = newState.data.startTimeUTC } }, props { searchLocStage = ChooseYourRide } })
        void $ lift $ lift $ toggleLoader false
        (App.BackT $ App.BackPoint <$> pure unit)
          >>= ( \_ -> do
                searchLocationFlow
            )


enterRideSearchFLow :: FlowBT String Unit
enterRideSearchFLow = do

  (GlobalState globalState) <- getState
  let
    slsState = globalState.searchLocationScreen

    loc = slsState.data.currentLoc

    { currLat, currLon, currAddressComponents, currPlace, currCity } = { currLat: fromMaybe 0.0 loc.lat, currLon: fromMaybe 0.0 loc.lon, currAddressComponents: loc.addressComponents, currPlace: loc.address, currCity: loc.city }

    { srcLat, srcLon, srcAddressComponents, srcAddress, srcCity, destLat, destLon, destAddressComponents, destAddress, destCity } = getSrcAndDestLoc slsState
  setUserCity CUSTOMER_LOCATION $ show srcCity
  modifyScreenState
    $ HomeScreenStateType
        ( \homeScreen ->
            HomeScreenData.initData
              { data
                { source = srcAddress
                , destination = destAddress
                , sourceAddress = srcAddressComponents
                , destinationAddress = destAddressComponents
                , polygonCoordinates = slsState.data.specialZoneCoordinates
                , nearByPickUpPoints = slsState.data.nearByGates
                }
              , props
                { sourceLat = srcLat
                , sourceLong = srcLon
                , city = srcCity
                , destCity = if destCity == AnyCity then Nothing else Just destCity
                , destinationLat = destLat
                , isSpecialZone = not $ null slsState.data.nearByGates
                , destinationLong = destLon
                , currentStage = RideSearch
                , rideRequestFlow = true
                , currentLocation =
                  { lat: currLat
                  , lng: currLon
                  , place: currPlace
                  , city: Nothing
                  , address: Just currPlace
                  , isSpecialPickUp: Nothing
                  }
                }
              }
        )
  updateLocalStage RideSearch
  (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> homeScreenFlow)

fetchSrcAndDestLoc :: HomeScreenState -> FlowBT String SearchLocationController.SrcAndDestLocations
fetchSrcAndDestLoc state = do
  srcFullAddress <- getPlaceName state.props.sourceLat state.props.sourceLong HomeScreenData.dummyLocation true
  destFullAddress <- getPlaceName state.props.destinationLat state.props.destinationLong HomeScreenData.dummyLocation true
  let
    address = maybe "" (\(PlaceName address) -> address.formattedAddress) srcFullAddress

    destAddress = maybe "" (\(PlaceName address) -> address.formattedAddress) destFullAddress
  let
    currentLoc = { lat: Just state.props.currentLocation.lat, lon: Just state.props.currentLocation.lng, city: state.props.city, addressComponents: encodeAddress "" [] Nothing state.props.currentLocation.lat state.props.currentLocation.lng, placeId: Nothing, address: "", metroInfo: Nothing, busStopInfo : Nothing, stationCode: "" }

    sourceLoc = { lat: Just state.props.sourceLat, lon: Just state.props.sourceLong, city: state.props.city, addressComponents: encodeAddress address [] Nothing state.props.sourceLat state.props.sourceLong, placeId: Nothing, address: address, metroInfo: Nothing , busStopInfo : Nothing , stationCode: "" }

    destLoc = if state.props.destinationLat /= 0.0 then Just { lat: Just state.props.destinationLat, lon: Just state.props.destinationLong, city: state.props.city, addressComponents: encodeAddress destAddress [] Nothing state.props.destinationLat state.props.destinationLong, placeId: Nothing, address: destAddress, metroInfo: Nothing, busStopInfo : Nothing, stationCode: "" } else Nothing
  pure $ { currentLoc, sourceLoc, destLoc, address, destAddress }

updateRideScheduledTime :: Maybe RideBookingListRes -> String -> FlowBT String Unit
updateRideScheduledTime rideBookingListResponse _ = do
  let _ = spy "updateRideScheduledTime" rideBookingListResponse
  case rideBookingListResponse of
    Just (RideBookingListRes listResp) -> do
      let
        filteredList = filter (\item ->
                    let
                      (RideBookingRes bookingRes) = item
                      rideScheduledTime = fromMaybe (EHC.getCurrentUTC "") bookingRes.rideScheduledTime
                    in
                    (any (_ == item ^. _status) ["CONFIRMED" , "TRIP_ASSIGNED"]) && (item ^._isScheduled || (rideScheduledTime > (EHC.getCurrentUTC ""))))  listResp.list
        multipleScheduled = length filteredList > 1
      case (head filteredList) of
        Just (RideBookingRes resp) -> do
          let
            fareProductType = getFareProductType $ resp.bookingDetails ^. _fareProductType
          if (any (_ == fareProductType) [ FPT.RENTAL, FPT.INTER_CITY ]) then do
            let
              rideScheduledTime = fromMaybe "" resp.rideScheduledTime
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { rentalsInfo = if rideScheduledTime == "" then Nothing else Just { rideScheduledAtUTC: rideScheduledTime, bookingId: resp.id, multipleScheduled: multipleScheduled, fareProductType: fareProductType, nearestRideScheduledAtUTC: maybe "" (_.rideStartTime) $ head $ Arr.sortWith (_.rideStartTime) $ decodeBookingTimeList FunctionCall, vehicleVariant: fromMaybe "" resp.vehicleServiceTierType ,driverInformation : HU.fetchDriverInformation resp.rideList } } })
            pure unit
          else
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { rentalsInfo = Nothing } })
          pure unit
        Nothing -> do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { rentalsInfo = Nothing } })
          pure unit
    Nothing -> do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { rentalsInfo = Nothing } })
          pure unit
  pure unit

enterRentalRideSearchFlow :: String -> FlowBT String Unit
enterRentalRideSearchFlow bookingId = do
  (GlobalState globalState) <- getState
  setValueToLocalStore CONFIRM_QUOTES_POLLING "false"
  updateLocalStage ConfirmingQuotes
  setValueToLocalNativeStore CONFIRM_QUOTES_START_TIME $ getCurrentUTC ""
  void $ liftFlowBT $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreen")
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { currentStage = ConfirmingQuotes, rideRequestFlow = true, bookingId = bookingId, isPopUp = NoPopUp } })
  homeScreenFlow

updateInvalidBookingPopUpConfig :: Maybe BookingTime -> FlowBT String Unit
updateInvalidBookingPopUpConfig maybeInvalidBookingDetail = case maybeInvalidBookingDetail of
  Just invalidBookingDetails -> do
    updateLocalStage HomeScreen
    modifyScreenState $ SearchLocationScreenStateType (\_ -> SearchLocationScreenData.initData)
    modifyScreenState $ RentalScreenStateType (\_ -> RentalScreenData.initData)
    let
      invalidBookingId = invalidBookingDetails.bookingId
    (RideBookingRes resp) <- Remote.rideBookingBT invalidBookingId
    modifyScreenState $ HomeScreenStateType (\_ -> HomeScreenData.initData { data { invalidBookingId = Just invalidBookingId, invalidBookingPopUpConfig = Just $ getInvalidBookingPopUpConfig invalidBookingDetails $ RideBookingRes resp }, props { showScheduledRideExistsPopUp = true } })
  Nothing -> pure unit
  where
  textDetailsForInvalidBookingPopUp :: ST.FareProductType -> BookingLocationAPIEntity -> String
  textDetailsForInvalidBookingPopUp fareProductType (BookingLocationAPIEntity address) = do
    let
      door = fromMaybe "" address.door

      street = fromMaybe "" address.street

      area = fromMaybe "" address.area
    if (fareProductType == FPT.INTER_CITY || (DS.null street && DS.null door)) then
      area
    else if (not $ DS.null door && (not $ DS.null door)) then
      door <> ", " <> street <> ", " <> area
    else if (not $ DS.null door) then
      door <> ", " <> area
    else
      street <> ", " <> area

  getInvalidBookingPopUpConfig :: BookingTime -> RideBookingRes -> InvalidBookingPopUpConfig
  getInvalidBookingPopUpConfig invalidBookingDetails (RideBookingRes resp) =
    let
      fareProductType = getFareProductType $ resp.bookingDetails ^. _fareProductType
    in
      { fareProductType: fareProductType
      , fromLocation: textDetailsForInvalidBookingPopUp fareProductType resp.fromLocation
      , toLocation: textDetailsForInvalidBookingPopUp fareProductType $ fromMaybe dummyBookingDetails (resp.bookingDetails ^. _contents ^. _toLocation)
      , bookingId: invalidBookingDetails.bookingId
      , rideScheduledTime: fromMaybe "" resp.rideScheduledTime
      , maxEstimatedDuration: invalidBookingDetails.estimatedDuration
      }
fcmHandler :: String -> HomeScreenState -> NotificationBody-> FlowBT String Unit
fcmHandler notification state notificationBody= do
  logField_ <- lift $ lift $ getLogFields
  let
    rideID = state.data.driverInfoCardState.rideId

    srcLat = state.data.driverInfoCardState.sourceLat

    srcLon = state.data.driverInfoCardState.sourceLng

    dstLat = state.data.driverInfoCardState.destinationLat

    dstLon = state.data.driverInfoCardState.destinationLng
  setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
  setValueToLocalStore FINDING_QUOTES_POLLING "false"
  setValueToLocalStore CONFIRM_QUOTES_POLLING "false"
  setValueToLocalStore TRACKING_DRIVER "False"
  if not state.props.isInApp then do
    setValueToLocalStore TRACKING_ENABLED "False"
    pure unit
  else do
    setValueToLocalStore TRACKING_ENABLED "True"
    pure unit
  case notification of
    "TRIP_STARTED" -> do -- OTP ENTERED'
      logStatus "trip_started_notification" ("bookingId : " <> state.props.bookingId)
      checkRideStatus true false
      (GlobalState updatedState) <- getState
      let
        homeScreenState = updatedState.homeScreen
      void $ pure $ JB.exitLocateOnMap ""
      when (homeScreenState.props.currentStage == RideStarted)
        $ do
            let
              shareAppCount = getValueToLocalStore SHARE_APP_COUNT
            if shareAppCount == "__failed" then do
              setValueToLocalStore SHARE_APP_COUNT "1"
            else if shareAppCount /= "-1" then do
              setValueToLocalStore SHARE_APP_COUNT (show ((INT.round $ (fromMaybe 0.0 (fromString (shareAppCount)))) + 1))
            else
              pure unit
            void $ pure $ clearTimerWithId <$> state.props.waitingTimeTimerIds
            let
              newState = homeScreenState { data { route = Nothing }, props { chatcallbackInitiated = false, isCancelRide = false, waitingTimeTimerIds = [], showShareAppPopUp = (INT.round $ (fromMaybe 0.0 (fromString (getValueToLocalStore SHARE_APP_COUNT)))) `mod` 4 == 0, showChatNotification = false, cancelSearchCallDriver = false } }

              currTrip =
                { sourceLat: srcLat
                , sourceLong: srcLon
                , destLat: dstLat
                , destLong: dstLon
                , source: state.data.driverInfoCardState.source
                , destination: state.data.driverInfoCardState.destination
                , sourceAddress: state.data.driverInfoCardState.sourceAddress
                , destinationAddress: state.data.driverInfoCardState.destinationAddress
                , locationScore: Just 1.0
                , recencyDate: Nothing
                , frequencyCount: Just 1
                , isSpecialZone: state.props.isSpecialZone
                , vehicleVariant: Just state.data.driverInfoCardState.vehicleVariant
                , serviceTierNameV2: state.data.driverInfoCardState.serviceTierName
                }

              currentSourceGeohash = runFn3 encodeGeohash srcLat srcLon state.data.config.suggestedTripsAndLocationConfig.geohashPrecision

              currentMap = getSuggestionsMapFromLocal FunctionCall
            if (cacheSuggested state.data.fareProductType && currTrip.destination /= "") then do
              let
                updatedMap = addOrUpdateSuggestedTrips currentSourceGeohash currTrip false currentMap state.data.config.suggestedTripsAndLocationConfig false
              void $ pure $ setSuggestionsMap updatedMap
            else
              pure unit
            modifyScreenState $ HomeScreenStateType (\homeScreen -> newState { data { suggestionsData { suggestionsMap = getSuggestionsMapFromLocal FunctionCall } }, props { showAcWorkingPopup = true, showDeliveryImageAndOtpModal = false } })
            lift $ lift $ triggerRideStatusEvent notification Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
            homeScreenFlow
            where
              cacheSuggested :: FPT.FareProductType -> Boolean
              cacheSuggested fareProduct = notElem fareProduct [FPT.RENTAL, FPT.AMBULANCE, FPT.DELIVERY, FPT.INTER_CITY]
    "TRIP_FINISHED" -> do -- TRIP FINISHED
      logStatus "trip_finished_notification" ("bookingId : " <> state.props.bookingId)
      void $ pure $ JB.exitLocateOnMap ""
      firstRideCompletedEvent ""
      let
        sourceSpecialTagIcon = zoneLabelIcon state.props.zoneType.sourceTag

        destSpecialTagIcon = zoneLabelIcon state.props.zoneType.destinationTag
        userPhoneNo = getValueToLocalStore MOBILE_NUMBER
        isPersonDeliveryInitiator = HU.isDeliveryInitiator state.data.requestorPartyRoles

      void $ pure $ metaLogEvent "ny_user_ride_completed"
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_ride_completed_with_props" $ rideCompletedEventParams state
      void $ pure $ JB.destroySignedCall unit
      void $ updateLocalStage HomeScreen
      setValueToLocalStore IS_SOS_ACTIVE "false"
      deleteValueFromLocalStore SELECTED_VARIANT
      removeChatService ""
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { data { sosId = "" } })
      if (state.props.bookingId /= "" && isPersonDeliveryInitiator) then do
        (RideBookingRes resp) <- Remote.rideBookingBT (state.props.bookingId)
        let
          (RideBookingAPIDetails bookingDetails) = resp.bookingDetails

          (RideBookingDetails contents) = bookingDetails.contents

          (RideAPIEntity ride) = fromMaybe dummyRideAPIEntity (resp.rideList !! 0)

          finalAmount = getFinalAmount (RideBookingRes resp)

          differenceOfDistance = fromMaybe 0 contents.estimatedDistance - (fromMaybe 0 ride.chargeableRideDistance)

          waitingChargesApplied = isJust $ DA.find (\entity  -> entity ^._description == "WAITING_OR_PICKUP_CHARGES") ((RideBookingRes resp) ^._fareBreakup)

          isRecentRide = EHC.getExpiryTime (fromMaybe "" (state.data.ratingViewState.rideBookingRes ^. _rideEndTime)) true / 60 < state.data.config.safety.pastRideInterval

          nightSafetyFlow = showNightSafetyFlow resp.hasNightIssue resp.rideStartTime resp.rideEndTime

        lift $ lift $ triggerRideStatusEvent notification (Just finalAmount) (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
        setValueToLocalStore PICKUP_DISTANCE "0"
        liftFlowBT $ logEventWithMultipleParams logField_ "ny_rider_ride_completed" (rideCompletedDetails (RideBookingRes resp))
        let
          isBlindPerson = getValueToLocalStore DISABILITY_NAME == "BLIND_LOW_VISION"
          hasAccessibilityIssue' = resp.hasDisability == Just true
          hasSafetyIssue' = case state.props.safetySettings of
                              Just (API.GetEmergencySettingsRes settings) -> do
                                let safetyCheckStartTime = fromMaybe 0 settings.safetyCheckStartTime
                                    safetyCheckEndTime = fromMaybe 0 settings.safetyCheckEndTime
                                settings.enablePostRideSafetyCheck == ALWAYS_SHARE || showNightSafetyFlow resp.hasNightIssue resp.rideStartTime resp.rideEndTime safetyCheckStartTime safetyCheckEndTime settings.enablePostRideSafetyCheck
                              Nothing -> false
          finalFareHasToll = DA.any (\entity  -> entity ^._description == "TOLL_CHARGES") (resp.fareBreakup)
          estimateFareHasToll =  DA.any (\entity  -> entity ^._description == "TOLL_CHARGES") (resp.estimatedFareBreakup)
          hasTollIssue' = finalFareHasToll && not isBlindPerson
          demandExtraTollAmountIssue' = estimateFareHasToll && (not finalFareHasToll)
          parkingCharges = DA.find (\entity  -> entity ^._description == "PARKING_CHARGE") (resp.fareBreakup)

        updateScheduledRides true true
        modifyScreenState
          $ HomeScreenStateType
              ( \homeScreen ->
                  homeScreen
                    { data
                      { startedAtUTC = fromMaybe "" resp.rideStartTime
                      , rideRatingState
                        { driverName = ride.driverName
                        , rideId = ride.id
                        , distanceDifference = differenceOfDistance
                        , rideStartTime = convertUTCtoISC (fromMaybe "" resp.rideStartTime) "h:mm A"
                        , rideEndTime = convertUTCtoISC (fromMaybe "" resp.rideEndTime) "h:mm A"
                        }
                        , ratingViewState
                        { rideBookingRes = (RideBookingRes resp)
                        }
                      , driverInfoCardState
                        { initDistance = Nothing
                        , startedAt = convertUTCtoISC (fromMaybe "" resp.rideStartTime) "h:mm A"
                        , rentalData
                          { finalDuration = (fromMaybe 0 resp.duration) / 60
                          , finalDistance = (fromMaybe 0 ride.chargeableRideDistance) / 1000
                          }
                        }
                      , vehicleVariant = ride.vehicleVariant
                      , rideCompletedData {
                        issueReportData {
                          hasAccessibilityIssue = hasAccessibilityIssue'
                        , hasSafetyIssue = hasSafetyIssue'
                        , hasTollIssue = hasTollIssue'
                        , showIssueBanners = hasAccessibilityIssue' || hasSafetyIssue' || hasTollIssue'
                          }
                        }
                       , toll {
                          confidence = ride.tollConfidence
                        , showAmbiguousPopUp = ride.tollConfidence == Just CTA.Unsure
                        }
                      }
                    , props
                      { currentStage = RideCompleted
                      , showDeliveryImageAndOtpModal = false
                      , estimatedDistance = contents.estimatedDistance
                      }
                    }
              )

        modifyScreenState
          $ RiderRideCompletedScreenStateType
              ( \riderRideCompletedScreen ->
                  riderRideCompletedScreen
                    {
                      topCard {
                        title = getString LT.RIDE_COMPLETED,
                        finalAmount = finalAmount,
                        initialAmount = state.data.driverInfoCardState.price,
                        fareUpdatedVisiblity = finalAmount /= state.data.driverInfoCardState.price && contents.estimatedDistance /= Nothing,
                        infoPill {
                          text = getFareUpdatedStr differenceOfDistance waitingChargesApplied,
                          imageVis = VISIBLE,
                          visible = if finalAmount == state.data.driverInfoCardState.price || contents.estimatedDistance == Nothing then GONE else VISIBLE
                        }
                      }
                    , driverInfoCardState
                        {
                          driverName =  state.data.driverInfoCardState.driverName,
                          fareProductType = state.data.fareProductType,
                          isAlreadyFav = state.data.driverInfoCardState.isAlreadyFav,
                          favCount = state.data.driverInfoCardState.favCount
                        }
                    , rideId = ride.id
                    , rideDuration = resp.duration
                    , rentalRowDetails
                      { rideTime = getString LT.RIDE_TIME
                      , rideDistance = getString LT.RIDE_DISTANCE
                      , rideDistanceInfo = "( " <> getString LT.CHARGEABLE <> " / " <> getString LT.BOOKED <> " )"
                      , rideStartedAt = getString LT.RIDE_STARTED_AT
                      , rideEndedAt = getString LT.RIDE_ENDED_AT
                      , estimatedFare = getString LT.ESTIMATED_FARE
                      , extraTimeFare = getString LT.EXTRA_TIME_FARE
                      , extraDistanceFare = getString LT.EXTRA_DISTANCE_FARE
                      , totalFare = getString LT.TOTAL_FARE
                      , rideDetailsTitle = getString LT.RIDE_DETAILS
                      , fareUpdateTitle = getString LT.FARE_UPDATE
                      , surcharges = getString LT.SURCHARGES
                      }
                    , rentalBookingData
                      { baseDuration = state.data.driverInfoCardState.rentalData.baseDuration
                      , baseDistance = state.data.driverInfoCardState.rentalData.baseDistance
                      , finalDuration = (fromMaybe 0 resp.duration) / 60
                      , finalDistance = (fromMaybe 0 ride.chargeableRideDistance) / 1000
                      , rideStartedAt = convertUTCtoISC (fromMaybe "" resp.rideStartTime) "h:mm A"
                      , rideEndedAt = convertUTCtoISC (fromMaybe "" resp.rideEndTime) "h:mm A"
                      , extraTimeFare = state.data.driverInfoCardState.rentalData.extraTimeFare
                      , extraDistanceFare = state.data.driverInfoCardState.rentalData.extraDistanceFare
                      }
                    , showRentalRideDetails = state.data.fareProductType == FPT.RENTAL
                    , ratingCard
                      {
                        feedbackPillData = customerFeedbackPillData (RideBookingRes resp) ride.vehicleVariant
                      }
                    , rideRatingState
                        { driverName = ride.driverName
                        , rideId = ride.id
                        , distanceDifference = differenceOfDistance
                        , rideStartTime = convertUTCtoISC (fromMaybe "" resp.rideStartTime) "h:mm A"
                        , rideEndTime = convertUTCtoISC (fromMaybe "" resp.rideEndTime) "h:mm A"
                        }
                    , ratingViewState
                        { rideBookingRes = (RideBookingRes resp)
                        }
                    , isSafetyCenterDisabled = state.props.isSafetyCenterDisabled
                    , bookingId = state.props.bookingId
                    , additionalCharges = [
                        {
                          text :  getString if ride.tollConfidence == (Just Unsure) then  STR.TOLL_ROAD_CHANGED else if finalFareHasToll then  STR.TOLL_CHARGES_INCLUDED else STR.TOLL_ROAD_CHANGED
                        , visibility : boolToVisibility $ finalFareHasToll || estimateFareHasToll
                        , image :  fetchImage FF_COMMON_ASSET "ny_ic_grey_toll"
                        , textColor : Color.black700
                        },
                        {
                          text : maybe "" (\parking ->  getString $ STR.PARKING_CHARGES_INCLUDED $ (getCurrency appConfig) <>  (show $ ceil $ parking ^. _amount)) parkingCharges
                        , visibility : boolToVisibility $ isJust parkingCharges
                        , image : fetchImage FF_COMMON_ASSET "ny_ic_parking_logo_grey"
                        , textColor : Color.black700
                        }
                      ]
                    , customerIssue = riderRideCompletedScreen.customerIssue
                        { showIssueBanners = hasAccessibilityIssue' || hasSafetyIssue' || demandExtraTollAmountIssue'
                        , hasAccessibilityIssue = hasAccessibilityIssue'
                        , hasSafetyIssue = hasSafetyIssue'
                        , demandExtraTollAmountIssue = demandExtraTollAmountIssue'
                        }
                    , showSafetyCenter = state.data.config.feature.enableSafetyFlow && isRecentRide && not state.props.isSafetyCenterDisabled
                  }
              )
        riderRideCompletedScreenFlow
      else if (not isPersonDeliveryInitiator) then do
        when (HU.isParentView FunctionCall) $ pure $ HU.emitTerminateApp Nothing true
        modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
        homeScreenFlow
      else if (not isPersonDeliveryInitiator) then do
        when (HU.isParentView FunctionCall) $ pure $ HU.emitTerminateApp Nothing true
        modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
        homeScreenFlow
      else
        riderRideCompletedScreenFlow

    "CANCELLED_PRODUCT" -> do -- REMOVE POLYLINES
      logStatus "ride_cancelled_notification" ("bookingId : " <> state.props.bookingId)
      updateScheduledRides true true
      let
          bookingScheduledTime = fromMaybe (getCurrentUTC "") notificationBody.rideTime
          scheduledBufferTime = 1800
          currentUtcAfterScheduledTime = EHC.getUTCAfterNSeconds (getCurrentUTC "") scheduledBufferTime
          timeDiff =  EHC.compareUTCDate bookingScheduledTime (currentUtcAfterScheduledTime)
          fcmBookingId = fromMaybe "null" notificationBody.bookingId
          _ = spy "Printing for checking" notificationBody
      when (HU.isParentView FunctionCall) $ pure $ HU.emitTerminateApp Nothing true
      if (fcmBookingId /= state.props.bookingId && state.props.bookingId /= "") then do
        currentFlowStatus false
      else if (fcmBookingId /= state.props.bookingId && state.props.bookingId == "" && timeDiff > 0) then do
        homeScreenFlow
      else do
        void $ pure $ JB.exitLocateOnMap ""
        void $ pure $ removeAllPolylines ""
        void $ updateLocalStage HomeScreen
        void $ pure $ JB.destroySignedCall unit
        modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { data { sosId = "" } })
        setValueToLocalStore IS_SOS_ACTIVE "false"
        removeChatService ""
        setValueToLocalStore PICKUP_DISTANCE "0"
        lift $ lift $ triggerRideStatusEvent notification Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
        updateUserInfoToState state
        void $ pure $ clearTimerWithId <$> state.props.waitingTimeTimerIds
        permissionConditionA <- lift $ lift $ liftFlow $ isLocationPermissionEnabled unit
        permissionConditionB <- lift $ lift $ liftFlow $ isLocationEnabled unit
        if not (permissionConditionA && permissionConditionB) then do
          modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen { stage = LOCATION_DISABLED })
          permissionScreenFlow
        else do
          currentFlowStatus false
          homeScreenFlow
    "DRIVER_ASSIGNMENT" -> do
      logStatus "ride_assigned_notification" ("bookingId : " <> state.props.bookingId)
      (updateScheduledRides true true)
      let bookingScheduledTime = fromMaybe (getCurrentUTC "") notificationBody.rideTime
          scheduledBufferTime = 1800
          currentUtcAfterScheduledTime =  EHC.getUTCAfterNSeconds (getCurrentUTC "") scheduledBufferTime
          timeDiff = EHC.compareUTCDate bookingScheduledTime (currentUtcAfterScheduledTime)
          fcmBookingId = fromMaybe "null" notificationBody.bookingId
      if (state.props.bookingId /= "" && fcmBookingId /= state.props.bookingId && fcmBookingId /= "null" && timeDiff >= -1800 && timeDiff <= 0) then do
        let rideScheduledAt = fromMaybe "" notificationBody.rideTime
            rideSchTimeInIST = convertUTCtoISC rideScheduledAt "D" <> " " <> convertUTCtoISC rideScheduledAt "MMMM" <> " " <> convertUTCtoISC rideScheduledAt "YYYY" <> " , " <> convertUTCtoISC rideScheduledAt "HH" <> ":" <> convertUTCtoISC rideScheduledAt "mm"
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{upcomingRideDetails = Just { bookingId : fcmBookingId, rideScheduledAt : rideSchTimeInIST}}})
        homeScreenFlow
      else do
        if (not (isLocalStageOn RideAccepted || isLocalStageOn RideStarted)) then do
          setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_DRIVER_ARRIVAL"
          setValueToLocalStore DRIVER_REACHED_DESTINATION_ACTION "TRIGGER_DRIVER_REACHED_DESTINATION"
          lift $ lift $ triggerRideStatusEvent notification Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
          void $ liftFlowBT $ logEvent logField_ "ny_fs_driver_assignment"
          checkRideStatus true false
          homeScreenFlow
        else
          homeScreenFlow
    "REALLOCATE_PRODUCT" -> do
      logStatus "ride_reallocated_notification" ("estimateId : " <> state.props.estimateId)
      (updateScheduledRides true true)
      let bookingScheduledTime = fromMaybe (getCurrentUTC "") notificationBody.rideTime
          scheduledBufferTime = 1800 -- need to configure this
          currentUtcAfterScheduledTime = EHC.getUTCAfterNSeconds (getCurrentUTC "") scheduledBufferTime
          timeDiff = EHC.compareUTCDate bookingScheduledTime (currentUtcAfterScheduledTime)
          fcmBookingId = fromMaybe "null" notificationBody.bookingId

      if (fcmBookingId /= state.props.bookingId && state.props.bookingId /= "" && timeDiff > -1800 && timeDiff <= 0) then do
       pure unit
      else if (fcmBookingId /= state.props.bookingId && state.props.bookingId == "" && timeDiff > 0) then do
        updateLocalStage HomeScreen
        pure unit
      else do
        void $ pure $ spy "after scheduled checks" notificationBody
        void $ pure $ JB.exitLocateOnMap ""
        void $ pure $ removeAllPolylines ""
        void $ pure $ JB.destroySignedCall unit
        removeChatService ""
        setValueToLocalStore PICKUP_DISTANCE "0"
        (GlobalState updatedState) <- getState
        let enableBoostSearch = fetchRemoteConfigString "enable_boost_search" == "true"
            enableTipView = any (_ /= updatedState.homeScreen.data.fareProductType) [FPT.ONE_WAY, FPT.DRIVER_OFFER] && not enableBoostSearch
        let
          homeScreenState = updatedState.homeScreen { data { quoteListModelState = [] }, props { isBanner = state.props.isBanner, currentStage = ReAllocated, estimateId = updatedState.homeScreen.props.estimateId, reAllocation { showPopUp = true }, tipViewProps { isVisible = updatedState.homeScreen.props.tipViewProps.activeIndex >= 0 && enableTipView }, selectedQuote = Nothing, isCancelRide = false, cancelSearchCallDriver = false, showRateCard = false } }
        let
          updatedState = case (getTipViewData "LazyCheck") of
            Just (TipViewData tipView) -> homeScreenState { props { tipViewProps { stage = tipView.stage, activeIndex = tipView.activeIndex, isVisible = tipView.activeIndex >= 0 && enableTipView } } }
            Nothing -> homeScreenState { props { tipViewProps = HomeScreenData.initData.props.tipViewProps } }
        modifyScreenState $ HomeScreenStateType (\homeScreen -> updatedState {data {driverInfoCardState {driverArrived = false}}})
        void $ pure $ clearTimerWithId <$> state.props.waitingTimeTimerIds
        void $ pure $ setValueToLocalNativeStore FINDING_QUOTES_START_TIME (getCurrentUTC "LazyCheck")
        updateLocalStage ReAllocated
      homeScreenFlow
    _
      | any (_ == notification) [ "FOLLOW_RIDE", "SHARE_RIDE", "SOS_RESOLVED" ] -> do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { followers = Nothing } })
        currentFlowStatus false
    _
      | any (_ == notification) [ "SOS_MOCK_DRILL", "SOS_MOCK_DRILL_NOTIFY" ] -> do
        updateFollower true false $ Just notification
    "SOS_TRIGGERED" -> do
      logStatus "sos_triggered_notification" ""
      updateFollower true false Nothing
    "SAFETY_ALERT_DEVIATION" -> do
      logStatus "safety_alert_deviation_notification" ""
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { safetyAlertType = if state.data.fareProductType == FPT.DELIVERY then Nothing else Just ST.DEVIATION } })
      homeScreenFlow
    "STOP_REACHED" -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { driverInfoCardState {destination = "" , destinationLat = 0.0, destinationLng =0.0, destinationAddress = getAddressFromBooking dummyBookingDetails} } })
      homeScreenFlow
    "FILE_UPLOADED" -> do
      when (state.data.fareProductType == FPT.DELIVERY) $ do
        res <- lift $ lift $ Remote.getDeliveryImage state.data.driverInfoCardState.rideId
        case res of
          Right (API.GetDeliveryImageResponse resp) -> do
            let isNotValidImage = resp == "" || DS.contains (DS.Pattern "error") resp || DS.length resp < 100
            if isNotValidImage then do
              void $ void $ lift $ lift $ showToast $  "Image Not Uploaded, please try again"
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { deliveryImage = Nothing }})
            else do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { deliveryImage = Just resp }, props { showDeliveryImageAndOtpModal = true, loadingDeliveryImage = false} })
          Left _ -> do
            void $ void $ lift $ lift $ showToast $  "Image Not Uploaded, please try again"
      homeScreenFlow
    "DRIVER_HAS_REACHED_DESTINATION" -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { driverInfoCardState { destinationReached = true} } })
      homeScreenFlow
    _ -> homeScreenFlow


pickupInstructionsScreenFlow :: FlowBT String Unit
pickupInstructionsScreenFlow = do
  action <- UI.pickupInstructionsScreen
  case action of
    _ -> pickupInstructionsScreenFlow

parcelDeliveryFlow :: FlowBT String Unit
parcelDeliveryFlow = do
  (GlobalState currentState) <- getState
  action <- lift $ lift $ runScreen $ UI.parcelDeliveryScreen currentState.parcelDeliveryScreen
  case action of
    ParcelDeliveryScreenController.GoToHomeScreen state -> do
      modifyScreenState $ HomeScreenStateType (\_ -> HomeScreenData.initData)
      homeScreenFlow
    ParcelDeliveryScreenController.RefreshScreen state -> do
      modifyScreenState $ ParcelDeliveryScreenStateType (\_ -> state)
      parcelDeliveryFlow
    ParcelDeliveryScreenController.GoToSelectLocation state -> do
      setValueToLocalStore PARCEL_INSTRUCTIONS_VISITED "true"
      void $ pure $ updateLocalStage SearchLocationModel
      modifyScreenState $ ParcelDeliveryScreenStateType (\_ -> state { data { currentStage = ST.SENDER_DETAILS }})
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {
        props { homeScreenPrimaryButtonLottie = true, isSource = Just true, currentStage = SearchLocationModel, isSearchLocation = SearchLocation, searchLocationModelProps{crossBtnSrcVisibility = (STR.length (getString STR.CURRENT_LOCATION)) > 2},  rideSearchProps{ sessionId = generateSessionId unit } }
      , data { fareProductType = FPT.DELIVERY, source="", locationList = homeScreen.data.recentSearchs.predictionArray}
      })
      homeScreenFlow
    ParcelDeliveryScreenController.GoToChooseYourRide state -> do
      (GlobalState globalState) <- getState
      modifyScreenState $ ParcelDeliveryScreenStateType (\_ -> state)
      (GlobalState globalState) <- getState
      void $ drawMapRoute' state
      updateLocalStage SettingPrice
      homeScreenFlow
    ParcelDeliveryScreenController.GoToConfirmgDelivery state -> do
      let deliveryDetailsInfo = API.DeliveryDetails { senderDetails : mkPersonLocation state.data.senderDetails, receiverDetails : mkPersonLocation state.data.receiverDetails, initiatedAs : state.data.initiatedAs }
      updateLocalStage GoToConfirmgDelivery
      modifyScreenState $ ParcelDeliveryScreenStateType (\_ -> state)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { currentStage = GoToConfirmgDelivery }, data { deliveryDetailsInfo = Just deliveryDetailsInfo } })
      void $ drawMapRoute' state
      homeScreenFlow
      where
        mkPersonLocation :: ST.PersonDeliveryDetails -> API.PersonLocationAndInstruction
        mkPersonLocation details = API.PersonLocationAndInstruction { name : details.name, phoneNumber : details.phone, address : mkInstruction details}
        mkInstruction :: ST.PersonDeliveryDetails -> API.InstructionAndAddress
        mkInstruction details = API.InstructionAndAddress { instructions : details.instructions, extras : details.extras}
    ParcelDeliveryScreenController.GoToSelectContact state -> do
      selectContactsFlow (\contacts -> do
        case contacts !! 0 of
          Just contact -> do
            let updatedState =
                  case state.data.currentStage of
                    ST.SENDER_DETAILS ->
                      state { props { editDetails { name = contact.name, phone =  contact.number } }
                            , data { senderDetails { name = contact.name, phone =  contact.number } } }
                    _ ->
                      state { props { editDetails { name = contact.name, phone = contact.number } }
                            , data { receiverDetails { name = contact.name, phone = contact.number } } }
            modifyScreenState $ ParcelDeliveryScreenStateType (\_ -> updatedState)
          Nothing -> pure unit
        parcelDeliveryFlow
        ) [] 1
      parcelDeliveryFlow
    _ -> pure unit
  where
    drawMapRoute' :: ST.ParcelDeliveryScreenState -> FlowBT String (Maybe Route)
    drawMapRoute' state =
      let markers = normalRoute ""
          srcMarkerConfig = defaultMarkerConfig{ markerId = markers.srcMarker, pointerIcon = markers.srcMarker}
          destMarkerConfig = defaultMarkerConfig{ markerId = markers.destMarker, pointerIcon = markers.destMarker}
      in Remote.drawMapRoute state.data.sourceLat state.data.sourceLong state.data.destinationLat state.data.destinationLong srcMarkerConfig destMarkerConfig "NORMAL" state.data.route "pickup" (specialLocationConfig "" "" false getPolylineAnimationConfig)

selectContactsFlow ::  (Array NewContacts -> FlowBT String Unit) -> Array NewContacts -> Int -> FlowBT String Unit
selectContactsFlow callback selectedContacts selectionLimit = do
  modifyScreenState $ SelectContactsScreenStateType (\state -> state{ data{ contactSelectionLimit = selectionLimit
                                                                          , selectedContacts = selectedContacts
                                                                          , alreadySelectedContacts = selectedContacts } })
  action <- UI.selectContactsScreen
  case action of
    EXECUTE_CALLBACK state ->
      callback state.data.selectedContacts
    SELECT_CONTACTS_BACK_PRESSED -> pure unit

rideSummaryScreenFlow :: FlowBT String Unit
rideSummaryScreenFlow = do
  (GlobalState newState) <- getState
  config <- getAppConfigFlowBT appConfig
  modifyScreenState $ RideSummaryScreenStateType (\rideSummaryScreen -> rideSummaryScreen { data { config = config },props{hasApiFailed = false} })
  action <- UI.rideSummaryScreen
  case action of
    ACCEPT_SCHEDULED_RIDE quoteId startTimeUTC-> do
      response <- lift $ lift $ Remote.rideConfirm quoteId
      let
        state = newState.homeScreen
      case response of
        Right (ConfirmRes resp) -> do
          let
            bookingId = resp.bookingId
            diffInSeconds = EHC.compareUTCDate (if DS.null startTimeUTC then (getCurrentUTC "") else startTimeUTC) (getCurrentUTC "")
            isNow = diffInSeconds < 60 * 30
            _ = runFn2 EHC.updatePushInIdMap "EstimatePolling" true
          if isNow then do
            enterRentalRideSearchFlow bookingId
          else do
            logField_ <- lift $ lift $ getLogFields
            modifyScreenState $ RideSummaryScreenStateType (\rideSummaryScreen -> rideSummaryScreen
              {  data {bookingId = Just bookingId},
                props {
                  pickUpOpen = false,
                  termsAndConditionOpen = false,
                  excludedChargesOpen = false,
                  includedChargesOpen = false,
                  isBookingAccepted = true
                  }
                })
            let
              currentTime = convertUTCtoISC (getCurrentUTC "")  "hh:mm A"
            liftFlowBT $ logEventWithMultipleParams logField_ "user_intercity_scheduled_ride_confirmed" $ [{ key: "Booking Scheduled Time", value: unsafeToForeign currentTime }]
            rideSummaryScreenFlow
        Left err -> do
          if ((decodeError err.response.errorMessage "errorCode") == "INVALID_REQUEST" && (decodeError err.response.errorMessage "errorMessage") == "ACTIVE_BOOKING_PRESENT") then do
            void $ lift $ lift $ showToast "Active Booking Present"
            updateLocalStage HomeScreen
            updateUserInfoToState state
            homeScreenFlow
          else if ((decodeError err.response.errorMessage "errorCode") == "QUOTE_EXPIRED" || err.code == 400) then do
            void $ lift $ lift $ showToast "Quotes Expired , Trying Again!"
            let _ = runFn2 EHC.updatePushInIdMap "EstimatePolling" true
            rideSearchRequestFlow state
            homeScreenFlow
          else
            pure unit
    RIDE_CONFIRMED startTimeUTC fromScreen booking-> do
          let
            diffInSeconds = EHC.compareUTCDate (if DS.null startTimeUTC then (getCurrentUTC "") else startTimeUTC) (getCurrentUTC "")
            isNow = diffInSeconds < 60 * 30
            bookingId = fromMaybe "" booking
            _ = runFn2 EHC.updatePushInIdMap "EstimatePolling" true
            state = newState.homeScreen
          if isNow && isJust booking && not (any (_ == fromScreen) [Screen.getScreen Screen.HOME_SCREEN , Screen.getScreen Screen.MY_RIDES_SCREEN]) then do
            enterRentalRideSearchFlow bookingId
          else do
            when (not (fromScreen == (Screen.getScreen Screen.HOME_SCREEN))) ( do
              void $ updateScheduledRides true true
              pure unit)
            modifyScreenState $ RideSummaryScreenStateType (\rideSummaryScreen -> RideSummaryScreenData.initData)
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { settingSideBar { opened = SettingSideBarController.CLOSED } } })
            if(fromScreen == Screen.getScreen Screen.RIDE_SUMMARY_SCREEN) then do
              updateLocalStage HomeScreen
              updateUserInfoToState state
              homeScreenFlow
            else currentFlowStatus false
    CANCEL_SCHEDULED_RIDE bookingId fromScreen-> do
      resp <- lift $ lift $ Remote.cancelRide (Remote.makeCancelRequest  "Cancelling Scheduled Ride" "Cancel Scheduled Ride") (bookingId)
      case resp of
        Right resp -> do
            let
              _ = runFn2 EHC.updatePushInIdMap "EstimatePolling" true
              state = newState.homeScreen
            updateScheduledRides true true
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { settingSideBar { opened = SettingSideBarController.CLOSED } } })
            void $ pure $ JB.destroySignedCall unit
            if(fromScreen == Screen.getScreen Screen.RIDE_SUMMARY_SCREEN) then do
              updateLocalStage HomeScreen
              updateUserInfoToState state
              homeScreenFlow
            else currentFlowStatus false
        Left _ -> do
          void $ lift $ lift $ showToast "Failed To Cancel Ride"
          rideSummaryScreenFlow
    GO_TO_RIDE_REQUEST -> homeScreenFlow
    NOTIFICATION_LISTENER notification notificationBody -> do
      if notification == "DRIVER_ASSIGNMENT" then do
        let state = newState.homeScreen
        updateUserInfoToState state
        currentFlowStatus false
      else fcmHandler notification newState.homeScreen notificationBody
    REFRESH_RIDE_SUMMARY_SCREEN bookingId -> do
      modifyScreenState $ RideSummaryScreenStateType (\rideSummaryScreen -> RideSummaryScreenData.initData{data{fromScreen = (Screen.getScreen Screen.MY_RIDES_SCREEN),bookingId = bookingId}})
      rideSummaryScreenFlow
    CALL_DRIVER config callType exophoneNumber -> do
      (APISuccessResp res) <- Remote.onCallBT (Remote.makeOnCallReq (fromMaybe "" config.data.bookingId) (show callType) exophoneNumber)
      rideSummaryScreenFlow

busTicketBookingFlow :: FlowBT String Unit
busTicketBookingFlow = do
  (GlobalState currentState) <- getState
  action <- lift $ lift $ runScreen $ UI.busTicketBookingScreen currentState.busTicketBookingScreen
  case action of
    BusTicketBookingController.GoToHomeScreen state -> 
      homeScreenFlow
    BusTicketBookingController.RefreshScreen state -> do
      modifyScreenState $ BusTicketBookingScreenStateType (\_ -> state)
      parcelDeliveryFlow
    BusTicketBookingController.GoToMyTicketsScreen state -> do
      modifyScreenState $ MetroMyTicketsScreenStateType (\metroMyTicketsScreen -> metroMyTicketsScreen { props { ticketServiceType = BUS , entryPoint = ST.MetroTicketBookingToMetroMyTickets, fromScreen = Just $ Screen.getScreen Screen.BUS_TICKET_BOOKING_SCREEN} })
      metroMyTicketsFlow
    BusTicketBookingController.GoToSearchLocationScreenForRoutes state source ->do
     let 
       currentCity = getValueToLocalStore CUSTOMER_LOCATION
       searchLocationState = currentState.searchLocationScreen
     modifyScreenState $ MetroTicketBookingScreenStateType (\_ -> MetroTicketBookingScreenData.initData)
     (AutoCompleteResp routeStopresponse) <- Remote.busAutoCompleteBT "BUS" currentCity "0.0,0.0" Nothing "10" Nothing --(show currentState.homeScreen.props.sourceLat <> "," <> show currentState.homeScreen.props.sourceLong) (Nothing)
     let rideType = 
            if null routeStopresponse.stops && null routeStopresponse.routes then
              let (decodedCachedRoutes :: (Array FRFSRouteAPI)) = fromMaybe [] (decodeForeignAny (parseJSON (getValueToLocalStore RECENT_BUS_ROUTES)) Nothing)
              in  if null decodedCachedRoutes then STOP
                  else ROUTES
            else if null routeStopresponse.routes then STOP
            else ROUTES
         sortedStops = getSortedStops routeStopresponse.stops
     modifyScreenState $ SearchLocationScreenStateType (\slsState -> SearchLocationScreenData.initData{ props { actionType = BusSearchSelectionAction, canSelectFromFav = false, focussedTextField = Just SearchLocPickup , routeSearch = true , isAutoComplete = false , srcLat = state.props.srcLat , srcLong = state.props.srcLong }, data {fromScreen =(Screen.getScreen Screen.BUS_TICKET_BOOKING_SCREEN), rideType = rideType ,ticketServiceType = BUS , srcLoc = Nothing, destLoc = Nothing, routeSearchedList = routeStopresponse.routes , stopsSearchedList = sortedStops , updatedRouteSearchedList = routeStopresponse.routes , updatedStopsSearchedList = sortedStops } })
     searchLocationFlow
    BusTicketBookingController.GoToMetroTicketDetailsFlow bookingId -> do
      void $ lift $ lift $ toggleLoader true
      res <- lift $ lift $ Remote.getMetroStatus bookingId
      case res of
        Right (GetMetroBookingStatusResp resp) -> do
          void $ lift $ lift $ toggleLoader false
          let
            (FRFSTicketBookingStatusAPIRes metroTicketBookingStatus) = resp
            tickets = metroTicketBookingStatus.tickets
            busConfigs = RC.getBusFlowConfigs $ getValueToLocalStore CUSTOMER_LOCATION
          if (metroTicketBookingStatus.status == "CONFIRMED") then do 
            let goToTracking = busConfigs.showPostBookingTracking && maybe false (\(API.FRFSTicketAPI i) -> i.status /= "EXPIRED") (metroTicketBookingStatus.tickets !! 0)
            if metroTicketBookingStatus.vehicleType == "BUS" && goToTracking then  do
              let route = spy "route" $ (fromMaybe [] metroTicketBookingStatus.routeStations) !! 0
                  _ = spy "metroTicketBookingStatus" metroTicketBookingStatus
              let stationList = fromMaybe [] (getStationsFromBusRoute <$> route)
              let routeDetails = case route of 
                          Just (FRFSRouteAPI r) -> {code : r.code, shortName : r.shortName}
                          Nothing -> {code : "", shortName : ""}
    
              let source = maybe Nothing (\(FRFSStationAPI s) -> Just {stationName : s.name,stationCode :s.code}) (stationList !! 0)
              let dest = maybe Nothing (\(FRFSStationAPI s) -> Just {stationName : s.name,stationCode :s.code}) (stationList !! (length stationList -1))
              modifyScreenState $ BusTrackingScreenStateType (\busScreen -> BusTrackingScreenData.initData { data { sourceStation = source, destinationStation = dest, busRouteCode = routeDetails.code, bookingId = bookingId, routeShortName = routeDetails.shortName}, props{ showRouteDetailsTab = false } })
              busTrackingScreenFlow
            else do
              modifyScreenState
                $ MetroTicketDetailsScreenStateType
                    ( \metroTicketDetailsState ->
                        let
                          transformedState = metroTicketDetailsTransformer resp metroTicketDetailsState
                        in
                          transformedState { props { fromScreen = Just (Screen.getScreen Screen.BUS_TICKET_BOOKING_SCREEN), previousScreenStage = ST.MetroMyTicketsStage } }
                    )
              metroTicketDetailsFlow
          else if (metroTicketBookingStatus.status == "CANCELLED") then do
            modifyScreenState
              $ MetroTicketDetailsScreenStateType
                  ( \metroTicketDetailsState ->
                      let
                        transformedState = metroTicketDetailsTransformer resp metroTicketDetailsState
                      in
                        transformedState { props { fromScreen = Just (Screen.getScreen Screen.BUS_TICKET_BOOKING_SCREEN),  previousScreenStage = ST.MetroMyTicketsStage, stage = MetroHardCancelStatusStage } }
                  )
            metroTicketDetailsFlow
          else if (any (_ == metroTicketBookingStatus.status) [ "PAYMENT_PENDING", "FAILED" ]) then do
            modifyScreenState
              $ MetroTicketStatusScreenStateType
                  ( \metroTicketStatusScreen ->
                      let
                        transformedState = metroTicketStatusTransformer resp metroTicketStatusScreen
                      in
                        transformedState { props { entryPoint = BusTicketToMetroTicketStatus} }
                  )
            setValueToLocalStore METRO_PAYMENT_STATUS_POOLING "false"
            metroTicketStatusFlow
          else
            metroMyTicketsFlow
        Left errorPayload -> do
          void $ lift $ lift $ toggleLoader false
          void $ lift $ lift $ showToast (getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
          busTicketBookingFlow
      --------------------------------
    BusTicketBookingController.GoToMetroTicketDetailsScreen (FRFSTicketBookingStatusAPIRes metroTicketStatusApiResp) -> do
      let _ = spy "metroTicketStatusApiResp" metroTicketStatusApiResp
          routeCode = case (metroTicketStatusApiResp.routeStations :: Maybe (Array FRFSRouteAPI)) of
                            Nothing -> ""
                            Just routes -> case head routes of
                                            Just (FRFSRouteAPI route) -> route.code
                                            Nothing -> ""
          sourceName = case (metroTicketStatusApiResp.stations :: Array FRFSStationAPI) of
                            routes -> case head routes of
                                            Just (FRFSStationAPI route) -> route.name
                                            Nothing -> ""
                            _ -> ""
          destinationName = case (metroTicketStatusApiResp.stations :: Array FRFSStationAPI) of
                            routes -> case last routes of
                                            Just (FRFSStationAPI route) -> route.name
                                            Nothing -> ""
                            _ -> ""
          srcCode = case (metroTicketStatusApiResp.stations :: Array FRFSStationAPI) of
                            routes -> case head routes of
                                            Just (FRFSStationAPI route) -> route.code
                                            Nothing -> ""
                            _ -> ""
          destCode = case (metroTicketStatusApiResp.stations :: Array FRFSStationAPI) of
                            routes -> case last routes of
                                            Just (FRFSStationAPI route) -> route.code
                                            Nothing -> ""
                            _ -> ""
      modifyScreenState $ MetroTicketBookingScreenStateType (\_ -> MetroTicketBookingScreenData.initData { props { ticketServiceType = API.BUS, currentStage  = ST.BusTicketSelection , routeName = routeCode , srcLat = currentState.homeScreen.props.sourceLat , srcLong = currentState.homeScreen.props.sourceLong , isButtonActive = true , isRepeatRide = true}, data {ticketCount = 1 , srcLoc = sourceName , destLoc = destinationName,srcCode = srcCode, destCode = destCode} })
      metroTicketBookingFlow
    _ -> pure unit
    
busTrackingScreenFlow :: FlowBT String Unit
busTrackingScreenFlow = do
  action <- UI.busTrackingScreen 
  case action of
    BusTrackingScreen.GoToSearchLocation state -> do
      let currentCity = getValueToLocalStore CUSTOMER_LOCATION
          busRouteSelected = state.data.busRouteCode
      pure $ setText (getNewIDWithTag (show SearchLocPickup)) ""
      modifyScreenState $ SearchLocationScreenStateType (\slsState -> slsState { props { actionType = BusStopSelectionAction ,canSelectFromFav = false, focussedTextField = Just SearchLocPickup , routeSelected = busRouteSelected,isAutoComplete = false }, data { fromScreen =(Screen.getScreen Screen.BUS_ROUTE_STOPS_SEARCH_SCREEN) , srcLoc = Nothing, destLoc = Nothing, stopsSearchedList = state.data.stopsList , updatedStopsSearchedList = DA.dropEnd 1 state.data.stopsList } })
      case state.data.rideType of
        Just STOP -> do
          -- modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state { data { routeList = []}, props {routeName = busRouteName, isEmptyRoute = state.data.busRouteCode } })
          (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> metroTicketBookingFlow)
        _ -> searchLocationFlow
    BusTrackingScreen.GoToBusTicketBooking state -> do
      let srcCode = maybe "" (\item-> item.stationCode) state.data.sourceStation
          destCode = maybe "" (\item-> item.stationCode) state.data.destinationStation
      case state.props.previousScreen of
        PreStopRouteSelection -> selectBusRouteScreenFlow srcCode destCode
        _ -> busTicketBookingFlow
    BusTrackingScreen.GoToViewTicket state -> do 
      (GetMetroBookingStatusResp resp) <- Remote.getMetroStatusBT state.data.bookingId
      let
        (FRFSTicketBookingStatusAPIRes metroTicketBookingStatus) = resp
      -- if (metroTicketBookingStatus.status == "CONFIRMED") then do --check
      modifyScreenState
          $ MetroTicketDetailsScreenStateType
              ( \metroTicketDetailsState ->
                  let
                    transformedState = metroTicketDetailsTransformer resp metroTicketDetailsState
                  in
                    transformedState { props { previousScreenStage = ST.MetroMyTicketsStage, fromScreen = Just $ Screen.getScreen Screen.BUS_TRACKING_SCREEN } }
              )
      metroTicketDetailsFlow
    _ -> busTrackingScreenFlow

updateScheduledRides :: Boolean -> Boolean -> FlowBT String Unit
updateScheduledRides needApiCall updateRentals= do
    response <- FlowCache.fetchAndUpdateScheduledRides needApiCall
    updateRideScheduledTime response ""
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data {latestScheduledRides =response} })
    when updateRentals (do
      modifyScreenState $ RentalScreenStateType (\rentalScreen -> rentalScreen {data{latestScheduledRides = response}})
      pure unit
    )
    pure unit

aadhaarVerificationFlow :: String -> FlowBT String Unit
aadhaarVerificationFlow offerType = do
  (GlobalState currentGlobalState) <- getState
  action <- UI.aadhaarVerificationScreen
  case action of
    ENTER_AADHAAR_OTP state -> do
      void $ lift $ lift $ loaderText "Validating" "Please wait while in progress"
      void $ lift $ lift $ toggleLoader true
      res <- lift $ lift $ Remote.triggerAadhaarOtp state.data.aadhaarNumber
      void $ lift $ lift $ toggleLoader false
      case res of
        Right (GenerateAadhaarOTPResp resp) -> do
          -- let _ = toast resp.message
          case resp.statusCode of
            "1001" -> do
              modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = VerifyAadhaar, btnActive = false}})
              aadhaarVerificationFlow offerType
            _ -> aadhaarVerificationFlow offerType
        Left errorPayload -> do
          let errorCode = HU.decodeErrorCode errorPayload.response.errorMessage
          case errorCode of
            "AADHAAR_NUMBER_NOT_EXIST" -> void $ lift $ lift $ showToast "Aadhaar Does not exist"
            "AADHAAR_ALREADY_LINKED" -> void $ lift $ lift $ showToast $  HU.decodeErrorMessage errorPayload.response.errorMessage
            "INVALID_AADHAAR" -> void $ lift $ lift $ showToast $  HU.decodeErrorMessage errorPayload.response.errorMessage
            _ -> void $ lift $ lift $ showToast $  HU.decodeErrorMessage errorPayload.response.errorMessage
          aadhaarVerificationFlow offerType
    VERIFY_AADHAAR_OTP state -> do
      let _ = spy "coming aa rha hai?" ""
      void $ lift $ lift $ toggleLoader true
      res <- lift $ lift $ Remote.verifyAadhaarOtp state.data.otp
      void $ lift $ lift $ toggleLoader false
      case res of
        Right (VerifyAadhaarOTPResp resp) -> do
          if resp.code == "1002"
            then do
              let appliedDiscountItem = Just $ [ API.FRFSDiscountReq
                { code: offerType
                , quantity: 1
                } ]
              metroBookingStatus <- lift $ lift $ Remote.confirmMetroQuoteV2 currentGlobalState.metroTicketBookingScreen.data.quoteId $ API.FRFSQuoteConfirmReq $ {discounts: fromMaybe [] appliedDiscountItem}
              updateMetroBookingQuoteInfo metroBookingStatus
              modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state { data {applyDiscounts = appliedDiscountItem}, props { currentStage = GetMetroQuote} })
              metroTicketBookingFlow
            else do
              void $ lift $ lift $ showToast "Error Occured please try again later"
              modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = EnterAadhaar, btnActive = false}})
              aadhaarVerificationFlow offerType
        Left errorPayload -> do
          let stage = if (HU.decodeErrorCode errorPayload.response.errorMessage) == "INVALID_OTP" then VerifyAadhaar else AadhaarDetails
          void $ lift $ lift $ if (HU.decodeErrorCode errorPayload.response.errorMessage) == "INVALID_OTP" then showToast "Invalid OTP" else showToast "Something went wrong please try again later"
          modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = VerifyAadhaar, btnActive = false}})
          aadhaarVerificationFlow offerType
    RESEND_AADHAAR_OTP state -> do
      res <- lift $ lift $ Remote.triggerAadhaarOtp state.data.aadhaarNumber
      case res of
        Right (GenerateAadhaarOTPResp resp) -> do
          case resp.statusCode of
            "1001" -> do
              modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = VerifyAadhaar}})
              aadhaarVerificationFlow offerType
            _ -> do
              void $ lift $ lift $ showToast "Verification Failed"
              modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = EnterAadhaar}})
              aadhaarVerificationFlow offerType
        Left errorPayload -> do
          let errorCode = HU.decodeErrorCode errorPayload.response.errorMessage
          case errorCode of
            "INVALID_AADHAAR" -> do
              void $ lift $ lift $ showToast "Verification Failed"
              modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = EnterAadhaar,showErrorAadhaar = true, btnActive = false}})
            "GENERATE_AADHAAR_OTP_EXCEED_LIMIT" -> void $ lift $ lift $ showToast "OTP Resend Limit Exceeded"
            _ -> void $ lift $ lift $ showToast $  HU.decodeErrorMessage errorPayload.response.errorMessage
          modifyScreenState $ AadhaarVerificationScreenType (\aadhaarVerification -> aadhaarVerification{props{currentStage = EnterAadhaar, btnActive = false}})
          aadhaarVerificationFlow offerType
    GO_TO_TICKET_BOOKING_FROM_AADHAAR -> metroTicketBookingFlow
    _ -> aadhaarVerificationFlow offerType

selectBusRouteScreenFlow :: String -> String -> FlowBT String Unit
selectBusRouteScreenFlow srcCode destCode = do
  action <- UI.selectBusRouteScreen srcCode destCode
  case action of
    TRACK_BUS state -> do
      case state.data.selectedQuote of
        Just quote -> 
          case getFirstRoute quote of
            Just (FRFSRouteAPI route) -> do
              (GlobalState allStates) <- getState
              modifyScreenState $ MetroTicketBookingScreenStateType (\metroBookingState -> metroBookingState { data { routeList = (getAllFirstRoutes state.data.quotes)}, props {routeName = route.shortName, isEmptyRoute = route.code } })
              let busConfig = RC.getBusFlowConfigs $ getValueToLocalStore CUSTOMER_LOCATION
              if busConfig.showBusTracking then do
                modifyScreenState $ BusTrackingScreenStateType (\busScreen -> BusTrackingScreenData.initData { data {sourceStation = Just $ mkStation state.data.srcLoc srcCode
                                                                                            , destinationStation = Just $ mkStation state.data.destLoc destCode
                                                                                            , busRouteCode = route.code
                                                                                            , routeShortName = route.shortName
                                                                                            , rideType = Just STOP
                                                                                            },
                                                                                        props { showRouteDetailsTab = true
                                                                                              , previousScreen = PreStopRouteSelection
                                                                                              , srcLat = allStates.homeScreen.props.sourceLat
                                                                                              , srcLon = allStates.homeScreen.props.sourceLong
                                                                                              }
                                                                                      })
                busTrackingScreenFlow
              else metroTicketBookingFlow
            Nothing -> pure unit
        Nothing -> pure unit
      modifyScreenState $ SelectBusRouteScreenType (\state -> state{ data{ quotes = Nothing } })
      selectBusRouteScreenFlow srcCode destCode
    GO_TO_SEARCH_LOCATION_FROM_SELECT_ROUTE -> do
      modifyScreenState $ SearchLocationScreenStateType (\state -> state{ data{ srcLoc = Nothing }, props{ focussedTextField = Just SearchLocPickup } })
      searchLocationFlow

  where
    mkStation name code = {stationName : name, stationCode : code}

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
  , dynamicAction : Nothing
  , types : Nothing
}   

updateMetroBookingQuoteInfo :: (Either ErrorResponse FRFSTicketBookingStatusAPIRes) -> FlowBT String Unit
updateMetroBookingQuoteInfo metroBookingStatus = do
  case metroBookingStatus of
    Right (FRFSTicketBookingStatusAPIRes metroBookingStatus) -> do
      modifyScreenState
          $ MetroTicketBookingScreenStateType
              ( \state ->
                  state
                    { data { bookingId = metroBookingStatus.bookingId }
                    , props { currentStage = ST.PaymentSDKPooling }
                    }
              )              
      let _ = runFn2 setInCache (show METRO_PAYMENT_SDK_POLLING) true
      pure unit
    Left err -> do
      let errResp = err.response
          errMsg = if err.code == 400 
                    then decodeError errResp.errorMessage "errorMessage"
                    else getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
      void $ lift $ lift $ showToast errMsg
      modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state { data {applyDiscounts = Nothing}, props { currentStage  = if state.props.ticketServiceType == BUS then ST.BusTicketSelection else  ST.MetroTicketSelection } })
      pure unit

rideCompletedEventParams :: HomeScreenState -> Array ClevertapEventParams
rideCompletedEventParams state = 
  [ {key: "Source", value: unsafeToForeign (take 99 (state.data.driverInfoCardState.source))},
    {key: "Destination", value: unsafeToForeign (take 99 (state.data.driverInfoCardState.destination))},
    {key: "Ride Amount", value: unsafeToForeign state.data.driverInfoCardState.price},
    {key: "Driver Name", value: unsafeToForeign state.data.driverInfoCardState.driverName},
    {key: "Driver Rating", value: unsafeToForeign state.data.driverInfoCardState.rating},
    {key: "Distance (m)", value: unsafeToForeign state.data.driverInfoCardState.distance},
    {key: "Destination Reached", value: unsafeToForeign state.data.driverInfoCardState.destinationReached},
    {key: "Service Tier Name", value: unsafeToForeign state.data.driverInfoCardState.serviceTierName},
    {key: "ETA", value: unsafeToForeign state.data.driverInfoCardState.eta},
    {key: "Vehicle Variant", value: unsafeToForeign state.data.driverInfoCardState.vehicleVariant} 
  ]