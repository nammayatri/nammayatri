module Screens.HomeScreen.View where

import Common.Types.App
import Screens.RideBookingFlow.HomeScreen.Config

import Accessor (_lat, _lon)
import Animation (fadeOut, translateYAnimFromTop, scaleAnim, translateYAnimFromTopWithAlpha, fadeIn)
import Animation.Config (Direction(..), translateFullYAnimWithDurationConfig, translateYAnimHomeConfig)
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..))
import Components.CancelRide as CancelRidePopUp
import Components.DriverInfoCard as DriverInfoCard
import Components.EmergencyHelp as EmergencyHelp
import Components.ErrorModal as ErrorModal
import Components.FareBreakUp as FareBreakUp
import Components.FavouriteLocationModel as FavouriteLocationModel
import Components.LocationListItem.View as LocationListItem
import Components.LocationTagBar as LocationTagBar
import Components.PopUpModal as PopUpModal
import Components.PopUpModal as PopUpModal
import Components.PricingTutorialModel as PricingTutorialModel
import Components.PrimaryButton as PrimaryButton
import Components.QuoteListModel.View as QuoteListModel
import Components.RateCard as RateCard
import Components.RatingCard as RatingCard
import Components.RequestInfoCard as RequestInfoCard
import Components.SaveFavouriteCard as SaveFavouriteCard
import Components.SearchLocationModel as SearchLocationModel
import Components.SettingSideBar as SettingSideBar
import Components.SourceToDestination as SourceToDestination
import Control.Monad.Except.Trans (lift)
import Data.Array (any, length, mapWithIndex, null, (!!), head, drop) 
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (length, split, trim, Pattern(..)) as Str
import Data.Time.Duration (Milliseconds(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (countDown, flowRunner, getNewIDWithTag, liftFlow, os, safeMarginBottom, safeMarginTop)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getLocationName, getNewTrackingId, parseFloat, storeCallBackCustomer, storeCallBackLocateOnMap, toString, waitingCountdownTimer, getDistanceBwCordinates, fetchAndUpdateCurrentLocation, isPreviousVersion, getCurrentLocationMarker, getPreviousVersion)
import JBridge (drawRoute, firebaseLogEvent, getCurrentPosition, getHeightFromPercent, isCoordOnPath, isInternetAvailable, removeAllPolylines, removeMarker, requestKeyboardShow, showMap, startLottieProcess, updateRoute)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import Prelude (Unit,Ordering,compare, bind, const, discard, map, negate, not, pure, show, unit, void, ($), (&&), (*), (+), (-), (/), (/=), (<), (<<<), (<>), (==), (>), (>=), (||))
import Presto.Core.Types.API (ErrorResponse)
import Presto.Core.Types.Language.Flow (Flow, doAff, delay)
import PrestoDOM (BottomSheetState(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alignParentBottom, background, clickable, color, cornerRadius, disableClickFeedback, ellipsize, fontStyle, frameLayout, gravity, halfExpandedRatio, height, id, imageUrl, imageView, lineHeight, linearLayout, lottieAnimationView, margin, maxLines, onBackPressed, onClick, orientation, padding, peakHeight, relativeLayout, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, imageWithFallback)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import PrestoDOM.Properties (cornerRadii, sheetState)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.HomeScreen.Controller (Action(..), ScreenOutput, eval, getCurrentCustomerLocation, flowWithoutOffers, checkCurrentLocation, getNearestCurrentLocation, checkSavedLocations, getNearestSavedLocation)
import Screens.AddNewAddressScreen.Controller as AddNewAddress
import Screens.Types (HomeScreenState, PopupType(..), SearchLocationModelType(..), Stage(..), PreviousCurrentLocations(..), CurrentLocationDetails(..), CurrentLocationDetailsWithDistance(..), LocationListItemState)
import Services.API (GetDriverLocationResp(..), GetQuotesRes(..), GetRouteResp(..), LatLong(..), RideAPIEntity(..), RideBookingRes(..), Route(..), SearchReqLocationAPIEntity(..), SelectListRes(..), Snapped(..), SavedLocationsListRes(..) )
import Services.Backend (getDriverLocation, getQuotes, getRoute, makeGetRouteReq, rideBooking, selectList, driverTracking, rideTracking, walkCoordinates, walkCoordinate, getSavedLocationList)
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore, isLocalStageOn, updateLocalStage,getValueToLocalNativeStore)
import Styles.Colors as Color
import Types.App (GlobalState)
import Control.Monad.Except (runExcept)
import Foreign.Generic (decodeJSON, encodeJSON)
import Foreign.Class (class Encode)
import Screens.HomeScreen.Transformer (transformSavedLocations)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)

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
            launchAff_ $ flowRunner $ checkForLatLongInSavedLocations push UpdateSavedLoc initialState
            if (not initialState.props.callbackInitiated) then do
              _ <- pure $ printLog "storeCallBackCustomer initiateCallback" "."
              _ <- storeCallBackCustomer push NotificationListener
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
                _ <- removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                launchAff_ $ flowRunner $ getEstimate GetEstimates 10 1000.0 push initialState
              FindingQuotes -> do
                _ <- countDown initialState.props.searchExpire "" push SearchExpireCountDown
                _ <- pure $ setValueToLocalStore GOT_ONE_QUOTE "FALSE"
                launchAff_ $ flowRunner $ getQuotesPolling GetQuotesList Restart 30 3000.0 push initialState
              ConfirmingRide -> launchAff_ $ flowRunner $ confirmRide GetRideConfirmation 5 1000.0 push initialState
              HomeScreen -> do
                _ <- pure $ removeAllPolylines ""
                fetchAndUpdateCurrentLocation push UpdateLocAndLatLong RecenterCurrentLocation
              RideAccepted -> do
                if ((getValueToLocalStore DRIVER_ARRIVAL_ACTION) == "TRIGGER_WAITING_ACTION") then waitingCountdownTimer initialState.data.driverInfoCardState.driverArrivalTime push WaitingTimeAction else pure unit
                if ((getValueToLocalStore TRACKING_DRIVER) == "False") then do
                  _ <- removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                  _ <- pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
                  launchAff_ $ flowRunner $ driverLocationTracking push UpdateCurrentStage DriverArrivedAction UpdateETA 2000.0 (getValueToLocalStore TRACKING_ID) initialState
                else
                  pure unit
              RideStarted -> do
                if ((getValueToLocalStore TRACKING_DRIVER) == "False") then do
                  _ <- removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                  _ <- pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
                  launchAff_ $ flowRunner $ driverLocationTracking push UpdateCurrentStage DriverArrivedAction UpdateETA 20000.0 (getValueToLocalStore TRACKING_ID) initialState
                else
                  pure unit
              ConfirmingLocation -> do
                _ <- removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                _ <- storeCallBackLocateOnMap push UpdatePickupLocation
                pure unit
              TryAgain -> do
                launchAff_ $ flowRunner $ getEstimate EstimatesTryAgain 10 1000.0 push initialState
              _ -> pure unit
            if ((initialState.props.sourceLat /= (-0.1)) && (initialState.props.sourceLong /= (-0.1))) then do
              case initialState.props.sourceLat, initialState.props.sourceLong of
                0.0, 0.0 -> do
                  if (initialState.props.currentStage == HomeScreen) then do
                    _ <- getCurrentPosition push CurrentLocation
                    pure (pure unit)
                  else do
                    getCurrentCustomerLocation push initialState
                _, _ ->
                  if (initialState.props.currentStage == HomeScreen) then do
                    pure (pure unit)
                  else do
                    let src = initialState.data.source 
                    if src == "" || src == "Current Location" then do
                        if (checkCurrentLocation initialState.props.sourceLat initialState.props.sourceLong initialState.data.previousCurrentLocations.pastCurrentLocations  && initialState.props.storeCurrentLocs )|| checkSavedLocations initialState.props.sourceLat initialState.props.sourceLong initialState.data.savedLocations
                          then push $ UpdateSourceFromPastLocations 
                        else do 
                          getLocationName push (show initialState.props.sourceLat) (show initialState.props.sourceLong) "Current Location" (if src == "Current Location" then UpdateSourceName else UpdateSource)
                        pure (pure unit)
                    else  pure (pure unit)
            else
              pure (pure unit)
        )
      ]
  , eval:
      \state action -> do
        let _ = spy "HomeScreen action " state
        let _ = spy "HomeScreen state " action
        eval state action
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
  -- screenAnimation $
  frameLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , onBackPressed push (const BackPressed)
    , clickable true
    , afterRender push (const AfterRender)
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , clickable true
        , afterRender
            (  \action -> do
                _ <- push action
                _ <- showMap (getNewIDWithTag "CustomerHomeScreenMap") isCurrentLocationEnabled "satellite" (17.0) push MAPREADY
                pure unit
            )
            (const MapReadyAction)
        ]
        [ relativeLayout
            [ width MATCH_PARENT
            , weight 1.0
            , orientation VERTICAL
            , background "#FAFAFA"
            , height MATCH_PARENT
            ]
            [ frameLayout
                [ width MATCH_PARENT
                , height MATCH_PARENT
                , clickable true
                ]
                [ linearLayout
                    [ height MATCH_PARENT
                    , width MATCH_PARENT
                    , id (getNewIDWithTag "CustomerHomeScreenMap")
                    ]
                    []
                , linearLayout
                    [ width MATCH_PARENT
                    , height MATCH_PARENT
                    , background Color.transparent
                    , padding (PaddingBottom if os == "IOS" then 20 else 35)
                    , gravity CENTER
                    ]
                    [ imageView
                        [ width $ V 35
                        , height $ V 35
                        , imageWithFallback $ case (state.props.currentStage == ConfirmingLocation) || state.props.isSource == (Just true) of
                            true  ->  (if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "src_marker" else "ny_ic_src_marker") <> ",https://assets.juspay.in/nammayatri/images/common/ny_ic_src_marker.png" 
                            false ->  (if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "dest_marker" else "ny_ic_dest_marker") <> ",https://assets.juspay.in/nammayatri/images/common/ny_ic_dest_marker.png"
                        , visibility if ((state.props.currentStage == ConfirmingLocation) || state.props.locateOnMap) then VISIBLE else GONE
                        ]
                    ]
                ]
            , homeScreenView push state
            , buttonLayoutParentView push state
            , if (not state.props.rideRequestFlow) || (state.props.currentStage == FindingEstimate || state.props.currentStage == ConfirmingRide) then emptyTextView state else topLeftIconView state push
            , linearLayout
                [ width MATCH_PARENT
                , height MATCH_PARENT
                , background if (state.props.currentStage == RideCompleted || state.props.currentStage == RideRating) then Color.black9000 else Color.transparent
                ]
                []
            , rideRequestFlowView push state
            , if state.props.currentStage == PricingTutorial then (pricingTutorialView push state) else emptyTextView state
            , rideTrackingView push state
            , if ((not state.props.ratingModal) && (state.props.showlocUnserviceablePopUp) && state.props.currentStage == HomeScreen) then (sourceUnserviceableView push state) else emptyTextView state
            , if state.data.settingSideBar.opened /= SettingSideBar.CLOSED then settingSideBarView push state else emptyTextView state
            , if (state.props.currentStage == SearchLocationModel || state.props.currentStage == FavouriteLocationModel) then searchLocationView push state else emptyTextView state
            , if (any (_ == state.props.currentStage) [ FindingQuotes, QuoteList ]) then (quoteListModelView push state) else emptyTextView state
            , if (state.props.isCancelRide) then (cancelRidePopUpView push state) else emptyTextView state
            , if (state.props.isPopUp /= NoPopUp) then (logOutPopUpView push state) else emptyTextView state
            , if (state.props.isLocationTracking) then (locationTrackingPopUp push state) else emptyTextView state
            , if (state.props.isEstimateChanged) then (estimateChangedPopUp push state) else emptyTextView state
            , if state.props.ratingModal then previousRideRatingView push state else emptyTextView state
            , if state.props.currentStage == DistanceOutsideLimits then (distanceOutsideLimitsView push state) else emptyTextView state
            , if state.props.currentStage == ShortDistance then (shortDistanceView push state) else emptyTextView state
            , if state.props.showRateCard then (rateCardView push state) else emptyTextView state
            , if state.props.currentStage == RideRating then rideRatingCardView state push else emptyTextView state
            , if state.props.isSaveFavourite then saveFavouriteCardView push state else emptyTextView state
            , if state.props.emergencyHelpModal then (emergencyHelpModal push state) else emptyTextView state
            , if state.props.showShareAppPopUp then (shareAppPopUp push state) else emptyTextView state
            , if state.props.showMultipleRideInfo then (requestInfoCardView push state) else emptyTextView state
            ]
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
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
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
        , margin if ((state.props.showlocUnserviceablePopUp) && state.props.currentStage == HomeScreen) then (MarginBottom (360 + safeMarginBottom)) else (Margin 0 0 0 0) --else if (state.props.currentStage == ConfirmingLocation) then (Margin ((screenWidth unit) - 66) 0 0 270) else(Margin ((screenWidth unit) - 66) 0 0 120)
        ]
        [ -- linearLayout
          --   [ width WRAP_CONTENT
          --   , height WRAP_CONTENT
          --   , stroke ("1," <> Color.grey900)
          --   , cornerRadii $ Corners 24.0 true true true true 
          --   ][ 
          imageView
            [ imageWithFallback "ny_ic_recenter_btn,https://assets.juspay.in/nammayatri/images/common/ny_ic_recenter_btn.png"
            , onClick
                ( \action -> do
                    _ <- push action
                    _ <- getCurrentPosition push UpdateCurrentLocation
                    _ <- pure $ firebaseLogEvent "ny_user_recenter_btn_click"
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
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , visibility if state.props.hasTakenRide then GONE else VISIBLE
    , stroke $ "1," <> if not state.props.isReferred then Color.blue900 else Color.black700
    , margin (MarginHorizontal 16 13)
    , cornerRadius 20.0
    , background Color.white900
    , gravity RIGHT
    , padding (Padding 16 12 16 12)
    , onClick push $ const $ if state.props.isReferred then ReferralFlowNoAction else ReferralFlowAction
    ][
      imageView [
         imageWithFallback "ny_ic_tick,https://assets.juspay.in/nammayatri/images/user/ny_ic_tick.png"
        , width $ V 20
        , height $ V 15
        , margin (Margin 0 3 5 0)
        , visibility if state.props.isReferred then VISIBLE else GONE
      ]
      , textView $ [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , color if not state.props.isReferred then Color.blue900 else Color.black700
      , text if not state.props.isReferred then (getString HAVE_REFERRAL_CODE) else (getString REFERRAL_CODE_APPLIED)
      ] <> FontStyle.tags TypoGraphy
    ]

sourceUnserviceableView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
sourceUnserviceableView push state =
  PrestoAnim.animationSet [ fadeIn true ]
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , alignParentBottom "true,-1"
        , gravity BOTTOM
        ]
        [ recenterButtonView push state
        , ErrorModal.view (push <<< SourceUnserviceableActionController) (sourceUnserviceableConfig state)
        ]

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
        [ RequestInfoCard.view (push <<< RequestInfoCardAction)  ]

buttonLayout :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
buttonLayout state push =
  PrestoAnim.animationSet (buttonLayoutAnimation state)
    $ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , alignParentBottom "true,-1"
        , orientation VERTICAL
        ]
        [ 
          linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          ][
            referralView push state
          , recenterButtonView push state
          ]
        , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background if (((state.data.savedLocations == []) && state.data.recentSearchs.predictionArray == []) || state.props.isSearchLocation == LocateOnMap) then Color.transparent else Color.grey900
            ]
            []
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , background if (((state.data.savedLocations == []) && state.data.recentSearchs.predictionArray == []) || state.props.isSearchLocation == LocateOnMap) then Color.transparent else Color.white900
            , orientation VERTICAL
            , padding (PaddingTop 16)
            ]
            [ PrimaryButton.view (push <<< PrimaryButtonActionController) (whereToButtonConfig state)
            , if (((state.data.savedLocations == []) && state.data.recentSearchs.predictionArray == []) || state.props.isSearchLocation == LocateOnMap) then emptyLayout state else recentSearchesAndFavourites state push
            ]
        ]

recentSearchesAndFavourites :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
recentSearchesAndFavourites state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $ Padding 16 0 16 (16+safeMarginBottom)
  , cornerRadii $ Corners (4.0) true true false false 
  , background Color.white900
  ][ savedLocationsView state push
   , recentSearchesView state push
  ]

savedLocationsView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
savedLocationsView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin $ MarginTop 16
        , visibility if (state.data.savedLocations /= []) then VISIBLE else GONE
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
    , visibility if state.data.recentSearchs.predictionArray == [] then GONE else VISIBLE
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
                  ]
                  [ LocationListItem.view (push <<< PredictionClickedAction) item
                  , linearLayout
                      [ height $ V 1
                      , width MATCH_PARENT
                      , background Color.lightGreyShade
                      , visibility if index == (length state.data.recentSearchs.predictionArray) - 1 then GONE else VISIBLE
                      ]
                      []
                  ]
            )
            state.data.recentSearchs.predictionArray
        )
    ]

buttonLayoutAnimation :: HomeScreenState -> Array PrestoAnim.Animation
buttonLayoutAnimation state = if os == "IOS" then [ fadeIn true ] else [ translateYAnimFromTop $ translateYAnimHomeConfig BOTTOM_TOP, fadeOut (state.props.showlocUnserviceablePopUp) ]

------------- settingSideBarView ------------
settingSideBarView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
settingSideBarView push state =
  linearLayout
    [ weight 1.0
    , height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ SettingSideBar.view (push <<< SettingSideBarActionController) (state.data.settingSideBar) ]

------------------------------- homeScreenView --------------------------
homeScreenView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
homeScreenView push state =
  PrestoAnim.animationSet
    [ fadeOut (state.props.currentStage == SearchLocationModel)
    ]
    $ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding (Padding 0 safeMarginTop 0 safeMarginBottom)
        , orientation VERTICAL
        ]
        [ if (not state.props.rideRequestFlow) then homeScreenTopIconView push state else emptyTextView state ]

homeScreenTopIconView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
homeScreenTopIconView push state =
  homeScreenAnimation TOP_BOTTOM
    $ -- 1000 (-100) 0 0 true $ PrestoAnim.Bezier 0.37 0.0 0.63 1.0] $ 
      linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin (Margin 16 26 16 0)
        , padding (Padding 0 16 16 16)
        , orientation HORIZONTAL
        , cornerRadius 8.0
        , background Color.white900
        , visibility if state.props.rideRequestFlow then GONE else VISIBLE
        , stroke $ "1," <> Color.grey900
        , gravity CENTER_VERTICAL
        ]
        [ linearLayout
            [ width WRAP_CONTENT -- $ V 54
            , height MATCH_PARENT
            , gravity CENTER
            , disableClickFeedback true
            , clickable if state.props.currentStage == SearchLocationModel then false else true
            , onClick push $ const OpenSettings
            ]
            [ imageView
                [ imageWithFallback "ny_ic_hamburger,https://assets.juspay.in/nammayatri/images/user/ny_ic_hamburger.png"
                , height $ V 24
                , width $ V 24
                , margin (Margin 16 16 16 16)
                ]
            ]
        , linearLayout
            [ height $ V 42
            , width $ V 1
            , background Color.grey900
            ]
            []
        , imageView
            [ imageWithFallback "ny_ic_source_dot,https://assets.juspay.in/nammayatri/images/common/ny_ic_source_dot.png"
            , height $ V 20
            , width $ V 20
            , margin (Margin 5 5 5 5)
            , onClick push (const $ OpenSearchLocation)
            , gravity BOTTOM
            ]
        , linearLayout
            [ orientation VERTICAL
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , disableClickFeedback true
            , onClick push (const $ OpenSearchLocation)
            ]
            [ textView
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , text (getString PICK_UP_LOCATION)
                , color Color.black800
                , gravity LEFT
                , fontStyle $ FontStyle.regular LanguageStyle
                , textSize FontSize.a_12
                , lineHeight "16"
                ]
            , textView
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , text if state.data.source /= "" then state.data.source else (getString CURRENT_LOCATION)
                , maxLines 1
                , ellipsize true
                , color Color.black800
                , gravity LEFT
                , fontStyle $ FontStyle.bold LanguageStyle
                , textSize FontSize.a_16
                , lineHeight "23"
                ]
            ]
        ]
  where
  homeScreenAnimation direction = PrestoAnim.animationSet [ translateYAnimFromTop $ translateYAnimHomeConfig direction ]
------------------------------- rideRequestFlowView --------------------------
rideRequestFlowView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rideRequestFlowView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadii $ Corners 24.0 true true false false
    , visibility if (any (_ == state.props.currentStage) [ SettingPrice, ConfirmingLocation, RideCompleted, FindingEstimate, ConfirmingRide, FindingQuotes, TryAgain ]) then VISIBLE else GONE
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
        ]
        [ PrestoAnim.animationSet [ fadeIn true ]
            $ if (state.props.currentStage == SettingPrice) then
                suggestedPriceView push state
              else if (state.props.currentStage == ConfirmingLocation) then
                confirmPickUpLocationView push state
              else
                emptyTextView state
        , if (any (_ == state.props.currentStage) [ FindingEstimate, ConfirmingRide, TryAgain ]) then
            (loaderView push state)
          else
            emptyTextView state
        , rideCompletedCardView state push
        ]
    ]

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

-------------- rideCompletedCardView ------------- 
rideCompletedCardView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rideCompletedCardView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (Padding 16 16 16 16)
    , stroke ("1," <> Color.grey900)
    , background Color.white900
    , cornerRadii $ Corners 24.0 true true false false
    , visibility if state.props.currentStage == RideCompleted then VISIBLE else GONE
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , gravity CENTER
        ]
        [ linearLayout
            [ width WRAP_CONTENT
            , height MATCH_PARENT
            , gravity CENTER
            ]
            [ textView
                [ text $ "₹" <> show state.data.finalAmount
                , color Color.black800
                , textSize FontSize.a_40
                , width WRAP_CONTENT
                , height WRAP_CONTENT
                , fontStyle $ FontStyle.bold LanguageStyle
                ]
            , textView
                [ textFromHtml $ "<strike> ₹" <> (show state.data.driverInfoCardState.price) <> "</strike>"
                , textSize FontSize.a_24
                , margin $ Margin 8 5 0 0
                , width WRAP_CONTENT
                , height WRAP_CONTENT
                , fontStyle $ FontStyle.medium LanguageStyle
                , lineHeight "40"
                , color Color.black600
                , visibility if state.data.finalAmount /= state.data.driverInfoCardState.price then VISIBLE else GONE
                ]
            ]
        , textView
            [ text $ getString PAY_DRIVER_USING_CASH_OR_UPI
            , textSize FontSize.a_16
            , lineHeight "20"
            , width MATCH_PARENT
            , gravity CENTER_HORIZONTAL
            , fontStyle $ FontStyle.medium LanguageStyle
            , color Color.black800
            , margin $ MarginVertical 4 24
            ]
        ]
    , fareUpdatedView state push
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , stroke ("1," <> Color.grey900)
        , clickable true
        , cornerRadius 8.0
        , padding (Padding 16 24 16 24)
        , margin (MarginBottom 24)
        ]
        [ FareBreakUp.view (push <<< FareBreakUpActionController) (fareBreakUpConfig state) ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ]
        [ PrimaryButton.view (push <<< SkipButtonActionController) (skipButtonConfig state)
        , PrimaryButton.view (push <<< RateRideButtonActionController) (rateRideButtonConfig state)
        ]
    ]

fareUpdatedView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
fareUpdatedView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , cornerRadius 8.0
    , stroke $ "1," <> Color.grey900
    , padding $ Padding 16 12 16 12
    , margin $ MarginBottom 20
    , gravity CENTER_VERTICAL
    , visibility if (state.data.finalAmount /= state.data.driverInfoCardState.price && state.props.estimatedDistance /= Nothing) then VISIBLE else GONE
    ]
    [ imageView
        [ width $ V 16
        , height $ V 16
        , imageWithFallback "ny_ic_parallel_arrows,https://assets.juspay.in/nammayatri/images/common/ny_ic_parallel_arrows.png"
        , margin $ MarginRight 12
        ]
    , textView
        $ ( [ height MATCH_PARENT
            , width WRAP_CONTENT
            , gravity CENTER_VERTICAL
            , text $ FareBreakUp.getFareUpdatedString state.data.previousRideRatingState.distanceDifference
            , color Color.black700
            ]
          )
        <> FontStyle.body3 TypoGraphy
    ]

----------- topLeftIconView -------------
topLeftIconView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
topLeftIconView state push =
  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , margin (Margin 16 48 0 0)
      ][
        linearLayout
          [ height $ V 48
          , width $ V 48
          , stroke ("1," <> Color.grey900)
          , background Color.white900
          , gravity CENTER
          , cornerRadius 24.0
          , visibility if (any (_ == state.props.currentStage) [ FindingEstimate, ConfirmingRide, FindingQuotes, TryAgain ]) then GONE else VISIBLE
          , clickable true
          , onClick push $ if (any (_ == state.props.currentStage) [ SettingPrice, ConfirmingLocation, PricingTutorial, DistanceOutsideLimits ]) then const BackPressed else const OpenSettings
          ]
          [ imageView
              [ imageWithFallback if (any (_ == state.props.currentStage) [ SettingPrice, ConfirmingLocation, PricingTutorial, DistanceOutsideLimits ]) then "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png" else "ny_ic_hamburger,https://assets.juspay.in/nammayatri/images/user/ny_ic_hamburger.png"
              , height $ V 25
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
      ]

----------- suggestedPriceView -------------
suggestedPriceView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
suggestedPriceView push state =
  linearLayout
  [ orientation VERTICAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.white900
  , clickable true
  , visibility if (state.props.currentStage == SettingPrice) then VISIBLE else GONE
  , padding (Padding 16 16 16 24)
  , stroke ("1," <> Color.grey900)
  , gravity CENTER
  , cornerRadii $ Corners 24.0 true true false false 
  ][  textView
      [ text $ getString REQUEST_AUTO_RIDE
      , textSize FontSize.a_22
      , color Color.black800
      , gravity CENTER_HORIZONTAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , fontStyle $ FontStyle.bold LanguageStyle
      ]
    , linearLayout
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
        ][  textView
            [ text $ if state.data.rateCard.additionalFare == 0 then "₹" <> (show state.data.suggestedAmount) else  "₹" <> (show state.data.suggestedAmount) <> "-" <> "₹" <> (show $ (state.data.suggestedAmount + state.data.rateCard.additionalFare))
            , textSize FontSize.a_32
            , color Color.black800
            , margin $ MarginTop 8
            , gravity CENTER_HORIZONTAL
            , width WRAP_CONTENT
            , height WRAP_CONTENT
            , fontStyle $ FontStyle.bold LanguageStyle
            , onClick push $ const ShowRateCard
            ]
            , estimatedTimeAndDistanceView push state
          ]
          , imageView
            [ imageWithFallback "ny_ic_info_blue,https://assets.juspay.in/nammayatri/images/common/ny_ic_info_blue.png"
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
                  , margin (Margin 0 0 0 8)
                  ][ 
                      textView
                      [ height $ V 24
                      , width WRAP_CONTENT
                      , color Color.darkDescriptionText
                      , text $ getString BOOKING_PREFERENCE
                      , textSize FontSize.a_16
                      , fontStyle $ FontStyle.regular LanguageStyle
                      
                      ],
                      imageView
                      [ width $ V 10
                      , height $ V 10
                      , margin (Margin 9 8 0 0)
                      , imageWithFallback if state.data.showPreferences then "ny_ic_chevron_up,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_up.png" else "ny_ic_chevron_down,https://assets.juspay.in/nammayatri/images/user/ny_ic_down_arrow.png"
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
                       ][showMenuButtonView push (getString AUTO_ASSIGN_DRIVER) "ny_ic_faster,https://assets.juspay.in/nammayatri/images/user/ny_ic_faster.png" true,
                         showMenuButtonView push (getString CHOOSE_BETWEEN_MULTIPLE_DRIVERS) "ny_ic_info,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png" false]
                  ]
                  
              ]
          ]
      ]
    , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonRequestRideConfig state)
  ]


showMenuButtonView :: forall w. (Action -> Effect Unit) -> String -> String -> Boolean -> PrestoDOM (Effect Unit) w 
showMenuButtonView push menuText menuImage autoAssign = 
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , gravity CENTER
  , margin $ (Margin 0 10 0 10)
  ][ linearLayout
      [ height $ V 20
      , width $ V 20
      , stroke if ( (flowWithoutOffers WithoutOffers) && autoAssign || not (flowWithoutOffers WithoutOffers) && not autoAssign ) then ("2," <> Color.black800) else ("2," <> Color.black600)
      , cornerRadius 10.0
      , gravity CENTER
      , onClick push (const $ CheckBoxClick autoAssign)
      ][  imageView
          [ width $ V 10
          , height $ V 10
          , imageWithFallback "ny_ic_radio_button,https://assets.juspay.in/nammayatri/images/common/ny_ic_radio_button.png"
          , visibility if ( (flowWithoutOffers WithoutOffers) && autoAssign || not (flowWithoutOffers WithoutOffers) && not autoAssign ) then VISIBLE else GONE
          ]
        ]
    , textView
      [ text menuText
      , textSize FontSize.a_14
      , width MATCH_PARENT
      , gravity CENTER
      , color Color.black700
      , height WRAP_CONTENT
      , margin (MarginHorizontal 10 10)
      , fontStyle $ FontStyle.regular LanguageStyle
      , onClick push (const $ CheckBoxClick autoAssign)
      ]
    , imageView
      [ height $ if autoAssign then V 30 else V 18
      , width $ if autoAssign then V 75 else V 18
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
  , gravity CENTER
  , margin $ MarginTop 4
  ][ textView
      [ text state.data.rideDistance
      , textSize FontSize.a_14
      , width MATCH_PARENT
      , gravity CENTER
      , color Color.black650
      , height WRAP_CONTENT
      , fontStyle $ FontStyle.regular LanguageStyle
      ]
    , linearLayout
      [height $ V 4
      , width $ V 4
      , cornerRadius 2.5
      , background Color.black600
      , margin (Margin 6 2 6 0)
      ][]
    , textView
      [ text state.data.rideDuration
      , textSize FontSize.a_14
      , width MATCH_PARENT
      , gravity CENTER
      , color Color.black650
      , height WRAP_CONTENT
      , fontStyle $ FontStyle.regular LanguageStyle
      ]
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
                          , visibility if (state.props.currentStage == RideAccepted && item.type == "GOOGLE_MAP") || (idx == (length locationTrackingData) - 1) then GONE else VISIBLE
                          ]
                          []
                      ]
                )
                locationTrackingData
            )
        ]
    ]

estimateChangedPopUp :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
estimateChangedPopUp push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
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
        [ imageWithFallback "ny_ic_chevron_right,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_right.png"
        , height $ V 20
        , width $ V 22
        , padding (Padding 3 3 3 3)
        ]
    ]

locationTrackingData :: Array { text :: String, imageWithFallback :: String, type :: String }
locationTrackingData =
  [ { text: (getString GOOGLE_MAP_)
    , imageWithFallback: "ny_ic_track_google_map,https://assets.juspay.in/nammayatri/images/user/ny_ic_track_google_map.png"
    , type: "GOOGLE_MAP"
    }
  , { text: (getString IN_APP_TRACKING)
    , imageWithFallback: "ny_ic_track_in_app,https://assets.juspay.in/nammayatri/images/user/ny_ic_track_in_app.png"
    , type: "IN_APP"
    }
  ]

----------- confirmPickUpLocationView -------------
confirmPickUpLocationView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
confirmPickUpLocationView push state =
  linearLayout
    [ orientation VERTICAL
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , clickable true
    , background Color.transparent
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
        , background Color.white900
        , stroke $ "1," <> Color.grey900
        , cornerRadii $ Corners 24.0 true true false false
        , padding $ Padding 16 16 16 32
        ]
        [ textView
            [ text (getString CONFIRM_PICKUP_LOCATION)
            , textSize FontSize.a_22
            , color Color.black800
            , gravity CENTER_HORIZONTAL
            , height WRAP_CONTENT
            , width MATCH_PARENT
            , fontStyle $ FontStyle.bold LanguageStyle
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , margin $ MarginVertical 20 10
            , onClick push $ const GoBackToSearchLocationModal
            , padding $ PaddingHorizontal 15 15
            , stroke $ "1," <> Color.grey900
            , gravity CENTER_VERTICAL
            , cornerRadius 5.0
            ]
            [ imageView
                [ imageWithFallback "ny_ic_source_dot,https://assets.juspay.in/nammayatri/images/common/ny_ic_source_dot.png"
                , height $ V 20
                , width $ V 20
                , gravity CENTER_VERTICAL
                ]
            , textView
                $
                  [ text state.data.source
                  , ellipsize true
                  , singleLine true
                  , gravity CENTER
                  , padding (Padding 10 16 10 16)
                  , color Color.black800
                  ]
                <> FontStyle.subHeading1 TypoGraphy
            ]
        , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfirmPickupConfig state)
        ]
    ]

----------- loaderView -------------
loaderView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
loaderView push state =
  linearLayout
    [ orientation VERTICAL
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , padding (Padding 0 40 0 32)
    , background Color.white900
    , cornerRadii $ Corners 24.0 true true false false
    , stroke ("1," <> Color.grey900)
    , clickable true
    , gravity CENTER_HORIZONTAL
    , visibility if (any (_ == state.props.currentStage) [ FindingEstimate, ConfirmingRide, TryAgain ]) then VISIBLE else GONE
    ]
    [ PrestoAnim.animationSet [ scaleAnim $ autoAnimConfig ]
        $ lottieLoaderView state push
    , PrestoAnim.animationSet [ fadeIn true ]
        $ textView
            [ text
                ( case state.props.currentStage of
                    ConfirmingRide -> (getString CONFIRMING_THE_RIDE_FOR_YOU)
                    FindingEstimate -> (getString GETTING_ESTIMATES_FOR_YOU)
                    _ -> (getString GETTING_ESTIMATES_FOR_YOU)
                )
            , textSize FontSize.a_16
            , color Color.black800
            , fontStyle $ FontStyle.semiBold LanguageStyle
            , height WRAP_CONTENT
            , width MATCH_PARENT
            , lineHeight "20"
            , gravity CENTER
            , margin (Margin 0 24 0 36)
            ]
    , PrestoAnim.animationSet [ translateYAnimFromTopWithAlpha $ translateFullYAnimWithDurationConfig 300 ]
        $ separator (V 1) Color.grey900 state.props.currentStage
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , onClick push $ const CancelSearch
        , visibility if (any (_ == state.props.currentStage) [ FindingEstimate, TryAgain ]) then VISIBLE else GONE
        , orientation VERTICAL
        , gravity CENTER
        ]
        [ PrestoAnim.animationSet [ translateYAnimFromTopWithAlpha $ translateFullYAnimWithDurationConfig 300 ]
            $ textView
                [ text (getString CANCEL_SEARCH)
                , textSize FontSize.a_14
                , lineHeight "18"
                , width MATCH_PARENT
                , height WRAP_CONTENT
                , padding (Padding 0 20 0 0)
                , color Color.red
                , gravity CENTER
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
  QuoteListModel.view (push <<< QuoteListModelActionController) $ quoteListModelViewState state
    

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
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (Padding 0 0 0 0)
    , background Color.transparent
    , alignParentBottom "true,-1" -- Check it in Android.
    , onBackPressed push (const $ BackPressed)
    , visibility $ if (state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted) then VISIBLE else GONE
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
            ]
            [ bottomSheetLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , background Color.transparent
                , sheetState COLLAPSED
                , peakHeight if state.props.currentStage == RideAccepted then getHeightFromPercent 58 else getHeightFromPercent 46
                , visibility VISIBLE
                , halfExpandedRatio 0.9
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    ]
                    [ if (state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted) then
                        DriverInfoCard.view (push <<< DriverInfoCardActionController) $ driverInfoCardViewState state
                         
                      else
                        emptyTextView state
                    ]
                ]
            ]
        ]
    ]

previousRideRatingView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
previousRideRatingView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity BOTTOM
    , background Color.black9000
    ]
    [ RatingCard.view (push <<< RatingCardAC) $ previousRideRatingViewState state ]

distanceOutsideLimitsView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
distanceOutsideLimitsView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity BOTTOM
    ]
    [ PopUpModal.view (push <<< DistanceOutsideLimitsActionController) (distanceOusideLimitsConfig state) ]

shortDistanceView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
shortDistanceView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity BOTTOM
    ]
    [ PopUpModal.view (push <<< ShortDistanceActionController) (shortDistanceConfig state) ]

saveFavouriteCardView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
saveFavouriteCardView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ SaveFavouriteCard.view (push <<< SaveFavouriteCardAction) (state.data.saveFavouriteCard) ]

logOutPopUpView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
logOutPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
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
    , visibility if currentStage == FindingQuotes then GONE else VISIBLE
    ]
    []

lottieLoaderView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
lottieLoaderView state push =
  lottieAnimationView
    [ id (getNewIDWithTag "lottieLoader")
    , afterRender
        ( \action -> do
            _ <- pure $ startLottieProcess "auto_rickshaw_processing" (getNewIDWithTag "lottieLoader") true 0.6 "Default"
            pure unit
        )
        (const LottieLoaderAction)
    , height $ V 96
    , width $ V 96
    ]

getEstimate :: forall action. (GetQuotesRes -> action) -> Int -> Number -> (action -> Effect Unit) -> HomeScreenState -> Flow GlobalState Unit
getEstimate action count duration push state = do
  if (isLocalStageOn FindingEstimate) || (isLocalStageOn TryAgain) then
    if (count > 0) then do
      resp <- getQuotes (state.props.searchId)
      _ <- pure $ printLog "caseId" (state.props.searchId)
      case resp of
        Right response -> do
          _ <- pure $ printLog "api Results " response
          let (GetQuotesRes resp) = response
          if (not (null resp.estimates)) then do
            doAff do liftEffect $ push $ action response
            pure unit
          else do
            if (count == 1) then do
              _ <- pure $ updateLocalStage SearchLocationModel
              doAff do liftEffect $ push $ action response
            else do
              void $ delay $ Milliseconds duration
              getEstimate action (count - 1) duration push state
        Left err -> do
          _ <- pure $ printLog "api error " err
          void $ delay $ Milliseconds duration
          if (count == 1) then do
            let response = GetQuotesRes { quotes: [], estimates: [], fromLocation: SearchReqLocationAPIEntity { lat: 0.0, lon: 0.0 }, toLocation: Nothing }
            _ <- pure $ updateLocalStage SearchLocationModel
            doAff do liftEffect $ push $ action response
          else do
            getEstimate action (count - 1) duration push state
    else
      pure unit
  else
    pure unit

getQuotesPolling :: forall action. (SelectListRes -> action) -> (ErrorResponse -> action) -> Int -> Number -> (action -> Effect Unit) -> HomeScreenState -> Flow GlobalState Unit
getQuotesPolling action retryAction count duration push state = do
  internetCondition <- liftFlow $ isInternetAvailable unit
  case (getValueToLocalStore LOCAL_STAGE) of
    "FindingQuotes" ->
      if not internetCondition then
        pure unit
      else do
        -- let tempA = (getValueToLocalStore GOT_ONE_QUOTE)
        let gotQuote = (getValueToLocalStore GOT_ONE_QUOTE)
        let usableCount = if gotQuote == "TRUE" && count > 9 then 9 else count
        if (spy "USABLECOUNT :- " usableCount > 0) then do
          resp <- selectList (state.props.estimateId)
          _ <- pure $ printLog "caseId" (state.props.estimateId)
          case resp of
            Right response -> do
              _ <- pure $ printLog "Quote api Results " response
              let (SelectListRes resp) = response
              if not (null resp.selectedQuotes) then do
                if (getValueToLocalStore GOT_ONE_QUOTE == "FALSE") then do
                  _ <- pure $ firebaseLogEvent "ny_user_received_quotes"
                  pure unit
                else pure unit 
                _ <- pure $ setValueToLocalStore GOT_ONE_QUOTE "TRUE"
                doAff do liftEffect $ push $ action response
              else
                pure unit
              void $ delay $ Milliseconds duration
              getQuotesPolling action retryAction (usableCount - 1) duration push state
            Left err -> do
              _ <- pure $ printLog "api error " err
              doAff do liftEffect $ push $ retryAction err
              void $ delay $ Milliseconds duration
              pure unit
              getQuotesPolling action retryAction (usableCount - 1) duration push state
        else do
          let response = SelectListRes { selectedQuotes: [] }
          _ <- pure $ updateLocalStage QuoteList
          doAff do liftEffect $ push $ action response
    _ -> pure unit

driverLocationTracking :: forall action. (action -> Effect Unit) -> (String -> action) -> (String -> action) -> (Int -> Int -> action) -> Number -> String -> HomeScreenState -> Flow GlobalState Unit
driverLocationTracking push action driverArrivedAction updateState duration trackingId state = do
  _ <- pure $ printLog "trackDriverLocation2_function" trackingId
  if ((isLocalStageOn RideAccepted) || (isLocalStageOn RideStarted)) && ((getValueToLocalStore TRACKING_ID) == trackingId) then do
    respBooking <- rideBooking (state.props.bookingId)
    case respBooking of
      Right (RideBookingRes respBooking) -> do
        if (length respBooking.rideList) > 0 then do
          case (respBooking.rideList !! 0) of
            Just (RideAPIEntity res) -> do
              let rideStatus = res.status
              doAff do liftEffect $ push $ action rideStatus
              if (os /= "IOS" && res.driverArrivalTime /= Nothing  && (getValueToLocalStore DRIVER_ARRIVAL_ACTION) == "TRIGGER_DRIVER_ARRIVAL" ) then doAff do liftEffect $ push $ driverArrivedAction (fromMaybe "" res.driverArrivalTime) 
                else pure unit  
            Nothing -> pure unit
        else
          pure unit
      Left err -> pure unit
    response <- getDriverLocation state.data.driverInfoCardState.rideId
    case response of
      Right (GetDriverLocationResp resp) -> do
        let
          rideID = state.data.driverInfoCardState.rideId
          srcLat = (resp ^. _lat)
          srcLon = (resp ^. _lon)
          dstLat = if state.props.currentStage == RideAccepted then state.data.driverInfoCardState.sourceLat else state.data.driverInfoCardState.destinationLat
          dstLon = if state.props.currentStage == RideAccepted then state.data.driverInfoCardState.sourceLng else state.data.driverInfoCardState.destinationLng
          markers = if (isLocalStageOn RideAccepted) then (driverTracking "" ) else (rideTracking "")
        if (getValueToLocalStore TRACKING_ENABLED) == "False" then do
          _ <- pure $ setValueToLocalStore TRACKING_DRIVER "True"
          _ <- pure $ removeAllPolylines ""
          _ <- liftFlow $ drawRoute (walkCoordinate srcLat srcLon dstLat dstLon) "DOT" "#323643" false markers.srcMarker markers.destMarker 8 "DRIVER_LOCATION_UPDATE" "" ""
          void $ delay $ Milliseconds duration
          driverLocationTracking push action driverArrivedAction updateState duration trackingId state
          pure unit
        else if ((getValueToLocalStore TRACKING_DRIVER) == "False" || not (isJust state.data.route)) then do
          _ <- pure $ setValueToLocalStore TRACKING_DRIVER "True"
          routeResponse <- getRoute $ makeGetRouteReq srcLat srcLon dstLat dstLon
          case routeResponse of
            Right (GetRouteResp routeResp) -> do
              let route = ((routeResp) !! 0)
              case route of
                Just (Route routes) -> do
                  let mWalkCoordinatesPath = walkCoordinates routes.points routes.boundingBox
                  _ <- case mWalkCoordinatesPath of
                    Just walkCoordinatesPath -> do
                      _ <- pure $ removeAllPolylines ""
                      liftFlow $ drawRoute walkCoordinatesPath "LineString" "#323643" true markers.srcMarker markers.destMarker 8 "DRIVER_LOCATION_UPDATE" "" (metersToKm routes.distance state)
                    Nothing -> pure unit
                  _ <- doAff do liftEffect $ push $ updateState routes.duration routes.distance
                  void $ delay $ Milliseconds duration
                  driverLocationTracking push action driverArrivedAction updateState duration trackingId state { data { route = (routeResp !! 0), speed = routes.distance / routes.duration } }
                Nothing -> pure unit
            Left err -> pure unit
        else do
          case state.data.route of
            Just (Route routes) -> do
              let mWalkCoordinatesPath = walkCoordinates routes.points routes.boundingBox
              case mWalkCoordinatesPath of
                Just walkCoordinatesPath -> do
                  locationResp <- liftFlow $ isCoordOnPath walkCoordinatesPath (resp ^. _lat) (resp ^. _lon) (state.data.speed)
                  if locationResp.isInPath then do
                    let newRoute = (routes { points = Snapped (map (\item -> LatLong { lat: item.lat, lon: item.lng }) locationResp.points) })
                    _ <-
                      maybe (pure unit)
                        (\walkCoordinatesPath -> liftFlow $ updateRoute walkCoordinatesPath markers.destMarker (metersToKm locationResp.distance state))
                        (walkCoordinates newRoute.points newRoute.boundingBox)
                    _ <- doAff do liftEffect $ push $ updateState locationResp.eta locationResp.distance
                    void $ delay $ Milliseconds duration
                    driverLocationTracking push action driverArrivedAction updateState duration trackingId state
                  else do
                    driverLocationTracking push action driverArrivedAction updateState duration trackingId state { data { route = Nothing } }
                Nothing -> driverLocationTracking push action driverArrivedAction updateState duration trackingId state { data { route = Nothing } }
            Nothing -> pure unit
      Left err -> do
        void $ delay $ Milliseconds (duration * 2.0)
        driverLocationTracking push action driverArrivedAction updateState duration trackingId state { data { route = Nothing } }
  else do
    pure unit

trackDriverLocation :: forall action. (GetDriverLocationResp -> action) -> Number -> (action -> Effect Unit) -> HomeScreenState -> Flow GlobalState Unit
trackDriverLocation action duration push state = do
  _ <- pure $ spy "driver location" state
  if (spy "driver current Stage " (isLocalStageOn RideAccepted)) || (spy "driver current Stage " (isLocalStageOn RideStarted)) then do
    if state.props.forFirst == true then do
      response <- getDriverLocation state.data.driverInfoCardState.rideId
      case response of
        Right (GetDriverLocationResp resp) ->
          doAff do liftEffect $ push $ action (GetDriverLocationResp resp)
        Left error -> do
          let resp = (GetDriverLocationResp (LatLong { lat: state.data.driverInfoCardState.driverLat, lon: state.data.driverInfoCardState.driverLng }))
          doAff do liftEffect $ push $ action resp
    else do
      void $ delay $ Milliseconds duration
      response <- getDriverLocation state.data.driverInfoCardState.rideId
      case response of
        Right (GetDriverLocationResp resp) ->
          doAff do liftEffect $ push $ action (GetDriverLocationResp resp)
        Left error -> do
          let resp = (GetDriverLocationResp (LatLong { lat: state.data.driverInfoCardState.driverLat, lon: state.data.driverInfoCardState.driverLng }))
          doAff do liftEffect $ push $ action resp
  else
    pure unit

trackDriverLocationTemp :: forall action. (GetRouteResp -> RideBookingRes -> action) -> Number -> (action -> Effect Unit) -> HomeScreenState -> Flow GlobalState Unit
trackDriverLocationTemp actionRoute duration push state = do
  responseDriverLoc <- getDriverLocation state.data.driverInfoCardState.rideId
  case responseDriverLoc of
    Right (GetDriverLocationResp respDriverLoc) -> do
      let
        rideID = state.data.driverInfoCardState.rideId
        reqLat = if spy "LOCAL_STAGE AT DRIVER TRACK" (isLocalStageOn RideAccepted) then state.data.driverInfoCardState.sourceLat else state.data.driverInfoCardState.destinationLat
        reqLon = if spy "LOCAL_STAGE AT DRIVER TRACK" (isLocalStageOn RideAccepted) then state.data.driverInfoCardState.sourceLng else state.data.driverInfoCardState.destinationLng
      routeResponse <- getRoute $ makeGetRouteReq (respDriverLoc ^. _lat) (respDriverLoc ^. _lon) reqLat reqLon
      case routeResponse of
        Right (GetRouteResp routeResp) -> do
          let route = ((routeResp) !! 0)
          case route of
            Just (Route routes) -> do
              void $ delay $ Milliseconds duration
              --RIDEBOOKING
              respBooking <- rideBooking (state.props.bookingId)
              _ <- pure $ printLog "response to confirm ride:- " (state.props.searchId)
              case respBooking of
                Right responseBooking -> do
                  _ <- pure $ printLog "api Results " responseBooking
                  let (RideBookingRes responseStatusBooking) = responseBooking
                  doAff do liftEffect $ push $ actionRoute (GetRouteResp routeResp) responseBooking
                Left err -> trackDriverLocationTemp actionRoute duration push state
            Nothing -> trackDriverLocationTemp actionRoute duration push state
        Left routeError -> trackDriverLocationTemp actionRoute duration push state
    Left error ->
      trackDriverLocationTemp actionRoute duration push state

confirmRide :: forall action. (RideBookingRes -> action) -> Int -> Number -> (action -> Effect Unit) -> HomeScreenState -> Flow GlobalState Unit
confirmRide action count duration push state = do
  if (count /= 0) then do
    resp <- rideBooking (state.props.bookingId)
    _ <- pure $ printLog "response to confirm ride:- " (state.props.searchId)
    case resp of
      Right response -> do
        _ <- pure $ printLog "api Results " response
        let (RideBookingRes resp) = response
        case resp.status of
          "TRIP_ASSIGNED" -> doAff do liftEffect $ push $ action response
          _ -> do
            void $ delay $ Milliseconds duration
            confirmRide action (count - 1) duration push state
      Left err -> do
        _ <- pure $ printLog "api error " err
        void $ delay $ Milliseconds duration
        confirmRide action (count - 1) duration push state
  else
    pure unit

updateTripStatus :: forall action. (RideBookingRes -> action) -> Number -> (action -> Effect Unit) -> HomeScreenState -> Flow GlobalState Unit
updateTripStatus action duration push state = do
  void $ delay $ Milliseconds duration
  resp <- rideBooking (state.props.bookingId)
  _ <- pure $ printLog "response to confirm ride:- " (state.props.searchId)
  case resp of
    Right response -> do
      _ <- pure $ printLog "api Results " response
      let (RideBookingRes resp) = response
      doAff do liftEffect $ push $ action response
    Left err -> updateTripStatus action duration push state

cancelRidePopUpView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
cancelRidePopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ][ CancelRidePopUp.view (push <<< CancelRidePopUpAction) (cancelRidePopUpConfig state)]

metersToKm :: Int -> HomeScreenState -> String
metersToKm distance state =
  if (distance == 0) then
    (if (state.props.currentStage == RideAccepted) then (getString AT_PICKUP) else (getString AT_DROP))
  else if (distance < 1000) then (toString distance <> " m " <> (getString AWAY_C)) else (parseFloat ((toNumber distance) / 1000.0)) 2 <> " km " <> (getString AWAY_C)
      

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

