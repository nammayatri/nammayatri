{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SearchLocationScreen.View where

import Prelude ((<<<), (==), Unit, ($), (<>), (&&), (-), (/), (>), (/=), bind, show, pure, const, unit, not, void, discard, map)
import Screens.SearchLocationScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.SearchLocationScreen.ComponentConfig (locationTagBarConfig, separatorConfig, primaryButtonConfig, mapInputViewConfig, menuButtonConfig, confirmLocBtnConfig)
import Components.InputView as InputView
import Effect (Effect)
import Screens.Types (SearchLocationScreenState, SearchLocationStage(..), SearchLocationTextField(..), SearchLocationActionType(..), LocationListItemState, GlobalProps,Station(..))
import Styles.Colors as Color
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM (Screen, PrestoDOM, Orientation(..), Length(..), Visibility(..), Padding(..), Gravity(..), Margin(..), AlignItems(..), linearLayout, relativeLayout, afterRender, height, width, orientation, background, id, visibility, editText, weight, text, color, fontSize, padding, hint, inputTypeI, gravity, pattern, hintColor, onChange, cornerRadius, margin, cursorColor, onFocus, imageWithFallback, imageView, scrollView, scrollBarY, textView, text, stroke, clickable, alignParentBottom, alignItems, ellipsize, layoutGravity, onClick, selectAllOnFocus, lottieAnimationView, disableClickFeedback, alpha, maxLines, singleLine, textSize)
import JBridge (showMap, debounceFunction, startLottieProcess, lottieAnimationConfig, storeCallBackLocateOnMap)
import Engineering.Helpers.Commons (getNewIDWithTag, flowRunner, os)
import Components.SeparatorView.View as SeparatorView
import Components.LocationListItem as LocationListItem
import Components.FavouriteLocationModel as FavouriteLocationModel
import Components.SaveFavouriteCard as SaveFavouriteCard
import Components.MenuButton as MenuButton
import Mobility.Prelude (boolToVisibility)
import Font.Size as FontSize
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import Helpers.Utils (fetchImage, FetchImageFrom(..), getAssetsBaseUrl)
import Components.LocationTagBarV2 as LocationTagBar
import Components.PrimaryButton as PrimaryButton
import PrestoDOM.Types.DomAttributes (Corners(..))
import Data.Array(mapWithIndex, length, null, any) as DA
import Control.Monad.Except.Trans (runExceptT , lift)
import Control.Transformers.Back.Trans (runBackT)
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState)
import Debug(spy)
import Control.Monad.Free (runFree)
import Helpers.Utils (fetchAndUpdateCurrentLocation)
import Data.Maybe (isNothing, maybe, Maybe(..), isJust ) as MB
import Resources.Constants (getDelayForAutoComplete)
import Engineering.Helpers.Commons (os, screenHeight, screenWidth) as EHC
import Data.String (length, null, take) as DS
import Language.Strings (getString)
import Language.Types (STR(..))
import Components.LocationListItem.Controller 

searchLocationScreen :: SearchLocationScreenState -> GlobalProps -> Screen Action SearchLocationScreenState ScreenOutput
searchLocationScreen initialState globalProps = 
  { initialState
  , view : view globalProps
  , name : "SearchLocationScreen"
  , globalEvents : [globalEventsFunc]
  , eval : 
      \action state -> do
        let _ = spy "SearchLocationScreen Action" action
            _ = spy "SearchLocationScreen State" state
        eval action state
  }
  where 
    globalEventsFunc push = do
      case initialState.props.searchLocStage of 
        LocateOnMapStage -> storeCallBackLocateOnMap push LocFromMap 
        ConfirmLocationStage -> do 
          void $ pure $ spy "inside confirmLocation Stage" "sdfdsf"
          storeCallBackLocateOnMap push LocFromMap
        _ -> pure unit 
      pure $ pure unit 

view :: forall w. GlobalProps -> (Action -> Effect Unit) -> SearchLocationScreenState ->  PrestoDOM (Effect Unit) w 
view globalProps push state = 
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , afterRender 
        (\action -> mapRenderAction action)
        $ const AfterRender
    ][  mapView push state
      , markerView push state
      , searchLocationView push state globalProps
      , locateOnMapFooterView push state 
      , popUpViews push state globalProps
      , locateOnMapView push state globalProps
      , confirmLocationView push state
    ]

  where
    mapRenderAction :: Action -> Effect Unit
    mapRenderAction action = do
      _ <- push action
      _ <- showMap (getNewIDWithTag "SearchLocationScreenMap") true "satellite" 17.0 push MapReady
      _ <- fetchAndUpdateCurrentLocation push (UpdateLocAndLatLong globalProps.recentSearches) RecenterCurrentLocation
      pure unit

    mapView :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState ->  PrestoDOM (Effect Unit) w
    mapView push state = 
      linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , id (getNewIDWithTag "SearchLocationScreenMap")
        ][]

    markerView :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState ->  PrestoDOM (Effect Unit) w
    markerView push state = let 
      imageName = MB.maybe "" ( \ currField -> if currField == SearchLocPickup then "ny_ic_src_marker" else "ny_ic_blue_marker") state.props.focussedTextField 
      labelText = if DS.length state.data.defaultGate > state.appConfig.mapConfig.labelTextSize then
                                  (DS.take (state.appConfig.mapConfig.labelTextSize - 3) state.data.defaultGate) <> "..."
                               else
                                  state.data.defaultGate
      labelVisibility = boolToVisibility $ not $ DS.null labelText
      in
      linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , gravity CENTER
        , background Color.transparent
        , padding $ PaddingBottom if os == "IOS" then 53 else 70
        , orientation VERTICAL
        ][  textView $ 
              [ text labelText
              , background Color.black800
              , color Color.white900
              , cornerRadius 5.0
              , padding (Padding 5 5 5 5)
              , margin (MarginBottom 5)
              , visibility labelVisibility
              , id $ getNewIDWithTag "LocateOnMapSLSPin"
              ] <> FontStyle.body3 TypoGraphy
          , imageView 
            [ width $ V 35
            , height $ V 35
            -- , accessibility DISABLE
            , imageWithFallback $ fetchImage FF_COMMON_ASSET $ imageName
            , visibility $ boolToVisibility $ DA.any (_ == state.props.searchLocStage) [LocateOnMapStage, ConfirmLocationStage]
            ]
          ]

confirmLocationView :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState ->  PrestoDOM (Effect Unit) w
confirmLocationView push state = let 
  zonePadding = 0 --if os == "IOS" then 0 else (ceil (toNumber (screenWidth unit))/8)
  viewVisibility = boolToVisibility $ currentStageOn state ConfirmLocationStage
  headerText = MB.maybe "" (\ currField -> if currField == SearchLocPickup then "Confirm Pickup Location" else "Confirm Stop Location") state.props.focussedTextField 
  in 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , visibility viewVisibility
    , orientation VERTICAL
    , background Color.transparent
    , gravity BOTTOM 
    ][  recenterButtonView push state
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
            , visibility $ boolToVisibility $ state.data.confirmLocCategory /= ""
            ] [ imageView
                [ width (V 20)
                , height (V 20)
                , margin (MarginRight 6)
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_zone_walk"
                ]
              , textView
                [ width if os == "IOS" && state.data.confirmLocCategory == "SureBlockedAreaForAutos" then (V 230) else WRAP_CONTENT
                , height WRAP_CONTENT
                , gravity CENTER
                , textSize FontSize.a_14
                , text if state.data.confirmLocCategory == "SureBlockedAreaForAutos" then
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
            -- , accessibility DISABLE
            ] [ textView $
                [ text headerText
                , color Color.black800
                -- , accessibility DISABLE
                , gravity CENTER_HORIZONTAL
                , height WRAP_CONTENT
                , width MATCH_PARENT
                ] <> FontStyle.h1 TypoGraphy
              , currentLocationView push state
              , nearByPickUpPointsView push state
              , PrimaryButton.view (push <<< PrimaryButtonAC) (confirmLocBtnConfig state)
             ]
        ]
      ]

currentLocationView :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState ->  PrestoDOM (Effect Unit) w
currentLocationView push state = let 
  viewVisibility = boolToVisibility $ currentStageOn state ConfirmLocationStage && (DS.null state.data.defaultGate)
  locToBeConfirmed = state.data.mapLoc.address
  in 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , margin $ MarginVertical 20 10
    -- , onClick push $ const GoBackToSearchLocationModal
    , padding $ PaddingHorizontal 15 15
    , stroke $ "1," <> state.appConfig.confirmPickUpLocationBorder
    , gravity CENTER_VERTICAL
    -- , accessibility DISABLE
    , cornerRadius 5.0
    , visibility viewVisibility
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_source_dot"
        , height $ V 16
        , width $ V 16
        , gravity CENTER_VERTICAL
        -- , accessibility DISABLE
        ]
    , textView
        $
          [ text locToBeConfirmed
          , ellipsize true
          , singleLine true
          -- , accessibility ENABLE
          -- , accessibilityHint $ "Pickup Location is " <>  (DS.replaceAll (DS.Pattern ",") (DS.Replacement " ") state.data.source)
          , gravity LEFT
          , width MATCH_PARENT
          , padding (Padding 10 16 10 16)
          , color Color.black800
          ]
        <> FontStyle.subHeading1 TypoGraphy
    ]

nearByPickUpPointsView :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState ->  PrestoDOM (Effect Unit) w
nearByPickUpPointsView push state =
  scrollView
  [ height $ V 130
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ Padding 5 20 0 5
  , visibility $ boolToVisibility (state.data.defaultGate /= "")
  , id $ getNewIDWithTag "scrollViewParentSLS"
  ][linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , id $ getNewIDWithTag "scrollViewChildSLS"
    , afterRender push (const AfterRender)
    ](DA.mapWithIndex (\index item ->
                    linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , margin $ MarginBottom 12
                      ][MenuButton.view (push <<< MenuButtonAC) (menuButtonConfig state item)]) state.data.nearByGates)
  ]


locateOnMapView :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState -> GlobalProps -> PrestoDOM (Effect Unit) w
locateOnMapView push state globalProps = let 
  viewVisibility = boolToVisibility $ currentStageOn state LocateOnMapStage
  in 
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , visibility viewVisibility
    ][  inputView push state false globalProps
      , primaryButtonView state push 
      ]
  where 
    primaryButtonView :: forall w. SearchLocationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
    primaryButtonView state push =
      linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , alignParentBottom "true,-1"
        , background Color.transparent
        , padding $ PaddingBottom 16
        ][ recenterButtonView push state
          , PrimaryButton.view (push <<< PrimaryButtonAC)(primaryButtonConfig state)]

inputView :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState -> Boolean -> GlobalProps -> PrestoDOM (Effect Unit) w
inputView push state isEditable globalProps = 
  InputView.view (push <<< InputViewAC globalProps ) $ mapInputViewConfig state isEditable

recenterButtonView :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState ->  PrestoDOM (Effect Unit) w
recenterButtonView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , background Color.transparent
  , gravity RIGHT
  , padding $ Padding 0 0 16 14
  , disableClickFeedback true
  ][
      imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_recenter_btn"
        , onClick push $ const $ RecenterCurrentLocation
        , height $ V 40
        , width $ V 40
        ]
  ]


popUpViews :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState -> GlobalProps -> PrestoDOM (Effect Unit) w
popUpViews push state globalProps = 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ] [if currentStageOn state AllFavouritesStage 
            then favouriteLocationModel state globalProps push
            else if (state.props.showSaveFavCard)
            then saveFavCardView push state globalProps
            else 
              linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , visibility GONE
              , orientation VERTICAL
              ][]]

locateOnMapFooterView :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState ->  PrestoDOM (Effect Unit) w
locateOnMapFooterView push state = let 
  viewVisibility = boolToVisibility $ currentStageOn state PredictionsStage
  locateOnMapData = getDataBasedActionType state.props.actionType 
  in linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , alignParentBottom "true,-1"
    , orientation VERTICAL
    , visibility viewVisibility
    , background Color.white900
    ][  verticalSeparatorView 2
      , linearLayout
        [ height WRAP_CONTENT
        , width $ MATCH_PARENT
        , padding $ PaddingVertical 16 16
        , gravity CENTER
        , alpha $ if MB.isNothing state.props.focussedTextField then 0.5 else 1.0
        , clickable $ MB.isJust state.props.focussedTextField
        ][  imageView 
            [ height $ V 20
            , width $ V 20
            , margin $ MarginRight 8
            , layoutGravity "center"
            , imageWithFallback $ fetchImage FF_ASSET locateOnMapData.imageUrl
            ]  
          , textView
            [ text locateOnMapData.text
            , layoutGravity "center"
            , height WRAP_CONTENT
            , gravity CENTER
            , onClick push $ const $ locateOnMapData.action
            ]
          -- , horizontalSeparatorView 2
          -- , textView
          --   [ text "Current Location"
          --   , weight 1.0
          --   , height WRAP_CONTENT
          --   , gravity CENTER
          --   ]
          ]
      ]
  where 
  
    -- horizontalSeparatorView :: Int -> PrestoDOM (Effect Unit) w
    -- horizontalSeparatorView wdth = 
    --   linearLayout
    --     [ height MATCH_PARENT
    --     , width $ V wdth
    --     , background Color.grey900
    --     ][]
    
    verticalSeparatorView :: Int -> PrestoDOM (Effect Unit) w
    verticalSeparatorView hght = 
      linearLayout
        [ width MATCH_PARENT
        , height $ V hght
        , background Color.grey900
        ][]

    getDataBasedActionType :: SearchLocationActionType -> {imageUrl :: String, text :: String, action :: Action}
    getDataBasedActionType actionType = 
      case actionType of 
        SearchLocationAction -> {imageUrl : "ny_ic_locate_on_map", text : "Set Location On Map", action : SetLocationOnMap}
        AddingStopAction -> {imageUrl : "ny_ic_locate_on_map", text : "Set Location On Map", action : SetLocationOnMap}
        MetroStationSelectionAction -> {imageUrl : "ny_ic_metro", text : "See Metro Map", action : SetLocationOnMap}


searchLocationView :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState -> GlobalProps ->  PrestoDOM (Effect Unit) w
searchLocationView push state globalProps = let
  viewVisibility = boolToVisibility $ currentStageOn state PredictionsStage 
  in 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility viewVisibility
    , background Color.white900
    ] $ [ inputView push state true globalProps
        , searchLottieLoader push state ] 
    <> if state.props.showLoader then []
        else 
        [ locationTagsView state push globalProps
        , findPlacesIllustration push state 
        , predictionsView push state globalProps]


  -- where 
    -- fetchDropImage :: SearchLocationScreenState ->  String 
    -- fetchDropImage state = 
    --   case state.props.actionType of 
    --     AddingStopAction -> "ny_ic_blue_circle"
    --     SearchLocationAction -> "ny_ic_red_circle"

locationTagsView :: forall w. SearchLocationScreenState -> (Action -> Effect Unit) -> GlobalProps -> PrestoDOM (Effect Unit) w
locationTagsView state push globalProps = 
  linearLayout
    [ height WRAP_CONTENT
    , margin $ MarginTop 16
    , width MATCH_PARENT
    , visibility $ boolToVisibility state.props.canSelectFromFav
    , gravity CENTER
    ][  LocationTagBar.view (push <<< LocationTagBarAC globalProps.savedLocations) (locationTagBarConfig state globalProps )]
    
predictionsView :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState -> GlobalProps ->  PrestoDOM (Effect Unit) w
predictionsView push state globalProps = let 
  viewVisibility = boolToVisibility $ 
    case state.props.actionType of 
      MetroStationSelectionAction -> not $ DA.null state.data.updatedMetroStations
      _ ->  not $ DA.null state.data.locationList
  headerText = spy "Header Text ::" $ if state.props.isAutoComplete then "Search Results" 
    else if state.props.actionType == MetroStationSelectionAction then "Metro Stations"
    else 
      MB.maybe "" (\ currField -> if currField == SearchLocPickup then "Past Searches" else "Suggested Destination") state.props.focussedTextField
  in
  scrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding (PaddingBottom 60)
    , background Color.white900
    , scrollBarY false
    , visibility viewVisibility
    ][  linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding $ PaddingHorizontal 16 16
        ][  textView $
              [ text headerText
              , color Color.black700
              , margin $ MarginVertical 14 8
              ] <> FontStyle.body3 TypoGraphy
          , predictionArrayView $ metroStationsArray state.data.updatedMetroStations --if state.props.actionType == MetroStationSelectionAction then metroStationsArray state.data.updatedMetroStations else state.data.locationList
          , footerView
        ]
      ]
  where
    footerView :: PrestoDOM (Effect Unit) w
    footerView = linearLayout
                  [ height $ V 80
                  , width MATCH_PARENT][]

    predictionArrayView :: Array LocationListItemState -> PrestoDOM (Effect Unit) w
    predictionArrayView locList =
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , cornerRadius state.appConfig.primaryButtonCornerRadius
        , stroke $ "1," <> Color.grey900
        , orientation VERTICAL
        ](  DA.mapWithIndex 
              (\index item -> locationListItemView item index ) 
              if (DA.null locList && state.props.actionType /= MetroStationSelectionAction) then globalProps.cachedSearches else locList )

    metroStationsArray:: Array Station -> Array LocationListItemState
    metroStationsArray metroStations = 
      map (\item -> { prefixImageUrl : fetchImage FF_ASSET "ny_ic_loc_grey"
              , postfixImageUrl : ""
              , postfixImageVisibility : false
              , title : item.stationName
              , subTitle : ""
              , placeId : MB.Nothing
              , lat : MB.Nothing
              , lon : MB.Nothing
              , description : ""
              , tag : ""
              , tagType : MB.Nothing
              , cardType : MB.Nothing
              , address : ""
              , tagName : ""
              , isEditEnabled : true
              , savedLocation : ""
              , placeName : ""
              , isClickable : true
              , alpha : 1.0
              , fullAddress : dummyAddress
              , locationItemType : MB.Nothing
              , distance : MB.Nothing
              , showDistance : MB.Just false
              , actualDistance : MB.Nothing
              , frequencyCount : MB.Nothing
              , recencyDate : MB.Nothing
              , locationScore : MB.Nothing
              }) metroStations

    locationListItemView :: LocationListItemState -> Int -> PrestoDOM (Effect Unit) w
    locationListItemView item index = let 
      enableErrorFeedback = 
        ( state.props.focussedTextField == MB.Just SearchLocPickup && 
          state.props.actionType == SearchLocationAction && 
          currentStageOn state PredictionsStage)
      in 
        linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ][  LocationListItem.view (push <<< LocationListItemAC globalProps.savedLocations ) item enableErrorFeedback 
          , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.lightGreyShade
            , visibility $ boolToVisibility $ not $ index == DA.length state.data.locationList - 1
            ][]
          ]
              
searchLottieLoader :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState -> PrestoDOM (Effect Unit) w
searchLottieLoader push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_HORIZONTAL
  ][  lottieAnimationView
      [ height $ if EHC.os == "IOS" then V 170 else V 130
      , width $ V 130
      , padding $ PaddingBottom 80
      , margin (MarginTop ((EHC.screenHeight unit)/ 7 - (if EHC.os == "IOS" then 140 else 90)))
      , gravity CENTER
      , id (getNewIDWithTag "searchLoader")
      , visibility $ boolToVisibility state.props.showLoader
      , afterRender (\action -> do
        void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = (getAssetsBaseUrl FunctionCall) <> "lottie/search_loader.json", lottieId = (getNewIDWithTag "searchLoader"), scaleType="CENTER_CROP", repeat = true, speed = 0.8 }
        push action
        ) (const NoAction)
      ]
    ]

currentStageOn :: SearchLocationScreenState ->  SearchLocationStage -> Boolean
currentStageOn state stage  = 
  stage == state.props.searchLocStage 


favouriteLocationModel :: forall w. SearchLocationScreenState -> GlobalProps ->  (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
favouriteLocationModel state globalProps push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    ]
    [ FavouriteLocationModel.view (push <<< FavouriteLocationModelAC) (globalProps.savedLocations) ]

saveFavCardView :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState -> GlobalProps -> PrestoDOM (Effect Unit) w
saveFavCardView push state globalProps =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    -- , accessibility if state.props.isSaveFavourite then DISABLE else DISABLE_DESCENDANT
    ]
    [ SaveFavouriteCard.view (push <<< SaveFavCardAC globalProps.savedLocations) (state.data.saveFavouriteCard) ]

findPlacesIllustration :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState -> PrestoDOM (Effect Unit) w
findPlacesIllustration push state = let 
  viewVisibility = boolToVisibility $ 
    case state.props.actionType of 
        MetroStationSelectionAction -> DA.null state.data.updatedMetroStations
        _ -> DA.null state.data.locationList
  in
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility viewVisibility
    , gravity CENTER_HORIZONTAL
    , margin $ Margin 7 ((EHC.screenHeight unit)/7) 16 0
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_empty_suggestions"
        , height $ V 99
        , width $ V 133
        , margin $ MarginBottom 12
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginBottom 5
        ]
        [ textView $
            [ text $ getString (WELCOME_TEXT "WELCOME_TEXT") <> "!"
            , color Color.black700
            , gravity CENTER
            ] <> FontStyle.body4 LanguageStyle
        ]
    , linearLayout
        [ width $ V (EHC.screenWidth unit - 40)
        , height WRAP_CONTENT
        , gravity CENTER
        ]
        [ textView $
            [ text $ getString START_TYPING_TO_SEARCH_PLACES
            , gravity CENTER
            , color Color.black700
            ] <> FontStyle.body3 LanguageStyle
        ]
    ]