module Screens.SearchLocationScreen.View where

import Prelude ((<<<), (==), Unit, ($), (<>), (&&), (-), (/), (>), bind, show, pure, const, unit, not, void, discard)
import Screens.SearchLocationScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.SearchLocationScreen.ComponentConfig (locationTagBarConfig, separatorConfig)
import Effect (Effect)
import Screens.Types (SearchLocationScreenState, SearchLocationStage(..), SearchLocationTextField(..), SearchLocationActionType(..), LocationListItemState, GlobalProps)
import Styles.Colors as Color
import PrestoDOM (Screen, PrestoDOM, Orientation(..), Length(..), Visibility(..), Padding(..), Gravity(..), Margin(..), AlignItems(..), linearLayout, relativeLayout, afterRender, height, width, orientation, background, id, visibility, editText, weight, text, color, fontSize, padding, hint, inputTypeI, gravity, pattern, hintColor, onChange, cornerRadius, margin, cursorColor, onFocus, imageWithFallback, imageView, scrollView, scrollBarY, textView, text, stroke, clickable, alignParentBottom, alignItems, ellipsize, layoutGravity, onClick, selectAllOnFocus, lottieAnimationView)
import JBridge (showMap, debounceFunction, startLottieProcess, lottieAnimationConfig)
import Engineering.Helpers.Commons (getNewIDWithTag, flowRunner)
import Components.SeparatorView.View as SeparatorView
import Components.LocationListItem as LocationListItem
import Components.FavouriteLocationModel as FavouriteLocationModel
import Components.SaveFavouriteCard as SaveFavouriteCard
import Mobility.Prelude (boolToVisibility)
import Font.Size as FontSize
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import Helpers.Utils (fetchImage, FetchImageFrom(..), getAssetsBaseUrl)
import Components.LocationTagBarV2 as LocationTagBar
import PrestoDOM.Types.DomAttributes (Corners(..))
import Data.Array(mapWithIndex, length, null) as DA
import Control.Monad.Except.Trans (runExceptT , lift)
import Control.Transformers.Back.Trans (runBackT)
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState)
import Debug(spy)
import Control.Monad.Free (runFree)
import Helpers.Utils (fetchAndUpdateCurrentLocation)
import Data.Maybe (isNothing, maybe, Maybe(..) ) as MB
import Resources.Constants (getDelayForAutoComplete)
import Engineering.Helpers.Commons (os, screenHeight) as EHC
import Data.String (length) as DS

searchLocationScreen :: SearchLocationScreenState -> GlobalProps -> Screen Action SearchLocationScreenState ScreenOutput
searchLocationScreen initialState globalProps = 
  { initialState
  , view : view globalProps
  , name : "SearchLocationScreen"
  , globalEvents : []
  -- [( \push -> do 
  --   if MB.isNothing initialState.data.currentLoc then do
  --     let _ = spy "Inside Global Event" initialState
  --     pure $ fetchAndUpdateCurrentLocation push UpdateLocAndLatLong RecenterCurrentLocation
  --     else pure $ pure unit)]
  , eval : 
      \action state -> do
        let _ = spy "SearchLocationScreen Action" action
            _ = spy "SearchLocationScreen State" state
        eval action state
  }
  -- where 
  -- updateCurrentLocEvent push = do 
  --   void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $
  --     fetchAndUpdateCurrentLocation push UpdateLocAndLatLong RecenterCurrentLocation
  --   pure $ pure unit

view :: forall w. GlobalProps -> (Action -> Effect Unit) -> SearchLocationScreenState ->  PrestoDOM (Effect Unit) w 
view globalProps push state = 
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , afterRender 
        (\action -> mapRenderAction action)
        $ const AfterRender
    ][  mapView push state
      , searchLocationView push state globalProps
      , locateOnMapFooterView push state 
      , popUpViews push state globalProps
      -- , confirmLocationView push state
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
locateOnMapFooterView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , alignParentBottom "true,-1"
    , orientation VERTICAL
    , background Color.white900
    ][  verticalSeparatorView 2
      , linearLayout
        [ height WRAP_CONTENT
        , width $ MATCH_PARENT
        , padding $ PaddingVertical 16 16
        , gravity CENTER
        ][  imageView 
            [ height $ V 20
            , width $ V 20
            , margin $ MarginRight 8
            , layoutGravity "center"
            , imageWithFallback "ny_ic_locate_on_map,https://assets.juspay.in/nammayatri/images/user/ny_ic_locate_on_map.png"
            ]  
          , textView
            [ text "Set Location On Map"
            , layoutGravity "center"
            , height WRAP_CONTENT
            , gravity CENTER
            , onClick push $ const $ SetLocationOnMap
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
    ] $ [ inputView push state
        , searchLottieLoader push state ] 
    <> if state.props.showLoader then []
        else 
        [ locationTagsView state push globalProps
        , predictionsView push state globalProps]

inputView :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState ->  PrestoDOM (Effect Unit) w
inputView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingHorizontal 16 16 
    , background Color.black900
    ][  backPressView state
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        , padding $ PaddingVertical 16 16
        ][  inputImageView push state
          , textFieldsView state]
    ]

  where 

    backPressView :: forall w.  SearchLocationScreenState ->  PrestoDOM (Effect Unit) w
    backPressView state = 
      linearLayout
        [ height MATCH_PARENT
        , width WRAP_CONTENT
        , padding $ Padding 0 16 0 0
        ][  imageView
            [ height $ V 24
            , width $ V 24
            , margin $ MarginRight 8
            , imageWithFallback state.appConfig.searchLocationConfig.backArrow
            ]
          , textView $
            [ text $ headerText state
            , color Color.white900  
            ] <> (FontStyle.subHeading2 LanguageStyle)
        ]
    
    headerText :: SearchLocationScreenState -> String
    headerText state = 
      MB.maybe ("Trip Details") ( \ currTextField -> if currTextField == SearchLocPickup then "Edit Pickup" else "Add Stop") state.props.focussedTextField
    
    textFieldsView :: forall w. SearchLocationScreenState ->  PrestoDOM (Effect Unit) w
    textFieldsView state =  
      linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        , padding $ PaddingLeft 8
        , orientation VERTICAL
        ][  inputTextField push state SearchLocPickup InputChanged $ MarginTop 0
          , inputTextField push state SearchLocDrop InputChanged $ MarginTop 12 
        ]

inputImageView :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState ->  PrestoDOM (Effect Unit) w
inputImageView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width $ V 20
    , orientation VERTICAL
    , margin $ MarginLeft 24
    , gravity CENTER
    ][ linearLayout
        [ height $ V 15
        , width $  V 15
        , gravity CENTER
        ][  imageView
            [ height $ V 15
            , width $ V 15
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_green_circle"
            ]
          ]
    , SeparatorView.view separatorConfig
    , linearLayout
        [ height $ V 15
        , width $  V 15
        , gravity CENTER
        ][  imageView
            [ height $ V 15
            , width $ V 15
            , imageWithFallback $ fetchImage FF_COMMON_ASSET $ fetchDropImage state 
            ]
        ]
    ]

  where 
    fetchDropImage :: SearchLocationScreenState ->  String 
    fetchDropImage state = 
      case state.props.actionType of 
        AddingStopAction -> "ny_ic_blue_circle"
        SearchLocationAction -> "ny_ic_red_circle"


nonEditableTextView :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState ->  PrestoDOM (Effect Unit) w
nonEditableTextView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , padding $ Padding 8 7 8 7
    , background "#303440"
    , cornerRadius 4.0
    ][  textView $ 
          [ text $ "state.data.pickUpLocation"
          , color Color.black600
          , width MATCH_PARENT
          , height WRAP_CONTENT
          , ellipsize true
          ] <> (FontStyle.body6 LanguageStyle)
     ]

inputTextField :: forall w. (Action -> Effect Unit) -> SearchLocationScreenState ->  SearchLocationTextField -> (String -> Action) -> Margin -> PrestoDOM (Effect Unit) w
inputTextField push state idTag action marginValue = 
  let isFocussed = MB.maybe false (\currTextField -> currTextField == idTag) state.props.focussedTextField
      bottomStrokeVisibility =  boolToVisibility isFocussed
      placeHolderText = case idTag of 
                          SearchLocPickup -> "Pickup Location"
                          SearchLocDrop -> "Next Stop"
      textValue = case idTag of 
                    SearchLocPickup -> MB.maybe "" (\srcLoc -> srcLoc.address) state.data.srcLoc
                    SearchLocDrop -> MB.maybe "" (\destLoc -> destLoc.address) state.data.destLoc

      canClearText = (state.props.canClearText) && isFocussed
  in  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin marginValue
        ][  linearLayout[
              height WRAP_CONTENT
            , width MATCH_PARENT
            , background "#303440"
            , cornerRadius 4.0 
            ][  editText $ 
                [ height $ V 32
                , weight 1.0
                , padding $ Padding 8 7 8 7
                , color Color.black600
                , gravity LEFT
                , clickable false
                , background "#303440"
                , cornerRadius 4.0
                , text textValue
                , selectAllOnFocus true
                , hint placeHolderText
                , hintColor $ Color.blueGrey
                , pattern "[^\n]*,255"
                , id $ getNewIDWithTag $ show idTag
                , onChange (\action -> do 
                    void $ debounceFunction getDelayForAutoComplete push AutoCompleteCallBack $ MB.maybe false (\currTextField -> currTextField == SearchLocPickup) state.props.focussedTextField
                    void $ push action
                  ) action
                , onFocus push $ const $ TextFieldFocusChanged idTag
                , cursorColor Color.yellow900
                ] <> FontStyle.body6 LanguageStyle
              , crossButtonView state canClearText
              ]
          , bottomStrokeView bottomStrokeVisibility 
          ]
  where 
    bottomStrokeView :: Visibility -> PrestoDOM (Effect Unit) w
    bottomStrokeView bottomStrokeVisibility =
      linearLayout
      [ height $ V 1
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , visibility $ bottomStrokeVisibility
      , background Color.white900
      ][] 
    
    crossButtonView :: forall w. SearchLocationScreenState -> Boolean -> PrestoDOM (Effect Unit) w
    crossButtonView state canClearText = 
      linearLayout
        [ height $ V 32 
        , width $ V 32
        , gravity CENTER 
        , padding $ PaddingVertical 10 2 
        , visibility $ boolToVisibility canClearText
        ][  imageView
            [ height $ V 19 
            , width $ V 19 
            , imageWithFallback $ fetchImage FF_ASSET state.appConfig.searchLocationConfig.clearTextImage
            ]
        ]

        

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
predictionsView push state globalProps = 
  scrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding (PaddingBottom 60)
    , background Color.white900
    , scrollBarY false
    ][  linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding $ PaddingHorizontal 16 16
        ][  textView $
              [ text "searchResultText"
              , color Color.black700
              , margin $ MarginVertical 14 8
              ] <> FontStyle.body3 TypoGraphy
          , predictionArrayView state.data.locationList
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
              if DA.null locList then globalProps.recentSearches else locList )

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
