module Components.SearchLocationModel.View where

import Common.Types.App

import Animation (translateYAnimFromTop)
import Animation.Config (translateFullYAnimWithDurationConfig, translateYAnimHomeConfig, Direction(..))
import Components.LocationListItem as LocationListItem
import Components.LocationTagBar as LocationTagBar
import Components.PrimaryButton as PrimaryButton
import Components.SearchLocationModel.Controller (Action(..), SearchLocationModelState)
import Data.Array (mapWithIndex, length)
import Data.Function (flip)
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag, os, safeMarginBottom, safeMarginTop, screenHeight, screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getLocationName, debounceFunction, isPreviousVersion, getPreviousVersion)
import JBridge (getBtnLoader, requestKeyboardShow, getCurrentPosition, firebaseLogEvent)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (+), (-), (/), (/=), (<<<), (<>), (==), (||), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), adjustViewWithKeyboard, afterRender, alignParentBottom, alpha, autoCorrectionType, background, color, cornerRadius, disableClickFeedback, editText, ellipsize, fontStyle, frameLayout, gravity, height, hint, hintColor, id, imageUrl, imageView, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, onFocus, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, inputTypeI, clickable, imageWithFallback)
import PrestoDOM.Animation as PrestoAnim
import Resources.Constants (getDelayForAutoComplete)
import Screens.Types (SearchLocationModelType(..), LocationListItemState)
import Storage (KeyStore(..), getValueToLocalStoreEff, getValueToLocalStore)
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> SearchLocationModelState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background case state.isSearchLocation of
                    LocateOnMap -> Color.transparent
                    SearchLocation -> if (state.isRideServiceable) then Color.grey800 else Color.white900
                    _           -> Color.white900 --"#FFFFFF"
      , margin $ MarginBottom (if state.isSearchLocation == LocateOnMap then bottomSpacing else 0)
      , onBackPressed push (const $ GoBack)
      ]([PrestoAnim.animationSet [translateYAnimFromTop $ translateFullYAnimWithDurationConfig 400 ] $ 
          linearLayout
          [ height $ V ((screenHeight unit)/ 7)
          , width MATCH_PARENT
          , background Color.black900
          , clickable case state.isSearchLocation of 
              LocateOnMap -> false 
              _ -> true 
          , onClick push (const NoAction)
          , padding (Padding 0 safeMarginTop 0 0)
          ][]
      , PrestoAnim.animationSet 
          [ translateYAnimFromTop $ translateFullYAnimWithDurationConfig 450 ] $ 
            linearLayout
            [ height MATCH_PARENT
            , clickable case state.isSearchLocation of 
                LocateOnMap -> false 
                _ -> true 
            , onClick push (const NoAction)
            , width MATCH_PARENT
            , margin (MarginTop ((screenHeight unit) / 7))
            , padding (Padding 0 safeMarginTop 0 0)
            ]
            []
    , PrestoAnim.animationSet
        (if os == "IOS" then [] else [ translateYAnimFromTop $ translateFullYAnimWithDurationConfig 500 ])
        $ linearLayout
         -- Temporary fix for iOS. 
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , padding (Padding 0 safeMarginTop 0 safeMarginBottom)
            ]
            ( [ linearLayout
                  [ height $ V 35
                  , width $ V 35
                  , onClick push (const GoBack)
                  , disableClickFeedback true
                  , margin (Margin 16 10 16 0)
                  , gravity CENTER
                  ]
                  [ imageView
                      [ height $ V 25
                      , width $ V 25
                      , imageWithFallback "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left_white.png"
                      ]
                  ]
              , linearLayout
                [ height WRAP_CONTENT -- $ V 136
                , width MATCH_PARENT
                , background Color.white900
                , orientation HORIZONTAL
                , cornerRadius 8.0
                , clickable true
                , margin (Margin 16 20 16 10)
                , stroke "1,#E5E7EB"
                ][  sourceDestinationImageView 
                  , sourceDestinationEditTextView state push
                  ] 
            ]<> if state.isSearchLocation == SearchLocation && state.isRideServiceable then [(searchResultsParentView state push )] else  [] )
            , linearLayout
              [ width MATCH_PARENT
              , height MATCH_PARENT
              , margin (Margin 16 ((screenHeight unit)/2 - 70) 16 0)
              , visibility if (not state.isRideServiceable) then VISIBLE else GONE 
              ][locationUnserviceableView state push]
            , bottomBtnsView state push
            , primaryButtonView state push
        ] )
    where
      bottomSpacing = if safeMarginBottom == 0 then 16 else safeMarginBottom

searchResultsParentView :: forall w. SearchLocationModelState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
searchResultsParentView state push = 
  linearLayout 
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , margin $ MarginHorizontal 16 16
  , orientation VERTICAL
  , visibility if state.isSearchLocation == SearchLocation && state.isRideServiceable then VISIBLE else GONE
  ][ savedLocationBar state push
   , searchResultsView state push ]

locationUnserviceableView :: forall w. SearchLocationModelState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
locationUnserviceableView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , background Color.white900
    , gravity CENTER_HORIZONTAL
    ]
    [ imageView
        [ imageWithFallback "ny_ic_location_unserviceable,https://assets.juspay.in/nammayatri/images/user/ny_ic_location_unserviceable.png"
        , height $ V 99
        , width $ V 133
        , margin $ (MarginBottom 20)
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , margin (MarginBottom 10)
        ]
        [ textView
            [ text (getString LOCATION_UNSERVICEABLE)
            , textSize FontSize.a_18
            , color Color.black800
            , gravity CENTER
            , fontStyle $ FontStyle.bold LanguageStyle
            ]
        ]
    , linearLayout
        [ width (V (screenWidth unit - 40))
        , height WRAP_CONTENT
        , gravity CENTER
        ]
        [ textView
            [ text (getString CURRENTLY_WE_ARE_LIVE_IN_)
            , textSize FontSize.a_14
            , gravity CENTER
            , color Color.black700
            , fontStyle $ FontStyle.regular LanguageStyle
            ]
        ]
    ]

---------------------------- sourceDestinationImageView ---------------------------------
sourceDestinationImageView :: forall w. PrestoDOM (Effect Unit) w
sourceDestinationImageView =
  frameLayout
    [ height $ V 136
    , width $ V 50
    , margin (Margin 5 15 0 0)
    , gravity CENTER 
    ][ linearLayout
        [ height WRAP_CONTENT
        , width $ V 50
        , gravity CENTER
        , margin $ Margin 0 10 2 0
        ][  imageView
            [ height $ V 25
            , width $ V 25
            , imageWithFallback "ny_ic_source_dot,https://assets.juspay.in/nammayatri/images/common/ny_ic_source_dot.png"
            ]
          ]
      , imageView
        [ height $ V 45
        , width $ V 20
        , imageUrl if os == "IOS" then ( if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then  "ic_line_img" else "ny_ic_line_img") else "ic_line"
        , margin if os == "IOS" then (Margin 0 35 0 0) else (Margin 24 35 0 0)
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width $ V 50
        , gravity CENTER
        , margin (Margin 0 80 2 0)  
        ][  imageView
            [ height $ V 25
            , width $ V 25
            , imageWithFallback "ny_ic_loc_red,https://assets.juspay.in/nammayatri/images/common/ny_ic_loc_red.png"
            ]
        ]
    ]

---------------------------- sourceDestinationEditTextView ---------------------------------
sourceDestinationEditTextView :: forall w. SearchLocationModelState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
sourceDestinationEditTextView state push =
  linearLayout 
    [ width MATCH_PARENT 
    , orientation VERTICAL 
    , margin if os == "IOS" then (Margin 0 18 15 0) else (Margin 0 15 15 0)
    , height $ V 136
    , afterRender (\action -> do
      _ <- push action
      _ <- requestKeyboardShow case state.isSource of
                                Just true  -> (getNewIDWithTag "SourceEditText")
                                Just false -> (getNewIDWithTag "DestinationEditText")
                                Nothing    -> ""
      pure unit
      ) (const NoAction)
    ][linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      ][ editText
            [ height $ V 45
            , weight 1.0
            , text state.source
            , color Color.black800
            , stroke if state.isSource == Just true && state.isSearchLocation == LocateOnMap then "1,#FDD836" else "0,#FDD836"
            , background Color.white900
            , fontStyle $ FontStyle.semiBold LanguageStyle
            , singleLine true
            , ellipsize true
            , cornerRadius 10.0
            , padding (Padding 5 0 5 0)
            , margin (Margin 0 10 0 0)
            , textSize FontSize.a_16
            , lineHeight "24"
            , hint (getString START_)
            , hintColor "#A7A7A7"
            , id $ getNewIDWithTag "SourceEditText"
            , onChange
                ( \action -> do
                    _ <- debounceFunction getDelayForAutoComplete push DebounceCallBack
                    _ <- push action
                    pure unit
                )
                SourceChanged
            , inputTypeI if state.isSearchLocation == LocateOnMap then 0 else 1
            , onFocus push $ const $ EditTextFocusChanged "S"
            , autoCorrectionType 1
            ]
        , linearLayout
            [ height $ V 45
            , width WRAP_CONTENT
            , gravity CENTER
            , padding (Padding 0 10 0 5)
            , onClick push (const $ SourceClear)
            , visibility if state.source /= "" then VISIBLE else GONE
            ]
            [ imageView
                [ height $ V 16
                , width $ V 16
                , imageWithFallback "ny_ic_clear,https://assets.juspay.in/nammayatri/images/user/ny_ic_clear.png"
                ]
            ]
        ]
    , linearLayout
        [ height $ V 2
        , width MATCH_PARENT
        , margin (MarginBottom 5)
        , background if state.isSrcServiceable then "#FDD836" else Color.textDanger
        , visibility if state.isSource == Just true && state.isSearchLocation /= LocateOnMap then VISIBLE else GONE
        ]
        []
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.grey900
        ]
        []
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , margin (Margin 0 10 0 0)
        ]
        [ editText
            ( [ height $ V 45
              , weight 1.0
              , text state.destination
              , color Color.black800
              , cornerRadius 10.0
              , stroke if state.isSource == Just false && state.isSearchLocation == LocateOnMap then "1,#FDD836" else "0,#FDD836"
              , padding (Padding 5 0 5 0)
              , background Color.white900
              , hint (getString WHERE_TO)
              , hintColor "#A7A7A7"
              , singleLine true
              , ellipsize true
              , id $ getNewIDWithTag "DestinationEditText"
              , onChange
                  ( \action -> do
                      _ <- debounceFunction getDelayForAutoComplete push DebounceCallBack
                      _ <- push action
                      pure unit
                  )
                  DestinationChanged
              , inputTypeI if state.isSearchLocation == LocateOnMap then 0 else 1
              , onFocus push $ const $ EditTextFocusChanged "D"
              , autoCorrectionType 1
              ]
                <> FontStyle.subHeading1 TypoGraphy
            )
        , linearLayout -- TO BE ADDED LATER FOR CLEARING TEXT
            [ height $ V 45
            , width WRAP_CONTENT
            , gravity CENTER
            , visibility if state.destination /= "" then VISIBLE else GONE
            , onClick push (const $ DestinationClear)
            ]
            [ imageView
                [ height $ V 16
                , width $ V 16
                , imageWithFallback "ny_ic_clear,https://assets.juspay.in/nammayatri/images/user/ny_ic_clear.png"
                ]
            ]
        ]
    , linearLayout
        [ height $ V 2
        , width MATCH_PARENT
        , margin (MarginBottom 5)
        , background if state.isDestServiceable then "#FDD836" else Color.textDanger
        , visibility if state.isSource == Just false && state.isSearchLocation /= LocateOnMap then VISIBLE else GONE
        ]
        []
    ]

---------------------------- searchResultsView ---------------------------------
searchResultsView :: forall w . SearchLocationModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
searchResultsView state push = 
  PrestoAnim.animationSet [
    translateYAnimFromTop $ translateFullYAnimWithDurationConfig 550] $ 
    scrollView
    [ height  MATCH_PARENT
    , width MATCH_PARENT
    , cornerRadius 20.0
    , padding (PaddingVertical 10 60)
    , stroke "1,#E5E7EB"
    , background Color.white900
    , scrollBarY false
    ][  linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , cornerRadius 20.0
        , orientation VERTICAL
        ](mapWithIndex (\index item -> 
              linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              ]([ LocationListItem.view (push <<< LocationListItemActionController) item
               , linearLayout
                  [ height $ V 1
                  , width MATCH_PARENT
                  , background Color.lightGreyShade
                  ][] 
              ] <> (if (index == length state.locationList - 1) 
                      then [  linearLayout  
                              [ height $ V 100 
                              , width MATCH_PARENT  ][]
                            ]
                      else []) )
            ) state.locationList) 
      ]

primaryButtonConfig :: SearchLocationModelState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig
        { text = if state.isSearchLocation == LocateOnMap then if state.isSource == Just true then (getString CONFIRM_PICKUP_LOCATION) else (getString CONFIRM_DROP_LOCATION) else ""
        , color = Color.yellow900      
        , textSize = FontSize.a_16  
        }
      , height = V 60 
      , gravity = CENTER 
      , cornerRadius = 8.0 
      , background = Color.black900
      , margin = (MarginHorizontal 16 16)
      , isClickable = true 
      , id = "SelectLocationFromMap"
      }
  in primaryButtonConfig'

savedLocationBar :: forall w. SearchLocationModelState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
savedLocationBar state push = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginBottom 10
  ][ linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     ][ LocationTagBar.view (push <<< SavedAddressClicked) {savedLocations:state.savedlocationList}]
    ]

---------------------------- primaryButtonView ---------------------------------
primaryButtonView :: forall w. SearchLocationModelState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
primaryButtonView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , alignParentBottom "true,-1"
    , background Color.transparent 
    , visibility if state.isSearchLocation == LocateOnMap then VISIBLE else GONE
    ][ recenterButtonView push state
      , PrimaryButton.view
        ( \action -> do
            _ <- push $ PrimaryButtonActionController action
            stage <- getValueToLocalStoreEff LOCAL_STAGE
            pure unit
        )
        (primaryButtonConfig state)]



recenterButtonView :: forall w. (Action -> Effect Unit) -> SearchLocationModelState -> PrestoDOM ( Effect Unit) w 
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
        [ imageWithFallback "ny_ic_recenter_btn,https://assets.juspay.in/nammayatri/images/common/ny_ic_recenter_btn.png"
        , onClick (\action -> do
            _ <- push action
            _ <- getCurrentPosition push UpdateCurrentLocation
            _ <- pure $ firebaseLogEvent "ny_user_recenter_btn_click"
            pure unit 
        ) (const $ RecenterCurrentLocation)
        , height $ V 40
        , width $ V 40
        ]
  ]

bottomBtnsView :: forall w . SearchLocationModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
bottomBtnsView state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , alignParentBottom "true,-1"
    , background Color.white900
    , visibility if state.isSearchLocation == LocateOnMap || (not state.isRideServiceable) then GONE else VISIBLE
    , gravity CENTER_VERTICAL
    , adjustViewWithKeyboard "true"
    ]
    ( mapWithIndex
        ( \idx item ->
            linearLayout
              [ height WRAP_CONTENT
              , width $ V ((screenWidth unit / 2))
              , orientation HORIZONTAL
              , gravity CENTER_VERTICAL
              ]
              [ linearLayout
                  [ height WRAP_CONTENT
                  , width $ V 0
                  , weight 1.0
                  , gravity CENTER
                  ]
                  [ imageView
                      [ height $ V 20
                      , width $ V 20
                      , imageWithFallback item.imageUrl
                      ]
                  , textView
                      $ [ height WRAP_CONTENT
                        , width WRAP_CONTENT
                        , text item.text
                        , gravity CENTER
                        , color Color.black700
                        , padding (Padding 10 16 10 16)
                        , onClick
                            ( \action ->
                                if item.buttonType == "CurrentLocation" then do
                                  _ <- push action
                                  getLocationName push "9.9" "9.9" "Current Location" UpdateSource
                                else do
                                  _ <- push action
                                  pure unit
                            )
                            (const item.action)
                        ]
                      <> FontStyle.body1 TypoGraphy
                  ]
              , linearLayout
                  [ width $ V 2
                  , height $ V 20
                  , background Color.brownishGrey
                  , alpha 0.25
                  , visibility if length (btnData state) - 1 == idx then GONE else VISIBLE
                  ]
                  []
              ]
        )
        $ btnData state
    )

btnData :: SearchLocationModelState -> Array { text :: String, imageUrl :: String, action :: Action, buttonType :: String }
btnData state =
  [ { text: (getString SET_LOCATION_ON_MAP), imageUrl: "ny_ic_locate_on_map,https://assets.juspay.in/nammayatri/images/user/ny_ic_locate_on_map.png", action: SetLocationOnMap, buttonType: "LocateOnMap" }
  , { text: (getString CURRENT_LOCATION), imageUrl: "ny_ic_current_location,https://assets.juspay.in/nammayatri/images/user/ny_ic_current_location.png", action: SetCurrentLocation, buttonType: "CurrentLocation" }
  ]

