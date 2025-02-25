module Screens.MeterScreen.View where

import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Debug (spy)
import Effect (Effect)
import Prelude
import PrestoDOM
import Screens.MeterScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import JBridge (hideKeyboardOnNavigation, debounceFunction, showKeyboard, reallocateMapFragment, showMap)
import Resource.Constants (getDelayForAutoComplete)
import Components.LocationListItem as LocationListItem
import Helpers.Utils as HU
import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App as CTA
import Mobility.Prelude as MP
import Engineering.Helpers.Commons as EHC
import DecodeUtil (getAnyFromWindow)
import Language.Types (STR(..))
import PrestoDOM.Animation as PrestoAnim
import Engineering.Helpers.Commons (getNewIDWithTag, safeMarginTop)
import Data.Array (any, mapWithIndex, length, null)
import Components.SeparatorView.View as SeparatorView
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Function.Uncurried (runFn3)
import Language.Strings (getVarString)
import JBridge as JB
import Effect.Uncurried (runEffectFn2)

screen :: ST.MeterScreenState -> Screen Action ST.MeterScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "MeterScreen"
  , globalEvents:
      [  \push -> do
            _ <- reallocateMapFragment (EHC.getNewIDWithTag "MeterScreenMap")
            void $ runEffectFn2 JB.storeCallBackLocateOnMap (\key lat lon -> push $ LocateOnMapCallBack key lat lon) (JB.handleLocateOnMapCallback "MeterScreen")
            pure $ pure unit
      ]
  , eval:
      ( \action state -> do
          let _ = spy "MeterScreen ----- state" state
          let _ = spy "MeterScreen --------action" action
          eval action state
      )
  }

view :: forall w. (Action -> Effect Unit) -> ST.MeterScreenState -> PrestoDOM (Effect Unit) w
view push state =
  let isMapSearchLocation = any (_ == state.data.isSearchLocation) [ST.LocateOnMap, ST.RouteMap]
  in
  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      , onBackPressed push (const $ BackPressed)
     ][ PrestoAnim.animationSet
        [ Anim.fadeIn true ]
          $
          topView state push
          , relativeLayout 
            [ width MATCH_PARENT
            , height MATCH_PARENT
            ][searchResultsParentView state push
              , mapView state push isMapSearchLocation
              , bottomBtnsView state push
            ]
     ]

mapView :: forall w. ST.MeterScreenState -> (Action -> Effect Unit) -> Boolean -> PrestoDOM (Effect Unit) w
mapView state push isMapSearchLocation =
  frameLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , accessibility DISABLE
  , clickable isMapSearchLocation
  ][
    linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.transparent
    , clickable isMapSearchLocation
    ][googleMap state push]
    , mapMarker state push isMapSearchLocation
    , primaryButtonView state push
  ]

mapMarker :: forall w. ST.MeterScreenState -> (Action -> Effect Unit ) -> Boolean -> PrestoDOM (Effect Unit) w
mapMarker state _ isMapSearchLocation = 
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.transparent
    , padding $ PaddingBottom 36
    , gravity CENTER
    , accessibility DISABLE
    , orientation VERTICAL
    , visibility $ MP.boolToVisibility $ isMapSearchLocation
    ][
      imageView
      [ width $ V 35
      , height $ V 35
      , accessibility DISABLE
      , visibility $ MP.boolToInvisibility (isMapSearchLocation)
      , imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_dest_marker"
      , id (getNewIDWithTag "LocateOnMapPin1")
      ]
    ]

topView :: forall w. ST.MeterScreenState -> (Action -> Effect Unit ) -> PrestoDOM (Effect Unit) w
topView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.gunMetalBlue
    , padding $ PaddingVertical safeMarginTop 16
    ][
      linearLayout
      [ width MATCH_PARENT
      , height $ WRAP_CONTENT
      , orientation HORIZONTAL
      , padding $ Padding 4 4 4 4
      , gravity CENTER
      ][
        backPressView state push
      , destinationEditTextView state push
      ]
    ]
  where
    backPressView :: forall we. ST.MeterScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) we
    backPressView _ _ =
     linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , disableClickFeedback true
      , margin (Margin 5 17 0 0)
      , gravity CENTER
      , padding (Padding 4 4 4 4)
      , cornerRadius 20.0
      ][imageView
        [ height $ V 23
        , width $ V 23
        , accessibilityHint "Back : Button"
        , rippleColor Color.rippleShade
        , onClick push (const BackPressed)
        , accessibility ENABLE
        , imageWithFallback $ if state.data.isSearchLocation == ST.RouteMap then "ny_ic_close_white,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_close_white.png" else  "ny_ic_chevron_left_white,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_chevron_left_white.png"
        , margin $ Margin 4 4 4 4
        ]
      , textView  $
        [ text ""
        , visibility $ state.data.suffixButtonVisibility
        , color Color.white900  
        , margin $ MarginLeft 8
        ] <> (FontStyle.subHeading2 CTA.LanguageStyle)
      , linearLayout 
        [ weight 1.0
        , height WRAP_CONTENT
        , visibility $ state.data.suffixButtonVisibility
        ] []
    ]

destinationEditTextView :: forall w. ST.MeterScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
destinationEditTextView state push =
  let isMapSearchLocation = any (_ == state.data.isSearchLocation) [ST.LocateOnMap, ST.RouteMap]
  in
  linearLayout
    [ width MATCH_PARENT
    , orientation VERTICAL
    , margin if EHC.os == "IOS" then (Margin 0 18 15 0) else (Margin 0 16 16 0)
    , height WRAP_CONTENT
    ][
      linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , cornerRadius 4.0
        , orientation HORIZONTAL
        , margin $ MarginTop 9
        , background "#313440"
        ][ 
          editText
           ([  height $ V 37
            , weight 1.0
            , text state.data.destination
            , color "#A7A7A7"
            , stroke $ "0," <> Color.black
            , padding (Padding 8 10 32 10)
            , hint "Enter Destination" 
            , onChange
              ( \action -> do
                  _ <- debounceFunction getDelayForAutoComplete push DebounceCallBack false
                  _ <- push action
                  pure unit
              ) DestinationChanged
            , afterRender (\_ -> do
                  _ <- pure $ showKeyboard (getNewIDWithTag "DestinationEditTextMeterSreen")
                  pure unit
              ) (const NoAction)
            , hintColor  "#A7A7A7"
            , singleLine true
            , ellipsize true
            , accessibilityHint "Destination Location Editable field"
            , accessibility ENABLE
            , id $ getNewIDWithTag "DestinationEditTextMeterSreen"
            , inputTypeI if isMapSearchLocation then 0 else 1
            , inputType if isMapSearchLocation then Disabled else TypeText
            , onFocus push $ const $ EditTextFocusChanged
            , autoCorrectionType 1
            , selectAllOnFocus true
            ]
            <> FontStyle.subHeading1 CTA.TypoGraphy
            )
          , linearLayout
              [ height $ V 32
              , width $ V 30
              , gravity CENTER
              , margin $ MarginTop 2
              , visibility if state.props.searchLocationModelProps.crossBtnDestVisibility then VISIBLE else GONE
              , onClick (\action -> do
                          _ <- pure $ EHC.setText (getNewIDWithTag  "DestinationEditTextMeterSreen") "" 
                          _ <- push action
                          pure unit
                        )(const $ DestinationClear)
              , accessibilityHint "Clear Destination Text : Button"
              , accessibility ENABLE
              ]
              [ imageView
                  [ height $ V 19
                  , width $ V 19
                  , imageWithFallback $ HU.fetchImage HU.FF_ASSET  "ny_ic_close_grey"
                  ]
              ]
        ]
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background if state.props.isDestServiceable then Color.grey900 else Color.textDanger
        , visibility if not isMapSearchLocation then VISIBLE else GONE
        ][]
    ]

searchResultsView :: forall w . ST.MeterScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
searchResultsView state push =
    scrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding (PaddingBottom 60)
    , background Color.white900
    , scrollBarY false
    , visibility $ MP.boolToVisibility $ not $ null state.data.locationList
    ][  linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ][ textView $
              [ text "Search Results"
              , color Color.black700
              , margin $ MarginVertical 14 8
              ] <> FontStyle.body3 CTA.TypoGraphy
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , cornerRadius 8.0
            , stroke $ "1," <> Color.grey900
            , orientation VERTICAL
            ](mapWithIndex (\index item ->
                  linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  ][ LocationListItem.view (push <<< LocationListItemActionController) item (not any (_ == state.data.isSearchLocation) [ST.LocateOnMap, ST.RouteMap] && state.props.searchLocationModelProps.isAutoComplete)
                  , linearLayout
                    [ height $ V 1
                    , width MATCH_PARENT
                    , background Color.lightGreyShade
                    , visibility if (index == length state.data.locationList - 1) then GONE else VISIBLE
                    ][]
                  ]
                ) state.data.locationList)
          , linearLayout
              [ height $ V 80
              , width MATCH_PARENT  
              ][]
         ]
    ]


searchResultsParentView :: forall w. ST.MeterScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
searchResultsParentView state push =
  let isMapSearchLocation = any (_ == state.data.isSearchLocation) [ST.LocateOnMap, ST.RouteMap]
  in
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , margin $ MarginHorizontal 16 16
  , orientation VERTICAL
  , clickable $ not isMapSearchLocation
    ][ findPlacesIllustration push state
      , searchResultsView state push
     ]

findPlacesIllustration :: forall w. (Action -> Effect Unit) -> ST.MeterScreenState -> PrestoDOM (Effect Unit) w
findPlacesIllustration _ state  =
  let appName = fromMaybe "NammaYatri" $ runFn3 getAnyFromWindow "appName" Nothing Just
  in  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , visibility $ MP.boolToVisibility $ null state.data.locationList
      , gravity CENTER_HORIZONTAL
      , margin $ Margin 7 ((EHC.screenHeight unit)/7) 16 0
      ]
      [ imageView
          [ imageWithFallback $ HU.fetchImageCustomer HU.FF_ASSET "ny_ic_empty_search_chennai"
          , height $ V 99
          , width $ V 133
          , margin $ MarginBottom 12
          ]
      , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER
          , margin $ MarginBottom 5
          , visibility GONE -- Not required now.
          ]
          [ textView $
              [ text $ (getVarString WELCOME_TEXT [appName]) <> "!"
              , color Color.black700
              , gravity CENTER
              ] <> FontStyle.body4 CTA.LanguageStyle
          ]
      , linearLayout
          [ width $ V (EHC.screenWidth unit - 40)
          , height WRAP_CONTENT
          , gravity CENTER
          ]
          [ textView $
              [ text "Start typing to search places"
              , gravity CENTER
              , color Color.black700
              ] <> FontStyle.body3 CTA.LanguageStyle
          ]
      ]

primaryButtonView :: forall w. ST.MeterScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
primaryButtonView state push =
  let isMapSearchLocation = any (_ == state.data.isSearchLocation) [ST.LocateOnMap, ST.RouteMap]
  in
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.transparent
    , visibility $ MP.boolToVisibility isMapSearchLocation
    , gravity BOTTOM
    , padding $ Padding 8 8 8 8
    ][PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)]

primaryButtonConfig :: ST.MeterScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let config = PrimaryButton.config
      primaryButtonConfig' = config
        { textConfig
          { text = "Confirm Destination"
          , color = "#FCC32C"
          , height = V 40
          }
        , height = V 50
        , gravity = CENTER
        , cornerRadius = 8.0
        , background = "#2C2F3A"
        , margin = (MarginHorizontal 16 16)
        , isClickable = true
        , id = "SelectLocationFromMap"
        , enableRipple = true
        , rippleColor = Color.rippleShade
        }
  in primaryButtonConfig'

googleMap :: forall w . ST.MeterScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
googleMap state push =
  let isMapSearchLocation = any (_ == state.data.isSearchLocation) [ST.LocateOnMap, ST.RouteMap]
  in
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , visibility $ MP.boolToVisibility isMapSearchLocation
  , id (EHC.getNewIDWithTag "MeterScreenMap")
  , afterRender
      (\_ -> do
          _ <- showMap (EHC.getNewIDWithTag "MeterScreenMap") true "satellite" (17.0) 0.0 0.0 push ShowMap
          pure unit
      ) (const NoAction)
  ][]

destBtnData :: ST.MeterScreenState -> Array { text :: String, imageUrl :: String, action :: Action, buttonType :: String }
destBtnData _ =
  [ { text: "Select Location on Map", imageUrl: HU.fetchImage HU.COMMON_ASSET "ny_ic_locate_on_map", action: LocateOnMapClicked, buttonType: "LocateOnMap" }]

bottomBtnsView :: forall w . ST.MeterScreenState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
bottomBtnsView state push =
  let isMapSearchLocation = any (_ == state.data.isSearchLocation) [ST.LocateOnMap, ST.RouteMap]
  in
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingBottom 0
    , alignParentBottom "true,-1"
    , background Color.white900
    , accessibility DISABLE_DESCENDANT
    , visibility if (isMapSearchLocation) then GONE else VISIBLE
    , adjustViewWithKeyboard "true"
    ][  linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.grey900
        ][]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , background Color.white900
        , gravity CENTER
        ]
        ( mapWithIndex
            ( \idx item ->
                linearLayout
                  [ height MATCH_PARENT
                  , width $ MATCH_PARENT
                  , orientation HORIZONTAL
                  , gravity CENTER
                  ]
                  [ linearLayout
                      [ height WRAP_CONTENT
                      , width $ V 0
                      , weight 1.0
                      , gravity CENTER
                      ][
                       imageView
                          [ height $ V 20
                          , width $ V 20
                          , imageWithFallback item.imageUrl
                          , layoutGravity "center"
                          ]
                      , textView
                          $ [ height WRAP_CONTENT
                            , width WRAP_CONTENT
                            , text item.text
                            , layoutGravity "center"
                            , color Color.black800
                            , padding $ Padding 5 16 0 16
                            , onClick
                                ( \action -> do
                                    _ <- push action
                                    pure unit
                                )
                                (const item.action)
                            ]
                          <> FontStyle.body1 CTA.TypoGraphy
                      ]
                  , linearLayout
                      [ width $ V 2
                      , height $ V 20
                      , background Color.brownishGrey
                      , alpha 0.25
                      , layoutGravity "center"
                      , margin $ MarginVertical 0 0
                      , visibility $ if (length $ destBtnData state) - 1 == idx then GONE else VISIBLE
                      ]
                      []
                  ]
            )
            $ if not isMapSearchLocation then destBtnData state else []
        )]
