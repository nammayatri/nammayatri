{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SearchLocationModel.View where

import Common.Types.App

import Animation (translateYAnimFromTop)
import Animation.Config (translateFullYAnimWithDurationConfig, translateYAnimHomeConfig, Direction(..))
import Common.Types.App (LazyCheck(..))
import Components.LocationListItem as LocationListItem
import Components.LocationTagBar as LocationTagBar
import Components.PrimaryButton as PrimaryButton
import Components.SearchLocationModel.Controller (Action(..), SearchLocationModelState)
import Components.SeparatorView.View as SeparatorView
import Data.Array (mapWithIndex, length, take, null)
import Data.Function (flip)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag, isPreviousVersion, os, safeMarginBottom, safeMarginTop, screenHeight, screenWidth, setText)
import Engineering.Helpers.LogEvent (logEvent)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getLocationName, getSearchType, getAssetsBaseUrl, fetchImage, FetchImageFrom(..))
import JBridge (getBtnLoader, showKeyboard, getCurrentPosition, firebaseLogEvent, startLottieProcess, lottieAnimationConfig, debounceFunction)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude ((<>))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (+), (-), (/), (/=), (<<<), (<>), (==), (||), not, discard, (>=), void)
import PrestoDOM (Accessiblity(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), accessibility, accessibilityHint, adjustViewWithKeyboard, afterRender, alignParentBottom, alpha, autoCorrectionType, background, clickable, color, cornerRadius, cursorColor, disableClickFeedback, editText, ellipsize, fontStyle, frameLayout, gravity, height, hint, hintColor, id, imageUrl, imageView, imageWithFallback, inputTypeI, layoutGravity, lineHeight, linearLayout, lottieAnimationView, margin, onBackPressed, onChange, onClick, onFocus, orientation, padding, relativeLayout, scrollBarY, scrollView, selectAllOnFocus, singleLine, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Resources.Constants (getDelayForAutoComplete)
import Screens.Types (SearchLocationModelType(..), LocationListItemState)
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Mobility.Prelude (boolToVisibility)

view :: forall w. (Action -> Effect Unit) -> SearchLocationModelState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background case state.isSearchLocation of
                    SearchLocation -> Color.white900
                    _           -> Color.transparent --"#FFFFFF"
      , margin $ MarginBottom (if state.isSearchLocation == LocateOnMap then bottomSpacing else 0)
      , onBackPressed push (const $ GoBack)
      ][ PrestoAnim.animationSet
          [ translateYAnimFromTop $ translateFullYAnimWithDurationConfig 500 ]
          $ linearLayout
          [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , background state.appConfig.searchLocationConfig.backgroundColor
            , padding $ PaddingVertical safeMarginTop 16
            ][  linearLayout
                [ orientation HORIZONTAL
                , height $ V 105 
                , width MATCH_PARENT
                 ][ linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , onClick push (const GoBack)
                    , disableClickFeedback true
                    , margin (Margin 9 21 0 0)
                    , gravity CENTER
                    , padding (Padding 4 4 4 4)
                    ]
                  [ imageView
                      [ height $ V 23
                      , width $ V 23
                      , accessibilityHint "Back : Button"
                      , accessibility ENABLE
                      , imageWithFallback state.appConfig.searchLocationConfig.backArrow
                      ]
                  ]
                  , sourceDestinationImageView state
                  , sourceDestinationEditTextView state push
                  ]]
                  , PrestoAnim.animationSet
                    [ translateYAnimFromTop $ translateFullYAnimWithDurationConfig 500 ]
                    $ linearLayout
                      [ height $ V 1
                      , width MATCH_PARENT
                      , background state.appConfig.searchLocationConfig.separatorColor
                      , visibility if state.appConfig.searchLocationConfig.showSeparator then VISIBLE else GONE
                      ]
                      []
                  , relativeLayout 
                    [ width MATCH_PARENT
                    , height MATCH_PARENT
                    , weight 1.0
                    ][ PrestoAnim.animationSet
                      [translateYAnimFromTop $ translateFullYAnimWithDurationConfig 500]
                      $ searchResultsParentView state push 
                      , linearLayout
                        [ width MATCH_PARENT
                        , height MATCH_PARENT
                        , margin (Margin 16 ((screenHeight unit)/4) 16 0)
                        , visibility if (not state.isRideServiceable) then VISIBLE else GONE
                        ][locationUnserviceableView state push]
                      , searchLottieLoader push state
                      , bottomBtnsView state push
                      , primaryButtonView state push
                      ] ]
    where
      bottomSpacing = if safeMarginBottom == 0 then 16 else safeMarginBottom

searchResultsParentView :: forall w. SearchLocationModelState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
searchResultsParentView state push =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , margin $ MarginHorizontal 16 16
  , orientation VERTICAL
  , visibility $ boolToVisibility $ state.isSearchLocation == SearchLocation && state.isRideServiceable && not state.showLoader 
    ][  savedLocationBar state push
      , findPlacesIllustration  push state
      , searchResultsView state push ]

searchLottieLoader :: forall w. (Action -> Effect Unit) -> SearchLocationModelState -> PrestoDOM (Effect Unit) w
searchLottieLoader push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_HORIZONTAL
  ][  lottieAnimationView
      [ height $ if os == "IOS" then V 170 else V 130
      , width $ V 130
      , padding $ PaddingBottom 80
      , margin (MarginTop ((screenHeight unit)/ 7 - (if os == "IOS" then 140 else 90)))
      , gravity CENTER
      , id (getNewIDWithTag "searchLoader")
      , visibility if state.showLoader then VISIBLE else GONE
      , afterRender (\action -> do
        void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = (getAssetsBaseUrl FunctionCall) <> "lottie/search_loader.json", lottieId = (getNewIDWithTag "searchLoader"), scaleType="CENTER_CROP", repeat = true, speed = 0.8 }
        push action
        ) (const NoAction)
      ]
    ]

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
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_location_unserviceable"
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
        [ textView $
            [ text (getString LOCATION_UNSERVICEABLE)
            , color Color.black800
            , gravity CENTER
            ] <> FontStyle.h2 LanguageStyle
        ]
    , linearLayout
        [ width (V (screenWidth unit - 40))
        , height WRAP_CONTENT
        , gravity CENTER
        ]
        [ textView $
            [ text (getString$ CURRENTLY_WE_ARE_LIVE_IN_ "CURRENTLY_WE_ARE_LIVE_IN_")
            , gravity CENTER
            , color Color.black700
            ] <> FontStyle.paragraphText LanguageStyle
        ]
    ]

---------------------------- sourceDestinationImageView ---------------------------------
sourceDestinationImageView :: forall w. SearchLocationModelState -> PrestoDOM (Effect Unit) w
sourceDestinationImageView state =
  linearLayout
    [ height WRAP_CONTENT
    , width $ V 20
    , margin $ Margin 8 9 8 0
    , orientation VERTICAL
    , gravity CENTER
    ][ linearLayout
        [ height $ V 15
        , width $  V 15
        , gravity CENTER
        , margin $ Margin 2 20 2 0
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
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_red_circle"
            ]
        ]
    ]

---------------------------- sourceDestinationEditTextView ---------------------------------
sourceDestinationEditTextView :: forall w. SearchLocationModelState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
sourceDestinationEditTextView state push =
  linearLayout
    [ width MATCH_PARENT
    , orientation VERTICAL
    , margin if os == "IOS" then (Margin 0 18 15 0) else (Margin 0 16 16 0)
    , height $ V 121
  
    ][linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , background state.appConfig.searchLocationConfig.editTextBackground
      , cornerRadius 4.0 
      , stroke $ if state.isSource == Just true && state.isSearchLocation == LocateOnMap then "1," <> Color.yellowText else "0," <> Color.yellowText
      ][ editText $
            [ height $ V 37
            , weight 1.0
            , text state.source
            , color if state.isSource == Just true then state.appConfig.searchLocationConfig.editTextColor else state.appConfig.searchLocationConfig.editTextDefaultColor
            , singleLine true
            , ellipsize true
            , cornerRadius 4.0
            , padding (Padding 8 7 32 7)
            , lineHeight "24"
            , accessibilityHint "Pickup Location Editable field"
            , accessibility ENABLE
            , hint (getString START_)
            , hintColor state.appConfig.searchLocationConfig.hintColor
            , id $ getNewIDWithTag "SourceEditText"
            , afterRender (\_ -> do
                  _ <- pure $ showKeyboard case state.isSource of
                                            Just true  -> (getNewIDWithTag "SourceEditText")
                                            Just false -> (getNewIDWithTag "DestinationEditText")
                                            Nothing    -> ""
                  pure unit
                    ) (const NoAction)
            , onChange
                ( \action -> do
                    _ <- debounceFunction getDelayForAutoComplete push DebounceCallBack (fromMaybe false state.isSource)
                    _ <- push action
                    pure unit
                )
                SourceChanged
            , inputTypeI if state.isSearchLocation == LocateOnMap then 0 else 1
            , onFocus push $ const $ EditTextFocusChanged "S"
              , selectAllOnFocus true
            , autoCorrectionType 1
            ] <> FontStyle.subHeading1 LanguageStyle
        , linearLayout
            [ height $ V 32
            , width $ V 30
            , gravity CENTER
            , padding $ PaddingVertical 10 2
            , onClick (\action -> do
                        _ <- if state.isSource == Just true then pure $ setText (getNewIDWithTag "SourceEditText") "" else pure unit
                        _ <- push action
                        pure unit
                      )(const $ SourceClear)
            , accessibilityHint "Clear Source Text : Button"
            , accessibility ENABLE
            , visibility if state.crossBtnSrcVisibility && state.isSource == Just true && state.isSearchLocation /= LocateOnMap then VISIBLE else GONE
            ]
            [ imageView
                [ height $ V 19
                , width $ V 19
                , imageWithFallback $ fetchImage FF_ASSET state.appConfig.searchLocationConfig.clearTextImage
                ]
            ]
        ]
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.grey900
        , background if state.isSrcServiceable then Color.grey900 else Color.textDanger
        , visibility if state.isSource == Just true && state.isSearchLocation /= LocateOnMap then VISIBLE else GONE
        ]
        []
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , cornerRadius 4.0
        , orientation HORIZONTAL
        , margin $ MarginTop 12
        , background state.appConfig.searchLocationConfig.editTextBackground
        , stroke if state.isSource == Just false && state.isSearchLocation == LocateOnMap then "1," <> Color.yellowText else "0," <> Color.yellowText
        ]
        [ editText
            ( [ height $ V 37
              , weight 1.0
              , text state.destination
              , color if state.isSource == Just true then state.appConfig.searchLocationConfig.editTextDefaultColor else state.appConfig.searchLocationConfig.editTextColor
              , stroke $ "0," <> Color.black
              , padding (Padding 8 7 4 7)
              , hint (getString WHERE_TO)
              , hintColor state.appConfig.searchLocationConfig.hintColor
              , singleLine true
              , ellipsize true
              , accessibilityHint "Destination Location Editable field"
              , accessibility ENABLE
              , id $ getNewIDWithTag "DestinationEditText"
              , afterRender (\action -> do
                  _ <- pure $ showKeyboard case state.isSource of
                                            Just true -> (getNewIDWithTag "SourceEditText")
                                            Just false -> (getNewIDWithTag "DestinationEditText")
                                            Nothing    -> ""
                  pure unit
                    ) (const NoAction)
              , onChange
                  ( \action -> do
                      _ <- debounceFunction getDelayForAutoComplete push DebounceCallBack (fromMaybe false state.isSource)
                      _ <- push action
                      pure unit
                  )
                  DestinationChanged
              , inputTypeI if state.isSearchLocation == LocateOnMap then 0 else 1
              , onFocus push $ const $ EditTextFocusChanged "D"
              , selectAllOnFocus true
              , autoCorrectionType 1
              ]
                <> FontStyle.subHeading1 TypoGraphy
            )
        , linearLayout -- TO BE ADDED LATER FOR CLEARING TEXT
            [ height $ V 32
            , width $ V 30
            , gravity CENTER
            , margin $ MarginTop 2
            , visibility if state.crossBtnDestVisibility && state.isSource == Just false && state.isSearchLocation /= LocateOnMap then VISIBLE else GONE
            , onClick (\action -> do
                        _ <- if state.isSource == Just false then pure $ setText (getNewIDWithTag  "DestinationEditText") "" else pure unit
                        _ <- push action
                        pure unit
                      )(const $ DestinationClear)
            , accessibilityHint "Clear Destination Text : Button"
            , accessibility ENABLE
            ]
            [ imageView
                [ height $ V 19
                , width $ V 19
                , imageWithFallback $ fetchImage FF_ASSET state.appConfig.searchLocationConfig.clearTextImage
                ]
            ]
        ]
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , margin (MarginBottom 5)
        , background if state.isDestServiceable then Color.grey900 else Color.textDanger
        , visibility if state.isSource == Just false && state.isSearchLocation /= LocateOnMap then VISIBLE else GONE
        ]
        []
    ]

---------------------------- searchResultsView ---------------------------------
searchResultsView :: forall w . SearchLocationModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
searchResultsView state push =
    let searchResultText = if state.isAutoComplete 
                            then getString SEARCH_RESULTS
                            else if state.isSource == Just false then getString SUGGESTED_DESTINATION
                            else getString PAST_SEARCHES
    in 
    scrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding (PaddingBottom 60)
    , background Color.white900
    , scrollBarY false
    , visibility $ boolToVisibility $ not (null state.locationList || state.findPlaceIllustration) 
    ][  linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ][  textView $
              [ text searchResultText
              , color Color.black700
              , margin $ MarginVertical 14 8
              ] <> FontStyle.body3 TypoGraphy
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , cornerRadius state.appConfig.primaryButtonCornerRadius
            , stroke $ "1," <> Color.grey900
            , orientation VERTICAL
            ](mapWithIndex (\index item ->
                  linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  ][ LocationListItem.view (push <<< LocationListItemActionController) item (if (state.isSource == Just true && state.isSearchLocation /= LocateOnMap && state.isAutoComplete) then true else false) 
                  , linearLayout
                    [ height $ V 1
                    , width MATCH_PARENT
                    , background Color.lightGreyShade
                    , visibility if (index == length state.locationList - 1) then GONE else VISIBLE
                    ][]
                  ]
                ) state.locationList)
          , linearLayout
              [ height $ V 80
              , width MATCH_PARENT  ][]
              ]
      ]

primaryButtonConfig :: SearchLocationModelState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        { text = if state.isSearchLocation == LocateOnMap then if state.isSource == Just true then (getString CONFIRM_PICKUP_LOCATION) else (getString CONFIRM_DROP_LOCATION) else ""
        , color = state.appConfig.primaryTextColor
        , height = V 40
        }
      , height = V state.appConfig.searchLocationConfig.primaryButtonHeight
      , gravity = CENTER
      , cornerRadius = state.appConfig.primaryButtonCornerRadius
      , background = state.appConfig.primaryBackground
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
  , margin $ MarginTop 15 
  , accessibility DISABLE_DESCENDANT
  , visibility if (not state.isAutoComplete) then VISIBLE else GONE
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
      , PrimaryButton.view (push <<< PrimaryButtonActionController)(primaryButtonConfig state)]



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
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_recenter_btn"
        , onClick (\action -> do
            _ <- push action
            _ <- getCurrentPosition push UpdateCurrentLocation
            _ <- logEvent state.logField "ny_user_recenter_btn_click"
            pure unit 
        ) (const $ RecenterCurrentLocation)
        , height $ V 40
        , width $ V 40
        ]
  ]

findPlacesIllustration :: forall w. (Action -> Effect Unit) -> SearchLocationModelState -> PrestoDOM (Effect Unit) w
findPlacesIllustration push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility if state.findPlaceIllustration then VISIBLE else GONE
    , gravity CENTER_HORIZONTAL
    , margin $ Margin 7 ((screenHeight unit)/7) 16 0
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
        [ width $ V (screenWidth unit - 40)
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

bottomBtnsView :: forall w . SearchLocationModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
bottomBtnsView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (PaddingBottom if os == "IOS" then 10 else 0)
    , alignParentBottom "true,-1"
    , background Color.white900
    , accessibility DISABLE_DESCENDANT
    , visibility if state.isSearchLocation == LocateOnMap || (not state.isRideServiceable) then GONE else VISIBLE
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
                  , width if (state.isSource == Just true && state.isSearchLocation /= LocateOnMap) then (V 190) else MATCH_PARENT
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
                            , padding if (state.isSource == Just true && state.isSearchLocation /= LocateOnMap) then (Padding 5 16 0 16) else (Padding 5 16 0 16)
                            , onClick
                                ( \action ->
                                    if item.buttonType == "CurrentLocation" then do
                                      _ <- push action
                                      getLocationName push 9.9 9.9 "Current Location" UpdateSource
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
                      , layoutGravity "center"
                      , margin $ if os == "IOS" then MarginVertical 7 7 else MarginVertical 0 0
                      , visibility if length (if (state.isSource == Just true && state.isSearchLocation /= LocateOnMap) then srcBtnData state else destBtnData state) - 1 == idx then GONE else VISIBLE
                      ]
                      []
                  ]
            )
            $ if (state.isSource == Just true && state.isSearchLocation /= LocateOnMap) then srcBtnData state else destBtnData state
        )]

srcBtnData :: SearchLocationModelState -> Array { text :: String, imageUrl :: String, action :: Action, buttonType :: String }
srcBtnData state =
  [ { text: (getString SELECT_ON_MAP), imageUrl: "ny_ic_locate_on_map,https://assets.juspay.in/nammayatri/images/user/ny_ic_locate_on_map.png", action: SetLocationOnMap, buttonType: "LocateOnMap" }
  , { text: (getString CURRENT_LOCATION), imageUrl: "ny_ic_current_location,https://assets.juspay.in/nammayatri/images/user/ny_ic_current_location.png", action: SetCurrentLocation, buttonType: "CurrentLocation" }
  ]

destBtnData :: SearchLocationModelState -> Array { text :: String, imageUrl :: String, action :: Action, buttonType :: String }
destBtnData state =
  [ { text: (getString SELECT_LOCATION_ON_MAP), imageUrl: "ny_ic_locate_on_map,https://assets.juspay.in/nammayatri/images/user/ny_ic_locate_on_map.png", action: SetLocationOnMap, buttonType: "LocateOnMap" }]


separatorConfig :: SeparatorView.Config
separatorConfig = 
  {
    orientation : VERTICAL
  , count : 3
  , height : V 4
  , width : V 1
  , layoutWidth : V 12
  , layoutHeight : V 15
  }
