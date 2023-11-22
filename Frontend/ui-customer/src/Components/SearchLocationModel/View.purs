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
import Data.Array (mapWithIndex, length)
import Data.Function (flip)
import Data.Maybe (Maybe(..), fromMaybe)
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag, isPreviousVersion, os, safeMarginBottom, safeMarginTop, screenHeight, screenWidth, setText)
import Engineering.Helpers.LogEvent (logEvent)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getLocationName, getPreviousVersion, getSearchType, getAssetsBaseUrl, fetchImage, FetchImageFrom(..))
import JBridge (getBtnLoader, showKeyboard, getCurrentPosition, firebaseLogEvent, startLottieProcess, lottieAnimationConfig, debounceFunction)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude ((<>))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (+), (-), (/), (/=), (<<<), (<>), (==), (||), not, discard, (>=), void)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Accessiblity(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), accessibilityHint ,adjustViewWithKeyboard, afterRender, alignParentBottom, alpha, autoCorrectionType, background, clickable, color, cornerRadius, cursorColor, disableClickFeedback, editText, ellipsize, fontStyle, frameLayout, gravity, height, hint, hintColor, id, imageUrl, imageView, imageWithFallback, inputTypeI, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, onFocus, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, accessibility, lottieAnimationView, layoutGravity, selectAllOnFocus)
import PrestoDOM.Animation as PrestoAnim
import Resources.Constants (getDelayForAutoComplete)
import Screens.Types (SearchLocationModelType(..), LocationListItemState)
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Data.String as DS
import Engineering.Helpers.MobilityPrelude

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
      ][PrestoAnim.animationSet
        (if os == "IOS" then [] else [ translateYAnimFromTop $ translateFullYAnimWithDurationConfig 500 ])
        $ linearLayout
         -- Temporary fix for iOS.
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , background state.appConfig.primaryBackground
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
  , margin $ Margin 16 15 16 0
  , orientation VERTICAL
  , visibility if state.isSearchLocation == SearchLocation && state.isRideServiceable && not state.showLoader then VISIBLE else GONE
    ][  savedLocationBar state push
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
            [ text (getString CURRENTLY_WE_ARE_LIVE_IN_)
            , gravity CENTER
            , color Color.black700
            ] <> FontStyle.paragraphText LanguageStyle
        ]
    ]

---------------------------- sourceDestinationImageView ---------------------------------
sourceDestinationImageView :: forall w. SearchLocationModelState -> PrestoDOM (Effect Unit) w
sourceDestinationImageView state =
  frameLayout
    [ height $ V 100
    , width $ V 35
    , margin $ MarginTop 9
    ][ linearLayout
        [ height WRAP_CONTENT
        , width $ V 30
        , gravity CENTER
        , margin $ Margin 2 20 2 0
        ][  imageView
            [ height $ V 15
            , width $ V 15
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_green_circle"
            ]
          ]
      , imageView
        [ height $ V 40
        , width $ V 20
        , gravity CENTER
        , imageUrl if os == "IOS" then ( if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then  "ic_line_img" else "ny_ic_line_img") else "ic_line"
        , margin if os == "IOS" then (Margin 7 35 0 0) else (Margin 16 30 0 0)
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width $ V 30
        , gravity CENTER
        , margin (Margin 2 70 2 0)
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
      , stroke $ if isJustTrue state.isSource && state.isSearchLocation == LocateOnMap then "1," <> Color.yellowText else "0," <> Color.yellowText
      ][ editText $
            [ height $ V 37
            , weight 1.0
            , text state.source
            , color if not(isJustTrue state.isSource) then state.appConfig.searchLocationConfig.editTextDefaultColor else Color.white900
            , singleLine true
            , ellipsize true
            , cornerRadius 4.0
            , padding (Padding 8 7 32 7)
            , lineHeight "24"
            , cursorColor state.appConfig.primaryTextColor
            , accessibilityHint "Pickup Location Editable field"
            , accessibility ENABLE
            , hint (getString START_)
            , hintColor state.appConfig.searchLocationConfig.editTextDefaultColor
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
                        _ <- if isJustTrue state.isSource then pure $ setText (getNewIDWithTag "SourceEditText") "" else pure unit
                        _ <- push action
                        pure unit
                      )(const $ SourceClear)
            , accessibilityHint "Clear Source Text : Button"
            , accessibility ENABLE
            , visibility if state.crossBtnSrcVisibility && isJustTrue state.isSource && state.isSearchLocation /= LocateOnMap then VISIBLE else GONE
            ]
            [ imageView
                [ height $ V 19
                , width $ V 19
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_close_grey"
                ]
            ]
        ]
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.grey900
        , background if state.isSrcServiceable then Color.grey900 else Color.textDanger
        , visibility if isJustTrue state.isSource && state.isSearchLocation /= LocateOnMap then VISIBLE else GONE
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
              , color if (isJustTrue state.isSource) then state.appConfig.searchLocationConfig.editTextDefaultColor else Color.white900
              , stroke $ "0," <> Color.black
              , padding (Padding 8 7 4 7)
              , hint (getString WHERE_TO)
              , hintColor state.appConfig.searchLocationConfig.editTextDefaultColor
              , singleLine true
              , ellipsize true
              , accessibilityHint "Destination Location Editable field"
              , accessibility ENABLE
              , cursorColor state.appConfig.primaryTextColor
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
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_close_grey"
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
    scrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding (PaddingBottom 60)
    , background Color.white900
    , scrollBarY false
    , visibility if (length state.locationList == 0) then GONE else VISIBLE
    ][  linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ][  linearLayout
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
                  ][ LocationListItem.view (push <<< LocationListItemActionController) item (if (isJustTrue state.isSource && state.isSearchLocation /= LocateOnMap && state.isAutoComplete) then true else false) 
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
        { text = if state.isSearchLocation == LocateOnMap then if isJustTrue state.isSource then (getString CONFIRM_PICKUP_LOCATION) else (getString CONFIRM_DROP_LOCATION) else ""
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
  , margin $ MarginBottom 15
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
                  , width if (isJustTrue state.isSource && state.isSearchLocation /= LocateOnMap) then (V 190) else MATCH_PARENT
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
                            , padding if (isJustTrue state.isSource && state.isSearchLocation /= LocateOnMap) then (Padding 5 16 0 16) else (Padding 5 16 0 16)
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
                      , margin $ MarginTop if os == "IOS" then 15 else 0
                      , visibility if length (if (isJustTrue state.isSource && state.isSearchLocation /= LocateOnMap) then srcBtnData state else destBtnData state) - 1 == idx then GONE else VISIBLE
                      ]
                      []
                  ]
            )
            $ if (isJustTrue state.isSource && state.isSearchLocation /= LocateOnMap) then srcBtnData state else destBtnData state
        )]

srcBtnData :: SearchLocationModelState -> Array { text :: String, imageUrl :: String, action :: Action, buttonType :: String }
srcBtnData state =
  [ { text: (getString SELECT_ON_MAP), imageUrl: "ny_ic_locate_on_map,https://assets.juspay.in/nammayatri/images/user/ny_ic_locate_on_map.png", action: SetLocationOnMap, buttonType: "LocateOnMap" }
  , { text: (getString CURRENT_LOCATION), imageUrl: "ny_ic_current_location,https://assets.juspay.in/nammayatri/images/user/ny_ic_current_location.png", action: SetCurrentLocation, buttonType: "CurrentLocation" }
  ]

destBtnData :: SearchLocationModelState -> Array { text :: String, imageUrl :: String, action :: Action, buttonType :: String }
destBtnData state =
  [ { text: (getString SELECT_LOCATION_ON_MAP), imageUrl: "ny_ic_locate_on_map,https://assets.juspay.in/nammayatri/images/user/ny_ic_locate_on_map.png", action: SetLocationOnMap, buttonType: "LocateOnMap" }]


