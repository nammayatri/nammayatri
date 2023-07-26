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
import Components.LocationListItem as LocationListItem
import Components.LocationTagBar as LocationTagBar
import Components.PrimaryButton as PrimaryButton
import Components.SearchLocationModel.Controller (Action(..), SearchLocationModelState)
import Data.Array (mapWithIndex, length)
import Data.Function (flip)
import Data.Maybe (Maybe(..), fromMaybe)
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag, os, safeMarginBottom, safeMarginTop, screenHeight, screenWidth, isPreviousVersion, setText')
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getLocationName, debounceFunction, getPreviousVersion)
import JBridge (getBtnLoader, requestKeyboardShow, getCurrentPosition, firebaseLogEvent, startLottieProcess)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (+), (-), (/), (/=), (<<<), (<>), (==), (||), not, (>=),(<), discard)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), adjustViewWithKeyboard, afterRender, alignParentBottom, alpha, autoCorrectionType, background, color, cornerRadius, disableClickFeedback, editText, ellipsize, fontStyle, frameLayout, gravity, height, hint, hintColor, id, imageUrl, imageView, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, onFocus, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, inputTypeI, clickable, imageWithFallback, lottieAnimationView)
import PrestoDOM.Animation as PrestoAnim
import Resources.Constants (getDelayForAutoComplete)
import Screens.Types (SearchLocationModelType(..), LocationListItemState)
import Storage (KeyStore(..), getValueToLocalStoreEff, getValueToLocalStore)
import Styles.Colors as Color
import Data.String (split, Pattern(..), length) as STR

view :: forall w. (Action -> Effect Unit) -> SearchLocationModelState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background case state.isSearchLocation of
                    LocateOnMap -> Color.transparent
                    SearchLocation -> if (state.isRideServiceable) then Color.white900 else Color.white900
                    _           -> Color.white900 --"#FFFFFF"
      , margin $ MarginBottom (if state.isSearchLocation == LocateOnMap then bottomSpacing else 0)
      , onBackPressed push (const $ GoBack)
      ][ PrestoAnim.animationSet
        (if os == "IOS" then [] else [ translateYAnimFromTop $ translateFullYAnimWithDurationConfig 500 ])
        $ linearLayout
         -- Temporary fix for iOS.
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , background Color.black900
            , padding $ PaddingVertical safeMarginTop 16
            ]
            [linearLayout[
                orientation HORIZONTAL
                , height $ V 105 
                , width MATCH_PARENT
                 ]
                 [ linearLayout
                  [ height $ V 30
                  , width $ V 30
                  , onClick push (const GoBack)
                  , disableClickFeedback true
                  , margin (Margin 9 21 0 0)
                  , gravity CENTER
                  ]
                  [ imageView
                      [ height $ V 23
                      , width $ V 28
                      , imageWithFallback "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left_white.png"
                      ]
                  ]
                  , sourceDestinationImageView
                  , sourceDestinationEditTextView state push
                  
            ]]
            , relativeLayout [
              width MATCH_PARENT
            , height MATCH_PARENT
            , weight 1.0
            ][ PrestoAnim.animationSet
              (if os == "IOS" then [] else [ translateYAnimFromTop $ translateFullYAnimWithDurationConfig 500 ])
              $ searchResultsParentView state push 
              , linearLayout
                [ width MATCH_PARENT
                , height MATCH_PARENT
                , margin (Margin 16 ((screenHeight unit)/2 - 200) 16 0)
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
  , height WRAP_CONTENT
  , margin $ MarginHorizontal 16 16
  , orientation VERTICAL
  , visibility if state.isSearchLocation == SearchLocation && state.isRideServiceable && state.showLoader == false then VISIBLE else GONE
  ][
    savedLocationBar state push
  , searchResultsView state push
    ]

searchLottieLoader :: forall w. (Action -> Effect Unit) -> SearchLocationModelState -> PrestoDOM (Effect Unit) w
searchLottieLoader push state =
  lottieAnimationView
  [ height $ V 200
  , width $ V 200
  , padding (Padding 0 0 0 90)
  , margin (Margin 95 ((screenHeight unit)/ 7 - 110) 0 0)
  , gravity CENTER
  , id (getNewIDWithTag "searchLoader")
  , visibility if state.showLoader == true then VISIBLE else GONE
  , afterRender (\action -> do
    _ <- pure $ startLottieProcess "primary_button_loader" (getNewIDWithTag "searchLoader") true 0.8 "CENTER_CROP"
    push action
    ) (const NoAction)
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
            , imageWithFallback "ny_ic_green_circle,https://assets.juspay.in/nammayatri/images/common/ny_ic_green_circle.png"
            ]
          ]
      , imageView
        [ height $ V 45
        , width $ V 20
        , gravity CENTER
        , imageUrl if os == "IOS" then ( if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then  "ic_line_img" else "ny_ic_line_img") else "ic_line"
        , margin if os == "IOS" then (Margin 7 35 0 0) else (Margin 16 30 0 0)
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width $ V 30
        , gravity CENTER
        , margin if os == "IOS" then (Margin 2 70 2 0) else (Margin 2 69 2 0)
        ][  imageView
            [ height $ V 15
            , width $ V 15
            , imageWithFallback "ny_ic_red_circle,https://assets.juspay.in/nammayatri/images/common/ny_ic_red_circle.png"
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
      , background Color.darkGreyishBlue
      , cornerRadius 4.0 
      , stroke if state.isSource == Just true && state.isSearchLocation == LocateOnMap then "1,#FDD836" else "0,#FDD836"
      ][ editText
            [ height $ V 37
            , weight 1.0
            , text state.source
            , color if not(state.isSource == Just true && state.isSearchLocation /= LocateOnMap) then Color.black600 else Color.white900
            , background Color.darkGreyishBlue
            , fontStyle $ FontStyle.semiBold LanguageStyle
            , singleLine true
            , ellipsize true
            , padding (Padding 8 7 32 7)
            , textSize FontSize.a_16
            , lineHeight "24"
            , hint (getString START_)
            , hintColor Color.black600
            , id $ getNewIDWithTag "SourceEditText"
            , onChange
                ( \action -> do
                    if state.source /= "Current Location" then do
                      _ <- debounceFunction getDelayForAutoComplete push DebounceCallBack (fromMaybe false state.isSource)
                      pure unit
                      else pure unit
                    _ <- push action
                    pure unit
                )
                SourceChanged
            , inputTypeI if state.isSearchLocation == LocateOnMap then 0 else 1
            , onFocus push $ const $ EditTextFocusChanged "S"
            , autoCorrectionType 1
            ]
        , linearLayout
            [ height $ V 32
            , width $ V 30
            , gravity CENTER
            , padding $ PaddingVertical 10 2
            , onClick (\action -> do
                        _ <- if state.isSource == Just true then setText' (getNewIDWithTag "SourceEditText") "" else pure unit
                        _ <- push action
                        pure unit
                      )(const $ SourceClear)
            , visibility if state.sourceLength >= 3 && state.isSource == Just true && state.isSearchLocation /= LocateOnMap then VISIBLE else GONE
            ]
            [ imageView
                [ height $ V 19
                , width $ V 19
                , imageWithFallback "ny_ic_close_grey,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_close_grey.png"
                ]
            ]
        ]
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
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
        , background Color.darkGreyishBlue
        , stroke if state.isSource == Just false && state.isSearchLocation == LocateOnMap then "1,#FDD836" else "0,#FDD836"
        ]
         [ editText
            ( [ height $ V 37
              , weight 1.0
              , text state.destination
              , color if (state.isSource == Just true && state.isSearchLocation /= LocateOnMap) then Color.black600 else Color.white900
              , stroke $ "0," <> Color.black
              , padding (Padding 8 7 4 7)
              , fontStyle $ FontStyle.semiBold LanguageStyle
              , hint (getString WHERE_TO)
              , hintColor Color.black600
              , singleLine true
              , ellipsize true
              , id $ getNewIDWithTag "DestinationEditText"
              , onChange
                  ( \action -> do
                      _ <- debounceFunction getDelayForAutoComplete push DebounceCallBack (fromMaybe false state.isSource)
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
            [ height $ V 32
            , width $ V 30
            , gravity CENTER
            , margin $ MarginTop 2
            , visibility if state.destinationLength >= 3 && state.isSource == Just false && state.isSearchLocation /= LocateOnMap then VISIBLE else GONE
            , onClick (\action -> do
                        _ <- if state.isSource == Just false then setText' (getNewIDWithTag "DestinationEditText") "" else pure unit
                        _ <- push action
                        pure unit
                      )(const $ DestinationClear)
            ]
            [ imageView
                [ height $ V 19
                , width $ V 19
                , imageWithFallback "ny_ic_close_grey,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_close_grey.png"
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
  PrestoAnim.animationSet [
    translateYAnimFromTop $ translateFullYAnimWithDurationConfig 550] $
    scrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 8.0
    , margin $ MarginTop 15
    , background Color.white900
    , scrollBarY false
    , visibility if (length state.locationList == 0) then GONE else VISIBLE
    ][  linearLayout
    [height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ][  linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , cornerRadius 8.0
            , stroke "1,#E5E7EB"
            , orientation VERTICAL
            ](mapWithIndex (\index item ->
                  linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  ][ LocationListItem.view (push <<< LocationListItemActionController) item (if (state.isSource == Just true && state.isSearchLocation /= LocateOnMap && state.isAutoComplete == true) then 1 else 0) 
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
        , color = Color.yellow900
        , textSize = FontSize.a_16
        , height = V 40
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
  , margin $ MarginTop 15
  , visibility if state.isAutoComplete == false then VISIBLE else GONE
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
    , orientation VERTICAL
    , padding (PaddingBottom if os == "IOS" then 10 else 0)
    , alignParentBottom "true,-1"
    , background Color.white900
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
        , gravity CENTER_VERTICAL
        ]
        ( mapWithIndex
            ( \idx item ->
                linearLayout
                  [ height WRAP_CONTENT 
                  , width if (state.isSource == Just true && state.isSearchLocation /= LocateOnMap) then (V 190) else MATCH_PARENT 
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
                            , color Color.black800
                            , padding if (state.isSource == Just true && state.isSearchLocation /= LocateOnMap) then (Padding 10 16 0 16) else (Padding 10 16 0 16)
                            , textSize FontSize.a_14
                            , fontStyle $ FontStyle.semiBold LanguageStyle
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
                      , visibility if length (if (state.isSource == Just true && state.isSearchLocation /= LocateOnMap) then btnData1 state else btnData2 state) - 1 == idx then GONE else VISIBLE
                      ]
                      []
                  ]
            )
            $ if (state.isSource == Just true && state.isSearchLocation /= LocateOnMap) then btnData1 state else btnData2 state
        )]
btnData1 :: SearchLocationModelState -> Array { text :: String, imageUrl :: String, action :: Action, buttonType :: String }
btnData1 state =
  [ { text: (getString SELECT_ON_MAP), imageUrl: "ny_ic_locate_on_map,https://assets.juspay.in/nammayatri/images/user/ny_ic_locate_on_map.png", action: SetLocationOnMap, buttonType: "LocateOnMap" }
  , { text: (getString CURRENT_LOCATION), imageUrl: "ny_ic_current_location,https://assets.juspay.in/nammayatri/images/user/ny_ic_current_location.png", action: SetCurrentLocation, buttonType: "CurrentLocation" }
  ]

btnData2 :: SearchLocationModelState -> Array { text :: String, imageUrl :: String, action :: Action, buttonType :: String }
btnData2 state =
  [ { text: (getString SET_LOCATION_ON_MAP), imageUrl: "ny_ic_locate_on_map,https://assets.juspay.in/nammayatri/images/user/ny_ic_locate_on_map.png", action: SetLocationOnMap, buttonType: "LocateOnMap" }
  ]
