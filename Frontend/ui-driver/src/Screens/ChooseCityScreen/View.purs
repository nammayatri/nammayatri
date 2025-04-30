{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ChooseCityScreen.View where

import Screens.ChooseCityScreen.ComponentConfig

import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..), CarouselData)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.SelectMenuButton as MenuButton
import Data.Array as DA
import Data.Function.Uncurried (runFn2)
import Data.Maybe as Mb
import Debug (spy)
import DecodeUtil as DU
import Data.Function.Uncurried (runFn3)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge (isLocationPermissionEnabled)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, map, not, pure, unit, ($), (<<<), (<>), (==), (&&), when, void)
import PrestoDOM (Accessiblity(..), Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), accessibility, afterRender, alignParentBottom, alpha, background, color, cornerRadius, fontStyle, gradient, gravity, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Screens.ChooseCityScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (ChooseCityScreenStage(..), ChooseCityScreenState)
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import MerchantConfig.Utils as MU
import MerchantConfig.Types
import PrestoDOM.Properties as PP
import PrestoDOM.Types.DomAttributes as PTD
import Components.ErrorModal as ErrorModal
import Mobility.Prelude
import Locale.Utils
import Common.RemoteConfig as RC

screen :: ChooseCityScreenState -> Screen Action ChooseCityScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "ChooseCity"
  , globalEvents:  [(\ push -> do    
    if not initialState.props.isLocationPermissionGiven then do 
      JB.storeCallBackDriverLocationPermission push LocationPermissionCallBack
      pure unit
    else pure unit
    when (initialState.data.config.enableMockLocation) $ 
      JB.isMockLocation push IsMockLocation

    isLocationPermissionEnabled <- JB.isLocationPermissionEnabled unit
    when isLocationPermissionEnabled $
      JB.getCurrentPositionWithTimeout push CurrentLocationCallBack 3500 true

    pure $ pure unit)]
  , eval:
      ( \state action -> do
          let _ = spy "ChooseCityScreen ----- state" state
          let _ = spy "ChooseCityScreen --------action" action
          eval state action
      )
  }


view :: forall w. (Action -> Effect Unit) -> ChooseCityScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation  $ 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , onBackPressed push $ const BackPressed
  , afterRender (\action -> do
      _ <- push action
      pure unit
      )(const AfterRender)
  , gradient $ Linear 0.0 ["#F5F8FF", "#E2EAFF"]
  ][ if state.props.isMockLocation && state.data.config.enableMockLocation  && not cugUser
      then mockLocationEnabledView push state
      else
        relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , padding $ PaddingBottom 24
        ][ enableLocationPermission state push
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , visibility $ boolToVisibility (state.props.currentStage == DETECT_LOCATION)
            ] [ currentLocationView state push
              , currentLanguageViewV2 state push  ]
          , case state.props.currentStage of
              SELECT_LANG -> languageRadioButtonView state push 
              SELECT_CITY -> cityRadioButtonView state push
              _ -> dummyView
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , alignParentBottom "true,-1"
            ][ PrimaryButton.view (push <<< PrimaryButtonAC ) (primaryButtonConfig state) ]
          ]
    ]
    where cugUser = Mb.fromMaybe false $ runFn3 DU.getAnyFromWindow "isCUGUser" Mb.Nothing Mb.Just

headerView :: ChooseCityScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility $ boolToVisibility $ DA.any (_ == state.props.currentStage) [SELECT_LANG, SELECT_CITY]
  ][ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
   , linearLayout 
     [ height $ V 1
     , width MATCH_PARENT
     , background Color.grey900
     ][]
  ]


currentLocationView :: forall w. ChooseCityScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
currentLocationView state push = 
  let locSelectedVal = if Mb.isJust state.data.locationSelected then Mb.fromMaybe "" state.data.locationSelected else ""
      locSelected = 
        case locSelectedVal of
          "" -> ""
          "Paris" -> "Odisha"
          _ -> locSelectedVal
      appName = JB.getAppName unit
      selectCityConfig = RC.selectCityConfig appName
  in linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , padding $ Padding 24 24 24 24
    , margin $ Margin 16 16 16 16
    , cornerRadius 12.0
    , background Color.white900
    , stroke $ "1," <> Color.grey900
    ][  imageView
        [ height $ V 220
        , width $ V 220
        , imageWithFallback $ fetchImage FF_ASSET $ getLocationMapImage state
        ]
      , textView $
        [ text $ getString LOCATION_UNSERVICEABLE
        , gravity CENTER
        , color Color.black800
        , margin $ MarginTop 4
        , visibility $ boolToVisibility state.props.locationUnserviceable
        ] <> FontStyle.h2 TypoGraphy
      , textView $
        [ text $ getString case state.props.locationDetectionFailed, Mb.isNothing state.data.locationSelected, state.props.locationUnserviceable of
                              _ , _ , true -> WE_ARE_NOT_LIVE_IN_YOUR_AREA
                              false, true, _ -> DETECTING_LOCATION
                              true, true, _ -> UNABLE_TO_DETECT_YOUR_LOCATION
                              _, false, _ -> YOUR_DETECTED_LOCATION_IS
        , gravity CENTER
        , color Color.black700
        , margin $ MarginTop if state.props.locationUnserviceable then 4 else 24
        ] <> FontStyle.paragraphText TypoGraphy
      , textView $
        [ text if Mb.isJust state.data.locationSelected then locSelected else "--"
        , gravity CENTER
        , color Color.black800
        , margin $ MarginTop 4
        , visibility $ boolToVisibility (not state.props.locationUnserviceable)
        ] <> FontStyle.priceFont TypoGraphy
      , textView $
        [ text $ getString if Mb.isJust state.data.locationSelected then CHANGE_CITY else SELECT_CITY_STR
        , gravity CENTER
        , color state.data.config.themeColors.highlightedTextColor
        , margin $ MarginTop 16
        , onClick push $ const $ ChangeStage SELECT_CITY
        , visibility $ boolToVisibility selectCityConfig.enableChangeCity
        ] <> FontStyle.tags TypoGraphy
    ]

currentLanguageViewV2 :: forall w. ChooseCityScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
currentLanguageViewV2 state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginHorizontal 16 16
  ]
  [ textView $
    [ text $ getString SELECT_LANGUAGE
    , width WRAP_CONTENT
    , height WRAP_CONTENT
    , color Color.black900  
    , gravity LEFT
    , margin $ MarginLeft 4
    ] <> FontStyle.body3 TypoGraphy
  , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , cornerRadius 12.0
      , stroke $ "1," <> Color.grey900
      , onClick push $ const $ ChangeStage SELECT_LANG
      , background Color.white900
      , padding $ Padding 12 12 12 12
      , margin $ MarginTop 8
      ][textView $ 
          [ text $ getLangFromVal state.props.selectedLanguage
          , gravity LEFT
          , color Color.black900
          , weight 1.0
          ] <> FontStyle.subHeading1 TypoGraphy
        , imageView
          [ width $ V 20
          , height $ V 20
          , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right_black"
          ]
        ]
  ]

currentLanguageView :: forall w. ChooseCityScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
currentLanguageView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , padding $ Padding 12 12 12 12
  , margin $ MarginHorizontal 16 16
  , cornerRadius 12.0
  , background Color.white900
  , stroke $ "1," <> Color.grey900
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      ][  textView $ 
          [ text $ getString LANGUAGE_DETECTED <> ": "
          , gravity CENTER
          , color Color.black800
          , margin $ MarginTop 2
          ] <> FontStyle.body3 TypoGraphy
        , textView $ 
          [ text $ getLangFromVal state.props.selectedLanguage
          , gravity CENTER
          , color Color.black900
          ] <> FontStyle.subHeading1 TypoGraphy
        ]
    , textView $ 
      [ text $ getString CHANGE_LANGUAGE_STR <> if getLanguageLocale languageKey == "EN_US" && Mb.isJust state.data.locationSelected then " (" <> getChangeLanguageText state.data.locationSelected state.data.config <> ")" else ""
      , gravity CENTER
      , color state.data.config.themeColors.highlightedTextColor
      , onClick push $ const $ ChangeStage SELECT_LANG
      , margin $ MarginTop 16
      ] <> FontStyle.body1 TypoGraphy
  ]


languageRadioButtonView :: ChooseCityScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
languageRadioButtonView state push =
  let appName = JB.getAppName unit
      getLanguageList = RC.appLanguageConfig appName
      items = if not DA.null getLanguageList then getLanguageList else state.data.config.languageList
  in
  PrestoAnim.animationSet [ Anim.fadeIn true ] $
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  ][  headerView state push
    , textView $
      [ text $ getString SELECT_LANGUAGE_DESC
      , color Color.black800
      , margin $ Margin 16 24 16 16
      ] <> FontStyle.subHeading2 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ Margin 16 16 16 5
      , background Color.white900
      ](DA.mapWithIndex
          (\ index language ->  
          PrestoAnim.animationSet
          [ Anim.translateYAnimFromTopWithAlpha $ AnimConfig.translateYAnimMapConfig index
          ] $ MenuButton.view (push <<< MenuButtonAction) (menuButtonConfig index language state.props.radioMenuFocusedLang state)) items
      )
  ]


cityRadioButtonView :: ChooseCityScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
cityRadioButtonView state push =
  let appName = JB.getAppName unit
      selectCityConfig = RC.selectCityConfig appName
      cityList = selectCityConfig.cityNames
      items = transformCityConfig cityList
  in
  PrestoAnim.animationSet [ Anim.fadeIn true ] $
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  ][  headerView state push
    , textView $
      [ text $ getString SELECT_LOCATION_DESC
      , color Color.black800
      , margin $ Margin 16 24 16 16
      ] <> FontStyle.subHeading2 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ Margin 16 16 16 5
      , background Color.white900
      ](DA.mapWithIndex
          (\ index language ->  
          PrestoAnim.animationSet
          [ Anim.translateYAnimFromTopWithAlpha $ AnimConfig.translateYAnimMapConfig index
          ] $ MenuButton.view (push <<< MenuButtonAction) (menuButtonConfig index language state.props.radioMenuFocusedCity state )) items
      )
  ]

enableLocationPermission :: ChooseCityScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
enableLocationPermission state push = 
  let appName = Mb.fromMaybe state.data.config.appData.name $ runFn3 DU.getAnyFromWindow "appName" Mb.Nothing Mb.Just
  in 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , padding $ Padding 20 20 20 20
    , margin $ MarginTop 14
    , visibility $ boolToVisibility (state.props.currentStage == ENABLE_PERMISSION)
    , margin $ Margin 16 16 16 16
    , cornerRadius 12.0
    , background Color.white900
    , stroke $ "1," <> Color.grey900
    ][  textView $
        [ text $ getString ENABLE_LOCATION_PERMISSION
        , gravity CENTER
        , color Color.black800
        , margin $ MarginBottom 20
        ] <> FontStyle.body8 TypoGraphy
      , linearLayout 
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background Color.blue600
        , cornerRadius 8.0
        , orientation VERTICAL
        , gravity CENTER_HORIZONTAL
        ][ imageView
          [ height $ V 220
          , width $ V 220 
          , imageWithFallback $ fetchImage FF_ASSET "ny_driver_location_permission"
          ]
        ]
      , textView $
        [ text $ getString PLEASE_ENABLE_LOCATION_PERMISSION_FOR <> appName <> " from your device settings to start riding\n\n" <> "Background location access is used to get you ride requests even when your app is closed and not in use."
        , gravity CENTER
        , color Color.black700
        , margin $ MarginTop 4
        ] <> FontStyle.paragraphText TypoGraphy
    ]

mockLocationEnabledView :: forall w. (Action -> Effect Unit) -> ChooseCityScreenState -> PrestoDOM (Effect Unit) w
mockLocationEnabledView push state =
  PrestoAnim.animationSet [ Anim.fadeIn true ]
  $ relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , PP.cornerRadii $ PTD.Corners 24.0 true true false false
    , alignParentBottom "true,-1"
    , gravity BOTTOM
    ][ ErrorModal.view (push <<< ErrorModalActionController) (mockLocationConfig state)]
 

dummyView :: forall w. PrestoDOM (Effect Unit) w
dummyView = linearLayout [visibility GONE][]

transformCityConfig :: Array String -> Array MenuButton.Text
transformCityConfig cityConfig = map (\city -> {name: city, value: city, subtitle: ""}) cityConfig
