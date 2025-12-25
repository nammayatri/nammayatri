{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.CancellationRateScreen.View where

import Prelude
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.ComplaintsModel as ComplaintsModel
import Components.PopUpModal as PopUpModal
import Data.Maybe (Maybe(..),fromMaybe)
import Data.Function.Uncurried (runFn3)
import Effect (Effect)
import Font.Style as FontStyle
import Styles.Types
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, ($), (<>), (==), bind, pure, unit, (<<<))
import PrestoDOM (Gravity(..), textSize, Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, LoggableScreen, Visibility(..), afterRender, background, color, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, text, textView, visibility, weight, width, cornerRadius, rippleColor, rotation)
import Screens.CancellationRateScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore)
import DecodeUtil (getAnyFromWindow)
import Styles.Colors as Color
import ConfigProvider
import Mobility.Prelude
import Data.Maybe
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Commons (screenWidth)
import Font.Size as FontSize
import Data.Int (round, toNumber)
import RemoteConfig.Types (CancellationRateConfig, CancellationRateEntity(..))
import RemoteConfig.Utils (reduceCancellationRate)
import Data.String as DS
import Locale.Utils(getLanguageLocale)
import Constants (languageKey)
import Debug (spy)

screen :: ST.CancellationRateScreenState -> LoggableScreen Action ST.CancellationRateScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "CancellationRateScreen"
  , globalEvents: []
  , eval
  , parent : Nothing
  , logWhitelist : initialState.appConfig.logWhitelistConfig.cancellationRateScreenLogWhitelist
  }

view :: forall w. (Action -> Effect Unit) -> ST.CancellationRateScreenState -> PrestoDOM (Effect Unit) w
view push state =
  let configs = reduceCancellationRate $ "cancellation_rate_trivia" <> (getLanguage $ getLanguageLocale languageKey)
  in
    Anim.screenAnimation $ linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , onBackPressed push $ const BackPressed
    ]
    [ headerLayout state push
    , scrollView
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , scrollBarY false
      ]
      [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin $ MarginHorizontal 16 16
        ]
        [ cancellationRateGauge state push
        , cancellationRateData state push
        , cancellationRateReduceTrivia state push configs
        , cancellationRateDownsidesTrivia state push configs
        ]
      ]
    ]
  where
    getLanguage lang =
      let language = DS.toLower $ DS.take 2 lang
      in if not (DS.null language) then "_" <> language else "_en"

headerLayout :: forall w. ST.CancellationRateScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerLayout state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , layoutGravity "center_vertical"
        , padding $ Padding 5 12 5 12
        ]
        [ imageView
            [ width $ V 40
            , height $ V 40
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_back"
            , gravity CENTER_VERTICAL
            , onClick push $ const BackPressed
            , padding $ Padding 10 10 10 10
            , margin $ MarginLeft 5
            , cornerRadius 20.0
            , rippleColor Color.rippleShade
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height MATCH_PARENT
              , text $ getString RIDE_CANCELLATION_RATE
              , margin $ MarginLeft 10
              , color Color.black
              , weight 1.0
              , gravity CENTER_VERTICAL
              ]
            <> FontStyle.h3 TypoGraphy
        ]
    , horizontalLine
    ]

cancellationRateGauge :: forall w. ST.CancellationRateScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
cancellationRateGauge state push =
   linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.aliceBlueLight
    , cornerRadius 24.0
    , margin $ MarginTop 20
    , padding $ Padding 16 16 16 16
    ]
    [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      ]
      [
        relativeLayout
        [ height $ V 103
        , width $ V $ ((screenWidth unit)/ 2) - 32
        , gravity CENTER
        ][
          imageView
          [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_gauge_image"
          , width $ V $ ((screenWidth unit)/ 2) - 32
          , height $ V 100
          , gravity LEFT
          ]
        , linearLayout
          [ gravity CENTER
          , width MATCH_PARENT
          ][
            imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_gauge_tip"
            , width $ V $ round (((toNumber (screenWidth unit - 32)/ 2.0) ) * (11.0 / 75.0))
            , height $ V 120
            , rotation (((toNumber state.data.cancellationRate) * 1.8) - 90.0)
            , margin $ MarginTop 35
            ]
          ]
          ]
      , linearLayout
        [ height MATCH_PARENT
        , weight 1.0
        , orientation VERTICAL
        , gravity CENTER
        ] [ textView $
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text $ show state.data.cancellationRate <> "%"
            , color Color.black900
            , padding $ PaddingBottom 4
            , gravity CENTER
            ] <> FontStyle.h0 LanguageStyle
          , textView $
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text (getString CANCELLATION_RATE)
            , color Color.black700
            , padding $ PaddingHorizontal 11 11
            , gravity CENTER
            ] <> FontStyle.paragraphText LanguageStyle
          , textView $
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text (getString $ LAST_N_DAYS $ show (fromMaybe 0 state.data.cancellationWindow))
            , color Color.black700
            , gravity CENTER
            , visibility $ boolToVisibility (isJust state.data.cancellationWindow)
            ] <> FontStyle.paragraphText LanguageStyle
        ]
      ]
    ]

cancellationRateData :: forall w. ST.CancellationRateScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
cancellationRateData state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 20
    ]
  [ textView $
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , text $ getString LIFETIME_STATS
      , color Color.black900
      , margin $ MarginBottom 12
      ] <> FontStyle.body7 TypoGraphy
  , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , background Color.aliceBlueLight
      , cornerRadius 10.0
      ]
      [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ]
        [ infoTileView state { primaryText: show state.data.cancelledRides, secondaryText: "/" <> show state.data.assignedRides, subText: getString TOTAL_RIDES_CANCELLED, margin: Margin 0 0 0 0 }
        , linearLayout
            [ height MATCH_PARENT
            , width $ V 1
            , margin $ MarginVertical 16 16
            , background Color.lightGreyShade
            ]
            []
        , infoTileView state { primaryText: "₹ " <> EHC.formatCurrencyWithCommas (show state.data.missedEarnings), secondaryText: "", subText: getString TOTAL_EARNINGS_MISSED, margin: Margin 0 0 0 0 }
        ]
      ]
    ]

infoTileView :: forall w. ST.CancellationRateScreenState -> { primaryText :: String, secondaryText :: String, subText :: String, margin :: Margin } -> PrestoDOM (Effect Unit) w
infoTileView state config =
  linearLayout
    [ weight 1.0
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ config.margin
    , background Color.blue600
    , padding $ Padding 16 16 16 16
    ]
    [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , gravity BOTTOM
      ]
      [ textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text config.primaryText
        , margin (MarginLeft 7)
        , color Color.black900
        ] <> FontStyle.h0 LanguageStyle
        , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text config.secondaryText
        , color Color.black700
        ] <> FontStyle.body23 LanguageStyle
      ]
    , textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text config.subText
      , margin $ MarginLeft 7
      , color Color.black700
      ] <> FontStyle.paragraphText TypoGraphy
    ]

cancellationRateReduceTrivia :: forall w. ST.CancellationRateScreenState -> (Action -> Effect Unit) -> Array CancellationRateConfig -> PrestoDOM (Effect Unit) w
cancellationRateReduceTrivia state push configs =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 32
    ] $
    [ textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ getString CANCELLATION_RATE_TRIVIA
      , color Color.black900
      ] <> FontStyle.h3 TypoGraphy
    ] <> map (makePoints state push HOW_TO_REDUCE) configs

cancellationRateDownsidesTrivia :: forall w. ST.CancellationRateScreenState -> (Action -> Effect Unit) -> Array CancellationRateConfig -> PrestoDOM (Effect Unit) w
cancellationRateDownsidesTrivia state push configs =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginVertical 32 40
    ] $
    [ textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ getString CANCELLATION_RATE_TRIVIA_2
      , color Color.black900
      ] <> FontStyle.h3 TypoGraphy
    ] <> map (makePoints state push WHAT_HAPPENS) configs

makePoints :: forall w. ST.CancellationRateScreenState -> (Action -> Effect Unit) -> CancellationRateEntity -> CancellationRateConfig -> PrestoDOM (Effect Unit) w
makePoints state push entityType config =
  linearLayout
    [ margin $ MarginTop 12
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , visibility $ boolToVisibility (entityType == config.entity_type)
    ]
    [ imageView
      [ width $ V 16
      , height $ V 18
      , margin $ MarginTop 2
      , imageWithFallback $ fetchImage FF_ASSET if (entityType == HOW_TO_REDUCE) then "ny_ic_check_circle_green" else "ny_ic_exclamation_circle"
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , padding $ PaddingLeft 12
      ]
      [ textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , text config.title
        , color Color.black800
        ] <> FontStyle.body1 TypoGraphy
      , textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , text config.description
        , color Color.black700
        , margin $ MarginTop 6
        ] <> FontStyle.paragraphText TypoGraphy
      ]
    ]

horizontalLine :: forall w. PrestoDOM (Effect Unit) w
horizontalLine =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.greyBackDarkColor
    ]
    []
