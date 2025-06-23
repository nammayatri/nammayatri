{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RateCardScreen.View where

import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (<<<), (<>), (==), (>), (<), not, void, discard, (-), show, (*), (<=), (>=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, LoggableScreen, Visibility(..), afterRender, background, color, cornerRadius, fontStyle, relativeLayout, gravity, height, alpha, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, weight, width, singleLine, id, frameLayout, scrollBarY, fillViewport, onAnimationEnd, rippleColor, shimmerFrameLayout, clickable)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.RateCardScreen.Controller (Action(..), eval, ScreenOutput)
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import Mobility.Prelude (boolToVisibility)
import Data.Number.Format (fixed, toStringWith)
import Engineering.Helpers.Commons as EHC
import Components.RateCard as RateCard
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App as CT
import Helpers.Utils as HU
import Screens.BookingOptionsScreen.ComponentConfig as BOP
import Data.Array as DA
import Data.Ord as DO
import JBridge as JB
import ConfigProvider as CP
import Services.API as API
import Constants as CS
import Data.Int as DI
import Data.Array as DA
import Debug (spy)

screen :: ST.RateCardScreenState -> LoggableScreen Action ST.RateCardScreenState ScreenOutput
screen initialState =
    { initialState
    , view: view
    , name: "RateCardScreen"
    , globalEvents: []
    , eval:
        ( \state action -> do
            let _ = spy "RateCardScreen state" state
            let _ = spy "RateCardScreen action" action
            eval state action
        ) 
  , parent : Nothing
  , logWhitelist: initialState.data.config.logWhitelistConfig.rateCardScreenLogWhitelist
    }


view :: forall w. (Action -> Effect Unit) -> ST.RateCardScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
    relativeLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , background Color.grey700
     , onBackPressed push $ const BackClick
     ] $
     [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        ]
        [ headerLayout push state
        , peakTimeView push state
        , scrollView
          [ width MATCH_PARENT
          , weight 1.0
          , scrollBarY false
          , margin $ MarginTop 16
          , fillViewport true
          ][ linearLayout
              [ width MATCH_PARENT
              , height MATCH_PARENT
              , orientation VERTICAL
              ][ linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  ] ( vehicleListView push state )
              , linearLayout[weight 1.0][]
              , rateSlider push state
              ]

          ]
        ]
    ] <> if state.props.showRateCard then [ rateCardView push state ] else []


headerLayout :: forall w. (Action -> Effect Unit) -> ST.RateCardScreenState -> PrestoDOM (Effect Unit) w
headerLayout push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.white900
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , layoutGravity "center_vertical"
        , padding $ PaddingVertical 10 10
        ]
        [ imageView
            [ width $ V 30
            , height $ V 30
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
            , gravity CENTER_VERTICAL
            , onClick push $ const BackClick
            , padding $ Padding 2 2 2 2
            , margin $ MarginLeft 5
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height MATCH_PARENT
              , text $ (HU.appName true) <> " " <> getString RATE_CARD
              , margin $ MarginLeft 20
              , color Color.black
              , weight 1.0
              , gravity CENTER_VERTICAL
              , alpha 0.8
              ]
            <> FontStyle.h3 CT.TypoGraphy
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.greyLight
        ]
        []
    ]

vehicleListView :: forall w. (Action -> Effect Unit) -> ST.RateCardScreenState -> Array ( PrestoDOM (Effect Unit) w )
vehicleListView push state =
  DA.mapWithIndex
    ( \index item ->
        linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , margin $ MarginHorizontal 16 16
          ]
          [ serviceTierItem push state item $ getActualIndex index]
    ) sortedRidePreferences
  where
    getActualIndex index = DA.length sortedRidePreferences - index - 1
    compareRidePreferences a b = DO.compare a.priority b.priority
    sortedRidePreferences = DA.sortBy compareRidePreferences state.data.ridePreferences

serviceTierItem :: forall w. (Action -> Effect Unit) -> ST.RateCardScreenState -> ST.RidePreference -> Int -> PrestoDOM (Effect Unit) w
serviceTierItem push state service index =
  relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , weight 1.0
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , padding $ Padding 12 2 12 2
        , margin $ MarginVertical 5 5
        , orientation HORIZONTAL
        , stroke $ "1," <> Color.grey900
        , background Color.white900
        , cornerRadius 8.0
        , gravity CENTER_VERTICAL
        ]
        [ imageView
            [ imageWithFallback $ HU.getVehicleVariantImage $ HU.getVehicleMapping service.serviceTierType
            , width $ V 64
            , height $ V 48
            ]
        , linearLayout
          [ weight 1.0
          , height WRAP_CONTENT
          , orientation VERTICAL
          ][ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER_VERTICAL
            , onClick push $ const $ ShowRateCard service
            ][ textView $
                [ height WRAP_CONTENT
                , text service.name
                , margin $ MarginHorizontal 12 2
                , color Color.black800
                , singleLine true
                ] <> FontStyle.body25 CT.TypoGraphy
              , imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_grey"
                , width $ V 12
                , height $ V 12
                , visibility $ boolToVisibility $ isJust service.rateCardData
                ]
            ]
          ]
        , relativeLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , padding $ PaddingVertical 12 12
          , orientation VERTICAL
          , gravity RIGHT
          ][  PrestoAnim.animationSet [ Anim.fadeIn $ not state.props.sliderLoading ] $
              linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , gravity RIGHT
              ] [ textView $
                  [ height WRAP_CONTENT
                  , text primaryText
                  , color primaryTextColor
                  ] <> FontStyle.h2 CT.TypoGraphy
              , textView $
                [ height WRAP_CONTENT
                , text secondaryText
                , color secondaryTextColor
                , margin $ MarginTop 3
                ]  <> FontStyle.body3 CT.TypoGraphy
              ]
            , linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , gravity RIGHT
              , background Color.white900
              , visibility $ boolToVisibility $ state.props.sliderLoading
              ] [ shimmerFrameLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , cornerRadius 8.0
                    , background Color.grey900
                    , padding $ PaddingHorizontal 5 5
                    ][  textView $
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , text primaryText
                        , visibility INVISIBLE
                        ] <> FontStyle.h2 CT.TypoGraphy
                    ]
                , shimmerFrameLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , cornerRadius 4.0
                    , background Color.grey900
                    , padding $ PaddingHorizontal 5 5
                    , margin $ MarginTop 3
                    ][  textView $
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , text secondaryText
                        , visibility INVISIBLE
                        ]  <> FontStyle.body3 CT.TypoGraphy
                    ]
              ]
          ]
        ]
    ]
    where primaryTextColor = if peakTime then Color.green900 else Color.black800
          secondaryTextColor = if peakTime then Color.green900 else Color.black600
          curr = CP.getCurrency CS.appConfig
          primaryText = curr <> show (DI.round $  (DI.toNumber state.props.sliderVal) * (fromMaybe 0.0 service.perKmRate))
          secondaryText = curr <> (toStringWith (fixed 2) $ fromMaybe 0.0 service.perKmRate) <> "/km"
          peakTime = service.farePolicyHour == Just API.Peak

peakTimeView :: forall w. (Action -> Effect Unit) -> ST.RateCardScreenState -> PrestoDOM (Effect Unit) w
peakTimeView push state =
  PrestoAnim.animationSet [ Anim.fadeIn true ] $
    relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , visibility $ boolToVisibility peakTime
    ][ textView $
        [ text $ "↑  " <> getString HIGHEST_EARNING_PEAK_TIME
        , height WRAP_CONTENT 
        , color Color.white900
        , background Color.green900
        , padding $ Padding 16 8 16 8
        , width MATCH_PARENT
        , gravity CENTER
        ] <> FontStyle.body1 CT.TypoGraphy
      , shimmerFrameLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , alpha 0.2
        ][ textView $
            [ text ""
            , height WRAP_CONTENT 
            , color Color.white900
            , padding $ Padding 16 8 16 8
            , width MATCH_PARENT
            , gravity CENTER
            ]
          ]
    ] 
  where peakTime = isJust $ DA.find (\item -> item.farePolicyHour == Just API.Peak) state.data.ridePreferences

rateSlider :: forall w. (Action -> Effect Unit) -> ST.RateCardScreenState -> PrestoDOM (Effect Unit) w
rateSlider push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , cornerRadius 12.0
    , background Color.white900
    , gravity CENTER
    , stroke $ "1," <> Color.grey900
    , padding $ PaddingVertical 16 16
    ][  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , cornerRadius 12.0
        , background Color.blue600
        , gravity CENTER
        , padding $ Padding 16 16 16 16
        , margin $ MarginHorizontal 16 16
        ][  textView
            $ [ text $ getString CHOOSE_RIDE_DIST
              , color Color.black700
              ]
            <> FontStyle.body1 CT.TypoGraphy
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            , margin $ MarginHorizontal 10 10
            ]
            [ imageView
                $ [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_minus_dec"
                  , width $ V 32
                  , height $ V 32
                  , stroke $ "1," <> Color.grey900
                  , onClick push $ const $ ChangeSlider false
                  , cornerRadius 24.0
                  , alpha decButtonAlpha
                  , clickable decButtonEnabled
                  ] <> if decButtonEnabled then [rippleColor Color.rippleShade] else []
            , textView
                $ [ text $ show state.props.sliderVal <> " km"
                  , color Color.black800
                  , gravity CENTER
                  , weight 1.0
                  ]
                <> FontStyle.priceFont CT.TypoGraphy
            , imageView
                $ [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_plus_inc"
                  , width $ V 32
                  , height $ V 32
                  , stroke $ "1," <> Color.grey900
                  , onClick push $ const $ ChangeSlider true
                  , cornerRadius 24.0
                  , alpha incButtonAlpha
                  , clickable incButtonEnabled
                  ] <> if incButtonEnabled then [rippleColor Color.rippleShade] else []
            ]
          , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , id $ EHC.getNewIDWithTag "RateSliderTool"
              ][]
          , PrestoAnim.animationSet [ Anim.triggerOnAnimationEnd true ]
            $ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , id $ EHC.getNewIDWithTag "RateSlider"
                , onAnimationEnd ( \action -> void $ JB.renderSlider 
                    ( \sliderAction -> do
                        void $ pure $ JB.updateInputString "input"
                        void $ JB.debounceFunction 800 push DebounceCallBack false
                        void $ push sliderAction
                        pure unit
                    )  
                    SliderCallback sliderConfig ) (const AfterRender)
                ][]
          , textView
            $ [ text $ getString RATES_CHANGE_AS_THE_DIST
              , color Color.black700
              ]
            <> FontStyle.body3 CT.TypoGraphy
          , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , gravity CENTER
            , margin $ MarginTop 4
            , gravity CENTER 
            , onClick push $ const $ OpenLink state.data.cityConfig.rateCardConfig.learnMoreVideoLink
            , visibility $ boolToVisibility $ state.data.cityConfig.rateCardConfig.showLearnMore
            ][  imageView
                [ width $ V 32
                , height $ V 32
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_youtube"
                , rippleColor Color.rippleShade
                , padding $ Padding 0 5 5 5
                ]
              , textView $
                [ text $ getString LEARN_MORE
                , color Color.blue900
                , rippleColor Color.rippleShade
                ] <> FontStyle.body3 CT.TypoGraphy

            ]
        ]
      , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
    ]
    where incButtonAlpha = if incButtonEnabled then 1.0 else 0.4
          decButtonAlpha = if decButtonEnabled then 1.0 else 0.4
          incButtonEnabled = state.props.sliderVal < state.props.sliderMaxValue
          decButtonEnabled = state.props.sliderVal > state.props.sliderMinValue
          sliderConfig = 
            JB.sliderConfig { 
              id= EHC.getNewIDWithTag "RateSlider", 
              toolTipId = EHC.getNewIDWithTag "RateSliderTool",
              sliderMinValue = state.props.sliderMinValue,
              sliderMaxValue = state.props.sliderMaxValue,
              sliderDefaultValue = state.props.sliderDefVal,
              stepFunctionForCoinConversion = state.props.incrementUnit,
              enableToolTip = false,
              getCallbackOnProgressChanged = true,
              thumbColor = Color.blue800,
              bgColor = Color.grey900,
              progressColor = Color.blue800,
              bgAlpha = 1000
              }

rateCardView :: forall w. (Action -> Effect Unit) -> ST.RateCardScreenState -> PrestoDOM (Effect Unit) w
rateCardView push state =
  PrestoAnim.animationSet [ Anim.fadeIn true ]
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        ]
        [ RateCard.view (push <<< RateCardAction) (BOP.rateCardConfig state.data.rateCard state.data.config.rateCardScreen.showTollCharges state.data.config.rateCardScreen.showDriverAdditions) ]

primaryButtonConfig :: ST.RateCardScreenState -> PrimaryButton.Config 
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig
      { text = getString VIEW_BOOKING_PREF
      , color = Color.primaryButtonColor
      }
      , margin = Margin 16 16 16 0
      , background = Color.black900
      , height = V 54
      , id = "ViewBookingPreferencesButton"
      }
  in primaryButtonConfig'