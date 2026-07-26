module Screens.MeterRideScreen.View
  ( distAndTimeView
  , driverProfile
  , enterDestinationView
  , fareView
  , getActiveRide
  , rideInitView
  , rideStartedView
  , screen
  , sevenSegmentView
  , sliderView
  , stopMeterView
  ) where

import Animation as Anim
import Common.Types.App
import Data.String (length)
import Data.String as DS
import Data.String.Unsafe (charAt)
import Effect.Uncurried (runEffectFn1)
import Data.Int (toNumber, round)
import Data.Either
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge as JB
import Mobility.Prelude
import Prelude
import Presto.Core.Types.Language.Flow (Flow, delay, doAff)
import PrestoDOM
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Events (onBackPressed, onClick)
import PrestoDOM.Properties (rippleColor, textSize, fontStyle, letterSpacing, width, height, orientation, visibility, margin, padding, text, color, background, cornerRadius, imageWithFallback, weight, gravity, cornerRadii)
import PrestoDOM.Types.DomAttributes (Gravity(..), Length(..), Orientation(..))
import PrestoDOM.Types.Core (LetterSpacing(..))
import Styles.Colors as Color
import Screens.BookingOptionsScreen.ComponentConfig as BOP
import Screens.MeterRideScreen.Controller (Action(..), ScreenOutput(..), eval)
import Screens.Types
import Services.Backend as Remote
import Services.API
import Debug
import Types.ModifyScreenState (modifyScreenState)
import Types.App (defaultGlobalState, GlobalState(..))
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))
import Data.Array (head)
import Data.Array (length) as DA
import Types.App (ScreenType(..), FlowBT)
import Language.Strings (getString)
import Control.Monad.Except.Trans (runExceptT)
import Language.Types (STR(..))
import Engineering.Helpers.Commons as EHC
import Components.TripStageTopBar as TripStageTopBar
import Screens.HomeScreen.ScreenData (dummyRideData)
import Data.Maybe
import Effect.Aff
import Timers (waitingCountdownTimerV2, clearTimerWithIdEffect)
import Control.Transformers.Back.Trans (runBackT)
import Effect.Class (liftEffect)
import Control.Monad.Trans.Class (lift)
import Helpers.API as HelperAPI
import Data.Function.Uncurried
import Helpers.Utils
import Animation.Config as AnimConfig

screen :: MeterRideScreenState -> Screen Action MeterRideScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "MeterRideScreen"
  , globalEvents:
      [ ( \push -> do
            if initialState.props.rateCardConfig.ratePerKM == 0.0 then do
              fiber <-
                launchAff $ EHC.flowRunner defaultGlobalState
                  $ do
                      response <- HelperAPI.callApi $ GetDriverRateCardReq Nothing (Just initialState.props.rateCardConfig.sliderDefVal)
                      case response of
                        Left _ -> pure unit
                        Right (GetDriverRateCardRes resp) -> case (head resp) of
                          Nothing -> pure unit
                          Just res -> do
                            EHC.liftFlow $ push $ UpdateRateCard res
                            pure unit
              pure $ launchAff_ $ killFiber (error "Failed to Cancel") fiber
            else
              pure $ pure $ unit
        )
      , ( \push -> do
            if isNothing initialState.data.ridesInfo then do
              fiber <- launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ getActiveRide 20 2000.0 push initialState
              pure $ launchAff_ $ killFiber (error "Failed to Cancel") fiber
            else
              pure $ pure $ unit
        )
      , ( \push -> do
            case initialState.data.ridesInfo of
              Just (RidesInfo resp) -> do
                void $ fetchAndUpdateLocationUpdateServiceVars "ride_started" true "MeterRide"
                fiber <- launchAff $ EHC.flowRunner defaultGlobalState $ getPrice push resp.id
                pure $ launchAff_ $ killFiber (error "Failed to Cancel") fiber
              Nothing -> pure $ pure unit
        )
      , ( \push -> do
            case initialState.data.ridesInfo of
              Just (RidesInfo resp) -> do
                case resp.tripStartTime of
                  Just time -> do
                    let
                      currentTime = (EHC.getCurrentUTC "")
                    let
                      diffSec = runFn2 JB.differenceBetweenTwoUTC currentTime time
                    void $ waitingCountdownTimerV2 (if diffSec > 0 then diffSec else 0) "1" "MeterRideStartedTimer" push MeterRideStartedTimerCB
                  Nothing -> pure unit
                pure $ runEffectFn1 clearTimerWithIdEffect "MeterRideStartedTimer"
              Nothing -> pure $ pure unit
        )
      ]
  , eval:
      ( \action state -> do
          let
            _ = spy "MeterRideScree state -----" state
          let
            _ = spy "MeterRideScree action -----" action
          eval action state
      )
  }

getPrice push rideId = do
  response <- Remote.getMeterPrice rideId
  case response of
    Left _ -> pure unit
    Right resp -> EHC.liftFlow $ push $ UpdateFare resp
  delay $ Milliseconds 3000.0
  getPrice push rideId

view :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ relativeLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , onBackPressed push $ const BackPressed
        ]
    $ [ Keyed.linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , background Color.white900
          ]
          [ if state.props.isMeterRideStarted then Tuple "rideStartedView" $ rideStartedView push state else Tuple "rideInitView" $ rideInitView push state
          ]
      , rateCardView push state
      ]

rateCardView :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
rateCardView push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blackLessTrans
    , gravity CENTER
    , onClick push $ const CloseRateCard
    , visibility $ boolToVisibility state.props.showRateCard
    ]
    [ PrestoAnim.animationSet
        [ PrestoAnim.Animation
            [ PrestoAnim.duration 100
            , PrestoAnim.fromScaleY 0.0
            , PrestoAnim.toScaleY 1.0
            , PrestoAnim.fromScaleX 0.0
            , PrestoAnim.toScaleX 1.0
            ]
            state.props.showRateCard
        ]
        $ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , cornerRadius 15.0
            , margin $ Margin 16 0 16 0
            , padding $ Padding 10 10 10 10
            , orientation VERTICAL
            , onClick push $ const NoAction
            , background Color.blue600
            ]
            [ textView
                $ [ text $ getString RATE_CARD
                  , height WRAP_CONTENT
                  , width MATCH_PARENT
                  , color Color.black800
                  , gravity CENTER
                  ]
                <> FontStyle.h2 TypoGraphy
            , linearLayout
                [ margin $ Margin 16 16 16 0
                , orientation VERTICAL
                , background Color.white900
                , width MATCH_PARENT
                , height WRAP_CONTENT
                , gravity CENTER
                , cornerRadius 12.0
                ]
                [ textView
                    $ [ text $ getString AUTO_RICKSHAW
                      , height WRAP_CONTENT
                      , width MATCH_PARENT
                      , color Color.black800
                      , gravity CENTER
                      , margin $ Margin 10 10 10 6
                      ]
                    <> FontStyle.subHeading1 TypoGraphy
                , imageView
                    $ [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_auto_right_view"
                      , width $ V 81
                      , height $ V 81
                      , margin $ Margin 10 6 10 0
                      , gravity CENTER
                      ]
                , relativeLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    ]
                    [ PrestoAnim.animationSet
                        [ Anim.fadeIn $ state.props.isRateCardLoading
                        , Anim.fadeOut $ not state.props.isRateCardLoading
                        ]
                        $ linearLayout
                            [ height WRAP_CONTENT
                            , width MATCH_PARENT
                            , orientation VERTICAL
                            , gravity CENTER
                            ]
                            [ shimmerFrameLayout
                                [ height WRAP_CONTENT
                                , width WRAP_CONTENT
                                , cornerRadius 8.0
                                , background Color.grey900
                                , margin $ Margin 10 0 10 2
                                ]
                                [ textView
                                    $ [ height WRAP_CONTENT
                                      , width MATCH_PARENT
                                      , text $ "₹" <> show (state.props.rateCardConfig.sliderFare)
                                      , visibility INVISIBLE
                                      ]
                                    <> FontStyle.priceFont TypoGraphy
                                ]
                            , shimmerFrameLayout
                                [ height WRAP_CONTENT
                                , width WRAP_CONTENT
                                , cornerRadius 4.0
                                , background Color.grey900
                                , margin $ Margin 10 3 10 20
                                ]
                                [ textView
                                    $ [ height WRAP_CONTENT
                                      , width MATCH_PARENT
                                      , text $ "₹" <> show state.props.rateCardConfig.ratePerKM <> "/km"
                                      , visibility INVISIBLE
                                      ]
                                    <> FontStyle.body3 TypoGraphy
                                ]
                            ]
                    , PrestoAnim.animationSet
                        [ Anim.fadeIn $ not state.props.isRateCardLoading
                        , Anim.fadeOut state.props.isRateCardLoading
                        ]
                        $ linearLayout
                            [ height WRAP_CONTENT
                            , width MATCH_PARENT
                            , orientation VERTICAL
                            ]
                            [ textView
                                $ [ text $ "₹" <> show (state.props.rateCardConfig.sliderFare)
                                  , height WRAP_CONTENT
                                  , width MATCH_PARENT
                                  , color Color.black800
                                  , margin $ Margin 10 0 10 2
                                  , gravity CENTER
                                  ]
                                <> FontStyle.priceFont TypoGraphy
                            , textView
                                $ [ text $ "₹" <> show state.props.rateCardConfig.ratePerKM <> "/km"
                                  , height WRAP_CONTENT
                                  , width MATCH_PARENT
                                  , color Color.black500
                                  , margin $ Margin 10 0 10 20
                                  , gravity CENTER
                                  ]
                                <> FontStyle.body23 TypoGraphy
                            ]
                    ]
                ]
            , sliderView push state
            , textView
                $ [ text $ getString GOT_IT
                  , height WRAP_CONTENT
                  , width MATCH_PARENT
                  , color Color.blue800
                  , gravity CENTER
                  , margin $ Margin 16 16 16 16
                  , onClick push $ const CloseRateCard
                  ]
                <> FontStyle.h2 TypoGraphy
            ]
    ]

sliderView :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
sliderView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , cornerRadius 12.0
    , background Color.white900
    , gravity CENTER
    , padding $ Padding 16 16 16 16
    , margin $ Margin 16 16 16 0
    ]
    [ textView
        $ [ text $ getString CHOOSE_RIDE_DIST
          , color Color.black700
          ]
        <> FontStyle.body1 TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , margin $ Margin 5 5 5 5
        ]
        [ imageView
            $ [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_min_blue"
              , width $ V 40
              , height $ V 40
              , stroke $ "1," <> Color.grey900
              , onClick push $ const $ ChangeSlider false
              , cornerRadius 24.0
              , alpha decButtonAlpha
              , clickable decButtonEnabled
              , gravity CENTER
              , margin $ Margin 0 5 15 5
              ]
            <> if decButtonEnabled then [ rippleColor Color.rippleShade ] else []
        , textView
            $ [ text $ show state.props.rateCardConfig.sliderVal <> " km"
              , color Color.black800
              , gravity CENTER
              , margin $ Margin 0 0 0 5
              ]
            <> FontStyle.priceFont TypoGraphy
        , imageView
            $ [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_plus_blue"
              , width $ V 40
              , height $ V 40
              , stroke $ "1," <> Color.grey900
              , onClick push $ const $ ChangeSlider true
              , cornerRadius 24.0
              , alpha incButtonAlpha
              , clickable incButtonEnabled
              , margin $ Margin 15 5 0 5
              , gravity CENTER
              ]
            <> if incButtonEnabled then [ rippleColor Color.rippleShade ] else []
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 5 5 5 5
        , id $ EHC.getNewIDWithTag "RateSliderTool"
        ]
        []
    , PrestoAnim.animationSet [ Anim.triggerOnAnimationEnd true ]
        $ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , id $ EHC.getNewIDWithTag "RateSlider"
            , margin $ Margin 5 5 5 5
            , onAnimationEnd
                ( \action ->
                    void
                      $ JB.renderSlider
                          ( \sliderAction -> do
                              void $ pure $ JB.updateInputString "MeterRideInput"
                              push sliderAction
                              void $ JB.debounceFunction 800 push DebounceCallBack false
                              pure unit
                          )
                          SliderCallback
                          sliderConfig
                )
                (const NoAction)
            ]
            []
    , textView
        $ [ text $ getString RATE_CHANGES_AS_THE_DISTANCE_CHANGES
          , color Color.black700
          , margin $ Margin 10 10 10 5
          ]
        <> FontStyle.body3 TypoGraphy
    ]
  where
  incButtonAlpha = if incButtonEnabled then 1.0 else 0.4

  decButtonAlpha = if decButtonEnabled then 1.0 else 0.4

  incButtonEnabled = state.props.rateCardConfig.sliderVal < state.props.rateCardConfig.sliderMaxValue

  decButtonEnabled = state.props.rateCardConfig.sliderVal > state.props.rateCardConfig.sliderMinValue

  sliderConfig =
    JB.sliderConfig
      { id = EHC.getNewIDWithTag "RateSlider"
      , toolTipId = EHC.getNewIDWithTag "RateSliderTool"
      , sliderMinValue = state.props.rateCardConfig.sliderMinValue
      , sliderMaxValue = state.props.rateCardConfig.sliderMaxValue
      , sliderDefaultValue = state.props.rateCardConfig.sliderDefVal
      , stepFunctionForCoinConversion = state.props.rateCardConfig.incrementUnit
      , enableToolTip = false
      , getCallbackOnProgressChanged = true
      , thumbColor = Color.blue800
      , bgColor = Color.grey900
      , progressColor = Color.blue800
      , bgAlpha = 1000
      }

rideInitView :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
rideInitView push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , onBackPressed push $ const BackPressed
    , margin $ Margin 26 24 26 0
    , orientation VERTICAL
    ]
    [ topBarView push state
    , fareView push state
    , startButtonView push state
    ]
  where
  topBarView :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
  topBarView push state =
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      ]
      [ linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , background Color.blue600
          , padding $ Padding 12 12 12 12
          , cornerRadius 32.0
          , onClick push $ const BackPressed
          , gravity CENTER
          ]
          [ imageView
              [ width $ V 24
              , height $ V 24
              , alpha 0.5
              , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
              ]
          , textView
              $ [ text $ DS.trim $ getString BACK
                , color Color.black700
                , margin $ MarginLeft 5
                , padding $ PaddingBottom 4
                ]
              <> FontStyle.body7 TypoGraphy
          ]
      , linearLayout
          [ height WRAP_CONTENT
          , weight 1.0
          ]
          []
      , imageView
          [ width $ V 101
          , height $ V 35
          , margin $ Margin 0 4 2 0
          , color Color.black700
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_logo_dark"
          ]
      ]

  startButtonView :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
  startButtonView push state =
    linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER
      ]
      [ relativeLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , gravity CENTER
          ]
          $ [ linearLayout
                [ width $ V 216
                , height $ V 216
                , cornerRadius 999.0
                , background Color.lightHulkGreen
                ]
                [ linearLayout
                    [ width $ V 108
                    , height $ V 216
                    , orientation VERTICAL
                    ]
                    [ linearLayout
                        [ width $ V 108
                        , height $ V 108
                        , cornerRadii $ Corners 999.0 true false false false
                        , background Color.darkHulkGreen
                        , visibility $ boolToInvisibility $ not $ state.props.startButtonCountDown > 1
                        ]
                        []
                    , linearLayout
                        [ width $ V 108
                        , height $ V 108
                        , cornerRadii $ Corners 999.0 false false false true
                        , background Color.darkHulkGreen
                        , visibility $ boolToInvisibility $ not $ state.props.startButtonCountDown > 2
                        ]
                        []
                    ]
                , linearLayout
                    [ width $ V 108
                    , height $ V 216
                    , orientation VERTICAL
                    ]
                    [ linearLayout
                        [ width $ V 108
                        , height $ V 108
                        , cornerRadii $ Corners 999.0 false true false false
                        , background Color.darkHulkGreen
                        , visibility $ boolToInvisibility $ not $ state.props.startButtonCountDown > 0
                        ]
                        []
                    , linearLayout
                        [ width $ V 108
                        , height $ V 108
                        , cornerRadii $ Corners 999.0 false false true false
                        , background Color.darkHulkGreen
                        , visibility $ boolToInvisibility $ not $ state.props.startButtonCountDown > 3
                        ]
                        []
                    ]
                ]
            , if state.props.rideStartingLoader then
                relativeLayout
                  [ width $ V 216
                  , height $ V 216
                  , gravity CENTER
                  ]
                  [ progressBar
                      [ width $ V 109
                      , height $ V 108
                      , progressBarColor Color.white900
                      ]
                  ]
              else
                Keyed.linearLayout
                  [ width $ V 216
                  , height $ V 216
                  , gravity CENTER
                  , cornerRadius 9999.0
                  , layoutGravity "center"
                  , onClick push $ const HandleStartButton
                  ]
                  [ if state.props.startButtonCountDown > 3 then Tuple "startPressView" $ startPressView push state else Tuple "startCounterView" $ startCounterView push state
                  ]
            ]
      ]

  startPressView :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
  startPressView push state =
    PrestoAnim.animationSet [ Anim.fadeIn true ]
      $ linearLayout
          [ orientation VERTICAL
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          ]
          [ textView
              $ [ text $ getString START'
                , height WRAP_CONTENT
                , color Color.white900
                , letterSpacing $ PX 1.0
                ]
              <> FontStyle.title3 TypoGraphy
          , textView
              $ [ text $ getString NAMMANMETER
                , gravity CENTER
                , height WRAP_CONTENT
                , color Color.white900
                , alpha 1.0
                ]
              <> FontStyle.title4 TypoGraphy
          ]

  startCounterView :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
  startCounterView push state =
    PrestoAnim.animationSet [ Anim.fadeIn true ]
      $ linearLayout
          [ orientation VERTICAL
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          ]
          [ textView
              $ [ text $ show state.props.startButtonCountDown
                , height WRAP_CONTENT
                , color Color.white900
                , letterSpacing $ PX 1.0
                ]
              <> FontStyle.title5 TypoGraphy
          , textView
              $ [ text $ getString PRESS_TO_CANCEL
                , gravity CENTER
                , height WRAP_CONTENT
                , color Color.white900
                , alpha 0.4
                ]
              <> FontStyle.subHeading2 TypoGraphy
          ]

rideStartedView :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
rideStartedView push state =
  relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ]
    $ [ linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , orientation VERTICAL
          ]
          [ linearLayout
              [ background Color.white900
              ]
              [ driverProfile push state
              , TripStageTopBar.view (push <<< TripStageTopBarAC) tripStageTopBarConfig
              ]
          , linearLayout
              [ width MATCH_PARENT
              , margin $ Margin 26 10 26 0
              , weight 1.0
              , orientation VERTICAL
              ]
              [ fareView push state
              , distAndTimeView push state
              , linearLayout
                  [ gravity BOTTOM
                  , weight 1.0
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  ]
                  [ enterDestinationView push state
                  , stopMeterView push state
                  ]
              ]
          ]
      ]
    <> if state.props.confirmMeterRideStop then
        [ PrestoAnim.animationSet
            [ PrestoAnim.Animation
                [ PrestoAnim.duration 150
                , PrestoAnim.fromAlpha 0.0
                , PrestoAnim.toAlpha 1.0
                ]
                true
            ]
            $ linearLayout
                [ width MATCH_PARENT
                , height MATCH_PARENT
                , background Color.blackLessTrans
                , gravity BOTTOM
                , padding $ Padding 24 24 24 24
                , orientation VERTICAL
                , onClick push $ const HideStopMeterRideConfirmCard
                , visibility $ boolToVisibility state.props.confirmMeterRideStop
                ]
                [ PrestoAnim.animationSet
                    [ PrestoAnim.Animation
                        [ PrestoAnim.duration 150
                        , PrestoAnim.fromY 200
                        , PrestoAnim.toY 0
                        ]
                        true
                    ]
                    $ linearLayout
                        [ width MATCH_PARENT
                        , height WRAP_CONTENT
                        , gravity CENTER
                        , padding $ Padding 16 16 16 16
                        , background Color.white900
                        , orientation VERTICAL
                        , cornerRadius 15.0
                        , clickable true
                        ]
                        [ textView
                            $ [ text $ getString CONFIRM_METER_STOP
                              , height WRAP_CONTENT
                              , width MATCH_PARENT
                              , color "#454545"
                              , gravity CENTER
                              , clickable true
                              ]
                            <> FontStyle.h2 TypoGraphy
                        , linearLayout
                            [ cornerRadius 8.0
                            , background Color.gunMetalBlue
                            , width MATCH_PARENT
                            , height WRAP_CONTENT
                            , gravity CENTER
                            , margin $ Margin 0 24 0 0
                            , padding $ Padding 0 14 0 14
                            , rippleColor Color.rippleShade
                            , onClick push $ const EndRide
                            ]
                            [ textView
                                $ [ text $ getString CONFIRM
                                  , height WRAP_CONTENT
                                  , width MATCH_PARENT
                                  , color Color.yellow900
                                  , gravity CENTER
                                  ]
                                <> FontStyle.subHeading3 TypoGraphy
                            ]
                        , textView
                            $ [ text $ getString CANCEL
                              , height WRAP_CONTENT
                              , width MATCH_PARENT
                              , margin $ Margin 0 24 0 0
                              , color "#454545"
                              , gravity CENTER
                              , onClick push $ const HideStopMeterRideConfirmCard
                              ]
                            <> FontStyle.subHeading3 TypoGraphy
                        ]
                ]
        ]
      else
        []
  where
  config' = TripStageTopBar.defaultConfig

  tripStageTopBarConfig =
    config'
      { data
        { cityConfig
          { enableAdvancedBooking = true
          }
        , advancedRideData = Nothing
        }
      , props
        { currentStage = RideStarted
        }
      }

distAndTimeView :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
distAndTimeView push state =
  linearLayout
    [ width MATCH_PARENT
    , height $ WRAP_CONTENT
    , margin $ Margin 20 36 20 0
    , background $ Color.white900
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , orientation VERTICAL
        , weight 1.0
        ]
        [ linearLayout
            [ gravity CENTER
            ]
            [ imageView
                [ height $ V 25
                , width $ V 25
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_ny_location_arrow_in"
                , margin $ Margin 0 0 10 0
                ]
            , textView
                $ [ text $ getString DIST
                  , height WRAP_CONTENT
                  , letterSpacing $ PX 2.0
                  , color Color.black500
                  ]
                <> FontStyle.h2 TypoGraphy
            ]
        , linearLayout
            [ gravity BOTTOM
            , margin $ Margin 0 5 0 0
            ]
            [ textView
                $ [ text $ show state.data.distance
                  , height WRAP_CONTENT
                  , color Color.black900
                  ]
                <> FontStyle.body34 TypoGraphy
            , textView
                $ [ text $ getString KM
                  , height WRAP_CONTENT
                  , letterSpacing $ PX 2.0
                  , margin $ Margin 4 0 0 0
                  , color Color.black500
                  ]
                <> FontStyle.h2 TypoGraphy
            ]
        , linearLayout
            [ height $ WRAP_CONTENT
            , width $ WRAP_CONTENT
            , orientation VERTICAL
            , cornerRadius 10.0
            ]
            [ textView
                $ [ text $ getString UPDATED_AT_
                  , color Color.black500
                  ]
                <> FontStyle.body9 TypoGraphy
            , linearLayout
                [ height $ WRAP_CONTENT
                , width $ WRAP_CONTENT
                , gravity CENTER
                , onClick push $ const RefreshTime
                , rippleColor Color.rippleShade
                ]
                [ textView
                    $ [ text $ EHC.convertUTCtoISC state.data.lastUpdatedTime "h:mm A"
                      , color Color.black900
                      , padding $ PaddingBottom 3
                      ]
                    <> FontStyle.body9 TypoGraphy
                , PrestoAnim.animationSet
                    [ PrestoAnim.Animation
                        [ PrestoAnim.duration 500
                        , PrestoAnim.fromRotation 0
                        , PrestoAnim.toRotation 360
                        , PrestoAnim.repeatCount PrestoAnim.NoRepeat
                        ]
                        state.props.refreshAnimation
                    ]
                    $ imageView
                        [ width $ V 16
                        , height $ V 16
                        , onAnimationEnd push StopRotation
                        , margin $ MarginLeft 8
                        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_refresh"
                        ]
                ]
            ]
        ]
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity RIGHT
        , orientation VERTICAL
        ]
        [ linearLayout
            []
            [ imageView
                [ height $ V 19
                , width $ V 19
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_ny_clock_unfilled"
                , margin $ Margin 0 4 10 0
                ]
            , textView
                $ [ text $ getString TIME
                  , height WRAP_CONTENT
                  , margin $ Margin 0 0 5 0
                  , letterSpacing $ PX 2.0
                  , color Color.black500
                  ]
                <> FontStyle.h2 TypoGraphy
            ]
        , linearLayout
            [ gravity CENTER
            , margin $ Margin 0 5 0 0
            ]
            [ textView
                $ [ text $ getTime state.data.timeSec
                  , height WRAP_CONTENT
                  , color Color.black900
                  ]
                <> FontStyle.body34 TypoGraphy
            ]
        ]
    ]
  where
  getTime :: Int -> String
  getTime sec =
    if sec <= 0 then
      "--:--"
    else
      let
        sstr = if (div (mod sec 60) 10) == 0 then "0" <> (show (mod sec 60)) else show (mod sec 60)

        mstr = if (div (mod (div sec 60) 60) 10) == 0 then "0" <> (show (mod (div sec 60) 60)) else show (mod (div sec 60) 60)

        hrs = div sec 3600
      in
        if hrs == 0 then
          mstr <> ":" <> sstr
        else
          (show hrs) <> ":" <> mstr <> ":" <> sstr

enterDestinationView :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
enterDestinationView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.blue600
    , gravity CENTER
    , padding $ Padding 15 15 15 15
    , margin $ Margin 0 0 0 20
    , cornerRadius 15.0
    , onClick push (const $ EnterDestination)
    , rippleColor Color.rippleShade
    , clickable  (state.data.destinationAddress == "")
    ]
    [ imageView
        [ height $ V 20
        , width $ V 20
        , color Color.blue800
        , margin $ Margin 0 2 10 0
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_location_blue"
        , visibility $ boolToVisibility (state.data.destinationAddress == "")
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , visibility $ boolToVisibility (state.data.destinationAddress /= "")
        , orientation VERTICAL
        , width MATCH_PARENT
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ Padding 5 5 5 5
            ]
            [ textView
                $ [ text $ getString DESTINATION
                  , color "#A7A7A7"
                  , onClick
                      ( \action -> do
                          _ <- push action
                          pure unit
                      )
                      (const $ EnterDestination)
                  , clickable (state.data.destinationAddress == "")
                  ]
                <> FontStyle.body5 TypoGraphy
            ]
        , textView
            $ [ text state.data.destinationAddress
              , maxLines 2
              , ellipsize true
              , padding $ Padding 5 5 5 5
              , color Color.black800
              , clickable  (state.data.destinationAddress == "")
              , onClick
                  ( \action -> do
                      _ <- push action
                      pure unit
                  )
                  (const $ EnterDestination)
              ]
            <> FontStyle.body5 TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ Padding 10 10 10 10
            , gravity RIGHT
            , clickable true
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , padding $ Padding 10 10 10 10
                , gravity CENTER
                , cornerRadius 22.0
                , background Color.blue900
                , rippleColor Color.rippleShade
                , onClick
                    ( \action -> do
                        _ <- push action
                        pure unit
                    )
                    (const $ OnNavigate)
                ]
                [ imageView
                    [ height $ V 20
                    , width $ V 20
                    , imageWithFallback $ fetchImage FF_ASSET "ny_ic_google_map_tracking"
                    ]
                , textView
                    $ [ text $ getString MAPS
                      , height WRAP_CONTENT
                      , color Color.white900
                      , padding $ PaddingBottom 3
                      ]
                    <> FontStyle.body23 TypoGraphy
                ]
            ]
        ]
    , textView
        $ [ text $ getString ENTER_DESTINATION
          , height WRAP_CONTENT
          , color Color.blue800
          , visibility $ boolToVisibility (state.data.destinationAddress == "")
          ]
        <> FontStyle.h3 TypoGraphy
    ]

stopMeterView :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
stopMeterView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.brightRed
    , margin $ Margin 0 0 0 26
    , cornerRadius 15.0
    , padding $ Padding 15 15 15 15
    , gravity CENTER
    , onClick push $ const ConfirmStopMeter
    , rippleColor Color.rippleShade
    ]
    [ linearLayout
        [ width $ V 13
        , height $ V 13
        , margin $ Margin 0 2 10 0
        , background $ Color.white900
        , cornerRadius 4.0
        ]
        []
    , textView
        $ [ text $ getString STOP_METER
          , height WRAP_CONTENT
          , color Color.white900
          , letterSpacing $ PX 1.0
          ]
        <> FontStyle.title6 TypoGraphy
    ]

fareView :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
fareView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ if state.props.isMeterRideStarted then Margin 0 32 0 0 else Margin 0 24 0 0
    , cornerRadius 32.0
    , orientation VERTICAL
    , background Color.blue600
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , padding $ Padding 0 35 0 12
        , onClick push $ const $ if state.props.startButtonCountDown > 3 then ShowRateCard else NoAction
        ]
        [ textView
            $ [ text $ getString FARE
              , height WRAP_CONTENT
              , color Color.black600
              , letterSpacing $ PX 1.0
              , margin $ Margin 0 0 8 0
              , padding $ PaddingBottom 3
              ]
            <> FontStyle.body30 TypoGraphy
        , imageView
            [ height $ V 20
            , width $ V 20
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_info_blue_inverse"
            ]
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , padding $ Padding 0 13 0 44
        ]
        [ linearLayout
            [ width WRAP_CONTENT
            , height MATCH_PARENT
            , padding $ Padding 0 2 14 0
            ]
            [ textView
                $ [ text "₹"
                  , color Color.black600
                  , gravity TOP_VERTICAL
                  ]
                <> FontStyle.body36 TypoGraphy
            ]
        , sevenSegmentView push state
        , textView
            $ [ text $ getString UPTON2KM
              , color Color.black900
              , margin $ MarginLeft 5
              , height MATCH_PARENT
              , gravity BOTTOM
              , alpha $ if state.data.distance < 2.0 then 1.0 else 0.4
              ]
            <> (FontStyle.body6 TypoGraphy)
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , padding $ PaddingBottom 16
        , visibility $ boolToVisibility state.props.isMeterRideStarted
        ]
        [ linearLayout
            [ height $ V 12
            , width $ V 12
            , background Color.black
            , cornerRadius 6.0
            ]
            [ PrestoAnim.animationSet
                [ PrestoAnim.Animation
                    [ PrestoAnim.duration 600
                    , PrestoAnim.fromAlpha 0.0
                    , PrestoAnim.toAlpha 1.0
                    , PrestoAnim.repeatMode PrestoAnim.Reverse
                    , PrestoAnim.repeatCount PrestoAnim.Infinite
                    , PrestoAnim.interpolator PrestoAnim.EaseOut
                    ]
                    true
                ]
                $ linearLayout
                    [ height $ V 12
                    , width $ V 12
                    , background "#E91212"
                    , cornerRadius 6.0
                    ]
                    []
            ]
        , textView
            $ [ text $ getString METER_RUNNING
              , color Color.black600
              , margin $ MarginLeft 8
              , letterSpacing $ PX 2.0
              , padding $ PaddingBottom 3
              ]
            <> FontStyle.body22 TypoGraphy
        ]
    ]

sevenSegmentView :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
sevenSegmentView push state =
  let digits = getDigits state.props.meterFare
      len = DA.length digits
      finalDigits = if state.props.meterFare == 0 then [-1,-1,-1,-1] else if len > 4 then digits else getArray (4 - len) <> digits
  in
    linearLayout
      [ width WRAP_CONTENT
      , height MATCH_PARENT
      ](map (\item -> textView
          $ [ text $ if item == -1 then "0" else show item
            , textSize FontSize.a_48
            , fontWeight $ FontWeight 400
            , color if item == -1 then Color.black600 else Color.black900
            , fontStyle "https://assets.moving.tech/beckn/fonts/DigitalNumbers-Regular.ttf"
            , letterSpacing $ PX 1.0
            ]) finalDigits)


getDigits :: Int -> Array Int
getDigits input = if input > 9 then getDigits (div input 10) <> [mod input 10] else [input]

getArray :: Int ->Array Int
getArray count = if count <= 0 then [] else [-1] <> (getArray (count - 1))

driverProfile :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
driverProfile push state =
  let
    driverImage = "ny_ic_generic_mascot"
  in
    linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity CENTER
      , padding $ Padding 16 20 12 16
      , onClick push $ const GoToProfile
      ]
      [ imageView
          [ width $ V 42
          , height $ V 42
          , imageWithFallback $ fetchImage FF_ASSET driverImage
          ]
      ]

getActiveRide :: Int -> Number -> (Action -> Effect Unit) -> MeterRideScreenState -> FlowBT String Unit
getActiveRide count duration push state = do
  (GetRidesHistoryResp rideHistoryResponse) <- Remote.getRideHistoryReqBT "2" "0" "true" "null" "null"
  case (head rideHistoryResponse.list) of
    Nothing -> do
      if count > 0 then do
        void $ lift $ lift $ delay $ Milliseconds duration
        getActiveRide (count - 1) duration push state
      else
        pure unit
    Just (RidesInfo resp) -> do
      void $ lift $ lift $ doAff $ liftEffect $ push $ UpdateRidesInfo (RidesInfo resp)
      if isNothing resp.tripStartTime then do
        void $ lift $ lift $ delay $ Milliseconds duration
        getActiveRide (count - 1) duration push state
      else
        pure unit
