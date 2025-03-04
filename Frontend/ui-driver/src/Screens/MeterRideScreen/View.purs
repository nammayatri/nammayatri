module Screens.MeterRideScreen.View where

import Animation as Anim
import Common.Types.App
import Data.String (length)
import Data.String.Unsafe (charAt)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge as JB
import Mobility.Prelude (boolToVisibility)
import Prelude
import PrestoDOM 
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Events (onBackPressed, onClick)
import PrestoDOM.Properties (textSize, fontStyle, letterSpacing, width, height, orientation, visibility, margin, padding, text, color, background, cornerRadius, imageWithFallback, weight, gravity, cornerRadii)
import PrestoDOM.Types.DomAttributes (Gravity(..), Length(..), Orientation(..))
import PrestoDOM.Types.Core (LetterSpacing(..))
import Styles.Colors as Color
import Screens.BookingOptionsScreen.ComponentConfig as BOP
import Screens.MeterRideScreen.Controller (Action(..), ScreenOutput(..), eval)
import Screens.Types
import Debug
import PrestoDOM.Elements.Keyed as Keyed 
import Data.Tuple (Tuple(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Engineering.Helpers.Commons as EHC
import Components.TripStageTopBar as TripStageTopBar
import Screens.HomeScreen.ScreenData (dummyRideData)
import Data.Maybe

screen :: MeterRideScreenState -> Screen Action MeterRideScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "MeterRideScreen"
  , globalEvents : []
  , eval : (\action state -> do
      let _ = spy "MeterRideScree state -----" state
      let _ = spy "MeterRideScree action -----" action
      eval action state)
  }

view :: forall w . (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout[
    width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.white900
  ] $ [
    Anim.screenAnimationFadeInOut $ linearLayout
    [
      width MATCH_PARENT,
      height MATCH_PARENT,
      onBackPressed push $ const BackPressed
    ]
    [
      if state.props.isMeterRideStarted then rideStartedView push state else rideInitView push state
    ]
  ] <> (if state.props.showRateCard then [rateCardView push state] else [])

  where

  rateCardView :: forall w. (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
  rateCardView push state =
    linearLayout
      [
        width MATCH_PARENT,
        height MATCH_PARENT,
        background Color.blackLessTrans,
        gravity CENTER,
        onClick push $ const CloseRateCard 
      ]
      [
        linearLayout
        [
          width MATCH_PARENT,
          height WRAP_CONTENT,
          gravity CENTER,
          cornerRadius 15.0,
          margin $ Margin 16 0 16 0,
          padding $ Padding 10 10 10 10,
          orientation VERTICAL,
          onClick push $ const NoAction,
          background Color.blue600
        ]
        [
          textView $ 
          [ text "Rate Card"
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , color Color.black800
          , gravity CENTER
          ] <> FontStyle.h2 TypoGraphy
          , linearLayout
          [
            margin $ Margin 16 16 16 0,
            orientation VERTICAL,
            background Color.white900,
            width MATCH_PARENT,
            height WRAP_CONTENT,
            gravity CENTER,
            cornerRadius 12.0
          ]
          [
            textView $ 
            [ text "Auto Rickshaw"
            , height WRAP_CONTENT
            , width MATCH_PARENT
            , color Color.black800
            , gravity CENTER
            , margin $ Margin 10 10 10 6
            ] <> FontStyle.subHeading1 TypoGraphy
            , imageView $ 
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_auto_right_view"
            , width $ V 81
            , height $ V 81
            , margin $ Margin 10 6 10 0
            , gravity CENTER
            ]
            , textView $ 
            [ text $ "₹" <> show (state.props.sliderVal * state.props.ratePerKM)
            , height WRAP_CONTENT
            , width MATCH_PARENT
            , color Color.black800
            , margin $ Margin 10 0 10 2
            , gravity CENTER
            ] <> FontStyle.priceFont TypoGraphy
            , textView $ 
            [ text $ "₹" <> show state.props.ratePerKM <> "/km"
            , height WRAP_CONTENT
            , width MATCH_PARENT
            , color Color.black500
            , margin $ Margin 10 0 10 20
            , gravity CENTER
            ] <> FontStyle.body23 TypoGraphy
          ]
          , sliderView push state
          , textView $ 
          [ text "Got It!"
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , color Color.blue800
          , gravity CENTER
          , margin $ Margin 16 16 16 16 
          , onClick push $ const CloseRateCard
          ] <> FontStyle.h2 TypoGraphy
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
        ][  textView
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
                  ] <> if decButtonEnabled then [rippleColor Color.rippleShade] else []
            , textView
                $ [ text $ show state.props.sliderVal <> " km"
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
                  ] <> if incButtonEnabled then [rippleColor Color.rippleShade] else []
            ]
          , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , margin $ Margin 5 5 5 5
              , id $ EHC.getNewIDWithTag "RateSliderTool"
              ][]
          , PrestoAnim.animationSet [ Anim.triggerOnAnimationEnd true ]
            $ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , id $ EHC.getNewIDWithTag "RateSlider"
                , margin $ Margin 5 5 5 5
                , onAnimationEnd ( \action -> void $ JB.renderSlider 
                    ( \sliderAction -> do
                        void $ pure $ JB.updateInputString "MeterRideInput"
                        void $ push sliderAction
                        pure unit
                    )  
                    SliderCallback sliderConfig ) (const NoAction)
                ][]
          , textView
            $ [ text "Rate changes as the distance changes"
              , color Color.black700
              , margin $ Margin 10 10 10 5
              ]
            <> FontStyle.body3 TypoGraphy
          
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

rideInitView :: forall w . (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
rideInitView push state =
  linearLayout
  [
    width MATCH_PARENT,
    height MATCH_PARENT,
    onBackPressed push $ const BackPressed,
    margin $ Margin 26 24 26 0,
    orientation VERTICAL
  ]
  [
    topBarView push state,
    fareView push state,
    startButtonView push state
  ]
  where

    topBarView :: forall w . (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
    topBarView push state =
      linearLayout
      [
        width MATCH_PARENT,
        height WRAP_CONTENT
      ]
      [
        linearLayout
        [
          width WRAP_CONTENT,
          height WRAP_CONTENT,
          background Color.blue600,
          padding $ Padding 12 12 20 12,
          cornerRadius 32.0,
          onClick push $ const BackPressed
        ]
        [
          imageView
          [
            width $ V 24,
            height $ V 24,
            margin $ Margin 0 4 2 0,
            color Color.black700,
            imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
          ]
          ,textView $ 
          [ text "Back"
          , height WRAP_CONTENT
          , margin $ Margin 0 0 4 0
          , color Color.black700
          ] <> FontStyle.body7 TypoGraphy
        ]
        , linearLayout
        [
          height WRAP_CONTENT,
          weight 1.0
        ]
        [

        ]
        , imageView
        [
          width $ V 101,
          height $ V 35,
          margin $ Margin 0 4 2 0,
          color Color.black700,
          imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_logo_dark"
        ]
      ]

    startButtonView :: forall w . (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
    startButtonView push state =
      linearLayout
      [
        width MATCH_PARENT,
        orientation VERTICAL,
        gravity CENTER,
        weight 1.0
      ]
      [
        frameLayout
        [
          width MATCH_PARENT,
          height MATCH_PARENT,
          gravity CENTER
        ] $ 
        [
          linearLayout
          [
            width MATCH_PARENT,
            height MATCH_PARENT,
            gravity CENTER
          ]
          [
            linearLayout
            [
              weight 1.0,
              height MATCH_PARENT,
              orientation VERTICAL,
              gravity CENTER
            ]
            [
              linearLayout
              [
                -- width MATCH_PARENT,
                -- weight 1.0,
                width $ V 108,
                height $ V 108,
                layoutGravity "right",
                cornerRadii $ Corners 999.0 true false false false,
                background if state.props.startButtonCountDown > 1 then Color.lightHulkGreen else Color.darkHulkGreen
                -- background Color.blue900
              ]
              [

              ]
              ,linearLayout
              [
                -- width MATCH_PARENT,
                -- weight 1.0,
                width $ V 108,
                height $ V 108,
                cornerRadii $ Corners 999.0 false false false true,
                layoutGravity "right",
                background if state.props.startButtonCountDown > 2 then Color.lightHulkGreen else Color.darkHulkGreen
              ]
              [

              ]
            ]
            , linearLayout
            [
              weight 1.0,
              height MATCH_PARENT,
              orientation VERTICAL,
              gravity CENTER
            ]
            [
              linearLayout
              [
                -- width MATCH_PARENT,
                -- weight 1.0,
                width $ V 108,
                height $ V 108,
                cornerRadii $ Corners 999.0 false true false false,
                background Color.lightHulkGreen,

                layoutGravity "left"
              ]
              [

              ]
              ,linearLayout
              [
                -- width MATCH_PARENT,
                -- weight 1.0,
                width $ V 108,
                height $ V 108,
                cornerRadii $ Corners 999.0 false false true false,
                background if state.props.startButtonCountDown > 3 then Color.lightHulkGreen else Color.darkHulkGreen,
                layoutGravity "left"
              ]
              [

              ]
            ]
          ]
        ] <>
        [
          Keyed.linearLayout
          [
            width $ V 216,
            height $ V 216,
            gravity CENTER,
            cornerRadius 9999.0,
            layoutGravity "center",
            onClick push $ const HandleStartButton
          ]
          [
            if state.props.startButtonCountDown > 3 then Tuple "startPressView" $ startPressView push state else Tuple "startCounterView" $ startCounterView push state
          ]
        ]
      ]
    
    startPressView :: forall w . (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
    startPressView push state = 
      PrestoAnim.animationSet [Anim.fadeIn true] $ linearLayout
      [
        orientation VERTICAL,
        width WRAP_CONTENT,
        height WRAP_CONTENT,
        gravity CENTER
      ]
      [
        textView $ 
        [ text "START"
        , height WRAP_CONTENT
        , color Color.white900
        , letterSpacing $ PX 1.0
        ] <> FontStyle.title3 TypoGraphy
        ,textView $ 
        [ text "namma\nmeter"
        , gravity CENTER
        , height WRAP_CONTENT
        , color Color.white900
        , alpha 1.0
        ] <> FontStyle.title4 TypoGraphy
      ]

    startCounterView :: forall w . (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
    startCounterView push state = 
      PrestoAnim.animationSet [Anim.fadeIn true] $ linearLayout
      [
        orientation VERTICAL,
        width WRAP_CONTENT,
        height WRAP_CONTENT,
        gravity CENTER
      ]
      [
        textView $ 
        [ text $ show state.props.startButtonCountDown
        , height WRAP_CONTENT
        , color Color.white900
        , letterSpacing $ PX 1.0
        ] <> FontStyle.title5 TypoGraphy
        ,textView $ 
        [ text "Press to Cancel"
        , gravity CENTER
        , height WRAP_CONTENT
        , color Color.white900
        , alpha 0.4
        ] <> FontStyle.subHeading2 TypoGraphy  
      ]

rideStartedView :: forall w . (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
rideStartedView push state =
  linearLayout
  [
    width MATCH_PARENT,
    height MATCH_PARENT,
    orientation VERTICAL
  ]
  [
    linearLayout[
      background Color.white900
    ]
    [
      driverProfile push state,
      TripStageTopBar.view (push <<< TripStageTopBarAC) tripStageTopBarConfig
    ]
    , linearLayout
    [
      width MATCH_PARENT,
      margin $ Margin 26 10 26 0,
      weight 1.0,
      orientation VERTICAL

    ]
    [fareView push state
      ,distAndTimeView push state
      ,linearLayout
      [
        gravity BOTTOM,
        weight 1.0,
        width MATCH_PARENT,
        orientation VERTICAL
      ]
      [
        enterDestinationView push state
        ,stopMeterView push state
      ]
    ]
  ]

  where
    config' = TripStageTopBar.defaultConfig
    tripStageTopBarConfig = config' {
      data {
        cityConfig {
          enableAdvancedBooking = true
        }
        , advancedRideData = Just dummyRideData
      }
      ,props 
        {
          currentStage = RideStarted
        }
    }

distAndTimeView :: forall w . (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
distAndTimeView push state =
  linearLayout
  [
    width MATCH_PARENT,
    height $ WRAP_CONTENT,
    margin $ Margin 20 30 20 0
  ]
  [
    linearLayout
    [
      height WRAP_CONTENT,
      orientation VERTICAL,
      weight 1.0
    ] 
    [
      linearLayout
      [
        gravity CENTER 
      ]
      [
        imageView
        [
          height $ V 25,
          width $ V 25,
          imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_ny_location_arrow_in",
          margin $ Margin 0 0 10 0
        ]
        , textView $ 
        [ text "DIST."
        , height WRAP_CONTENT
        , letterSpacing $ PX 2.0
        , color Color.black500
        ] <> FontStyle.h2 TypoGraphy
      ]
      , linearLayout
      [
        gravity BOTTOM,
        margin $ Margin 0 5 0 0
      ]
      [
        textView $ 
        [ text $ show state.data.distance
        , height WRAP_CONTENT
        , color Color.black900
        ] <> FontStyle.body34 TypoGraphy
        , textView $ 
        [ text "KM"
        , height WRAP_CONTENT
        , letterSpacing $ PX 2.0
        , margin $ Margin 4 0 0 0
        , color Color.black500
        ] <> FontStyle.h2 TypoGraphy
      ]
    ]
    , linearLayout
    [
      weight 1.0,
      height WRAP_CONTENT,
      gravity RIGHT,
      orientation VERTICAL
    ]
    [
      linearLayout
      [
        
      ]
      [
        imageView
        [
          height $ V 19,
          width $ V 19,
          imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_ny_clock_unfilled",
          margin $ Margin 0 4
           10 0
        ]
        , textView $ 
        [ text "TIME"
        , height WRAP_CONTENT
        , letterSpacing $ PX 2.0
        , color Color.black500
        ] <> FontStyle.h2 TypoGraphy
      ]
      , linearLayout
      [
        gravity CENTER,
        margin $ Margin 0 5 0 0
      ]
      [
        textView $ 
        [ text $ getTime state.data.timeMin 
        state.data.timeSec
        , height WRAP_CONTENT
        , color Color.black900
        ] <> FontStyle.body34 TypoGraphy
      ]
    ]
  ]

  where
    getTime :: Int -> Int -> String
    getTime min sec = 
      let
        sstr = if (div min 10) == 0 then "0" <> (show min) else show min
        mstr = if (div sec 10) == 0 then "0" <> (show sec) else show sec 
      in 
        mstr <> ":" <> sstr
        

enterDestinationView :: forall w . (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
enterDestinationView push state =
  linearLayout
  [
    width MATCH_PARENT,
    height WRAP_CONTENT,
    background Color.blue600,
    gravity CENTER,
    padding $ Padding 15 15 15 15,
    margin $ Margin 0 0 0 20,
    cornerRadius 15.0
  ]
  [
    imageView
    [
      height $ V 20,
      width $ V 20,
      color Color.blue800,
      margin $ Margin 0 2 10 0,
      imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_location_blue"
      , visibility $ boolToVisibility (state.data.destinationLat == 0.0)
    ]
    , linearLayout 
      [ height WRAP_CONTENT
      , visibility $ boolToVisibility (state.data.destinationLat /= 0.0)
      , orientation VERTICAL
      ][linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 5 5 5 5
        ][textView $
          [ text "Destination"
          , color "#A7A7A7"
          , onClick (\action -> do
                _ <- push action
                pure unit
            )(const $ EnterDestination)
          ] <> FontStyle.body5 TypoGraphy
        ]
        , textView $ [ 
            text state.data.destinationAddress
          , maxLines 2
          , ellipsize true
          , padding $ Padding 5 5 5 5
          , color Color.black800
          , onClick (\action -> do
              _ <- push action
              pure unit
            )(const $ EnterDestination)
          ] <> FontStyle.body5 TypoGraphy
      ]
      , textView $ 
      [ text "Enter Destination"
      , height WRAP_CONTENT
      , color Color.blue800
      , visibility $ boolToVisibility (state.data.destinationLat == 0.0)
      , onClick (\action -> do
            _ <- push action
            pure unit
        )(const $ EnterDestination)
      ] <> FontStyle.h3 TypoGraphy
  ]

stopMeterView :: forall w . (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
stopMeterView push state =
  linearLayout
  [
    width MATCH_PARENT,
    height WRAP_CONTENT,
    background Color.brightRed,
    margin $ Margin 0 0 0 26,
    cornerRadius 15.0,
    padding $ Padding 15 15 15 15,
    gravity CENTER
  ]
  [
    linearLayout
    [
      width $ V 13,
      height $ V 13,
      margin $ Margin 0 2 10 0,
      background $ Color.white900,
      cornerRadius 4.0
    ]
    [

    ]
    ,textView $ 
    [ text "STOP METER"
    , height WRAP_CONTENT
    , color Color.white900
    , letterSpacing $ PX 1.0
    ] <> FontStyle.title6 TypoGraphy
  ]

fareView :: forall w . (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
fareView push state =
  linearLayout
  [
    width MATCH_PARENT,
    height WRAP_CONTENT,
    margin $ if state.props.isMeterRideStarted then Margin 0 32 0 0 else Margin 0 24 0 0,
    cornerRadius 32.0,
    orientation VERTICAL,
    background Color.blue600
  ]
  [
    linearLayout
    [
      width MATCH_PARENT,
      height WRAP_CONTENT,
      gravity CENTER,
      padding $ Padding 0 35 0 12
    ]
    [
      textView $ 
      [ text "FARE"
      , height WRAP_CONTENT
      , color Color.black600
      , letterSpacing $ PX 1.0
      , margin $ Margin 0 0 8 0
      ] <> FontStyle.body30 TypoGraphy
      , imageView
      [
        height $ V 16,
        width $ V 16,
        imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_info_blue_inverse",
        onClick push $ const $ if state.props.startButtonCountDown > 3 then ShowRateCard else NoAction
      ]
    ]
    ,linearLayout
    [
      width MATCH_PARENT,
      height WRAP_CONTENT,
      gravity CENTER,
      padding $ Padding 0 13 0 44
    ]
    [
      linearLayout
      [
        width WRAP_CONTENT,
        height MATCH_PARENT,
        padding $ Padding 0 2 14 0
      ]
      [
        textView $ 
        [ text "₹"
        , color Color.black600
        , gravity TOP_VERTICAL
        ] <> FontStyle.body36 TypoGraphy
      ]
      , sevenSegmentView push state
    ]
  ]
  
sevenSegmentView :: forall w . (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
sevenSegmentView push state = 
  let
    charA = show $ div state.props.meterFare 1000
    charB = show $ mod (div state.props.meterFare 100) 10
    charC = show $ mod (div state.props.meterFare 10) 10
    charD = show $ mod state.props.meterFare 10
  in
    linearLayout
    [
      width WRAP_CONTENT,
      height MATCH_PARENT
    ]
    [ 
      textView $ 
      [ text charA
      , textSize FontSize.a_72
      , fontWeight $ FontWeight 400
      , color if charA == "0" then Color.black600 else Color.black900
      , fontStyle FontStyle.sevenSegment
      , letterSpacing $ PX 1.0
      ]
      ,textView $ 
      [ text charB
      , textSize FontSize.a_72
      , fontWeight $ FontWeight 400
      , color if charA == "0" && charB == "0" then Color.black600 else Color.black900
      , fontStyle FontStyle.sevenSegment
      , letterSpacing $ PX 1.0
      ]
      ,textView $ 
      [ text charC
      , textSize FontSize.a_72
      , fontWeight $ FontWeight 400
      , color if charA == "0" && charB == "0" && charC == "0" then Color.black600 else Color.black900
      , fontStyle FontStyle.sevenSegment
      , letterSpacing $ PX 1.0
      ]
      ,textView $ 
      [ text charD
      , textSize FontSize.a_72
      , fontWeight $ FontWeight 400
      , color if charA == "0" && charB == "0" && charC == "0" && charD == "0" then Color.black600 else Color.black900
      , fontStyle FontStyle.sevenSegment
      , letterSpacing $ PX 1.0
      ]
    ]

driverProfile :: forall w . (Action -> Effect Unit) -> MeterRideScreenState -> PrestoDOM (Effect Unit) w
driverProfile push state = 
  let driverImage = "ny_ic_generic_mascot"
  in
   linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER
    , padding $ Padding 16 20 12 16
    ][ imageView
          [ width $ V 42
          , height $ V 42
          , imageWithFallback $ fetchImage FF_ASSET driverImage
          ]
    ]
  
