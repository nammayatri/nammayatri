module Components.RideTrackingModal.View where

import Common.Types.App
import ConfigProvider
import Locale.Utils
import Prelude (class Eq, class Show, ($))
import Animation (scaleYAnimWithDelay)
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.RideTrackingModal.Controller (Action(..), Config)
import Components.SeparatorView.View as SeparatorView
import Data.Array as DA
import Data.Function.Uncurried (runFn2)
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Ord (abs)
import Data.Tuple
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (screenWidth, getNewIDWithTag, convertUTCtoISC, os)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getRideLabelData, getRequiredTag, getCurrentUTC, fetchImage, FetchImageFrom(..), dummyLabelConfig)
import Helpers.Utils (getRideTypeColor, getCategorizedVariant)
import Helpers.Utils (getRideTypeColor, getVariantRideType)
import Helpers.Utils as HU
import JBridge (getVersionCode)
import JBridge as JB
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Mobility.Prelude (boolToVisibility, boolToInvisibility)
import Prelude ((<>), div, mod, Unit, bind, when, const, not, discard, pure, show, unit, void, ($), (<), (/=), (<>), (&&), (==), (-), (>), (||), (/), (*), (+), negate, (<$>), (>>=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alpha, background, clickable, color, ellipsize, fillViewport, fontSize, fontStyle, gravity, height, horizontalScrollView, id, imageUrl, imageView, imageWithFallback, layoutGravity, lineHeight, linearLayout, margin, maxLines, onAnimationEnd, onClick, orientation, padding, pivotY, relativeLayout, rippleColor, scrollBarX, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, alignParentBottom, nestedScrollView, scrollBarY)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii, cornerRadius)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (HomeScreenStage(..), TimerStatus(..), DisabilityType(..))
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Styles.Colors as Color
import Timers as ET
import Types.App (defaultGlobalState)
import Mobility.Prelude
import Common.Types.Config as CTC
import Resource.Constants as RC
import Debug
import PrestoDOM.Elements.Keyed as Keyed
import Data.String as DS
import JBridge (fromMetersToKm)
import Data.Maybe
import Data.Int
import Components.RateCard.Controller 


view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = do
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][ 
        linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ][ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , id $ getNewIDWithTag "rideActionHeaderLayout"
          , padding $ PaddingBottom 16
          ][  linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation HORIZONTAL
              , gravity CENTER
              ][ 
              ]
            ]
          ],
        rideActionView (MarginTop 0) push config
    ]


rideActionView :: forall w . Margin -> (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rideActionView layoutMargin push config =
  
  (if os == "IOS" then linearLayout else scrollView)
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , nestedScrollView true
  , scrollBarY false
  ]
  [
  Keyed.linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , cornerRadii $ Corners 25.0 true true false false
  , orientation VERTICAL
  , background Color.white900
  , padding $ PaddingTop 6
  , gravity CENTER
  , margin layoutMargin
  , stroke $ "1," <> Color.grey800
  ][ Tuple "rideActionView_Child_1" $ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , id $ getNewIDWithTag "rideActionLayout"
      ]([  
          rideActionDataView push config
        ])
    , Tuple "rideActionView_Child_2" $ endRide push config
  ]]


rideActionDataView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rideActionDataView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (PaddingHorizontal 16 16)
    , gravity CENTER
    ][  linearLayout
          [ width (V 34)
          , height (V 4)
          , cornerRadius 4.0
          , background Color.black500
          ][]
      , textView $[
          height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity START
        , text $ "You are on ride...."
        , color Color.black800
        , ellipsize true
        , singleLine false
        , margin $ MarginVertical 16 20
        ] <> FontStyle.subHeading2 TypoGraphy
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ][  linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ][ 
              rideInfoView push config
            , sourceAndDestinationView push config 
            ]
          ]
      ]

rideInfoView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rideInfoView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , stroke $ "1," <> Color.grey900
    , cornerRadius 8.0
    , padding $ Padding 14 14 5 14
    , afterRender push $ const NoAction
    ] [  horizontalScrollView
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , scrollBarX false
          , fillViewport true
          ][ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              ][normalRideInfoView push config]
            ]
      ]


normalRideInfoView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
normalRideInfoView push config =
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ]
      [ 
          infoView "Bus No." config.busNumber
        , separator true
        , infoView "Route No." config.routeNumber
        , separator true
        , infoView "Bus Type" config.busType
      ] 

infoView :: forall w . String -> String -> PrestoDOM (Effect Unit) w
infoView title value = 
  linearLayout
    [
      height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    , gravity LEFT
    , weight 1.0
    ][
      -- linearLayout
      -- [ height WRAP_CONTENT
      -- , width WRAP_CONTENT
      -- , orientation VERTICAL
      -- ][
        textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text title
          , color Color.black650
          , ellipsize true
          , singleLine true
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text value
          , color Color.black800
          , ellipsize true
          , singleLine true
          -- , margin (MarginLeft 8)
          ] <> FontStyle.subHeading1 TypoGraphy
      -- ]
    ]

sourceAndDestinationView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
sourceAndDestinationView push config =
  relativeLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginVertical 24 24
    , afterRender push $ const NoAction
    ][  sourceDestinationImageView config
      , sourceDestinationTextView push config
      ]



sourceDestinationImageView :: forall w . Config -> PrestoDOM (Effect Unit) w
sourceDestinationImageView  config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    ][ imageView
        [ height $ V 14
        , width $ V 14
        , margin $ MarginTop 4
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_source_dot"
        ],
        SeparatorView.view (separatorConfig config)
      , imageView
        [ height $ V 14
        , width $ V 14
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_destination"
        , margin $ MarginTop 0
        ]
      ]

separatorConfig :: Config -> SeparatorView.Config
separatorConfig config = {
    orientation : VERTICAL
  , count : 3
  , height : V 4
  , width : V 2
  , layoutWidth : V 14
  , layoutHeight : V 16
  , color : Color.black500
  }

sourceDestinationTextView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
sourceDestinationTextView push config =
  linearLayout
    [ width WRAP_CONTENT
    , orientation VERTICAL
    , height WRAP_CONTENT
    , margin (MarginLeft 25)
    , afterRender push $ const NoAction
    ][  
      textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.sourceStopName
        , id (getNewIDWithTag "tripSource")
        , color Color.black800
        , ellipsize true
        , singleLine true
        , afterRender push $ const NoAction
        , margin $ MarginBottom 15
        ] <> FontStyle.subHeading1 TypoGraphy
      -- , textView $
      --   [ height WRAP_CONTENT
      --   , width WRAP_CONTENT
      --   , text "config.sourceAddress.detailText"
      --   , id (getNewIDWithTag "tripSourceAddress")
      --   , color Color.black650
      --   , margin (MarginBottom 25)
      --   , ellipsize true
      --   , singleLine true
      --   , afterRender push $ const NoAction
      --   ] <> FontStyle.body1 TypoGraphy
      , destAddressTextView config push
      ] 

destAddressTextView :: forall w . Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
destAddressTextView config push =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][  textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text $ config.destinationStopName
        , id (getNewIDWithTag "tripDestination")
        , color Color.black800
        , ellipsize true
        , singleLine true
        ] <> FontStyle.subHeading1 TypoGraphy
      -- , textView $
      --   [ height WRAP_CONTENT
      --   , width WRAP_CONTENT
      --   , text $ ""
      --   , id (getNewIDWithTag "tripDestination")
      --   , color Color.black650
      --   , ellipsize true
      --   , margin (MarginBottom 0)
      --   ]<> FontStyle.body1 TypoGraphy
      ]


endRide :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
endRide push config =
  linearLayout
  [ width MATCH_PARENT
  , height (V 48)
  , background Color.grey800
  , cornerRadius 8.0
  , gravity CENTER
  , onClick push (const $ EndRide)
  , rippleColor Color.rippleShade
  , afterRender push $ const NoAction
  , margin $ Margin 16 14 16 34
  ][ textView $
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , text $ getString END_RIDE
    , color Color.black700
    -- , padding (Padding 0 0 0 4)
    , afterRender push $ const NoAction
    ] <> FontStyle.subHeading1 TypoGraphy 
]


separator :: forall w . Boolean -> PrestoDOM (Effect Unit) w
separator visibility' =
  linearLayout
    [ weight 1.0
    , height MATCH_PARENT
    , margin $ MarginHorizontal 5 5
    , visibility if visibility' then VISIBLE else GONE
    ][ linearLayout
      [ width $ V 1
      , background Color.lightGrey
      , height MATCH_PARENT
      ][]
    ]
