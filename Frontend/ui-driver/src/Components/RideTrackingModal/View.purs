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
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alpha, background, clickable, color, ellipsize, fillViewport, fontSize, fontStyle, gravity, height, horizontalScrollView, id, imageUrl, imageView, imageWithFallback, layoutGravity, lineHeight, linearLayout, frameLayout, margin, maxLines, onAnimationEnd, onClick, orientation, padding, pivotY, relativeLayout, rippleColor, scrollBarX, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, alignParentBottom, nestedScrollView, scrollBarY, accessibilityHint)
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
import Resource.Localizable.TypesV2 as LT2
import Resource.Localizable.StringsV2 as StringV2


view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  frameLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , afterRender push $ const NoAction
    ][ 
        modalHeader, 
        activeTrackingView push config
    ]

modalHeader :: forall w . PrestoDOM (Effect Unit) w
modalHeader = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , id $ getNewIDWithTag "rideActionHeaderLayout"
    , padding $ PaddingBottom 16
    ][ 
      linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER
        ][]
    ]

activeTrackingView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
activeTrackingView push config =
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
      , margin $ MarginTop 0
      , stroke $ "1," <> Color.grey800
      ][ 
        Tuple "activeTrackingDetails" $ activeTrackingDetails push config
      , Tuple "endRideButton" $ endRideButton push config
      ]
    ]

activeTrackingDetails :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
activeTrackingDetails push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , id $ getNewIDWithTag "rideActionLayout"
    , padding (PaddingHorizontal 16 16)
    , gravity CENTER
    ][  
      dragHandle
    , activeRideStatus
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ][  
          busDetailsCard push config
        , journeyDetailsView push config 
        ]
    ]
  where
    dragHandle =
      linearLayout
        [ width (V 34)
        , height (V 4)
        , cornerRadius 4.0
        , background Color.black500
        ][]
    
    activeRideStatus = 
      textView $
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity START
        , text $ StringV2.getStringV2 LT2.you_are_on_a_ride
        , color Color.black800
        , ellipsize true
        , singleLine false
        , margin $ MarginVertical 16 20
        ] <> FontStyle.subHeading2 TypoGraphy

busDetailsCard :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
busDetailsCard push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , stroke $ "1," <> Color.grey900
    , cornerRadius 8.0
    , padding $ Padding 14 14 5 14
    ] [  
        horizontalScrollView
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , scrollBarX false
          , fillViewport true
          ][ 
            busInfoRow push config
          ]
    ]

busInfoRow :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
busInfoRow push config =
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ][ 
          busInfoItem (StringV2.getStringV2 LT2.bus_no) config.busNumber
        , infoSeparator true
        , busInfoItem (StringV2.getStringV2 LT2.route_no) config.routeNumber
        , infoSeparator true
        , busInfoItem (StringV2.getStringV2 LT2.bus_type_) $ if config.busType == "BUS_AC" then StringV2.getStringV2 LT2.ac else StringV2.getStringV2 LT2.non_ac
      ] 

busInfoItem :: forall w . String -> String -> PrestoDOM (Effect Unit) w
busInfoItem label value = 
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    , gravity LEFT
    , weight 1.0
    ][
        textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text label
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
          ] <> FontStyle.subHeading1 TypoGraphy
    ]

journeyDetailsView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
journeyDetailsView push config =
  relativeLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginVertical 24 24
    , afterRender push $ const NoAction
    ][  
      journeyIndicators config
    , stopsInfo push config
    ]

journeyIndicators :: forall w . Config -> PrestoDOM (Effect Unit) w
journeyIndicators config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    ][ 
      imageView
        [ height $ V 14
        , width $ V 14
        , margin $ MarginTop 4
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_source_dot"
        ]
    , SeparatorView.view (journeySeparatorConfig config)
    , imageView
        [ height $ V 14
        , width $ V 14
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_destination"
        , margin $ MarginTop 0
        ]
    ]

journeySeparatorConfig :: Config -> SeparatorView.Config
journeySeparatorConfig config = {
    orientation : VERTICAL
  , count : 3
  , height : V 4
  , width : V 2
  , layoutWidth : V 14
  , layoutHeight : V 16
  , color : Color.black500
  }

stopsInfo :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
stopsInfo push config =
  linearLayout
    [ width WRAP_CONTENT
    , orientation VERTICAL
    , height WRAP_CONTENT
    , margin (MarginLeft 25)
    , afterRender push $ const NoAction
    ][  
      sourceStopText config push
    , destinationStopText config push
    ] 

sourceStopText :: forall w . Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
sourceStopText config push =
  textView $
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , text config.sourceStopName
    , id (getNewIDWithTag "tripSource")
    , color Color.black800
    , ellipsize true
    , singleLine true
    , margin $ MarginBottom 15
    , accessibilityHint "Trip Source"
    ] <> FontStyle.subHeading1 TypoGraphy

destinationStopText :: forall w . Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
destinationStopText config push =
  textView $
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , text config.destinationStopName
    , id (getNewIDWithTag "tripDestination")
    , color Color.black800
    , ellipsize true
    , singleLine true
    , accessibilityHint "Destination Source"
    ] <> FontStyle.subHeading1 TypoGraphy

endRideButton :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
endRideButton push config =
  linearLayout
    [ width MATCH_PARENT
    , height (V 48)
    , background Color.grey800
    , cornerRadius 8.0
    , gravity CENTER
    , onClick push (const $ EndRide)
    , rippleColor Color.rippleShade
    , margin $ Margin 16 14 16 34
    ][ 
      textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString END_RIDE
        , color Color.black700
        ] <> FontStyle.subHeading1 TypoGraphy 
    ]

infoSeparator :: forall w . Boolean -> PrestoDOM (Effect Unit) w
infoSeparator visibility' =
  linearLayout
    [ weight 1.0
    , height MATCH_PARENT
    , margin $ MarginHorizontal 5 5
    , visibility if visibility' then VISIBLE else GONE
    ][ 
      linearLayout
        [ width $ V 1
        , background Color.lightGrey
        , height MATCH_PARENT
        ][]
    ]