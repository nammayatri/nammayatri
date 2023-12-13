{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.RideActionModal.View where

import Common.Types.App
import PrestoDOM.Animation as PrestoAnim
import Animation (scaleYAnimWithDelay)
import Common.Types.App (LazyCheck(..))
import Components.RideActionModal.Controller (Action(..), Config)
import Components.SeparatorView.View as SeparatorView
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (screenWidth, getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getRideLabelData, getRequiredTag, clearTimer, getCurrentUTC, fetchImage, FetchImageFrom(..))
import Helpers.Utils (getRideTypeColor, getCategorizedVariant)
import JBridge (getVersionCode)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Prelude ((<>))
import Prelude (Unit, bind, const, not, discard, pure, show, unit, ($), (/=), (<>), (&&), (==), (-), (>), (||), (/), (*), (+), negate)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alpha, background, clickable, color, ellipsize, fontSize, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, lineHeight, linearLayout, margin, maxLines, onClick, orientation, padding, relativeLayout, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, id, pivotY, onAnimationEnd, id, layoutGravity, horizontalScrollView, scrollBarX, fillViewport)
import PrestoDOM.Properties (cornerRadii, cornerRadius)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (HomeScreenStage(..), TimerStatus(..), DisabilityType(..))
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import Helpers.Utils (getRideTypeColor, getVariantRideType)
import Helpers.Utils as HU
import JBridge as JB
import Data.Int as Int
import Animation as Anim
import ConfigProvider

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , id $ getNewIDWithTag "rideActionHeaderLayout"
            , padding $ PaddingBottom 16
            ]
            [ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , gravity CENTER
                ]
                [ messageButton push config
                , callButton push config
                , openGoogleMap push config
                ]
            ]
        ]
    , if isSpecialRide config then
        rideActionViewWithLabel push config
      else
        rideActionView (MarginTop 0) push config
    ]

messageButton :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
messageButton push config =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER
    , visibility if (config.currentStage == RideAccepted || config.currentStage == ChatWithCustomer) && checkVersionForChat (getCurrentAndroidVersion (getMerchant FunctionCall)) then VISIBLE else GONE
    , padding $ Padding 20 16 20 16
    , margin $ MarginLeft 16
    , background Color.white900
    , stroke $ "1," <> Color.black500
    , cornerRadius 30.0
    , afterRender push $ const $ LoadMessages
    , onClick push $ const $ if config.accessibilityTag == Maybe.Just BLIND_AND_LOW_VISION then VisuallyImpairedCustomer else MessageCustomer
    , alpha if config.accessibilityTag == Maybe.Just BLIND_AND_LOW_VISION then 0.5 else 1.0
    , clickable true
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET $ if config.unReadMessages then "ic_chat_badge" else "ic_chat"
        , height $ V 20
        , width $ V 20
        ]
    ]

getCurrentAndroidVersion :: Merchant -> Int
getCurrentAndroidVersion merchant = case merchant of
  NAMMAYATRI -> 54
  YATRI -> 47
  YATRISATHI -> 1
  _ -> 1

checkVersionForChat :: Int -> Boolean
checkVersionForChat reqVersion =
  let
    currVersion = unsafePerformEffect getVersionCode
  in
    currVersion > reqVersion

callButton :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
callButton push config =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER
    , padding $ Padding 20 16 20 16
    , margin $ MarginLeft 8
    , background Color.white900
    , stroke $ "1," <> Color.black500
    , cornerRadius 30.0
    , alpha if config.accessibilityTag == Maybe.Just HEAR_IMPAIRMENT then 0.5 else 1.0
    , visibility if (config.currentStage == RideAccepted || config.currentStage == ChatWithCustomer) then VISIBLE else GONE
    , onClick push (const $ CallCustomer)
    , clickable (not (config.accessibilityTag == Maybe.Just HEAR_IMPAIRMENT))
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_phone"
        , height $ V 20
        , width $ V 20
        ]
    ]

rideActionViewWithLabel :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rideActionViewWithLabel push config =
  let
    specialZoneConfig = getRideLabelData config.specialLocationTag
  in
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , background $ specialZoneConfig.backgroundColor
      , cornerRadii $ Corners 25.0 true true false false
      , orientation VERTICAL
      , padding $ PaddingTop 5
      , gravity CENTER
      ]
      [ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER
          , id $ getNewIDWithTag "rideActionLabelLayout"
          ]
          [ imageView
              [ width $ V 18
              , height $ V 18
              , imageWithFallback $ specialZoneConfig.imageUrl
              ]
          , textView
              $ [ width WRAP_CONTENT
                , height MATCH_PARENT
                , text $ specialZoneConfig.text
                , gravity CENTER_VERTICAL
                , color Color.white900
                , margin $ MarginLeft 5
                ]
              <> FontStyle.getFontStyle FontStyle.Tags TypoGraphy
          , linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , visibility if Maybe.isJust config.accessibilityTag then VISIBLE else GONE
              ]
              [ textView
                  $ [ width WRAP_CONTENT
                    , height MATCH_PARENT
                    , text "|"
                    , gravity CENTER_VERTICAL
                    , color Color.white900
                    , margin $ MarginLeft 5
                    ]
                  <> FontStyle.getFontStyle FontStyle.Tags TypoGraphy
              , linearLayout
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  , margin $ MarginLeft 5
                  , onClick push $ const SecondaryTextClick
                  ]
                  [ textView
                      $ [ width WRAP_CONTENT
                        , height MATCH_PARENT
                        , text $ specialZoneConfig.secondaryText
                        , gravity CENTER_VERTICAL
                        , color Color.white900
                        ]
                      <> FontStyle.getFontStyle FontStyle.Tags TypoGraphy
                  , linearLayout
                      [ height $ V 1
                      , width MATCH_PARENT
                      , background Color.white900
                      , margin $ MarginHorizontal 1 2
                      ]
                      []
                  ]
              ]
          ]
      , rideActionView (MarginTop 6) push config
      ]

rideTypeView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rideTypeView push config =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding $ PaddingHorizontal 16 16
    , visibility if config.appConfig.rideActionModelConfig.showVehicleVariant && config.requestedVehicleVariant /= Maybe.Nothing then VISIBLE else GONE
    ]
    [ linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.grey800
        ]
        []
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ MarginTop 16
        ]
        [ textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ getString RIDE_TYPE <> ":"
              , color Color.black650
              ]
            <> FontStyle.body1 TypoGraphy
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text
                  $ case config.requestedVehicleVariant of
                      Maybe.Just variant -> getVariantRideType variant
                      Maybe.Nothing -> ""
              , margin $ MarginLeft 8
              , color $ getRideTypeColor config.requestedVehicleVariant
              ]
            <> FontStyle.body1 TypoGraphy
        ]
    ]

rideActionView :: forall w. Margin -> (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rideActionView layoutMargin push config =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , cornerRadii $ Corners 25.0 true true false false
    , orientation VERTICAL
    , background Color.white900
    , padding $ PaddingTop 6
    , gravity CENTER
    , margin layoutMargin
    , stroke $ "1," <> Color.grey800
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , id $ getNewIDWithTag "rideActionLayout"
        ]
        [ rideActionDataView push config
        , rideTypeView push config
        , linearLayout
            [ width MATCH_PARENT
            , height $ V 1
            , background Color.lightGrey
            , margin $ MarginTop 24
            ]
            []
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , padding $ Padding 16 16 16 24
            ]
            [ if config.startRideActive then startRide push config else endRide push config ]
        ]
    , cancelRide push config
    ]

openGoogleMap :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
openGoogleMap push config =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity RIGHT
    ]
    [ linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , background Color.blue900
        , padding $ Padding 24 16 24 16
        , margin $ MarginRight 16
        , cornerRadius 30.0
        , gravity CENTER
        , orientation HORIZONTAL
        , onClick push (const OnNavigate)
        ]
        [ imageView
            [ width $ V 20
            , height $ V 20
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_navigation"
            ]
        , textView
            ( [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , margin (MarginLeft 8)
              , text (getString MAPS)
              , gravity CENTER
              , color Color.white900
              ]
                <> FontStyle.body1 TypoGraphy
            )
        ]
    ]

rideActionDataView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rideActionDataView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (PaddingHorizontal 16 16)
    , gravity CENTER
    ]
    [ linearLayout
        [ width (V 34)
        , height (V 4)
        , cornerRadius 4.0
        , background Color.black500
        ]
        []
    , customerNameView push config
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            [ rideInfoView push config
            , if config.startRideActive then sourceAndDestinationView push config else destinationView config push
            ]
        ]
    ]

totalDistanceView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
totalDistanceView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity LEFT
    , orientation VERTICAL
    , weight 1.0
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text (getString RIDE_DISTANCE)
          , color Color.black650
          , ellipsize true
          , singleLine true
          ]
        <> FontStyle.body1 TypoGraphy
    , textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text config.totalDistance
          , color Color.black650
          , ellipsize true
          , singleLine true
          ]
        <> FontStyle.body11 TypoGraphy
    ]

sourceAndDestinationView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
sourceAndDestinationView push config =
  relativeLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginVertical 24 24
    , afterRender push $ const NoAction
    ]
    [ sourceDestinationImageView config
    , sourceDestinationTextView push config
    ]

startRide :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
startRide push config =
  PrestoAnim.animationSet
    [ scaleYAnimWithDelay (getAnimationDelay config)
    ]
    $ linearLayout
        [ width MATCH_PARENT
        , height (V 50)
        , background Color.darkMint
        , cornerRadius 8.0
        , gravity CENTER
        , onClick push (const $ StartRide)
        , pivotY 0.0
        , onAnimationEnd push $ const NoAction
        , afterRender push $ const NoAction
        ]
        [ textView
            ( [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text (getString START_RIDE)
              , color Color.white900
              , afterRender push $ const NoAction
              , padding (Padding 0 0 0 4)
              ]
                <> FontStyle.subHeading1 TypoGraphy
            )
        ]

endRide :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
endRide push config =
  linearLayout
    [ width MATCH_PARENT
    , height (V 50)
    , background Color.red
    , cornerRadius 8.0
    , gravity CENTER
    , onClick push (const $ EndRide)
    , afterRender push $ const NoAction
    ]
    [ textView
        ( [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text (getString END_RIDE)
          , color Color.white900
          , padding (Padding 0 0 0 4)
          , afterRender push $ const NoAction
          ]
            <> FontStyle.subHeading1 TypoGraphy
        )
    ]

cancelRide :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
cancelRide push config =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , background Color.white900
    , visibility if config.startRideActive then VISIBLE else GONE
    , padding $ PaddingBottom 16
    ]
    [ textView
        ( [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , padding $ Padding 16 8 16 8
          , text (getString CANCEL_RIDE)
          , color Color.red
          , onClick push (const CancelRide)
          ]
            <> FontStyle.body1 TypoGraphy
        )
    ]

customerNameView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
customerNameView push config =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , margin $ MarginVertical 16 20
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation VERTICAL
        , gravity START
        ]
        $ [ textView
              $ [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text $ getTitle config
                , color Color.greyTextColor
                , ellipsize true
                , singleLine false
                ]
              <> FontStyle.subHeading2 TypoGraphy
          ]
    ]

estimatedFareView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
estimatedFareView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity LEFT
    , orientation VERTICAL
    , weight 1.0
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text (getString RIDE_FARE)
          , color Color.black650
          , ellipsize true
          , singleLine true
          ]
        <> FontStyle.body1 TypoGraphy
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text $ currency <> (show config.estimatedRideFare)
              , color Color.black900
              , ellipsize true
              , singleLine true
              ]
            <> FontStyle.body10 TypoGraphy
        , if config.waitTimeSeconds > (config.thresholdTime + 60) then yellowPill push pillText (not config.startRideActive) else linearLayout [ visibility GONE ] []
        ]
    ]
  where
  currency = getCurrency appConfig

  pillText = "+" <> currency <> " " <> show (calculateCharges (config.waitTimeSeconds - config.thresholdTime))

  calculateCharges :: Int -> Number
  calculateCharges sec =
    let
      min = Int.floor $ Int.toNumber sec / 60.0
    in
      1.5 * Int.toNumber min

waitTimeView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
waitTimeView push config =
  linearLayout
    [ height WRAP_CONTENT
    , gravity START
    , orientation VERTICAL
    , weight 1.0
    , visibility if config.waitTimeSeconds /= -1 && config.notifiedCustomer && config.waitTimeStatus == ST.PostTriggered then VISIBLE else GONE
    ]
    [ linearLayout
        [ orientation HORIZONTAL
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width $ V 80
              , text (getString WAIT_TIME)
              , color Color.black650
              , textSize FontSize.a_14
              , ellipsize true
              , singleLine true
              ]
            <> FontStyle.body1 TypoGraphy
        , imageView
            [ height MATCH_PARENT
            , width $ V 25
            , visibility if config.notifiedCustomer then VISIBLE else GONE
            , onClick push (const WaitingInfo)
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_info_blue"
            ]
        ]
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text if config.waitTimeSeconds > config.thresholdTime then HU.formatSecIntoMinSecs config.thresholdTime else HU.formatSecIntoMinSecs config.waitTimeSeconds
              , color Color.black900
              , ellipsize true
              , textSize FontSize.a_20
              , singleLine true
              , fontStyle $ FontStyle.semiBold TypoGraphy
              ]
        , if config.waitTimeSeconds > config.thresholdTime then
            yellowPill push ("+ " <> HU.formatSecIntoMinSecs (config.waitTimeSeconds - config.thresholdTime)) false
          else
            linearLayout [ visibility GONE ] []
        ]
    ]

yellowPill :: forall w. (Action -> Effect Unit) -> String -> Boolean -> PrestoDOM (Effect Unit) w
yellowPill push text' showInfo =
  PrestoAnim.animationSet [ Anim.fadeIn true ]
    $ linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , background Color.yellow800
        , padding $ Padding 3 2 3 2
        , gravity CENTER_VERTICAL
        , margin $ Margin 2 2 0 0
        , cornerRadius 10.0
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text text'
              , color Color.black800
              , ellipsize true
              , singleLine true
              ]
            <> FontStyle.body9 TypoGraphy
        , imageView
            [ height $ V 12
            , width $ V 12
            , margin $ Margin 1 1 0 0
            , visibility if showInfo then VISIBLE else GONE
            , onClick push $ const WaitingInfo
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_info_blue"
            ]
        ]

rideInfoView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rideInfoView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , stroke $ "1," <> Color.grey900
    , cornerRadius 8.0
    , padding $ Padding 14 14 5 14
    , afterRender push $ const NoAction
    ]
    [ horizontalScrollView
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , scrollBarX false
        , fillViewport true
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            ]
            [ estimatedFareView push config
            , separator true
            , totalDistanceView push config
            , separator $ config.waitTimeSeconds /= -1 && config.notifiedCustomer && config.waitTimeStatus == ST.PostTriggered
            , waitTimeView push config
            , linearLayout
                [ weight 1.0
                , height MATCH_PARENT
                ]
                []
            ]
        ]
    ]

separator :: forall w. Boolean -> PrestoDOM (Effect Unit) w
separator visibility' =
  linearLayout
    [ weight 1.0
    , height MATCH_PARENT
    , margin $ MarginHorizontal 5 5
    , visibility if visibility' then VISIBLE else GONE
    ]
    [ linearLayout
        [ width $ V 1
        , background Color.lightGrey
        , height MATCH_PARENT
        ]
        []
    ]

sourceDestinationImageView :: forall w. Config -> PrestoDOM (Effect Unit) w
sourceDestinationImageView config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ imageView
        [ height $ V 14
        , width $ V 14
        , margin $ MarginTop 4
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_source_dot"
        ]
    , SeparatorView.view separatorConfig
    , imageView
        [ height $ V 14
        , width $ V 14
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_destination"
        ]
    ]

sourceDestinationTextView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
sourceDestinationTextView push config =
  linearLayout
    [ width WRAP_CONTENT
    , orientation VERTICAL
    , height WRAP_CONTENT
    , margin (MarginLeft 25)
    , afterRender push $ const NoAction
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text config.sourceAddress.titleText
          , id (getNewIDWithTag "sourceArea")
          , color Color.black800
          , ellipsize true
          , singleLine true
          , afterRender push $ const NoAction
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text config.sourceAddress.detailText
          , id (getNewIDWithTag "sourceAddress")
          , color Color.black650
          , margin (MarginBottom 25)
          , ellipsize true
          , singleLine true
          , afterRender push $ const NoAction
          ]
        <> FontStyle.body1 TypoGraphy
    , destAddressTextView config push
    ]

destinationView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
destinationView config push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin $ MarginVertical 24 24
    ]
    [ imageView
        [ height $ V 24
        , width $ V 24
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_loc_red"
        , margin $ Margin 0 3 8 0
        ]
    , destAddressTextView config push
    ]

lineImageView :: forall w. Int -> PrestoDOM (Effect Unit) w
lineImageView val =
  imageView
    [ height $ V val
    , width $ V 15
    , imageUrl "ic_line"
    , margin $ MarginLeft 7
    ]

dummyView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
dummyView push config =
  textView
    [ afterRender push $ const NoAction
    , width $ V 0
    , height $ V 0
    ]

destAddressTextView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
destAddressTextView config push =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text config.destinationAddress.titleText
          , id (getNewIDWithTag "destinationArea")
          , color Color.black800
          , ellipsize true
          , singleLine true
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text config.destinationAddress.detailText
          , id (getNewIDWithTag "destinationAddress")
          , color Color.black650
          , ellipsize true
          , maxLines if config.currentStage == RideAccepted || config.currentStage == ChatWithCustomer then 1 else 2
          ]
        <> FontStyle.body1 TypoGraphy
    ]

getTitle :: Config -> String
getTitle config = case config.startRideActive, config.notifiedCustomer of
  false, _ -> (getString YOU_ARE_ON_A_RIDE)
  true, false -> (config.customerName <> " " <> (getString IS_WAITING_FOR_YOU) <> "...")
  true, true -> case (getValueToLocalStore LANGUAGE_KEY) of
    "TA_IN" -> config.customerName <> (getString WAITING_FOR_CUSTOMER)
    "HI_IN" -> "आप" <> config.customerName <> "की प्रतीक्षा कर रहे हैं"
    _ -> (getString WAITING_FOR_CUSTOMER) <> config.customerName

separatorConfig :: SeparatorView.Config
separatorConfig =
  { orientation: VERTICAL
  , count: 6
  , height: V 4
  , width: V 2
  , layoutWidth: V 14
  , layoutHeight: V 16
  }

isSpecialRide :: Config -> Boolean
isSpecialRide config = (Maybe.isJust config.specialLocationTag) && Maybe.isJust (getRequiredTag config.specialLocationTag)

getAnimationDelay :: Config -> Int
getAnimationDelay config = 50
