module Components.ExtraChargeCard where

import PrestoDOM
import Prelude hiding(zero)
import Font.Style as FontStyle
import Styles.Colors as Colors
import Engineering.Helpers.Commons
import Components.PrimaryButton as PrimaryButton
import Effect
import Data.Maybe
import Common.Types.App
import Helpers.Utils
import Animation
import PrestoDOM.Animation as PrestoAnim
import Services.API
import Data.Array as DA
import Data.Int
import Resource.Localizable.StringsV2
import Resource.Localizable.TypesV2
import Language.Types
import Language.Strings (getString)
import Data.Function.Uncurried
import RemoteConfig as RC
import Storage

data Action = LearnMoreExtraChargeBtnAC PrimaryButton.Action

type ExtraChargeCardType = {
  showGotItBtnView :: Visibility
, badge :: BadgeConfig
}

type BadgeConfig = {
  pillTitle :: String
, title :: String
, subTitle :: Maybe String
, description :: Maybe String
, image :: String

, pillBackground :: String
, imageBackground :: String

, gaugeAngle :: Maybe Number
}


getBadgeConfig :: OverchargingTag -> Maybe Int -> Maybe Int -> BadgeConfig
getBadgeConfig stage mbRidesWithFareIssues mbTotalRidesConsideredForFareIssues =

  case stage of
    NoOverCharging -> zeroConfig
    VeryLowOverCharging -> lowConfig
    LowOverCharging -> lowConfig
    ModerateOverCharging ->  highConfig
    MediumOverCharging -> suspendedConfig
    HighOverCharging -> blockedConfig
    SuperOverCharging -> blockedConfig

  where
    city = getValueToLocalStore DRIVER_LOCATION
    config = RC.getExtraChargeConfig city

    degreeMapping = case  mbRidesWithFareIssues, mbTotalRidesConsideredForFareIssues of
      Just ridesWithFareIssues, Just totalRidesConsideredForFareIssues -> (toNumber(ridesWithFareIssues) * 60.0) / toNumber(totalRidesConsideredForFareIssues)  --runFn4 percentageToAngle ridesWithFareIssues totalRidesConsideredForFareIssues 60 [{ min: 0, max: 20 },{ min: 21, max: 30 },{ min: 31, max: 100}]
      _, _ -> 0.0

    outOfRidesStr = case  mbRidesWithFareIssues, mbTotalRidesConsideredForFareIssues of
      Just ridesWithFareIssues, Just totalRidesConsideredForFareIssues -> getString $ OUT_OF_RIDES (show ridesWithFareIssues) (show totalRidesConsideredForFareIssues)
      _, _ -> ""

    zeroConfig = {
      pillTitle: getStringV2 zero,
      title : getStringV2 fair_price_driver,
      subTitle : Nothing,
      description : Nothing,
      image : config.zeroImage,
      pillBackground : Colors.green900,
      imageBackground : "#1253BB6F",
      gaugeAngle : Nothing
    }

    lowConfig =  {
      pillTitle: getStringV2 low,
      title : outOfRidesStr,
      subTitle : Just $ getStringV2 extra_charged,
      description : Nothing,
      image : fetchImage COMMON_ASSET "ny_ic_ekd_low_gauge",
      pillBackground : Colors.yellow900,
      imageBackground : "#12FCC32C",
      gaugeAngle : Nothing
    }

    highConfig = {
      pillTitle: getStringV2 high,
      title :  outOfRidesStr,
      subTitle : Just $ getStringV2 extra_charged,
      description : Nothing,
      image : fetchImage COMMON_ASSET "ny_ic_ekd_heigh_gauge",

      pillBackground : Colors.orange900,
      imageBackground : "#12FF8B33",

      gaugeAngle : Nothing
    }

    suspendedConfig = {
      pillTitle: getStringV2 suspended,
      title :  outOfRidesStr,
      subTitle : Just $ getStringV2 extra_charged,
      description : Nothing,
      image : fetchImage COMMON_ASSET "ny_ic_ekd_suspended_gauge",

      pillBackground : Colors.red900,
      imageBackground : "#12E55454",

      gaugeAngle : Nothing
    }

    blockedConfig = {
      pillTitle: getStringV2 blocked,
      title :  outOfRidesStr,
      subTitle : Just $ getStringV2 extra_charged,
      description : Nothing,
      image : fetchImage COMMON_ASSET "ny_ic_blocked",

      pillBackground : Colors.red900,
      imageBackground : "#12E55454",

      gaugeAngle : Nothing
    }

extraChargesConfig :: OverchargingTag -> Maybe Int -> Maybe Int -> Visibility -> ExtraChargeCardType
extraChargesConfig overchargingTag mbRidesWithFareIssues mbTotalRidesConsideredForFareIssues gotItVis = {
  showGotItBtnView : gotItVis,
  badge: getBadgeConfig overchargingTag mbRidesWithFareIssues mbTotalRidesConsideredForFareIssues
}


view :: forall w. (Action -> Effect Unit) -> ExtraChargeCardType -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , background Colors.aliceBlueLight
  , margin $ MarginBottom 16
  , orientation VERTICAL
  , cornerRadius 16.0
  , padding $ PaddingBottom $ if config.showGotItBtnView == VISIBLE then 0 else 16
  ] $ [
    linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 16
    ][
      linearLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT
      ][
        linearLayout[
          width $ V $ ((screenWidth unit) - 64 ) / 2
        , height WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER
        ][
          pillView
        , titleView
        , subTitleView
        , descriptionView
        ]
      , linearLayout [weight 1.0, height WRAP_CONTENT] []
      , linearLayout[
          cornerRadius 16.0
        , background config.badge.imageBackground
        , padding $ PaddingVertical 15 15
        , gravity CENTER
        , width $ V $ (((screenWidth unit) - 64) / 2)
        , margin $ MarginHorizontal 16 16
        ][
          imageView[
            height $ V 80
          , width $ V $ ((screenWidth unit) - 64) / 2
          , imageWithFallback config.badge.image
          ]
        ]
      ]
    ]
  ] <> if config.showGotItBtnView == VISIBLE then [learnMoreBtn] else []

  where
    pillView =
      linearLayout[
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , padding $ Padding 16 4 16 4
      , background config.badge.pillBackground
      , cornerRadius 20.0
      , gravity CENTER
      , margin $ MarginBottom 8
      ][
        textView $ [
          text config.badge.pillTitle
        , color Colors.white900
        , padding $ PaddingBottom 4
        ] <> (FontStyle.body30 TypoGraphy)
      ]

    titleView =
      textView $ [
        text config.badge.title
      , height WRAP_CONTENT
      , width WRAP_CONTENT
      , color Colors.black800
      , gravity CENTER
      , margin $ MarginBottom 4
      ] <> (FontStyle.body4 TypoGraphy)

    subTitleView =
      case config.badge.subTitle of
        Just subTitle ->
          textView $ [
            text subTitle
          , color Colors.black800
          , gravity CENTER
          ] <> (FontStyle.body3 TypoGraphy)
        Nothing -> linearLayout [width $ V 0, height $ V 0][]

    descriptionView =
      case config.badge.description of
        Just description ->
          textView $ [
            text description
          , color Colors.black800
          , gravity CENTER
          ] <> (FontStyle.body3 TypoGraphy)
        Nothing -> linearLayout [width $ V 0, height $ V 0][]

    gaugeView =
      case config.badge.gaugeAngle of
        Just angle ->
          linearLayout
            [
              gravity CENTER
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , layoutGravity "bottom"
            , alignParentBottom "true,-1"

            ] [
                PrestoAnim.animationSet [ PrestoAnim.Animation [PrestoAnim.fromRotation 270, PrestoAnim.toRotation $ ceil(angle), PrestoAnim.interpolator interpolator, PrestoAnim.duration 2000] true ]$
                  imageView[
              height $ V 120
            , width $ V 20
             , margin $ MarginTop 35
            , rotation $ 270.0
            , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_ec_gauge"
            ]
            ]
        Nothing -> linearLayout [width $ V 0, height $ V 0][]


    learnMoreBtn  = PrimaryButton.view (push <<< LearnMoreExtraChargeBtnAC) learnMoreExtraChargeBtnConfig


learnMoreExtraChargeBtnConfig :: PrimaryButton.Config
learnMoreExtraChargeBtnConfig = PrimaryButton.config {
  textConfig {
    text = "Learn More"
  , color = Colors.blue800
  }
  , background = "#E2EFFF"
  , margin = Margin 16 16 16 16
}
