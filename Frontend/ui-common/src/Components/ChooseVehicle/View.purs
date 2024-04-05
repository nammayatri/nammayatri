module Components.ChooseVehicle.View where

import Common.Types.App

import Components.ChooseVehicle.Controller (Action(..), Config, SearchType(..))
import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, ($), (<>), (==), (&&), not, pure, unit, (+), show, (||), negate)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, relativeLayout, stroke, text, textView, visibility, weight, width, id, afterRender, layoutGravity, singleLine, ellipsize)
import Common.Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Debug
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Mobility.Prelude (boolToVisibility)
import ConfigProvider
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Animation.Config (translateFullYAnimWithDurationConfig, translateYAnimConfig, Direction(..), AnimConfig, animConfig)
import Mobility.Prelude (boolToInvisibility)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  relativeLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ]
  [
  PrestoAnim.animationSet
  [ --Anim.scaleYAnim autoAnimConfig{ifAnim = isActiveIndex}
  --, Anim.inverseScaleYAnim autoAnimConfig{ifAnim = not isActiveIndex}
  Anim.fadeInWithDuration 400 isActiveIndex,
  Anim.fadeOutWithDuration 400 $ not isActiveIndex
  ] $ cardView push config isActiveIndex false
  , cardView push config false true
  ]
  where
    isActiveIndex = config.index == config.activeIndex

cardView :: forall w. (Action -> Effect Unit) -> Config -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w
cardView push config showBackground isVisible = 
  let isActiveIndex = spy "isActiveIndex" $ (spy "config.index" config.index) == (spy "config.activeIndex" config.activeIndex)
      stroke' = if isActiveIndex then "2," <> Color.blue800 else "1," <> Color.white900
      background' = if isActiveIndex then Color.blue600 else Color.white900
      padding' = Padding 8 16 12 16
  in 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , background $ if showBackground then background' else Color.transparent
  , cornerRadius 6.0
  , id $ EHC.getNewIDWithTag config.id
  , stroke $ if showBackground then stroke' else "0," <> Color.transparent
  , margin $ config.layoutMargin
  , padding padding'
  , clickable config.isEnabled
  , onClick push $ const $ OnSelect config
  , afterRender push (const NoAction)
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , afterRender push (const NoAction)
      , visibility $ boolToInvisibility isVisible
      ][ linearLayout
          [ height $ V 48
          , width $ V 60
          ][imageView
            [ imageWithFallback config.vehicleImage
            , height $ V if config.vehicleVariant == "AUTO_RICKSHAW" then 45 else 48
            , width $ V 60
            ]
          ]
        , linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , weight 1.0
          ][ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              ][ linearLayout
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , orientation VERTICAL
                  , gravity CENTER_VERTICAL
                  , padding $ PaddingLeft 8
                  ][ vehicleDetailsView push config
                  , linearLayout
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , padding $ PaddingTop 5
                    , gravity CENTER_VERTICAL
                    ][ capacityView push config 
                      , PrestoAnim.animationSet
                        [ Anim.fadeOut isActiveIndex
                        ]
                        $ shortDescriptionView "Basic" --config.shortDescription
                    ]
                  ]
                  , linearLayout [weight 1.0][]
                  , linearLayout
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , gravity RIGHT
                    , afterRender push (const NoAction)
                    ][priceDetailsView push config]
              ]
              , descriptionView "Air Conditioned, comfortable rides" isActiveIndex
        ]
      ]
  ]

vehicleDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
vehicleDetailsView push config =
  linearLayout
    [ height WRAP_CONTENT
    , weight 1.0
    , orientation HORIZONTAL
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , singleLine true
          , ellipsize true
          , text $ getVehicleName config
          , color Color.black800
          ]
        <> FontStyle.body7 TypoGraphy
    ]
  where 
    getVehicleName :: Config -> String
    getVehicleName config = 
      case (getMerchant FunctionCall) of
        YATRISATHI -> case config.vehicleVariant of
                        "TAXI" -> "Non AC Taxi"
                        "SUV"  -> "AC SUV"
                        _      -> "AC Cab"
        _          -> case config.vehicleVariant of
                        "AUTO_RICKSHAW" -> "Auto Rickshaw"
                        "TAXI" -> "Non-AC Taxi"
                        "TAXI_PLUS" -> "AC Taxi"
                        "SEDAN" -> "Sedan"
                        "SUV" -> "SUV"
                        "HATCHBACK" -> "Hatchback"
                        _ -> "Non-AC Taxi"

priceDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
priceDetailsView push config =
  let isActiveIndex = config.index == config.activeIndex
      infoIcon = if isActiveIndex then "ny_ic_info_blue_lg" else "ny_ic_info_grey"
  in
  linearLayout
    [ height MATCH_PARENT
    , width $  WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ PaddingLeft 8
    , gravity CENTER_VERTICAL
    , clickable isActiveIndex
    , onClick push $ const $ ShowRateCard config
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text config.price
          , color Color.black800
          ]
        <> FontStyle.body7 TypoGraphy
      , imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET infoIcon
        , width $ V 15
        , height $ V 15
        , gravity CENTER_VERTICAL
        , margin $ MarginLeft 4
        , visibility $ boolToVisibility $ config.showInfo
        ]
    ]

capacityView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
capacityView push config = 
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    ][ vehicleInfoView "ic_user_filled" config.capacity]

vehicleInfoView :: forall w. String -> String -> PrestoDOM (Effect Unit) w
vehicleInfoView imageName description = do
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER
    ][ imageView
        [ imageWithFallback $ fetchImage FF_ASSET imageName
        , width $ V 14
        , height $ V 14
        ]
      , textView
          $ [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text description
            , color Color.black700
            ]
          <> FontStyle.tags TypoGraphy
    ]

shortDescriptionView :: forall w. String -> PrestoDOM (Effect Unit) w
shortDescriptionView description = 
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
    ][ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_circle"
        , width $ V 3
        , height $ V 3
        , margin $ MarginHorizontal 2 0
        ]
     , imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_circle"
        , width $ V 14
        , height $ V 14
        , margin $ MarginHorizontal 2 2
        ]   
     ,  textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text description
          , color Color.black700
          ]
        <> FontStyle.tags TypoGraphy
    ]

descriptionView :: forall w. String -> Boolean -> PrestoDOM (Effect Unit) w
descriptionView description isSelected = 
  PrestoAnim.animationSet
   [ Anim.translateYAnimFromTopWithAlpha $ translateYAnimConfig{ifAnim = isSelected, fromY = -10, toY = 0, duration = 400} ]
   $ linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , padding $ Padding 8 8 0 0
    , visibility $ boolToVisibility isSelected
    ][  imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_circle"
        , width $ V 14
        , height $ V 14
        , margin $ MarginHorizontal 2 2
        ]   
      , textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text description
          , color Color.black700
          ]
        <> FontStyle.tags TypoGraphy
    ]

autoAnimConfig :: AnimConfig
autoAnimConfig =
  let
    config = animConfig
    autoAnimConfig' =
      config
        { duration = 400
        , toScaleX = 1.0
        , toScaleY = 1.0
        }
  in
    autoAnimConfig'