module Components.ProviderModel.View where

import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, (<>), bind, ($), pure, unit, show, (+), (>=), (&&), (>), map, (==), negate, (<<<))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Orientation(..), Visibility(..), Accessiblity(..), PrestoDOM, alignParentBottom, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, stroke, text, textSize, textView, weight, width, imageWithFallback, lottieAnimationView, id, afterRender, visibility, background, padding, accessibilityHint, accessibility, rippleColor, cornerRadius, relativeLayout)
import Styles.Colors as Color
import Components.ProviderModel.Controller (Action(..), Config(..))
import Data.Array as Array
import Data.Maybe as Maybe
import Common.Types.App (LazyCheck(..))
import Helpers.Utils as HU
import PrestoDOM.Animation as PrestoAnim
import Components.PrimaryButton as PrimaryButton
import Animation as Anim
import Animation.Config as AnimConfig
import Mobility.Prelude (boolToVisibility)
import Language.Strings (getString)
import Language.Types (STR(..))
import Engineering.Helpers.Commons (os)

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  let active = config.isActive
      showAnim = if os == "IOS" then false else (active && config.showExpandAnim)
  in  PrestoAnim.animationSet [Anim.listExpandingAnimation $ listExpandConfigs showAnim] $
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , padding $ Padding 16 16 16 16
      , cornerRadius 8.0
      , stroke $ "1," <> if active then Color.blue900 else Color.grey900
      , background if active then Color.blue600 else Color.white900
      , gravity CENTER_VERTICAL
      , onClick push $ const $ FavClick config
      , margin $ MarginTop 16
      ][  linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER_VERTICAL
          ][  providerLogoAndVehicle push config
            , titleAndCapacity push config
            , linearLayout[weight 1.0][]
            , textView $
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text config.priceRange
              , margin $ MarginRight 10
              ] <> FontStyle.subHeading1 LanguageStyle
          ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , visibility $ boolToVisibility active
            ][  linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , visibility config.pillsVisibility
                ][  favPills active (getString LIVE_CHAT) (getString DRIVER_TIP_ADDITION)
                  , favPills active (getString LIVE_RIDE_SHARING) (getString ENHANCED_SAFETY)
                ]
              , PrimaryButton.view (push <<< ButtonClick) (buttonConfig config)
            ]
      ]

providerLogoAndVehicle :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
providerLogoAndVehicle push config = 
  relativeLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , padding $ PaddingVertical 10 10
  ][  imageView[ 
        height $ V 36
      , width $ V 36
      , margin $ MarginLeft 20
      , imageWithFallback $ HU.fetchImage HU.FF_ASSET config.vehicleImage
      ]
    , imageView
      [ height $ V 36
      , width $ V 36
      , imageWithFallback $ HU.fetchImage HU.FF_ASSET config.logo
      ]
  ]


titleAndCapacity :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
titleAndCapacity push config =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , orientation VERTICAL
  , margin $ MarginLeft 5
  ][  textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , color Color.black800
        , text config.name
        , padding $ PaddingBottom 1
        ] <> FontStyle.subHeading1 LanguageStyle
      , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER
        ][  textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text config.vehicleType
            , color Color.black700
            ] <> FontStyle.tags TypoGraphy
          , linearLayout
            [ width $ V 2
            , height $ V 2
            , cornerRadius 5.0
            , margin $ MarginHorizontal 5 5
            , background Color.grey900
            ][]
          , imageView
            [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ic_user_filled"
            , width $ V 14
            , height $ V 14
            ]
          , textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text config.capacity
            , color Color.black700
            ] <> FontStyle.tags TypoGraphy
        ]
  ]

favPills :: forall w. Boolean -> String -> String -> PrestoDOM (Effect Unit) w 
favPills isActive item1 item2 = 
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , margin $ MarginTop 10
  ](map (\item ->  
      linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , cornerRadius 14.0
      , background if isActive then Color.white900 else Color.blue600
      , gravity CENTER
      , margin $ MarginRight 10
      , padding $ Padding 10 7 10 7
      ][  imageView
          [ height $ V 12
          , width $ V 12
          , margin $ MarginRight 4
          , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_star"
          ]
        , textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text item
          , color Color.blue900
          ] <> FontStyle.body9 LanguageStyle
      ]
  ) [item1,item2])

listExpandConfigs :: Boolean -> AnimConfig.AnimConfig
listExpandConfigs isAnim = 
  AnimConfig.animConfig 
  { fromScaleY = if isAnim then 0.0 else 1.0
  , toScaleY =if isAnim then 1.0 else 0.0
  , fromY = if isAnim then -100 else 0
  , toY = if isAnim then 0 else -100
  , repeatCount = (PrestoAnim.Repeat 0)
  , ifAnim = isAnim
  , duration = 50}

buttonConfig :: Config -> PrimaryButton.Config
buttonConfig config =
  PrimaryButton.config
    { textConfig
      { text = getString CONFIRM_PROVIDER
      }
    , margin = MarginVertical 12 12
    , id = "ConfirmProvider" <> config.id
    , enableRipple = true
    , rippleColor = Color.rippleShade
    }
