module Components.DropDownCard.View
  ( view
  )
  where

import Prelude
import Font.Style as FontStyle
import Styles.Colors as Color 
import PrestoDOM
import Components.DropDownCard.Controller
import Effect (Effect)
import Common.Types.App
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Mobility.Prelude (boolToVisibility)
import Debug (spy)
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Engineering.Helpers.Commons(os)
import Common.Animation.Config (estimateExpandingAnimationConfig)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ config.cardMargin
    , padding $ config.cardPadding
    , cornerRadius 16.0
    , stroke ("1," <> Color.grey900)
    , gravity CENTER_VERTICAL
    ]
    ([ cardHeadingLayout config push 
    , linearLayout
        [ width WRAP_CONTENT
        , visibility $ boolToVisibility $ config.isOpen
        ][]
    ] <> if config.isOpen then [config.layout] else [])


cardHeadingLayout :: Config -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
cardHeadingLayout config push = 
    PrestoAnim.animationSet
    ([ Anim.fadeInWithDuration 200 true ] <>
    (if os == "IOS" then []
    else [ Anim.listExpandingAnimation $ estimateExpandingAnimationConfig (0) (0.0) true ]))
  $linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ config.headingPadding
    , cornerRadius config.headingCornerRadius
    , background $ config.titleBackground
    , onClick push $ const $ OnClick config
    , gravity CENTER_VERTICAL
    ]
    [ textView $ 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , text config.title
        , weight 1.0
        , color Color.black800
        , margin $ MarginRight 4
        ] <> FontStyle.subHeading1 TypoGraphy
    , imageView 
        [ width $ config.imageWidth
        , height $ config.imageHeight
        , gravity CENTER_VERTICAL
        , imageWithFallback $ if config.isOpen then config.openArrowImage else config.closeArrowImage
        , color Color.black800
        ]
    ]