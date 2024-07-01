module Components.DropDownCard.View where

import Prelude
import Font.Style as FontStyle
import Styles.Colors as Color 
import PrestoDOM
import Components.DropDownCard.Controller
import Effect (Effect)
import Common.Types.App
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Mobility.Prelude (boolToVisibility)




view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ Margin 16 0 16 16
    , padding $ Padding 16 16 16 16
    , cornerRadius 16.0
    , stroke ("1," <> Color.grey900)
    , gravity CENTER_VERTICAL
    ]
    ([ cardHeadingLayout config.title config push 
    , linearLayout
        [ width WRAP_CONTENT
        , height $ V 20
        , visibility $ boolToVisibility $ config.isOpen
        ][]
    ] <> if config.isOpen then [config.layout] else [])


cardHeadingLayout :: String -> Config -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
cardHeadingLayout heading config push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ Padding 2 2 2 2
    , onClick push $ const OnClick
    , gravity CENTER_VERTICAL
    ]
    [ textView $ 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
        , text heading
        , weight 1.0
        , color Color.black800
        ] <> FontStyle.h3 TypoGraphy
    , imageView 
        [ width $ V 26
        , height $ V 26
    , gravity CENTER_VERTICAL
        , imageWithFallback $ if config.isOpen then config.openArrowImage else config.closeArrowImage
        , color Color.black800
        ]
    ]