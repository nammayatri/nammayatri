module Components.DateTimeSelector.View where 



import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import Prelude
import Data.String (take)
import PrestoDOM (Gravity(..), Length(..), Margin(..),layoutGravity, Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), imageWithFallback,background, color, fontStyle, gravity, height, linearLayout, imageView, margin, orientation, padding, text, textSize, textView, weight, width, onClick, alpha, cornerRadius, id, visibility, stroke)
import Components.DateTimeSelector.Controller 
import Effect (Effect)
import Helpers.Utils (FetchImageFrom(..), fetchImage)



view :: forall w. (Action -> Effect Unit) -> DateSelectorConfig -> PrestoDOM (Effect Unit) w
view push config =  linearLayout
    [ width config.baseWidth
    , height config.baseHeight
    , orientation config.baseOrientation
    , margin $ ( Margin 10 12 10 12)
    , onClick push $ const $ OnClick config.titleConfig
    ]
    [ textView $
        [ text config.titleConfig
        , color config.textColor
        , margin config.textMargin
        ] <> FontStyle.tags TypoGraphy
    , linearLayout
        [ height config.pickerHeight
        , width config.pickerWidth
        , cornerRadius config.pickerCornerRadius
        , background config.pickerBackground
        , padding config.pickerPadding
        , stroke $ "1," <> Color.grey900
        ]
        [ textView $
            [ text config.selectDateText
            , height config.dateHeight
            , color config.dateColor
            , weight 0.8
            ] <> FontStyle.h3 TypoGraphy 
        , imageView
            [ height config.iconHeight
            , width config.iconWidth
            , gravity RIGHT
            , weight 0.2
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_calendar"
            ]
        ]
    ]
