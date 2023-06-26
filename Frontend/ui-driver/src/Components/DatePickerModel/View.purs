module Components.DatePickerModel.View where

import Prelude

import Common.Types.App (LazyCheck(..))
import Components.DatePickerModel.Controller (Action(..), Config)
import Data.Array (mapWithIndex)
import Effect (Effect)
import Effect.Uncurried (runEffectFn2)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Font.Style as FontStyle
import JBridge (scrollToBottom)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), PrestoDOM, afterRender, background, color, cornerRadius, gravity, height, horizontalScrollView, id, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, scrollBarX, scrollBarY, stroke, text, textView, weight, width)
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginVertical 18 20
    ]
    [ horizontalScrollView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , scrollBarX false
        , scrollBarY false
        , id $ getNewIDWithTag config.id
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            ]
            ( mapWithIndex
                ( \index item ->
                    linearLayout
                      [ height $ V 48
                      , width $ V 48
                      , orientation VERTICAL
                      , background Color.grey700
                      , cornerRadius 24.0
                      , gravity CENTER
                      , margin $ MarginHorizontal 4 4
                      , stroke $ if index == config.activeIndex then "2," <> Color.blue800 else "0," <> Color.blue800
                      , onClick push $ const $ OnDateSelect index item
                      , id $ getNewIDWithTag (config.id <> show index)
                      ]
                      [ textView
                          $ [ height WRAP_CONTENT
                            , width WRAP_CONTENT
                            , text $ show item.date
                            , color if index == config.activeIndex then Color.blue800 else Color.black700
                            ]
                          <> FontStyle.subHeading1 LanguageStyle
                      , textView
                          $ [ height WRAP_CONTENT
                            , width WRAP_CONTENT
                            , text item.month
                            , color if index == config.activeIndex then Color.blue800 else Color.black700
                            ]
                          <> FontStyle.captions LanguageStyle
                      ]
                ) config.dates
                -- (runFn1 getAllDates 25)
            )
        ]
    ]
