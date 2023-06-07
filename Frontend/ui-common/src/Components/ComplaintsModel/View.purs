module Components.ComplaintsModel.View
  ( view
  ) where

import Common.Types.App
import Components.ComplaintsModel.Controller (CardData, Config)
import Data.Maybe (fromMaybe, isJust)
import Data.String (Pattern(..), contains, indexOf, lastIndexOf)
import Data.String.CodeUnits (slice)
import Effect (Effect)
import Font.Style as FontStyle
import JBridge (getWidthFromPercent, openUrlInMailApp)
import JBridge as JB
import Prelude (Unit, bind, const, map, pure, unit, ($), (+), (<>))
import PrestoDOM (Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, background, color, cornerRadius, height, horizontalScrollView, linearLayout, margin, onClick, orientation, padding, scrollBarX, stroke, text, textFromHtml, textView, width)
import Styles.Colors as Color

view :: forall w. Config -> PrestoDOM (Effect Unit) w
view config =
  horizontalScrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , scrollBarX false
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        ]
        (map (\item -> infoComponentView item) config.cardData)
    ]

infoComponentView :: forall w. CardData -> PrestoDOM (Effect Unit) w
infoComponentView item =
  linearLayout
    [ height WRAP_CONTENT
    , width $ V (getWidthFromPercent 60)
    , orientation VERTICAL
    , padding $ Padding 12 16 12 16
    , stroke $ "1," <> Color.grey900
    , cornerRadius 8.0
    , background Color.white900
    , margin $ MarginRight 12
    ]
    ( [ textView
          $ [ height WRAP_CONTENT
            , width MATCH_PARENT
            , color Color.black900
            , text item.title
            , margin $ MarginBottom 8
            ]
          <> FontStyle.body3 TypoGraphy
      , textView
          $ [ height WRAP_CONTENT
            , width MATCH_PARENT
            , textFromHtml item.subTitle
            , color Color.black650
            ]
          <> (FontStyle.body3 TypoGraphy)
      ]
        <> if isJust item.addtionalData then
            [ textView
                $ [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , textFromHtml $ fromMaybe "" item.addtionalData
                  , color Color.black650
                  ]
                <> (FontStyle.body3 TypoGraphy)
                <> ( if contains (Pattern "<u>") (fromMaybe "" item.addtionalData) then
                      [ onClick
                          ( \_ -> do
                              _ <- JB.openUrlInApp $ fromMaybe "" (slice ((fromMaybe 0 $ indexOf (Pattern (">")) (fromMaybe "" item.addtionalData)) + 1) (fromMaybe 0 $ lastIndexOf (Pattern ("<")) (fromMaybe "" item.addtionalData)) (fromMaybe "" item.addtionalData))
                              pure unit
                          )
                          (const unit)
                      ]
                    else
                      []
                  )
            ]
          else
            []
    )