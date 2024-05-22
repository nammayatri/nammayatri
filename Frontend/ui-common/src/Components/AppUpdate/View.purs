module Components.AppUpdate.View where

import Effect (Effect)
import Prelude (Unit, pure, unit, void, const, ($), (&&), (==), (<>))
import PrestoDOM
import Font.Style
import Font.Size as FontSize
import Styles.Types (FontStyle(..), FontSize(..))
import Components.AppUpdate.Controller (Action(..), Config)
import Common.Types.App (LazyCheck(..))
import Engineering.Helpers.Commons

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  linearLayout
  [  width MATCH_PARENT
    , height MATCH_PARENT
    , cornerRadius 8.0
    , gravity CENTER 
    , background config.background
    , margin $ Margin 16 16 16 0
    , padding $ Padding 12 12 12 12
    , visibility VISIBLE
    , orientation HORIZONTAL
    , singleLine false
  ][
    textView $
        [ height WRAP_CONTENT
        , text config.titleText
        , color config.titleTextColor
        , weight if os == "IOS" then 2.0 else 1.0
        , textSize FontSize.a_14
        , margin $ Margin 0 0 10 0
        , padding $ Padding 2 0 2 2
      
        ] <> (getFontStyle SubHeading1 TypoGraphy)
    , textView $
        [ 
        height WRAP_CONTENT
        , text config.actionText
        , color config.actionTextColor
        , background config.actionTextBgColor
        , weight if os == "IOS" then 1.0 else 0.0
        , padding $ Padding 12 8 12 8
        , margin $ Margin 5 2 0 2
        , gravity CENTER
        , cornerRadius 4.0
        , textSize FontSize.a_14
        , onClick push (const OnClick)
        ] <> (getFontStyle SubHeading1 TypoGraphy)
  ]
