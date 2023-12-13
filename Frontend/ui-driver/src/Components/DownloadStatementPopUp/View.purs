module Components.DownloadStatementPopUp.View where

import Prelude
import PrestoDOM
import Common.Types.App (LazyCheck(..))
import Components.DownloadStatementPopUp.Controller (Action, Config)
import Components.MenuButton as MenuButton
import Effect (Effect)
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push action =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , padding $ PaddingHorizontal 16 16
    , background Color.blackLessTrans
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , orientation VERTICAL
        , padding $ PaddingVertical 24 16
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , background Color.blue600
            ]
            [ textView
                $ [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , color Color.black800
                  , text $ getString DOWNLOAD_STATEMENT
                  ]
                <> FontStyle.h1 LanguageStyle
            ]
        , textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , color Color.black800
              , text $ getString SELECT_A_DATE_RANGE
              ]
            <> FontStyle.body3 LanguageStyle
        ]
    ]
