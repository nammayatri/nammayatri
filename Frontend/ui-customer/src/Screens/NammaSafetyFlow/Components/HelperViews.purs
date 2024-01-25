module Screens.NammaSafetyFlow.Components.HelperViews where

import Prelude
import PrestoDOM

import Common.Types.App (LazyCheck(..))
import Effect (Effect)
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Styles.Colors as Color

emptyTextView :: forall w. PrestoDOM (Effect Unit) w
emptyTextView = textView [ visibility GONE ]

layoutWithWeight :: forall w. PrestoDOM (Effect Unit) w
layoutWithWeight = linearLayout [ weight 1.0 ] []

separatorView :: forall w. String -> Margin -> PrestoDOM (Effect Unit) w
separatorView color' margin' =
  linearLayout
    [ height (V 1)
    , width MATCH_PARENT
    , margin margin'
    , background color'
    ]
    []

recommendContactsToInstallView :: forall w. LazyCheck -> PrestoDOM (Effect Unit) w
recommendContactsToInstallView _lazy =
  textView
    $ [ text $ getString RECOMMEND_EMERGENCY_CONTACTS_TO_INSTALL
      , color Color.black700
      , background Color.blue600
      , padding $ Padding 8 12 8 12
      , margin $ MarginVertical 16 16
      , cornerRadius 8.0
      ]
    <> FontStyle.body1 LanguageStyle

