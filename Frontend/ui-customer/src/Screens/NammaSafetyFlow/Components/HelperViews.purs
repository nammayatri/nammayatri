module Screens.NammaSafetyFlow.Components.HelperViews where

import Prelude
import PrestoDOM
import Common.Types.App (LazyCheck(..))
import Effect (Effect)
import Font.Style as FontStyle
import Helpers.Utils
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude
import Storage
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
    $ [ text $ getString $ RECOMMEND_EMERGENCY_CONTACTS_TO_INSTALL "RECOMMEND_EMERGENCY_CONTACTS_TO_INSTALL"
      , color Color.black700
      , background Color.blue600
      , padding $ Padding 8 12 8 12
      , margin $ MarginVertical 16 16
      , cornerRadius 8.0
      ]
    <> FontStyle.body1 LanguageStyle

safetyPartnerView :: forall w. LazyCheck -> PrestoDOM (Effect Unit) w
safetyPartnerView _lazy =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.blackOpacity12
    , gravity CENTER_VERTICAL
    , cornerRadius 12.0
    , margin $ MarginBottom 16
    , padding $ Padding 16 16 16 16
    , visibility $ boolToVisibility $ getValueToLocalStore CUSTOMER_LOCATION == "Bangalore"
    ]
    [ textView
        $ [ text $ getString OUR_SAFETY_PARTNER
          , color Color.white900
          , weight 1.0
          ]
        <> FontStyle.tags TypoGraphy
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_bangalore_police"
            , width $ V 36
            , height $ V 36
            , margin $ MarginRight 8
            ]
        , textView
            $ [ text $ getString BANGALURU_CITY_POLICE
              , color Color.white900
              ]
            <> FontStyle.tags TypoGraphy
        ]
    ]
