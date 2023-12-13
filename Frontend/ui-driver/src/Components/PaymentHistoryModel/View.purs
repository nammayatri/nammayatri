module Components.PaymentHistoryModel.View where

import Prelude
import Animation (translateInXAnim, translateInXForwardAnim)
import Animation.Config (animConfig, translateYAnimConfig)
import Common.Types.App (LazyCheck(..))
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.PaymentHistoryListItem as PaymentHistoryListItem
import Components.PaymentHistoryModel.Controller (Action(..))
import Data.Array (length, take)
import Effect (Effect)
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, color, height, imageView, imageWithFallback, linearLayout, margin, orientation, padding, relativeLayout, scrollBarY, scrollView, text, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Screens.Types (PaymentHistoryModelState)
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> PaymentHistoryModelState -> PrestoDOM (Effect Unit) w
view push state =
  PrestoAnim.animationSet
    [ translateInXAnim
        animConfig
          { fromX = 10
          , toX = 0
          , duration = 200
          }
    ]
    $ linearLayout
        [ weight 1.0
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding $ PaddingHorizontal 16 16
        ]
        [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
        , relativeLayout
            [ width MATCH_PARENT
            , weight 1.0
            ]
            $ [ linearLayout
                  [ weight 1.0
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  , background Color.white900
                  ]
                  [ downloadStatementView push state
                  , paymentScrollView push state
                  ]
              ]
            <> if (length state.paymentHistoryList) == 0 then [ ErrorModal.view (push <<< ErrorModalActionController) (errorModalConfig state) ] else []
        ]

downloadStatementView :: forall w. (Action -> Effect Unit) -> PaymentHistoryModelState -> PrestoDOM (Effect Unit) w
downloadStatementView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , margin $ MarginVertical 16 24
    , padding $ Padding 5 5 5 5
    , visibility GONE -- TODO
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ getString DOWNLOAD_STATEMENT
          , color Color.blue900
          , margin $ MarginRight 12
          ]
        <> FontStyle.captions LanguageStyle
    , imageView
        [ height $ V 16
        , width $ V 16
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_download_button"
        ]
    ]

paymentScrollView :: forall w. (Action -> Effect Unit) -> PaymentHistoryModelState -> PrestoDOM (Effect Unit) w
paymentScrollView push state =
  scrollView
    [ width MATCH_PARENT
    , weight 1.0
    , height WRAP_CONTENT
    , scrollBarY false
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        (map (\item -> PaymentHistoryListItem.view (push <<< PaymentHistoryListItemAC) item) (take 15 state.paymentHistoryList))
    ]

---------------- genericHeaderConfig ----------------
genericHeaderConfig :: PaymentHistoryModelState -> GenericHeader.Config
genericHeaderConfig state =
  GenericHeader.config
    { height = WRAP_CONTENT
    , prefixImageConfig
      { height = (V 30)
      , width = (V 30)
      , margin = (Margin 0 16 16 16)
      , imageUrl = fetchImage FF_ASSET "ny_ic_back"
      , padding = (Padding 5 5 5 5)
      }
    , textConfig
      { text = getString FEE_PAYMENT_HISTORY
      , color = Color.black
      }
    , suffixImageConfig
      { visibility = GONE
      }
    }

errorModalConfig :: PaymentHistoryModelState -> ErrorModal.Config
errorModalConfig state =
  ErrorModal.config
    { imageConfig
      { imageUrl = fetchImage FF_ASSET "ny_ic_no_payments"
      , height = V 110
      , width = V 124
      , margin = (MarginBottom 61)
      }
    , errorConfig
      { text = (getString NO_PAYMENT_HISTORY_AVAILABLE)
      , margin = (MarginBottom 7)
      , color = Color.black900
      }
    , errorDescriptionConfig
      { text = (getString YOU_DONT_HAVE_ANY_PAYMENTS)
      , color = Color.black700
      }
    , buttonConfig
      { text = (getString OKAY)
      , margin = (Margin 0 0 0 24)
      , background = Color.black900
      , color = Color.yellow900
      , visibility = VISIBLE
      }
    , height = MATCH_PARENT
    }
