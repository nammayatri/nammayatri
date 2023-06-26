module Components.PaymentHistoryModel.View where

import Prelude
import PrestoDOM

import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PaymentHistoryModel.Controller (Action(..))
import Components.PaymentHistoryListItem as PaymentHistoryListItem
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Screens.Types (PaymentHistoryModelState)
import Styles.Colors as Color

view :: forall w . (Action -> Effect Unit) -> PaymentHistoryModelState -> PrestoDOM (Effect Unit) w
view push state = 
  linearLayout
  [ weight 1.0
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , padding $ PaddingHorizontal 16 16
  ][ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
  , downloadStatementView push state
  , paymentScrollView push state 
    ]

downloadStatementView :: forall w. (Action -> Effect Unit) -> PaymentHistoryModelState -> PrestoDOM (Effect Unit) w
downloadStatementView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , margin $ MarginVertical 16 24
    , padding $ Padding 5 5 5 5
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text "Download statement"
          , color Color.blue900
          , margin $ MarginRight 12
          ]
        <> FontStyle.captions LanguageStyle
    , imageView
        [ height $ V 16
        , width $ V 16
        , imageWithFallback "ny_ic_download_button,https://assets.juspay.in/nammayatri/images/driver/ny_ic_download_button.png"
        ]
    ]


paymentScrollView ::forall w. (Action -> Effect Unit) -> PaymentHistoryModelState -> PrestoDOM (Effect Unit) w
paymentScrollView push state = 
  scrollView
  [ width MATCH_PARENT
  , weight 1.0
  , height WRAP_CONTENT
  , scrollBarY false
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ](map (\item -> PaymentHistoryListItem.view (push <<< PaymentHistoryListItemAC) item) state.paymentHistoryList)
      

  ]

---------------- genericHeaderConfig ----------------
genericHeaderConfig :: PaymentHistoryModelState -> GenericHeader.Config 
genericHeaderConfig state = GenericHeader.config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = (V 30)
      , width = (V 30)
      , margin = (Margin 0 16 16 16)
      , imageUrl = "ny_ic_back,https://assets.juspay.in/nammayatri/images/driver/ny_ic_back.png"
      , padding = (Padding 5 5 5 5 )
      }
    , textConfig {
        text = "Fee payment history"
      , textSize = FontSize.a_20
      , color = Color.black
      , fontStyle = FontStyle.semiBold LanguageStyle
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }