module Components.PaymentHistoryListItem.View where

import Animation (translateInXForwardAnim)
import Common.Types.App (LazyCheck(..))
import Domain.Payments (PaymentStatus(..))
import Components.PaymentHistoryListItem.Controller (Action(..), Config)
import Effect (Effect)
import Engineering.Helpers.Commons (convertUTCtoISC, screenWidth, formatCurrencyWithCommas)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, map, show, unit, ($), (-), (/), (<>), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, circularLoader, color, cornerRadius, fontStyle, gravity, height, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  let styleConfig = getPaymentCardConfig config.status
  in
    PrestoAnim.animationSet
    [ translateInXForwardAnim true] $
   linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background if config.isSelected then styleConfig.bgColor else Color.white900
    , stroke $ "1," <> Color.grey900
    , cornerRadius 8.0
    , margin $ MarginVertical 6 6
    , padding $ Padding 16 16 16 16
    , onClick push $ const $ OnClick config.id
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , orientation HORIZONTAL
        , margin $ MarginVertical 6 6
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text $ convertUTCtoISC config.date "Do MMM YYYY"
              , color Color.black700
              ]
            <> FontStyle.tags LanguageStyle
        , linearLayout
            [ height $ V 5
            , width $ V 5
            , cornerRadius 2.5
            , margin $ MarginHorizontal 5 5
            , background Color.black700
            ]
            []
        , textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text $ show config.totalRides <> " " <> getString RIDES
              , color Color.black700
              ]
            <> FontStyle.tags LanguageStyle
        , linearLayout
            [ height WRAP_CONTENT
            , weight 1.0
            ]
            []
        , textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text $ "₹" <> formatCurrencyWithCommas (show config.charges)
              , color styleConfig.textColor
              ]
            <> FontStyle.h2 LanguageStyle
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ PaddingVertical 6 6
        , gravity CENTER
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text "You earned"
              , color Color.black700
              ]
            <> FontStyle.tags LanguageStyle
        , textView
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ " ₹" <> formatCurrencyWithCommas (show config.totalEarning)
            , color Color.black800
            , textSize FontSize.a_14
            , fontStyle $ FontStyle.semiBold TypoGraphy
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , weight 1.0
            ]
            []
        , textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text styleConfig.paymentStatusText 
              , color styleConfig.textColor
              ]
            <> FontStyle.tags LanguageStyle
        ]
     , paymentBreakupView config
    ]

paymentBreakupView :: forall w. Config -> PrestoDOM (Effect Unit) w
paymentBreakupView config = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility if config.isSelected then VISIBLE else GONE
    ] $ [ breakupView (getString TOTAL_PAYABLE) (" ₹" <> formatCurrencyWithCommas (show config.charges))
      , dashDividerView
    ] <> (map (\item -> breakupView item.description ("₹ " <> formatCurrencyWithCommas (show item.amount))) config.paymentBreakUp)

breakupView :: forall w. String -> String -> PrestoDOM (Effect Unit) w
breakupView description amount = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , margin $ MarginVertical 7 7
  ][ textView
      $ [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text description
        , color Color.black700
        ]
      <> FontStyle.tags LanguageStyle
    , linearLayout
      [ height WRAP_CONTENT
      , weight 1.0
      ][]
    , textView
      $ [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text amount
        , color Color.black700
        ]
      <> FontStyle.tags LanguageStyle
  ]

dashDividerView :: forall w. PrestoDOM (Effect Unit) w
dashDividerView = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ MarginVertical 4 4
  ](map (\_ -> linearLayout[height $ V 1
  , width $ V 5
  , background Color.black500
  , cornerRadius 0.1
  , margin $ MarginHorizontal 2 2
  ][]) (getArray $ (screenWidth unit) / 5))


getArray :: Int ->Array Int
getArray count = if count == 0 then [count] else [count] <> (getArray (count - 1))

getPaymentCardConfig :: PaymentStatus -> PaymentCardConfig
getPaymentCardConfig status = case status of
  Pending ->{
    textColor : Color.yellow900 
  , bgColor : Color.yellow100
  , paymentStatusText : getString PAYMENT_PENDING
  } 
  Success -> {
    textColor : Color.darkMint
  , bgColor : Color.green100
  , paymentStatusText : getString PAYMENT_SUCCESSFUL
  }
  Failed -> {
    textColor : Color.red 
  , bgColor : Color.red100
  , paymentStatusText : getString PAYMENT_FAILED
  }
  Scheduled -> {
    textColor : Color.yellow900 
  , bgColor : Color.yellow100
  , paymentStatusText : getString PAYMENT_SCHEDULED
  } 

type PaymentCardConfig =  {
  textColor :: String
  , bgColor :: String
  , paymentStatusText :: String
}
