module Screens.PaymentHistoryScreen.View where


import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Debug (spy)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Prelude (Unit, const, map, not, unit, ($), (&&), (-), (<<<), (<>), (>))
import PrestoDOM (Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, VDom, Visibility(..), afterRender, alignParentBottom, background, color, cornerRadius, fontStyle, gradient, gravity, height, imageView, imageWithFallback, lineHeight, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.PaymentHistoryScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (PaymentHistoryScreenState, PaymentHistorySubview(..))
import Styles.Colors as Color


screen :: PaymentHistoryScreenState -> Screen Action PaymentHistoryScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "PaymentHistoryScreen"
  , globalEvents: []
  , eval:
      ( \state action -> do
          let _ = spy "PaymentHistoryScreen ----- state" state
          let _ = spy "PaymentHistoryScreen --------action" action
          eval state action
      )
  }


view :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , onBackPressed push $ const BackPressed
  , afterRender push $ const AfterRender
  , background Color.white900
  ][ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
    , linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , weight 1.0
      , orientation VERTICAL
      ][ case state.props.subView of
          PaymentHistory -> paymentHistoryView push state
          TransactionDetails -> transactionDetails push state
          RideDetails -> rideDetails push state
      ]
  ]

paymentHistoryView :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> PrestoDOM (Effect Unit) w
paymentHistoryView push state = 
  PrestoAnim.animationSet [Anim.fadeIn (not state.props.autoPayHistory)] $
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginHorizontal 16 16
  ][ tabView state push
   , autoPayHistory push state

  ]

autoPayHistory :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> PrestoDOM (Effect Unit) w
autoPayHistory push state = 
  PrestoAnim.animationSet [Anim.fadeIn (not state.props.autoPayHistory)] $
  scrollView
    [ width MATCH_PARENT
    , weight 1.0
    , height WRAP_CONTENT
    , scrollBarY false
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ] (map (\item -> 
          linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , stroke $ "1," <> Color.grey900
            , cornerRadius 8.0
            , margin $ MarginVertical 6 6
            , padding $ Padding 16 16 16 16
            -- , onClick push $ const $ OnClick 
            ][ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                , orientation HORIZONTAL
                , margin $ MarginVertical 6 6
                ][ commonTV push "Pain on" Color.black700 (FontStyle.tags TypoGraphy) 0 LEFT
                , commonTV push "2 Oct 2023" Color.black700 (FontStyle.tags TypoGraphy) 0 LEFT -- convertUTCtoISC config.date "Do MMM YYYY"
                , linearLayout
                    [ height WRAP_CONTENT
                    , weight 1.0
                    ][]
                , commonTV push "₹ 23" Color.black700 (FontStyle.h2 TypoGraphy) 0 RIGHT
                , linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , cornerRadius 24.0
                    , padding $ Padding 5 5 5 5
                    , background Color.green600
                    ][ commonTV push "Success" Color.green900 (FontStyle.subHeading2 TypoGraphy) 0 CENTER ]
                ]
              , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , margin $ MarginTop 8
                , gravity CENTER
                ][ commonTV push "Rides taken on 2 days" Color.black700 (FontStyle.tags TypoGraphy) 0 CENTER
                , linearLayout
                    [ height WRAP_CONTENT
                    , weight 1.0
                    ][]
              , linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , orientation HORIZONTAL
                , gravity CENTER_VERTICAL
                , cornerRadius 20.0
                , background Color.grey700
                , padding $ Padding 8 5 8 5
                ][ imageView
                    [ width $ V 12
                    , height $ V 12
                    , margin (MarginRight 4)
                    , imageWithFallback "ny_ic_upi_logo,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_upi_logo.png"
                    ]
                  , commonTV push (if state.props.autoPayHistory then "UPI Autopay" else "UPI") Color.black700 (FontStyle.tags TypoGraphy) 0 CENTER
                  ]
                ]
            ]
        ) [1,2]) -- if state.props.autoPayHistory then autoPayArray else upiArray
    ]


commonTV :: forall w. (Action -> Effect Unit) -> String -> String -> (forall properties. (Array (Prop properties))) -> Int -> Gravity -> PrestoDOM (Effect Unit) w
commonTV push text' color' fontStyle marginTop gravity' =
  textView $
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text text'
  , color color'
  , gravity gravity'
  , margin $ MarginTop marginTop
  ] <> fontStyle

tabView :: forall w. PaymentHistoryScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
tabView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 24.0 
  , background Color.white900
  , padding $ Padding 6 6 6 6
  , gravity CENTER
  ][  textView
      [ height WRAP_CONTENT
      , weight 1.0 
      , background if state.props.autoPayHistory then Color.black900 else Color.white900
      , text "AutoPay Payments"
      , cornerRadius 24.0 
      , padding $ PaddingVertical 6 6
      , onClick push $ const $ ChangeTab
      , fontStyle $ FontStyle.medium LanguageStyle
      , gravity CENTER
      , color if state.props.autoPayHistory then Color.white900 else Color.black900
      ]
    , textView
      [ height WRAP_CONTENT
      , weight 1.0 
      , gravity CENTER
      , cornerRadius 24.0 
      , onClick push $ const $ ChangeTab
      , padding $ PaddingVertical 6 6
      , text "Manual Payments"
      , fontStyle $ FontStyle.medium LanguageStyle
      , background if not state.props.autoPayHistory then Color.black900 else Color.white900
      , color if not state.props.autoPayHistory then Color.white900 else Color.black900
      ]
  ]

transactionDetails :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> PrestoDOM (Effect Unit) w
transactionDetails push state = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      ][ imageView
          [ width MATCH_PARENT
          , height $ V 250
          , imageWithFallback "ny_ic_green_tick,"
          ]
        , commonTV push "Payment Successful!" Color.black900 (FontStyle.h2 TypoGraphy) 0 CENTER
        , commonTV push "Transaction On: 23 Aug 2023,5:50 PM" Color.black700 (FontStyle.body3 TypoGraphy) 0 CENTER
      ]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ] (map (\item -> 
            transactionHistoryRow push item $ case item of--(if item == "offer" then ) 
              "offer" -> promoCodeView push
              "Txn ID" -> rightItem push
              _ -> rightItem push 
        
            ) ["Txn ID", "amount Paid", "Txn ID", "amount Paid", "Txn ID", "amount Paid", "offer" , "Txn ID", "amount Paid", "Txn ID", "amount Paid"])

  ]

transactionHistoryRow ∷ ∀ (a ∷ Type). (Action → Effect Unit) -> String -> VDom (Array (Prop (Effect Unit))) a -> VDom (Array (Prop (Effect Unit))) a
transactionHistoryRow push item rightLayout = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginTop 2
  , background Color.blue600
  ][ commonTV push item Color.black700 (FontStyle.h2 TypoGraphy) 0 LEFT
   , linearLayout
      [ weight 1.0
      , height WRAP_CONTENT
      , gravity RIGHT
      ][ rightLayout ]
  ]

promoCodeView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
promoCodeView push =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , cornerRadius 100.0
  , padding $ Padding 10 4 10 4
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , margin $ MarginRight 4
  , gravity CENTER_VERTICAL
  , gradient (Linear 90.0 ["#FFE7C2", "#FFFFFF", "#DDFFEB"])
  ][ imageView
     [ width $ V 12
     , height $ V 12
     , margin (MarginRight 4)
    --  , visibility if state.prefix then VISIBLE else GONE
     , imageWithFallback "ny_ic_upi_logo"
     ] 
   , textView
     [ text "value"
     , textSize FontSize.a_10
     , fontStyle $ FontStyle.medium LanguageStyle
     , color Color.blue900
     , padding $ PaddingBottom 3
     ]
  , imageView
     [ width $ V 12
     , height $ V 12
     , margin (MarginRight 4)
    --  , visibility if state.postfix then VISIBLE else GONE
     , imageWithFallback ""
     ] 
  ]

rightItem :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
rightItem push =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity RIGHT
  ][ imageView
     [ width $ V 12
     , height $ V 12
     , margin (MarginRight 4)
    --  , visibility if state.prefix then VISIBLE else GONE
     , imageWithFallback ""
     ] 
   , textView
     [ text "value"
     , textSize FontSize.a_10
     , fontStyle $ FontStyle.medium LanguageStyle
     , color Color.blue900
     , padding $ PaddingBottom 3
     ]
  , imageView
     [ width $ V 12
     , height $ V 12
     , margin (MarginRight 4)
    --  , visibility if state.postfix then VISIBLE else GONE
     , imageWithFallback "ny_ic_copy,"
     ] 
  ]


rideDetails :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> PrestoDOM (Effect Unit) w
rideDetails push state = 
  linearLayout[][]





genericHeaderConfig :: PaymentHistoryScreenState -> GenericHeader.Config
genericHeaderConfig state =
  GenericHeader.config
    { height = WRAP_CONTENT
    , prefixImageConfig
      { height = V 30
      , width = V 30
      , margin = Margin 0 10 16 16
      , imageUrl = "ny_ic_back,https://assets.juspay.in/nammayatri/images/driver/ny_ic_back.png"
      , padding = Padding 5 5 5 5
      }
    , textConfig
      { text = "Payment History"
      , color = Color.black
      }
    , suffixImageConfig
      { visibility = GONE
      }
    }