{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PaymentHistoryScreen.View where


import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style (Style(..), getFontStyle)
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, map, not, unit, ($), (&&), (-), (<<<), (<>), (>), (==))
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
  Anim.screenAnimationFadeInOut $
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
      [ height $ V 1
      , width MATCH_PARENT
      , background Color.grey900
      , margin $ MarginBottom 16
      ][]
    , linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , weight 1.0
      , orientation VERTICAL
      ][  paymentHistoryView push state (state.props.subView == PaymentHistory)
        , transactionDetails push state (state.props.subView == TransactionDetails)
        , rideDetails push state (state.props.subView == RideDetails)
      ]
  ]

paymentHistoryView :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> Boolean -> PrestoDOM (Effect Unit) w
paymentHistoryView push state visibility' = 
  PrestoAnim.animationSet [Anim.fadeIn visibility'] $
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginHorizontal 16 16
  , visibility if visibility' then VISIBLE else GONE
  ][ tabView state push
   , if DA.length state.data.paymentListItem > 0 then paymentList push state else noPaymentsView state push
  ]

paymentList :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> PrestoDOM (Effect Unit) w
paymentList push state = 
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
        , visibility if DA.length state.data.paymentListItem > 0 then VISIBLE else GONE
        ] (map (\item -> 
          linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , stroke $ "1," <> Color.grey900
            , cornerRadius 8.0
            , margin $ MarginVertical 6 6
            , padding $ Padding 16 16 16 16
            -- , onClick push $ const $ ListItemClick
            ][ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                , orientation HORIZONTAL
                , margin $ MarginVertical 6 6
                ][ commonTV push (getString PAID_ON) Color.black700 (FontStyle.tags TypoGraphy) 0 LEFT
                , commonTV push " 2 Oct 2023" Color.black700 (FontStyle.tags TypoGraphy) 0 LEFT -- convertUTCtoISC config.date "Do MMM YYYY"
                , linearLayout
                    [ height WRAP_CONTENT
                    , weight 1.0
                    ][]
                , commonTV push "₹23" Color.green900 (FontStyle.h2 TypoGraphy) 0 RIGHT
                , linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , cornerRadius 24.0
                    , padding $ Padding 8 4 8 4
                    , background "#1653BB6F"
                    , margin $ MarginLeft 4
                    ] [textView' push Nothing (getString SUCCESS) Color.green900 Body16 (Just $ PaddingTop 1) Nothing]
                ]
              , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                ][ commonTV push ((getString RIDES_TAKEN_ON) <> " 2 days") Color.black700 (FontStyle.tags TypoGraphy) 0 CENTER
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
                , background Color.white900
                , padding $ Padding 8 5 8 5
                ][ imageView
                    [ width $ V 12
                    , height $ V 12
                    , margin (MarginRight 4)
                    , imageWithFallback "ny_ic_upi_logo,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_upi_logo.png"
                    ]
                  , commonTV push (if state.props.autoPayHistory then (getString UPI_AUTOPAY_S) else "UPI") Color.black700 (FontStyle.tags TypoGraphy) 0 CENTER
                  ]
                ]
            ]
        ) state.data.paymentListItem) -- if state.props.autoPayHistory then autoPayArray else upiArray
    ]

noPaymentsView :: forall w. PaymentHistoryScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
noPaymentsView state push =  
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity CENTER
  , padding $ PaddingHorizontal 48 48
  , orientation VERTICAL
  ][ imageView
    [ width $ V 200
    , height $ V 200
    , gravity CENTER
    , imageWithFallback if state.props.autoPayHistory then "ny_no_automatic_payments," else "ny_no_manual_payments,"
    ]
  , textView
    [ text $ (if state.props.autoPayHistory then (getString AUTOMATIC_PAYMENTS_WILL_APPEAR_HERE) else (getString MANUAL_PAYMENTS_WILL_APPEAR_HERE) )
    , textSize FontSize.a_18
    , fontStyle $ FontStyle.bold LanguageStyle
    , color Color.black900
    , margin $ MarginBottom 10
    , gravity CENTER
    ]
  , textView
    [ text if state.props.autoPayHistory then (getString NO_AUTOMATIC_PAYMENTS_DESC) else (getString NO_MANUAL_PAYMENTS_DESC)
    , textSize FontSize.a_14
    , fontStyle $ FontStyle.regular LanguageStyle
    , color Color.black700
    , gravity CENTER
    ]
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

textView' :: forall w. (Action -> Effect Unit) -> Maybe Action -> String -> String -> Style -> Maybe Padding -> Maybe Margin -> PrestoDOM (Effect Unit) w
textView' push action txt txtColor style padding' margin' =  
  textView $
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , color txtColor
  , text txt
  , gravity CENTER
  ] <> (getFontStyle style LanguageStyle)
    <> case padding' of  
         Just value -> [padding value]
         Nothing -> []
    <> case margin' of  
        Just value -> [margin value]
        Nothing -> []
    <> case action of  
        Just value -> [onClick push $ const $ value]
        Nothing -> []

tabView :: forall w. PaymentHistoryScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
tabView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 24.0 
  , background Color.grey700
  , padding $ Padding 4 4 4 4
  , margin $ MarginBottom 28
  , gravity CENTER
  ][  textView
      [ height WRAP_CONTENT
      , weight 1.0 
      , background if state.props.autoPayHistory then Color.black900 else Color.grey700
      , text (getString AUTOPAY_PAYMENTS)
      , cornerRadius 24.0 
      , padding $ PaddingVertical 6 8
      , onClick push $ const $ ChangeTab
      , fontStyle $ FontStyle.medium LanguageStyle
      , gravity CENTER
      , color if state.props.autoPayHistory then Color.white900 else Color.black700
      ]
    , textView
      [ height WRAP_CONTENT
      , weight 1.0 
      , gravity CENTER
      , cornerRadius 24.0  
      , onClick push $ const $ ChangeTab
      , padding $ PaddingVertical 6 8
      , text (getString MANUAL_PAYMENTS)
      , fontStyle $ FontStyle.medium LanguageStyle
      , background if not state.props.autoPayHistory then Color.black900 else Color.grey700
      , color if not state.props.autoPayHistory then Color.white900 else Color.black700
      ]
  ]

transactionDetails :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> Boolean -> PrestoDOM (Effect Unit) w
transactionDetails push state visibility' = 
  PrestoAnim.animationSet [Anim.fadeIn visibility'] $
  scrollView
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , scrollBarY false
    , visibility if visibility' then VISIBLE else GONE
    ][  linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        ][ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER
            , margin $ MarginTop 20
            ][ imageView
                [ width MATCH_PARENT
                , height $ V 150
                , imageWithFallback "ny_ic_green_tick,"
                ]
              , commonTV push (getString PAYMENT_SUCCESSFUL) Color.black900 (FontStyle.h2 TypoGraphy) 10 CENTER
              , commonTV push ((getString TRANSACTION_ON) <> ": 23 Aug 2023,5:50 PM") Color.black700 (FontStyle.body3 TypoGraphy) 5 CENTER
            ]
          , linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , margin $ Margin 16 20 16 0
              , padding $ Padding 16 8 16 8
              , cornerRadius 18.0
              , gravity CENTER_VERTICAL
              , background Color.blue600
              ][ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , padding $ PaddingVertical 8 8
                ](DA.mapWithIndex (\ index item -> 
                  transactionHistoryRow push item.title index (DA.length state.data.transactionListItem) $ case item.key of
                    "OFFER" -> promoCodeView item.val
                    "TXN_ID" -> rightItem item.val false true
                    "PAYMENT_METHOD" -> rightItem item.val true false
                    _ -> commonTV push item.val Color.black700 (FontStyle.body3 TypoGraphy) 0 RIGHT
              
                  ) state.data.transactionListItem)
            , manualPaymentRidesList push state -- if manualPayment
              ]
        ]
    ]
  

transactionHistoryRow ∷ ∀ (a ∷ Type). (Action → Effect Unit) -> String -> Int -> Int -> VDom (Array (Prop (Effect Unit))) a -> VDom (Array (Prop (Effect Unit))) a
transactionHistoryRow push item index length rightLayout = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginTop 2
      , background Color.blue600
      , padding $ PaddingVertical 8 8
      ][ commonTV push item Color.black700 (FontStyle.body3 TypoGraphy) 0 LEFT
      , linearLayout
          [ weight 1.0
          , height WRAP_CONTENT
          , gravity RIGHT
          ][ rightLayout ]
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , background Color.white900
      , visibility if index == (length -1) then GONE else VISIBLE
      ][]
    ]
promoCodeView :: forall w. String -> PrestoDOM (Effect Unit) w 
promoCodeView val =
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
     , imageWithFallback "ny_ic_discount,"
     ] 
   , textView
     [ text val
     , textSize FontSize.a_10
     , fontStyle $ FontStyle.medium LanguageStyle
     , color Color.blue900
     , padding $ PaddingBottom 3
     ]
  ]

rightItem :: forall w. String -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w 
rightItem val prefixImage postfixImage =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity RIGHT
  ][ imageView
     [ width $ V 12
     , height $ V 12
     , margin (MarginRight 4)
     , visibility if prefixImage then VISIBLE else GONE
     , imageWithFallback "ny_ic_upi_logo,"
     ] 
   , textView
     [ text val
     , textSize FontSize.a_10
     , fontStyle $ FontStyle.medium LanguageStyle
     , color Color.blue900
     , padding $ PaddingBottom 3
     ]
  , imageView
     [ width $ V 12
     , height $ V 12
     , margin (MarginRight 4)
     , visibility if postfixImage then VISIBLE else GONE
     , imageWithFallback "ny_ic_copy,"
     ] 
  ]

manualPaymentRidesList :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> PrestoDOM (Effect Unit) w
manualPaymentRidesList push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity CENTER
  , visibility GONE
  , background Color.blue600
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ](map (\item -> 
            linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            ][ textView $
                [ text item.key
                , weight 1.0
                ] <> FontStyle.tags TypoGraphy
              , textView $
                [ text item.title
                , weight 1.0
                ] <> FontStyle.tags TypoGraphy
              , textView $
                [ text item.val
                , weight 1.0
                , gravity RIGHT
                ] <> FontStyle.tags TypoGraphy
            ]
            ) state.data.manualPaymentRidesListItem)
    , textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text (getString VIEW_RIDE_DETAILS)
      , color Color.black650
      , onClick push $ const ViewRideDetails
      ] <> FontStyle.body1 TypoGraphy
    
  ]


rideDetails :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> Boolean -> PrestoDOM (Effect Unit) w
rideDetails push state visibility' = 
  PrestoAnim.animationSet [Anim.fadeIn visibility'] $
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , visibility if visibility' then VISIBLE else GONE
  ][]



genericHeaderConfig :: PaymentHistoryScreenState -> GenericHeader.Config
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , height = V 25
      , width = V 25
      , margin = Margin 16 16 16 16
      } 
    , padding = PaddingVertical 5 5
    , textConfig {
        text = case state.props.subView of
                  PaymentHistory -> (getString PAYMENT_HISTORY)
                  TransactionDetails -> (getString TRANSACTION_DETAILS)
                  RideDetails -> (getString RIDE_DETAILS)
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'