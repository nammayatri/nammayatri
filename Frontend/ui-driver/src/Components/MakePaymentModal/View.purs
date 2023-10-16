module Components.MakePaymentModal.View where

import Prelude

import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..))
import Components.MakePaymentModal.Controller (Action(..), MakePaymentModalState, FeeItem, FeeOptions(..))
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Font.Style as FontStyle
import Halogen.VDom.DOM.Prop (Prop)
import Helpers.Utils (getAssetStoreLink)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (imageView, textView, linearLayout)
import PrestoDOM.Events (afterRender, onBackPressed, onClick)
import PrestoDOM.Properties (background, clickable, color, cornerRadii, cornerRadius, gravity, height, imageWithFallback, margin, orientation, padding, text, textFromHtml, visibility, weight, width, stroke)
import PrestoDOM.Types.Core (PrestoDOM)
import PrestoDOM.Types.DomAttributes (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), Corners(..))
import Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import JBridge as JB


view :: forall w . (Action -> Effect Unit) -> MakePaymentModalState -> PrestoDOM (Effect Unit) w
view push state = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , gravity BOTTOM
  , clickable true
  ][ PrestoAnim.animationSet [ Anim.translateYAnim AnimConfig.translateYAnimConfig ] $
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , cornerRadii $ Corners 20.0 true true false false
      , orientation VERTICAL
      , background Color.white900
      , padding $ Padding 16 10 16 (if isJust state.cancelButtonText then 0 else 16)
      , gravity CENTER
      , stroke $ "1," <> Color.grey900
      , clickable true
      ][ commonTV push state.title Color.black800 FontStyle.h2 CENTER 8 NoAction false (PaddingTop 0)
        , commonTV push state.description Color.black800 FontStyle.subHeading2 CENTER 8 NoAction true (PaddingTop 0)
        , paymentReview push state
        , commonTV push state.description2 Color.black800 FontStyle.body3 CENTER 8 NoAction false (PaddingTop 0)
        , primaryButton push state
        , case state.cancelButtonText of
            Just text -> commonTV push text Color.black650 FontStyle.subHeading2 CENTER 0 Cancel false (PaddingVertical 8 16)
            Nothing -> linearLayout[][]
      ]
  ]

primaryButton :: forall w . (Action -> Effect Unit) -> MakePaymentModalState -> PrestoDOM (Effect Unit) w
primaryButton push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ MarginTop 12
  ][PrimaryButton.view (push <<< PrimaryButtonActionController) (buttonConfig state)]

paymentReview :: forall w . (Action -> Effect Unit) -> MakePaymentModalState -> PrestoDOM (Effect Unit) w
paymentReview push state = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 8
    , background Color.blue600
    , cornerRadius 8.0
    , padding $ Padding 10 10 10 10
    ](DA.mapWithIndex (\index item -> 
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][  feeItem push state item
        , imageView
          [ width MATCH_PARENT
          , height $ V 2 
          , padding $ PaddingHorizontal 10 10
          , imageWithFallback $ "ny_ic_horizontal_dash,"<> (getAssetStoreLink FunctionCall) <>"ny_ic_horizontal_dash.png"
          , visibility if index == 0 then VISIBLE else GONE
          ]
      ]
      ) state.feeItem )
      
      
feeItem :: forall w . (Action -> Effect Unit) -> MakePaymentModalState -> FeeItem -> PrestoDOM (Effect Unit) w
feeItem push state item = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , padding $ Padding 10 10 10 10
  , cornerRadius 8.0
  , background if item.feeType == GST_PAYABLE then Color.yellow800 else Color.blue600
  , gravity CENTER_VERTICAL
  ][  textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text item.title
      , color Color.black800
      ] <> FontStyle.body1 TypoGraphy
    , imageView 
      [ height $ V 18
      , width $ V 18
      , margin $ MarginLeft 5
      , imageWithFallback $ "ny_ic_info_blue," <> (getAssetStoreLink FunctionCall) <> "ny_ic_information_grey.png"
      , visibility if item.feeType == GST_PAYABLE then VISIBLE else GONE
      , onClick push $ const Info
      ]
    , textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity RIGHT
      , weight 1.0
      , color Color.black800
      , text $ "₹" <> EHC.formatCurrencyWithCommas (show item.val)
      ] <> FontStyle.body6 TypoGraphy
  ]

commonTV :: forall w .  (Action -> Effect Unit) -> String -> String -> (LazyCheck -> forall properties. (Array (Prop properties))) -> Gravity -> Int -> Action -> Boolean -> Padding -> PrestoDOM (Effect Unit) w
commonTV push text' color' theme gravity' marginTop action txtFromHtml padding' = 
  textView $
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , color color'
  , gravity gravity'
  , margin $ MarginTop marginTop
  , onClick push $ const action
  , padding padding'
  , (if txtFromHtml then textFromHtml else text) text'
  ] <> theme TypoGraphy

buttonConfig :: MakePaymentModalState -> PrimaryButton.Config
buttonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        { text = state.okButtontext
        , color = Color.yellow900
        , width = MATCH_PARENT
        }
      , height = V 55
      , gravity = CENTER
      , cornerRadius = 8.0
      , background = Color.black900
      , margin = MarginHorizontal 16 16
      , id = "MakePaymentButton"
      , enableLoader = JB.getBtnLoader "MakePaymentButton"
      }
  in primaryButtonConfig'