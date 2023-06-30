module Components.ChooseYourRide.View where

import Common.Types.App

import Components.ChooseVehicle as ChooseVehicle
import Components.ChooseYourRide.Controller (Action(..), Config)
import Components.PrimaryButton as PrimaryButton
import Components.MenuButton as MenuButton
import Data.Array (mapWithIndex)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), (<>), const, pure, unit, not, (<<<), map, (==), (/=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, letterSpacing, lineHeight, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width, relativeLayout, alignParentBottom, onBackPressed)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import Screens.Types (PaymentMode(..))
import Data.Maybe(fromMaybe, Maybe(..))

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , onBackPressed push $ const DisapperPaymentPage
  ]
  ([ ] <> (if (config.choosePaymentMode == true) then [choosePaymentModeView push config ]
    else [chooseRideView push config]))
 
chooseRideView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
chooseRideView push config =
 relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT 
    ][ 
       linearLayout
        [ orientation VERTICAL
        , alignParentBottom "true,-1"
        , height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , clickable true
        , padding $ PaddingVertical 16 24
        , stroke $ "1," <> Color.grey900
        , gravity CENTER
        , cornerRadii $ Corners 24.0 true true false false
        ]
        [ textView
            [ 
              text (getString CHOOSE_YOUR_RIDE)
            , textSize FontSize.a_22
            , color Color.black800
            , gravity CENTER_HORIZONTAL
            , height WRAP_CONTENT
            , width MATCH_PARENT
            , fontStyle $ FontStyle.bold LanguageStyle
            ]
        , estimatedTimeAndDistanceView push config
        , quoteListView push config
        , paymentMethodView push config
        , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonRequestRideConfig config)
        ]
    ]
choosePaymentModeView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
choosePaymentModeView push config =
  linearLayout
  [ orientation VERTICAL
    , height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , clickable true
    , padding $ PaddingVertical 16 24
    , gravity CENTER_HORIZONTAL
  ]
  [ textView
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , text (getString CHOOSE_MODE_OF_PAYMENT)
    , gravity CENTER_HORIZONTAL
    , color Color.black800
    , margin $ Margin 40 100 42 48
    , fontStyle $ FontStyle.bold LanguageStyle
    , textSize FontSize.a_26
    ] 
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ MarginHorizontal 16 16 
      , weight 1.0
      ]
      ( map
          ( \item ->
              linearLayout
              [
                height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
              ]
              [
                linearLayout
                [
                  height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER_VERTICAL
                ]
                [
                  imageView
                  [ imageWithFallback item.imageWithFallback
                  , height $ V 40
                  , width $ V 40
                  , margin (MarginRight 20)
                  ]
                  , linearLayout
                    [ height WRAP_CONTENT
                      , width $ V 252 
                      , orientation VERTICAL
                      , gravity CENTER_VERTICAL
                      , weight 1.0
                    ]
                    [
                      textView $ [
                        width WRAP_CONTENT
                      , height WRAP_CONTENT
                      , color Color.black800
                      , gravity CENTER_HORIZONTAL
                      , margin $ MarginBottom 4
                      , textSize FontSize.a_16
                      , text item.text
                      ] <> FontStyle.subHeading2 TypoGraphy
                      , textView $ [
                        width WRAP_CONTENT
                      , height WRAP_CONTENT
                      , color Color.black800
                      , gravity CENTER
                      , textSize FontSize.a_14
                      , text item.data
                      ] 
                    ]
                  ,linearLayout
                  [
                    height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , weight 1.0
                    , gravity CENTER_VERTICAL
                  ]
                  [
                    MenuButton.view (push <<< (MenuButtonActionController)) (menuButtonConfig config item.text)
                  ]
                  
                ]
                , if(item.type == ONLINE) then linearLayout
                    [ height $ V 1
                    , width MATCH_PARENT
                    , background Color.grey900
                    , margin $ MarginVertical 12 12
                    ]
                    []
                    else linearLayout[][]
              ]
          )
          (choosePaymentModeData)
      )
    , PrimaryButton.view (push <<< PrimaryButtonConfirmActionController) (primaryButtonConfirmRideConfig config)
  ]

menuButtonConfig :: Config -> String-> MenuButton.Config
menuButtonConfig item text = let
    config = MenuButton.config
    menuButtonConfig' = config {
      titleConfig{
        visibility = GONE
      }
      , subTitleConfig{
        visibility = GONE
      }
      , radioButtonConfig {
        height = V 16
        , width = V 16
        , imageHeight = V 10
        , imageWidth = V 10
        , cornerRadius = 10.0
      }
      , height = V 40
      , id = text
      , isSelected = text == fromMaybe "" item.modeOfPayment
    }
    in menuButtonConfig'

choosePaymentModeData :: Array { text :: String, imageWithFallback :: String, type :: PaymentMode, data :: String }
choosePaymentModeData  =
  [ { text: "Card / UPI"
    , imageWithFallback: "ny_ic_credit_card,https://assets.juspay.in/beckn/jatrisaathi/user/images/ny_ic_credit_card.png"
    , type: ONLINE
    , data: (getString PAY_ONLINE_ONCE_RIDE_HAS_ENDED)
    }
  , { text: "Cash"
    , imageWithFallback: "ny_ic_cash,https://assets.juspay.in/beckn/jatrisaathi/user/images/ny_ic_cash.png"
    , type: CASH
    , data: (getString PAY_THE_DRIVER_DIRECTLY_USING_CASH)
    }
  ]

paymentMethodView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
paymentMethodView push config =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , padding (Padding 16 16 16 0)
    , orientation VERTICAL
    ]
    [linearLayout
    [ height $ V 1
      , width MATCH_PARENT
      , background Color.grey900
      , margin $ MarginBottom 12 
    ][]
    , 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , onClick push (const (ShowPaymentMode))
    ]
    [
    if config.modeOfPayment /= Nothing then 
      imageView
        [ imageWithFallback if config.modeOfPayment == Just "Cash" then 
           "ny_ic_cash,https://assets.juspay.in/beckn/jatrisaathi/user/images/ny_ic_cash.png"
          else "ny_ic_credit_card,https://assets.juspay.in/beckn/jatrisaathi/user/images/ny_ic_credit_card.png"
        , height $ V 30
        , width $ V 30
        , padding (Padding 3 3 3 3)
        ]
    else linearLayout[][]
    , textView
    $
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , textSize FontSize.a_16
      , margin $ MarginLeft 8
      , text if config.modeOfPayment == Nothing then  (getString CHOOSE_PAYMENT_METHOD)
        else fromMaybe "" config.modeOfPayment 
      , gravity CENTER_VERTICAL
      , color Color.black800
      , weight 1.0
      ]
    , imageView
        [ imageWithFallback "ny_ic_chevron_right,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_right.png"
        , height $ V 30
        , width $ V 32
        , padding (Padding 3 3 3 3)
        ]
    ]
    ]

estimatedTimeAndDistanceView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
estimatedTimeAndDistanceView push config =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER
    , margin $ MarginTop 4
    ]
    [ textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.rideDistance
        , color Color.black650
        ]
        <> FontStyle.paragraphText TypoGraphy
    , linearLayout
        [ height $ V 4
        , width $ V 4
        , cornerRadius 2.5
        , background Color.black600
        , margin (Margin 6 2 6 0)
        ]
        []
    , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.rideDuration
        , color Color.black650
        ]
        <> FontStyle.paragraphText TypoGraphy
    ]

quoteListView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
quoteListView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 16
    ]
    ( mapWithIndex
        ( \index item ->
            ChooseVehicle.view (push <<< ChooseVehicleAC) (item)
        ) config.quoteList
    )

primaryButtonRequestRideConfig :: Config -> PrimaryButton.Config
primaryButtonRequestRideConfig config = PrimaryButton.config
  { textConfig
    { text = (getString CONFIRM_AND_BOOK)
    , color = Color.yellow900
    , textSize = FontSize.a_16
    }
  , background = Color.black900
  , margin = Margin 16 32 16 15
  }

primaryButtonConfirmRideConfig :: Config -> PrimaryButton.Config
primaryButtonConfirmRideConfig config = PrimaryButton.config
  { textConfig
    { text = (getString CONFIRM)
    , color = Color.yellow900
    , textSize = FontSize.a_16
    }
  , background = Color.black900
  , margin = Margin 16 32 16 15
  }

