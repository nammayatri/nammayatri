module Components.ParcelDeliveryInstruction.View where

import Prelude

import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (<<<), (<>), (==), (>), not, void, discard, (-), show, (*), (<=), (>=), (/))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, background, color, cornerRadius, fontStyle, relativeLayout, gravity, height, alpha, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, weight, width, singleLine, id, frameLayout, scrollBarY, fillViewport, onAnimationEnd, rippleColor, alignParentBottom, progressBar)
import Engineering.Helpers.Commons (safeMarginBottom, safeMarginTop)
import PrestoDOM.Animation as PrestoAnim
import Components.ParcelDeliveryInstruction.Controller (Action(..))
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Language.Types (STR(..))
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import Mobility.Prelude (boolToVisibility)
import Engineering.Helpers.Commons as EHC
import Components.RateCard as RateCard
import RemoteConfig as RC
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App as CT
import Helpers.Utils as HU
import Data.Array as DA
import Data.Ord as DO
import JBridge as JB
import ConfigProvider as CP
import Services.API as API
import Constants as CS
import Data.Int as DI
import Data.Array as DA
import Debug


view :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
view push =
  Anim.screenAnimation $
    relativeLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , background Color.white900
     , onBackPressed push $ const Back
     , margin $ MarginTop safeMarginTop
     ] $
     [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        ]
        [ headerLayout push
        , scrollView
          [ width MATCH_PARENT
          -- , weight 1.0
          , scrollBarY false
          -- , margin $ MarginBottom if EHC.os == "IOS" then 85 else 0
          , padding $ PaddingTop 16
          -- , gravity CENTER
          ][ 
            linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            -- , gravity CENTER
            ]
            [
              linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , background Color.blue600
              , margin $ MarginHorizontal 16 16
              , padding $ Padding 16 16 16 16
              , cornerRadius 16.0
              ]
              [ imageView
                [ width $ V (EHC.screenWidth unit - 64)
                , height $ V (EHC.screenWidth unit - 64)
                , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_delivery_instructions"
                , margin $ MarginBottom 20
                , cornerRadius 8.0
                , layoutGravity "left"
                ]
              , textView
                $ [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text "Quick Deliveries with Yatri Sathi!"
                  , color Color.black800
                  , margin $ MarginBottom 20
                  ]
                <> FontStyle.subHeading3 CT.TypoGraphy
              , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                ( map (\item -> instructionItem item) instructionData)
              , textView
                $ [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , text "View all Guidelines"
                  , color Color.blue800
                  , gravity CENTER_HORIZONTAL
                  , margin $ Margin 16 16 16 0
                  , visibility $ boolToVisibility false
                  ]
                <> FontStyle.body1 CT.TypoGraphy
              ]
          ]
          ]
          ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , alignParentBottom "true,-1"
            , gravity BOTTOM
            , stroke $ "1," <> Color.grey900
            , background Color.white900
            ][ PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig)]
        
    ]

instructionItem :: forall w. { title :: String, image :: String } -> PrestoDOM (Effect Unit) w
instructionItem item =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , margin $ MarginBottom 16
    ]
    [ imageView
      [ width $ V 20
      , height $ V 20
      , imageWithFallback $ fetchImage COMMON_ASSET item.image
      , margin $ MarginRight 8
      ]
    , textView
      $ [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text item.title
        , color Color.black800
        ]
      <> FontStyle.body20 CT.TypoGraphy
    ]

instructionData :: Array { title :: String, image :: String }
instructionData = 
  [ { title: getString ITEMS_SHOULD_FIT_IN_BACKPACK, image: "ny_ic_backpack" }
  , { title: getString AVOID_SENDING_HIGH_VALUE_ITEMS, image: "ny_ic_streamline_fragile_solid" }
  , { title: getString ILLEGAL_ITEMS_PROHIBITED, image: "ny_ic_prohibited" }
  ]

headerLayout :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerLayout push =
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , layoutGravity "center_vertical"
    , padding $ PaddingVertical 10 10
    , stroke $ "1," <> Color.grey900
    ]
    [ imageView
        [ width $ V 30
        , height $ V 30
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
        , gravity CENTER_VERTICAL
        , onClick push $ const Back
        , padding $ Padding 2 2 2 2
        , margin $ MarginLeft 5
        ]
    , textView
        $ [ width WRAP_CONTENT
            , height MATCH_PARENT
            , text "Delivery"
            , margin $ MarginLeft 20
            , color Color.black
            , weight 1.0
            , gravity CENTER_VERTICAL
            , alpha 0.8
            ]
        <> FontStyle.h3 CT.TypoGraphy
    ]

primaryButtonConfig :: PrimaryButton.Config
primaryButtonConfig = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = "Send Now"
      , color = Color.primaryButtonColor
      }
      , background = Color.black900
      , height = V 50
      , cornerRadius = 8.0
      , margin = Margin 16 16 16 16
      , id = "delivery_primary_button"
      }
  in primaryButtonConfig'