module Screens.DocumentDetailsScreen.View where

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Debug (spy)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Engineering.Helpers.Utils as EHU
import Language.Types (STR(..))
import Prelude (Unit, const, map, not, ($), (<<<), (<>), (==), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, LoggableScreen, Visibility(..), afterRender, alpha, background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, frameLayout, visibility, clickable, singleLine)
import Screens.DocumentDetailsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Data.Array as DA
import Mobility.Prelude as MP

screen :: ST.DocumentDetailsScreenState -> LoggableScreen Action ST.DocumentDetailsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "BookingDetailsScreen"
  , globalEvents: []
  , eval:
      ( \state action -> do
          let
            _ = spy "DocumentDetailsScreenState -----" state
          let
            _ = spy "DocumentDetailsScreenState--------action" action
          eval state action
      )
  , parent : Nothing
  , logWhitelist : initialState.data.config.logWhitelistConfig.documentDetailsScreenLogWhitelist
  }

logWhitelist :: Array String
logWhitelist = []

view :: forall w. (Action -> Effect Unit) -> ST.DocumentDetailsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , onBackPressed push $ const BackPressed
  , background Color.white900
  , padding $ PaddingBottom 24
  ] $ [ headerLayout push state
      ]


headerLayout :: forall w. (Action -> Effect Unit) -> ST.DocumentDetailsScreenState -> PrestoDOM (Effect Unit) w
headerLayout push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , layoutGravity "center_vertical"
        , padding $ PaddingVertical 10 10
        ]
        [ imageView
            [ width $ V 30
            , height $ V 30
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
            , gravity CENTER_VERTICAL
            , onClick push $ const BackPressed
            , padding $ Padding 2 2 2 2
            , margin $ MarginLeft 5
            ]
        , textView $
            [ width WRAP_CONTENT
            , height MATCH_PARENT
            , text $ getString BOOKING_OPTIONS
            , margin $ MarginLeft 20
            , color Color.black
            , weight 1.0
            , gravity CENTER_VERTICAL
            , alpha 0.8
            ] <> FontStyle.h3 TypoGraphy
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.greyLight
        ]
        []
    ]

customTV :: forall w. String -> Int -> (LazyCheck -> forall properties. (Array (Prop properties))) -> String -> PrestoDOM (Effect Unit) w
customTV text' textSize' fontStyle' color' =
  textView
    $ [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text text'
      , textSize textSize'
      , color color'
      ]
    <> fontStyle' TypoGraphy