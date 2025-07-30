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
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alpha, background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, frameLayout, visibility, clickable, singleLine, scrollBarY, scrollView)
import Screens.DocumentDetailsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Data.Array as DA
import Mobility.Prelude as MP
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Screens.DocumentDetailsScreen.ComponentConfig
import PrestoDOM.Animation as PrestoAnim

screen :: ST.DocumentDetailsScreenState -> Screen Action ST.DocumentDetailsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "DocumentDetailsScreen"
  , globalEvents: []
  , eval:
      ( \state action -> do
          let
            _ = spy "DocumentDetailsScreenState -----" state
          let
            _ = spy "DocumentDetailsScreenState--------action" action
          eval state action
      )
  }

view :: forall w. (Action -> Effect Unit) -> ST.DocumentDetailsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  frameLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ] $ [
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , onBackPressed push (const BackPressed)
    ][ PrestoAnim.animationSet  [ Anim.fadeIn true ] 
      $ headerView push state 
      , linearLayout
        [ width MATCH_PARENT
        , weight 1.0
        , orientation VERTICAL
        ][ scrollView
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , scrollBarY false
            ][ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , padding (PaddingHorizontal 20 20)
                ][
                ]
              ]
          ]
      ]
  ]    -- <> if state.props.menuOptions then [menuOptionModal push state] else []

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

headerView :: forall w. (Action -> Effect Unit) -> ST.DocumentDetailsScreenState -> PrestoDOM (Effect Unit) w
headerView push state= AppOnboardingNavBar.view (push <<< AppOnboardingNavBarAC) (appOnboardingNavBarConfig state)