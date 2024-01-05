module Screens.DueDetailsScreen.View where

import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, background, color, gravity, height, imageView, imageWithFallback, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textView, weight, width)
import Screens.Types as ST
import Styles.Colors as Color
import Effect (Effect)
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.DueDetailsList as DueDetailsList
import Prelude (Unit, const, ($), (<<<), (<>), (/=))
import Screens.DueDetailsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.DueDetailsScreen.ComponentConfig (dueDetailsListState)
import Font.Style as FontStyle
import Helpers.Utils as HU
import Debug (spy)
import Language.Strings (getString)
import Language.Types (STR(..))
import Services.API as API

screen :: ST.DueDetailsScreenState -> Screen Action ST.DueDetailsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "DueDetailsScreen"
  , globalEvents: []
  , eval:
      ( \action state -> do
          let
            _ = spy "DueDetailsScreen -----------state" state
          let
            _ = spy "DueDetailsScreen -----------action" action
          eval action state
      )
  }

view :: forall w. (Action -> Effect Unit) -> ST.DueDetailsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , gravity CENTER
        , onBackPressed push $ const BackPressed
        , background Color.white900
        ]
        [ headerView push state
        , linearLayout
            [ weight 1.0
            , width MATCH_PARENT
            , gravity CENTER_HORIZONTAL
            ]
            [ DueDetailsList.view (push <<< DueDetailsListAction) (dueDetailsListState state) ]
        ]

headerView :: forall w. (Action -> Effect Unit) -> ST.DueDetailsScreenState -> PrestoDOM (Effect Unit) w
headerView push state =
  linearLayout
    [ height $ V 55
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , padding $ PaddingLeft 16
    , background Color.white900
    , stroke $ "1," <> Color.grey900
    ]
    [ imageView
        [ width $ V 24
        , height $ V 24
        , margin $ MarginRight 16
        , onClick push $ const $ BackPressed
        , imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_chevron_left"
        ]
    , textView
        $ [ text duesHeader
          , color Color.darkCharcoal
          , padding $ PaddingBottom 4
          , weight 1.0
          ]
        <> FontStyle.h2 TypoGraphy
    ]
  where
  duesHeader =
    getString case state.props.myPlanProps.multiTypeDues, (state.props.myPlanProps.dueType /= API.AUTOPAY_PAYMENT) of
      true, false -> AUTOPAY_DUE_DETAILS
      true, true -> MANUAL_DUE_DETAILS
      _, _ -> DUE_DETAILS
