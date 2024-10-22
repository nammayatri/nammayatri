module Screens.MultiModalFlow.Components.AlertWidget  
  (view, Action(..), dummyAlertWidgetConfig)where

import Prelude
import PrestoDOM

import Common.Types.App (LazyCheck(..))
import Data.Array (take, (!!))
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String as DS
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.Utils as HU
import Mobility.Prelude (boolToVisibility, layoutWithWeight)
import Screens.EmergencyContactsScreen.ScreenData (neverShareRideOption)
import Screens.NammaSafetyFlow.Components.HelperViews as HV
import Screens.Types as ST
import Services.API as API
import Styles.Colors as Color
import Styles.Types (Color)

data Action = NoAction

view :: forall w. (Action -> Effect Unit) -> ST.AlertWidgetConfig -> PrestoDOM (Effect Unit) w
view push _ =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding $ Padding 16 16 16 16
  , cornerRadius 20.0
  , background Color.black900
  ]
  [ linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    ]
    [ linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , cornerRadius 100.0
      , background Color.blue600
      , gravity CENTER
      ]
      [ imageView 
        [ height $ V 20
        , width $ V 28
        , margin $ Margin 4 8 4 8
        , imageWithFallback $ HU.fetchImage HU.GLOBAL_COMMON_ASSET "ny_ic_blue_alarm_clock"
        ]
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      , margin $ MarginLeft 12
      ]
      [ textView $
        [ text $ "For a quick pickup on your next ride, book an " <> "auto" -- TODO: As per response
        , color Color.white900
        ] <> FontStyle.body1 TypoGraphy
      , textView $ 
        [ text "Book Now"
        , color Color.blue800
        ] <> FontStyle.body1 TypoGraphy
      ]
    ]
  ]

dummyAlertWidgetConfig :: ST.AlertWidgetConfig
dummyAlertWidgetConfig = {}