module Screens.MetroWarriorsScreen.ComponentConfig where

import Prelude
import PrestoDOM
import Common.Styles.Colors as Color
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.Types as ST
import Helpers.Utils as HU
import Components.GenericHeader as GenericHeader

genericHeaderConfig :: ST.MetroWarriorsScreenState -> GenericHeader.Config
genericHeaderConfig state = let
  config = GenericHeader.config
  text' = getString if state.props.showStationList then CHOOSE_PREFERRED_METRO else METRO_WARRIORS
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , background = Color.transparent
    , prefixImageConfig {
        height = (V 25)
      , width = (V 25)
      , margin = (Margin 8 8 8 8)
      , layoutMargin = (Margin 2 9 8 7)
      , accessibilityHint = "Back : Button"
      , visibility = VISIBLE
      , accessibility = ENABLE
      , imageUrl = HU.fetchImage HU.COMMON_ASSET "ny_ic_chevron_left"
      , enableRipple = true
      }
    , textConfig {
        text = text'
      , color = Color.black900
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'