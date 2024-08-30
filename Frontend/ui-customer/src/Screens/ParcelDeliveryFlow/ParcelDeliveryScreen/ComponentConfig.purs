module Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.ComponentConfig where

import Prelude
import Components.GenericHeader.Controller as GenericHeader
import Components.PrimaryButton.Controller as PrimaryButton
import Components.SeparatorView.View as SeparatorView
import Components.SourceToDestination.Controller as SourceToDestination
import Components.PopUpModal as PopUpModal
import Components.SelectListModal as CancelRidePopUpConfig
import Data.Maybe (Maybe(..), maybe)
import Font.Style (Style(..))
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..))
import Screens.Types as ST
import Styles.Colors as Color
import Data.Array as DA
import Data.String as DS
import PrestoDOM.Types.DomAttributes (Corners(..))

primaryButtonConfig :: ST.ParcelDeliveryScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
          { text = "Book For" 
          , color = Color.yellow900
          , height = V 40
          }
      , gravity = CENTER
      , margin = (MarginHorizontal 16 16)
      , isClickable = true -- state.data.bookingId /= ""
      -- , alpha = if state.data.bookingId == "" then 0.5 else 1.0
      , id = "GoHomeButton"
      , enableRipple = true
      , rippleColor = Color.rippleShade
      }
  in
    primaryButtonConfig'

genericHeaderConfig :: ST.ParcelDeliveryScreenState -> GenericHeader.Config
genericHeaderConfig _state = let
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , width = WRAP_CONTENT
    , background = Color.white900
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , margin = (Margin 12 12 12 12)
      }
    , textConfig {
        text = "Delivery Details"
      , color = Color.black800
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , padding = (Padding 0 5 0 0)
    }
  in genericHeaderConfig'