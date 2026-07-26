{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RentalBookingFlow.RideScheduledScreen.ComponentConfig where

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
import Screens.Types (RideScheduledScreenState)
import Styles.Colors as Color
import Data.Array as DA
import Data.String as DS
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (FareProductType(..)) as FPT

primaryButtonConfig :: RideScheduledScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
          { text = if state.data.primaryButtonText == "" then getString GO_HOME else state.data.primaryButtonText
          , color = Color.yellow900
          , height = V 40
          }
      , gravity = CENTER
      , margin = (MarginHorizontal 16 16)
      , isClickable =  state.data.bookingId /= ""
      , alpha = if state.data.bookingId == "" then 0.5 else 1.0
      , id = "GoHomeButton"
      , enableRipple = true
      , rippleColor = Color.rippleShade
      }
  in
    primaryButtonConfig'

separatorConfig :: SeparatorView.Config
separatorConfig =
  { orientation: VERTICAL
  , count: 3
  , height: V 4
  , width: V 2
  , layoutWidth: V 15
  , layoutHeight: V 15
  , color : Color.grey900
  }

sourceToDestinationConfig :: RideScheduledScreenState -> SourceToDestination.Config
sourceToDestinationConfig state =
  let
    isRentalRide = state.data.fareProductType == FPT.RENTAL
    config = SourceToDestination.config
    sourceToDestinationConfig' = config
      { sourceTextConfig
          { text = state.data.source.address
          , textStyle = ParagraphText
          , ellipsize = true
          , maxLines = 2
          , margin = MarginLeft 12
          , color = Color.black800
          }
      , sourceImageConfig
          { imageUrl = fetchImage FF_ASSET "ny_ic_pickup"
          , margin = MarginTop 7
          , height = V 16
          , width = V 16
          }
      , destinationImageConfig
          { imageUrl = fetchImage FF_ASSET $ maybe ("ny_ic_plus_circle") (\_ -> "ny_ic_drop") state.data.destination
          , margin = MarginTop 5
          , height = V 16
          , width = V 16
          }
      , destinationTextConfig
          { text = maybe (getString ADD_FIRST_STOP) (\dest -> dest.address) state.data.destination
          , color = maybe (Color.blue800) (\_ -> Color.black800) state.data.destination
          , isEditable = isRentalRide
          , textStyle = ParagraphText
          , ellipsize = true
          , maxLines = if isRentalRide then 1 else 2
          , margin = MarginLeft 12
          , isClickable = isRentalRide
          }
      , distanceConfig { distanceVisibility = GONE }
      , separatorMargin = 24
      }
  in
    sourceToDestinationConfig'

genericHeaderConfig :: RideScheduledScreenState -> GenericHeader.Config
genericHeaderConfig state = let
  config = GenericHeader.config
  header = case state.data.fareProductType of 
            FPT.RENTAL-> getString RENTAL_BOOKING
            FPT.INTER_CITY-> getString INTERCITY_BOOKING
            _ -> getString SCHEDULED <> " " <> getString BOOKING
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
        text = header
      , color = Color.black800
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , padding = (Padding 0 5 0 0)
    }
  in genericHeaderConfig'


cancelRidePopUpConfig :: RideScheduledScreenState -> CancelRidePopUpConfig.Config
cancelRidePopUpConfig state =
  let
    cancelRideconfig = CancelRidePopUpConfig.config
    lastIndex = (DA.length state.data.cancellationReasons) - 1
    cancelRideConfig = state.data.config.cancelReasonConfig
  in
    CancelRidePopUpConfig.config
        { selectionOptions = state.data.cancellationReasons
        , showAllOptionsText = (getString SHOW_ALL_OPTIONS)
        , primaryButtonTextConfig
          { firstText = getString WAIT_FOR_DRIVER
          , secondText = getString CANCEL_RIDE
          }
        , activeIndex = state.props.cancelRideActiveIndex
        , activeReasonCode = Just state.props.cancelReasonCode
        , isLimitExceeded = DS.length state.props.cancelDescription >= 100
        , cornerRadius = cancelRideConfig.buttonCornerRadius
        , isSelectButtonActive =
          ( case state.props.cancelRideActiveIndex of
              Just cancelRideIndex -> true
              Nothing -> false
          )
        , headingTextConfig{
          text = getString CANCEL_RIDE <> "?"
        }
        , subHeadingTextConfig{
          text = getString PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL
        }
        , hint = getString HELP_US_WITH_YOUR_REASON
        , strings
          { mandatory = getString MANDATORY
          , limitReached = getString MAX_CHAR_LIMIT_REACHED <> " 100 " <> getString OF <> " 100"
          }
        , config = state.data.config
        }
  
cancelScheduledRideConfig :: RideScheduledScreenState -> PopUpModal.Config
cancelScheduledRideConfig state = PopUpModal.config
        { optionButtonOrientation = "VERTICAL"
        , buttonLayoutMargin = Margin 24 0 24 20
        , gravity = CENTER
        , margin = MarginHorizontal 20 20
        , primaryText
          { text = getString CANCEL_SCHEDULED_RIDE
          , margin = Margin 16 16 16 10
          , textStyle = Heading2
          }
        , secondaryText
          { text = getString CANCEL_SCHEDULED_RIDE_DESC
          , margin = MarginHorizontal 16 16
          }
        , option1
          { text = getString CONFIRM_CANCELLATION
          , color = Color.yellow900
          , background = Color.black900
          , width = MATCH_PARENT
          , margin = MarginVertical 20 10
          , enableRipple = true
          }
        , option2
          { text = getString DISMISS
          , color = Color.black900
          , background = Color.white900
          , strokeColor = Color.transparent
          , width = MATCH_PARENT
          , enableRipple = true
          , margin = MarginBottom 0
          }
        , cornerRadius = Corners 15.0 true true true true
        , coverImageConfig
          { visibility = GONE
          }
        , backgroundClickable = false
        }

  