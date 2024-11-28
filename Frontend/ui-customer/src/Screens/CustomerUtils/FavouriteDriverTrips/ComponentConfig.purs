module Screens.CustomerUtils.FavouriteDriverTrips.ComponentConfig where

import Prelude

import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Language.Types (STR(..))
import Language.Strings (getString)
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Prelude (Unit, const, map, ($), (&&), (/=), (<<<), (<=), (<>), (==), (||), negate, (-))
import Screens.Types as ST
import Font.Size as FontSize
import Font.Style as FontStyle
import Components.GenericHeader as GenericHeader
import Components.SourceToDestination as SourceToDestination
import Styles.Colors as Color
import Common.Types.App
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Prelude ((<>))
import Data.Maybe (Maybe(..))
import ConfigProvider 
import Constants as Const
import Data.Array (filter, elem)
import Data.String as DS
import Common.Animation.Config
import PrestoDOM.Animation as PrestoAnim
import Engineering.Helpers.Commons (convertUTCtoISC)
import Resources.Constants as RC
import Debug(spy)
import Services.API (LocationAddress(..), LocationAPIEntity(..), BookingLocationAPIEntity(..))
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)

genericHeaderConfig :: ST.FavouriteDriverTripsState -> GenericHeader.Config 
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
     , width = WRAP_CONTENT
     , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , margin = (Margin 8 8 8 4)
      , visibility = VISIBLE
      , layoutMargin = (Margin 4 4 4 4)
      , enableRipple = true
      }
    , textConfig {
        text = if DS.length state.data.driverName > 10 then (DS.take 10 state.data.driverName) <> ".." else state.data.driverName
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , padding = (Padding 0 5 0 5)
    }
  in genericHeaderConfig'

sourceToDestinationConfig :: ST.Details -> SourceToDestination.Config
sourceToDestinationConfig item = let 
  (LocationAPIEntity fromLocation) = ( item.fromLocation)
  mbToLocationAddress = case item.toLocation of
    Just (LocationAPIEntity toLocation) -> Just toLocation.address
    Nothing -> Nothing
  sourceText = RC.decodeLocationAddress $ Just fromLocation.address
  destinationText = RC.decodeLocationAddress $ mbToLocationAddress
  sourceToDestinationConfig' = SourceToDestination.config
    { id = Nothing
    , sourceImageConfig {
        imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_green_circle"
      , margin = (MarginTop 3)
      }
    , sourceTextConfig {
        text = sourceText
      , padding = (Padding 2 0 2 2)
      , margin = (MarginHorizontal 12 15)
      , color = Color.black800
      , ellipsize = false
      }
    , destinationImageConfig {
        imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_red_circle"
      , margin = (MarginTop 3)
      }
    , destinationBackground = Color.blue600
    , destinationTextConfig {
        text = destinationText
      , padding = (Padding 2 0 2 2)
      , margin = MarginHorizontal 12 15
      , color = Color.black800
      , ellipsize = false
      }
    , showDestination =  true
    , overrideSeparatorCount = 5
    }
  in sourceToDestinationConfig'
