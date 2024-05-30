module Screens.RideSummaryScreen.ComponentConfig where

import Prelude

import Components.SourceToDestination as SourceToDestination
import Screens.RideSummaryScreen.ScreenData
import Data.Maybe
import PrestoDOM
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle



sourceToDestinationConfig :: RideSummaryScreenState -> SourceToDestination.Config
sourceToDestinationConfig state =
  let
    config = SourceToDestination.config

    sourceToDestinationConfig' =
        config
         { 
            id = Just "rideScreenPickupMap",
            separatorMargin = 0,
            sourceImageConfig
          { imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_source_dot"
          , margin = MarginTop 3
          , width = V 18
          , height = V 18
          }
        , sourceTextConfig
          { text = "getTripTitle state.data.source"
          , padding = Padding 2 0 2 2
          , margin = MarginHorizontal 12 15
          , color = Color.black800
          , ellipsize = true
          , maxLines = 1
          , textStyle = FontStyle.Body1
          }
        , rideStartedAtConfig
          { text = "getTripSubTitle state.data.sourcegetTripSubTitle state.data.sourcegetTripSubTitle state.data.sourcegetTripSubTitle state.data.source"
          , color = Color.black700
          , visibility = VISIBLE
          , padding = Padding 2 0 2 2
          , margin = MarginHorizontal 12 15
          , maxLines = 2
          , ellipsize = true
          }
        , rideEndedAtConfig
          { text = "getTripSubTitle state.data.destinationgetTripSubTitle state.data.destinationgetTripSubTitle state.data.destinationgetTripSubTitle state.data.destination"
          , color = Color.black700
          , visibility = VISIBLE
          , padding = Padding 2 0 2 2
          , margin = MarginHorizontal 12 15
          , maxLines = 2
          , ellipsize = true
          }
        , destinationImageConfig
          { imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_destination"
          , margin = MarginTop 0
          , width = V 20
          , height = V 23
          }
        , destinationTextConfig
          { text ="getTripTitle state.data.destination"
          , padding = Padding 2 0 2 2
          , margin = MarginHorizontal 12 15
          , color = Color.black800
          , ellipsize = true
          , maxLines = 1
          , textStyle = FontStyle.Body1
          }
        , horizontalSeperatorConfig
          { visibility = GONE
          , background = Color.grey900
          , padding = Padding 2 0 2 2
          , margin = Margin 12 12 15 0
          }
        , pillsConfig
          { visibility = VISIBLE,
            pillList = [{text:"280km",imageUrl:fetchImage FF_COMMON_ASSET "ny_ic_source_dot", imageVisibility: VISIBLE}]
          }
        }
  in
    sourceToDestinationConfig'
