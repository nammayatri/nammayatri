{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerUtils.TripDetailsScreen.ComponentConfig where

import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Language.Types (STR(..))
import Language.Strings (getString)
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Prelude (Unit, const, map, ($), (&&), (/=), (<<<), (<=), (<>), (==), (||), negate, (-), unit)
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
import Common.Animation.Config
import PrestoDOM.Animation as PrestoAnim
import Engineering.Helpers.Commons (convertUTCtoISC, screenWidth)
import Data.Maybe

genericHeaderConfig :: ST.TripDetailsScreenState -> GenericHeader.Config 
genericHeaderConfig state= let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
     , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , margin = (Margin 8 8 8 8)
      , visibility = if state.props.issueReported then GONE else VISIBLE
      , layoutMargin = (Margin 4 4 4 4)
      , enableRipple = true
      }
    , textConfig {
        text = if state.props.issueReported then "" else (getString RIDE_DETAILS)
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , padding = (Padding 0 5 0 5)
    }
  in genericHeaderConfig'
  
confirmLostAndFoundConfig :: ST.TripDetailsScreenState ->  PopUpModal.Config 
confirmLostAndFoundConfig state = let 
    config' = PopUpModal.config 
    popUpConfig' = config' {
      primaryText { text = (getString LOST_SOMETHING)},
      secondaryText {
        text = (getString TRY_CONNECTING_WITH_THE_DRIVER)
      , margin = (Margin 0 4 0 20)}
      , option1 { 
          background = state.data.config.popupBackground
        , strokeColor = state.data.config.primaryBackground
        , color = state.data.config.primaryBackground
        , text = (getString CANCEL_)
        }
      , option2 { 
          color = state.data.config.primaryTextColor
        , strokeColor = state.data.config.primaryBackground
        , background = state.data.config.primaryBackground
        , text = (getString REQUEST_CALLBACK)
        , margin = MarginLeft 12
        }
    }
    in popUpConfig'


sourceToDestinationConfig :: ST.TripDetailsScreenState -> SourceToDestination.Config
sourceToDestinationConfig state = let 
  startDate = state.data.selectedItem.rideStartTime <> " . " <> (convertUTCtoISC state.data.selectedItem.rideStartTimeUTC "DD/MM/YYYY") 
  endDate = state.data.selectedItem.rideEndTime <> " . " <> (convertUTCtoISC state.data.selectedItem.rideEndTimeUTC "DD/MM/YYYY")
  sourceText = if state.data.selectedItem.status /= "CANCELLED"
                  then startDate <> "\n" <> state.data.source
                  else state.data.source
  destinationText = if state.data.selectedItem.status /= "CANCELLED"
                  then endDate <> "\n" <> state.data.destination
                  else state.data.destination
  fareProductType = state.data.selectedItem.rideType
  sourceToDestinationConfig' = SourceToDestination.config
    { id = Just $ "TripDetailsSTDC_" <> state.data.tripId
    , width = V $ screenWidth unit - 70 
    , sourceImageConfig {
        imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_green_circle"
      , margin = (MarginTop 3)
      }
    , sourceTextConfig {
        text = sourceText
      , padding = (Padding 2 0 2 2)
      , margin = (MarginHorizontal 12 15)
      , color = Color.greyDavy
      , maxLines = 3
      , ellipsize = true
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
      , color = Color.greyDavy
      , ellipsize = false
      }
    , showDestination = destinationText /= "" && fareProductType /= ST.RENTAL
    }
  in sourceToDestinationConfig'

topicsList :: ST.TripDetailsScreenState -> Array CategoryListType
topicsList state =
  let appConfig = getAppConfig Const.appConfig
  in
  if appConfig.feature.enableSelfServe then
    filter (_.isRideRequired) state.data.categories
  else
      [{ categoryAction : Just "CONTACT_US"
      , categoryName : getString FOR_OTHER_ISSUES_WRITE_TO_US
      , categoryImageUrl : Just $ fetchImage FF_COMMON_ASSET "ny_ic_clip_board"
      , categoryId : "5"
      , isRideRequired : false
      , maxAllowedRideAge : Nothing
      , allowedRideStatuses : Nothing
      , categoryType: "Category"
      },
      { categoryAction : Just "CALL_SUPPORT"
      , categoryName : getString CONTACT_SUPPORT
      , categoryImageUrl : Just $ fetchImage FF_COMMON_ASSET "ny_ic_help"
      , categoryId : "6"
      , isRideRequired : false
      , maxAllowedRideAge : Nothing
      , allowedRideStatuses : Nothing
      , categoryType: "Category"
      }]

listExpandingAnimationConfig :: Boolean -> AnimConfig
listExpandingAnimationConfig isExpanded = let
  config = getConfig isExpanded
  animConfig' = animConfig
          { fromScaleY = config.fromScaleY
          , toScaleY = config.toScaleY
          , fromY = config.fromY
          , toY = config.toY
          , repeatCount = PrestoAnim.Repeat 0
          , ifAnim = isExpanded
          , duration = 150
          }
  in animConfig'

getConfig :: Boolean -> {fromScaleY :: Number , toScaleY :: Number, fromY :: Int, toY :: Int}
getConfig  isExpanded =
  if isExpanded then
    { fromScaleY : 0.0
    , toScaleY : 1.0
    , fromY : -100
    , toY : 0
    }
  else
    { fromScaleY : 1.0
    , toScaleY : 0.0
    , fromY : 0
    , toY : -100
    }