{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RentalBookingFlow.RentalScreen.ComponentConfig where

import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.IncrementDecrementModel.Controller as IncrementDecrement
import Components.InputView as InputView
import Components.PrimaryButton as PrimaryButton
import Components.SeparatorView.View as SeparatorView
import Data.Array ((!!))
import Data.Maybe as MB
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Prelude (map, show, (<>), (==), ($), (>), (-), (<))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..))
import Screens.Types (DateTimeConfig, RentalScreenStage(..), RentalScreenState)
import Styles.Colors as Color

primaryButtonConfig :: RentalScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    primaryButtonConfig' = PrimaryButton.config
      { textConfig
        { text = case state.data.currentStage of
            RENTAL_SELECT_PACKAGE -> getString SELECT_VEHICLE
            RENTAL_SELECT_VARIANT -> getString BOOK_RENTAL
            RENTAL_CONFIRMATION -> getString CONFIRM_RENTAL
        , color = Color.yellow900
        , height = V 40
        }
      , gravity = CENTER
      , margin = MarginHorizontal 0 0
      , isClickable = true
      , id = "RentalConfirmBooking"
      }
  in primaryButtonConfig'

genericHeaderConfig :: RentalScreenState -> GenericHeader.Config
genericHeaderConfig _ = let
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
      , margin = Margin 12 12 12 12
      }
    , textConfig {
        text = getString RENTAL_RIDE
      , color = Color.black800
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , padding = PaddingTop 5
    }
  in genericHeaderConfig'

incrementDecrementConfig :: RentalScreenState -> IncrementDecrement.Config
incrementDecrementConfig state = let
  config = IncrementDecrement.config
  incrementDecrementConfig' = config
    { initialValue = state.data.rentalBookingData.baseDistance
    , minValue = state.props.minDistance
    , maxValue = state.props.maxDistance
    , buttonTextConfig {
        color = Color.black800
      , background = Color.blue600
      , padding = Padding 18 2 18 8
      , fontStyle = FontStyle.h1 TypoGraphy
      , gravity = CENTER_VERTICAL
      }
    , countTextConfig {
        color = Color.black800
      , fontStyle = FontStyle.h1 TypoGraphy
      , background = Color.blue600
      , gravity = CENTER_HORIZONTAL
      }
    , background = Color.blue600
    , padding = Padding 32 10 32 10
    , gravity = CENTER_HORIZONTAL
    , width = MATCH_PARENT
    , orientation = HORIZONTAL
    , cornerRadius = 8.0
    }
  in incrementDecrementConfig'

mapInputViewConfig :: RentalScreenState -> InputView.InputViewConfig
mapInputViewConfig state = 
  let config = InputView.config 
      isSelectPackageStage = state.data.currentStage == RENTAL_SELECT_PACKAGE
      inputViewConfig' = config
        { headerText = getHeaderText state.data.currentStage
        , suffixButton {
            text = getDateString state.data.selectedDateTimeConfig true
          , fontStyle = FontStyle.subHeading2 LanguageStyle
          , prefixImage = "ny_ic_clock_unfilled"
          , suffixImage = "ny_ic_chevron_down"
          , padding = Padding 8 0 8 1
          , gravity = CENTER_VERTICAL
          }
        , suffixButtonVisibility = boolToVisibility isSelectPackageStage
        , imageLayoutVisibility = boolToVisibility isSelectPackageStage
        , inputView = map 
            ( \item -> inputViewArray isSelectPackageStage item ) $ inputTextConfigArray isSelectPackageStage
        }
  in inputViewConfig'
  where 
    inputViewArray :: Boolean -> InputView.InputTextConfig -> InputView.InputView
    inputViewArray isSelectPackageStage item =
      { padding : Padding 8 7 8 7 
      , height : WRAP_CONTENT
      , canClearText : false
      , isEditable : false
      , isClickable : false
      , prefixImage : { 
          imageName : item.imageName,
          height : V 15,
          width : V 15 ,
          padding : Padding 0 0 0 0 }
      , clearTextIcon : { 
          imageName : "ny_ic_close_circle",
          height : V 15,
          width : V 15 ,
          padding : Padding 0 0 0 0 }
      , stroke : ((if item.isFocussed then "1," else "0,") <> Color.yellow900)
      , imageSeparator : separatorConfig
      , fontStyle : if isSelectPackageStage then FontStyle.body6 LanguageStyle else FontStyle.subHeading2 LanguageStyle
      , gravity : if isSelectPackageStage then LEFT else CENTER_HORIZONTAL
      , inputTextConfig : item
      }

    inputTextConfigArray :: Boolean -> Array (InputView.InputTextConfig)
    inputTextConfigArray isSelectPackageStage = 
      [ { textValue : if isSelectPackageStage then getString CURRENT_LOCATION else getDateString state.data.selectedDateTimeConfig false
        , isFocussed : false
        , imageName : "ny_ic_green_circle"
        , margin : MarginTop 0
        , placeHolder : ""
        , id : if isSelectPackageStage then "CurrentLocation" else "DateAndTime"
        , cornerRadius : 4.0
        , textColor : if isSelectPackageStage then Color.black600 else Color.white900
        } ,
        { textValue : if isSelectPackageStage then getString FIRST_STOP_OPTIONAL else show state.data.rentalBookingData.baseDuration <> " hr · " <> show state.data.rentalBookingData.baseDistance <> " km"
        , isFocussed : false
        , imageName : "ny_ic_blue_circle"
        , margin : MarginVertical 12 8
        , placeHolder : ""
        , id : if isSelectPackageStage then "FirstStop" else "RentalPackage"
        , cornerRadius : 4.0
        , textColor : if isSelectPackageStage then Color.black700 else Color.white900
        }
      ]

    getDateString :: DateTimeConfig -> Boolean -> String
    getDateString dateTimeConfig isShort = 
      let
        day = dateTimeConfig.day
        month = dateTimeConfig.month
        year = dateTimeConfig.year
        minute = if dateTimeConfig.minute < 10 then "0" <> show dateTimeConfig.minute else show dateTimeConfig.minute
        hour = dateTimeConfig.hour
        hourAndMinute = if(hour > 12) then show (hour - 12) <> ":" <> minute <> " PM" else show hour <> ":" <> minute <> " AM"
      in
        if year == 0 then "Now" 
        else if isShort then hourAndMinute <> ", " <> getShortMonthFromInt month <> " " <> show day
        else show day <> " " <> getFullMonthFromInt month <> " " <> show year <> ", " <> show hour <> ":" <> minute

    getShortMonthFromInt :: Int -> String
    getShortMonthFromInt month = MB.fromMaybe "" $ ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"] !! (month)

    getFullMonthFromInt :: Int -> String
    getFullMonthFromInt month = MB.fromMaybe "" $ map getString [JANUARY, FEBRUARY, MARCH, APRIL, MAY, JUNE, JULY, AUGUST, SEPTEMBER, OCTOBER, NOVEMBER, DECEMBER] !! month

    getHeaderText :: RentalScreenStage -> String
    getHeaderText stage = case stage of
      RENTAL_SELECT_PACKAGE -> getString TRIP_DETAILS_
      _ -> getString CHOOSE_YOUR_RIDE

separatorConfig :: SeparatorView.Config
separatorConfig = 
  { orientation : VERTICAL
  , count : 3
  , height : V 8
  , width : V 1
  , layoutWidth : V 12
  , layoutHeight : V 15
  , color : Color.grey900
  }