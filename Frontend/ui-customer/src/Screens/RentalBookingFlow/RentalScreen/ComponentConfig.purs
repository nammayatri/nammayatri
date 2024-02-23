{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RentalBookingFlow.RentalScreen.ComponentConfig where

import Common.Types.App (LazyCheck(..), RateCardType(..))
import Components.GenericHeader as GenericHeader
import Components.IncrementDecrementModel.Controller as IncrementDecrement
import Components.InputView as InputView
import Components.PrimaryButton as PrimaryButton
import Components.PopUpModal as PopUpModal
import Components.RateCard as RateCard
import Components.SeparatorView.View as SeparatorView
import Components.RequestInfoCard as RequestInfoCard
import Data.Array ((!!), singleton, null)
import Data.Maybe as MB
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Prelude (map, show, not, (<>), (==), ($), (>), (-), (<), (+), (||), (&&))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..))
import Screens.Types (DateTimeConfig, RentalScreenStage(..), RentalScreenState, RentalQuoteList)
import Styles.Colors as Color
import Data.String
import JBridge 
import Data.Int (fromString)
import Debug (spy)
import PrestoDOM.Types.DomAttributes as PTD
import Resources.Localizable.EN (getEN)
import ConfigProvider

primaryButtonConfig :: RentalScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    primaryButtonConfig' = PrimaryButton.config
      { textConfig
        { text = case state.data.currentStage of
            RENTAL_SELECT_PACKAGE -> getString PROCEED
            RENTAL_SELECT_VARIANT -> getString BOOK_RENTAL
            RENTAL_CONFIRMATION -> getString CONFIRM_RENTAL
            _ -> ""
        , color = Color.yellow900
        , height = V 40
        }
      , gravity = CENTER
      , margin = MarginHorizontal 0 0
      , id = "RentalConfirmBooking"
      , alpha = if state.props.showPrimaryButton then 1.0 else 0.4
      , isClickable = state.props.showPrimaryButton
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
      suffixButtonText = if state.data.startTimeUTC == ""
                          then getString NOW
                          else formatDate "hh" <> ":" <> formatDate "mm" <> " " <> formatDate "A" <> ", " <> formatDate "MMM" <> " " <> formatDate "D"
      inputViewConfig' = config
        { headerText = getHeaderText state.data.currentStage
        , suffixButton {
            text = suffixButtonText
          , fontStyle = FontStyle.subHeading2 LanguageStyle
          , prefixImage = "ny_ic_clock_unfilled"
          , suffixImage = "ny_ic_chevron_down"
          , padding = Padding 8 0 8 1
          , gravity = CENTER_VERTICAL
          }
        , suffixButtonVisibility = boolToVisibility isSelectPackageStage
        , imageLayoutVisibility = boolToVisibility isSelectPackageStage
        , inputLayoutPading = if isSelectPackageStage then PaddingLeft 8 else PaddingLeft 0
        , inputView = map 
            ( \item -> inputViewArray isSelectPackageStage item ) $ inputTextConfigArray isSelectPackageStage
        }
  in inputViewConfig'
  where 
    formatDate :: String -> String
    formatDate formatSTR =
      let startTime = if state.data.startTimeUTC == "" then EHC.getCurrentUTC "" else state.data.startTimeUTC
      in EHC.convertUTCtoISC startTime formatSTR
  
    inputViewArray :: Boolean -> InputView.InputTextConfig -> InputView.InputView
    inputViewArray isSelectPackageStage item =
      { padding : Padding 8 7 8 7 
      , height : WRAP_CONTENT
      , canClearText : false
      , isEditable : false
      , isClickable : true
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
      let 
        pickUpText = if state.data.pickUpLoc.address == "" then getString CURRENT_LOCATION else state.data.pickUpLoc.address
        dropLocText = MB.maybe (getString FIRST_STOP_OPTIONAL)  (\loc -> loc.address) state.data.dropLoc
      in
      [ { textValue : if isSelectPackageStage then pickUpText else formatDate "DD" <> " " <> formatDate "MMMM" <> " " <> formatDate "YYYY" <> ", " <> formatDate "HH" <> ":" <> formatDate "mm"
        , isFocussed : false
        , imageName : "ny_ic_green_circle"
        , margin : MarginTop 0
        , placeHolder : ""
        , id : if isSelectPackageStage then "PickUpLoc" else "DateAndTime"
        , cornerRadius : 4.0
        , prefixImageVisibility : boolToVisibility $ not isSelectPackageStage
        , prefixImageConfig : {
            height : V 24,
            width : V 24,
            padding : Padding 0 0 0 0,
            imageName : "ny_ic_clock_unfilled_white"
          }
        , textColor : if isSelectPackageStage then Color.black600 else Color.white900
        } ,
        { textValue : if isSelectPackageStage then dropLocText else " " <> show state.data.rentalBookingData.baseDuration <> " hr · " <> show state.data.rentalBookingData.baseDistance <> " km"
        , isFocussed : false
        , imageName : "ny_ic_blue_circle"
        , margin : MarginVertical 12 8
        , placeHolder : ""
        , id : if isSelectPackageStage then "FirstStop" else "RentalPackage"
        , cornerRadius : 4.0
        , prefixImageVisibility : GONE 
        , prefixImageConfig : {
            height : V 0,
            width : V 0,
            padding : Padding 0 0 0 0,
            imageName : ""
          }
        , textColor : if isSelectPackageStage then Color.black700 else Color.white900
        }
      ]

    getDateString :: DateTimeConfig -> Boolean -> String -- TODO-codex : remove DateTimeConfig and use UTC
    getDateString dateTimeConfig isShort = 
      let
        currentUTC = EHC.getCurrentUTC ""
        minutee = if dateTimeConfig.minute == 0 then getFromUTC currentUTC "MINUTE" else show dateTimeConfig.minute
        day = if dateTimeConfig.day == 0 then getFromUTC currentUTC "DAY" else show dateTimeConfig.day
        month = if dateTimeConfig.month == 0 then getFromUTC currentUTC "MONTH" else show (dateTimeConfig.month + 1)
        year = if dateTimeConfig.year == 0 then getFromUTC currentUTC "YEAR" else show dateTimeConfig.year
        minute = if MB.fromMaybe 0 (fromString minutee) < 10 then "0" <> minutee else  minutee
        hour = if dateTimeConfig.hour == 0 then getFromUTC currentUTC "HOUR" else show dateTimeConfig.hour
        hourInNum = MB.fromMaybe 0 (fromString hour)
        hourAndMinute = if (hourInNum > 12) then show (hourInNum - 12) <> ":" <> minute <> " PM" else hour <> ":" <> minute <> " AM"
      in
        if year == "" then getString NOW 
        else if isShort then hourAndMinute <> ", " <> getShortMonthFromInt month <> " " <> day
        else day <> " " <> getFullMonthFromInt month <> " " <>  year <> ", " <> hour <> ":" <> minute

    getShortMonthFromInt :: String -> String
    getShortMonthFromInt month = MB.fromMaybe "" $ ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"] !! ((MB.fromMaybe 1 (fromString month)) - 1)

    getFullMonthFromInt :: String -> String
    getFullMonthFromInt month = MB.fromMaybe "" $ map getString [JANUARY, FEBRUARY, MARCH, APRIL, MAY, JUNE, JULY, AUGUST, SEPTEMBER, OCTOBER, NOVEMBER, DECEMBER] !! ((MB.fromMaybe 0 (fromString month)) - 1)

    getHeaderText :: RentalScreenStage -> String
    getHeaderText stage = case stage of
      RENTAL_SELECT_PACKAGE -> getString TRIP_DETAILS_
      _ -> getString CHOOSE_YOUR_RENTAL_RIDE


rentalRateCardConfig :: RentalQuoteList -> RateCard.Config
rentalRateCardConfig state =
  let config = RateCard.config
      currency = getCurrency appConfig
      rentalRateCardConfig' = config
        { currentRateCardType = RentalRateCard
        , title = getString RENTAL_PACKAGE
        , primaryButtonConfig {
            margin = MarginTop 16,
            text = getString GOT_IT,
            color = Color.blue800,
            height = V 40,
            cornerRadius = 8.0,
            background = Color.white900,
            visibility = VISIBLE
          }
        , additionalStrings = [
            {key : "FINAL_FARE_DESCRIPTION", val : (getString FINAL_FARE_DESCRIPTION)}
          , {key : "EXCESS_DISTANCE_CHARGE_DESCRIPTION", val : (getString EXCESS_DISTANCE_CHARGE_DESCRIPTION) <> " " <> currency <> (show state.fareDetails.perExtraKmRate) <> "/km."}
          , {key : "NIGHT_TIME_FEE_DESCRIPTION", val : (getVarString NIGHT_TIME_FEE_DESCRIPTION $ singleton $ currency <> (show state.fareDetails.nightShiftCharge))}
          , {key : "PARKING_FEES_AND_TOLLS_NOT_INCLUDED", val : (getString PARKING_FEES_AND_TOLLS_NOT_INCLUDED)}
          ]
        }
  in rentalRateCardConfig'

locUnserviceablePopUpConfig :: RentalScreenState -> PopUpModal.Config
locUnserviceablePopUpConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    gravity = CENTER,
    margin = (MarginHorizontal 16 16),
    buttonLayoutMargin = (Margin 0 16 16 0),
    editTextVisibility = GONE,
    dismissPopupConfig {
      visibility = GONE
      },
    primaryText {
      text = (getString LOCATION_UNSERVICEABLE), 
      gravity = CENTER,
      margin = MarginTop 16
      },
    secondaryText { 
      text = "Locations within special zone are not eligible for rental rides", -- TODO-codex : Add Translation
      margin = MarginTop 4
      },
    option1 {
      visibility = false
      },
    option2 { 
      text = (getString GOT_IT),
      padding = (Padding 16 0 16 0)
    },
    cornerRadius = (PTD.Corners 15.0 true true true true),
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET "ny_ic_unavailable"
      , visibility = VISIBLE
      , margin = Margin 16 16 16 24
      , width = MATCH_PARENT
      , height = V 200
    }
  }
  in popUpConfig'

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

rentalPolicyInfoConfig :: RentalScreenState -> RequestInfoCard.Config
rentalPolicyInfoConfig state = let
  config = RequestInfoCard.config
  requestInfoCardConfig' = config{
    title {
      text = getString RENTAL_POLICY,
      accessibilityHint = getEN RENTAL_POLICY,
      textStyle = FontStyle.Heading1
    }
  , primaryText {
      text = getString RENTAL_POLICY_DESC,
      padding = Padding 16 16 0 0,
      textStyle = FontStyle.ParagraphText,
      color = Color.black700,
      accessibilityHint =  getEN RENTAL_POLICY_DESC
    }
  , secondaryText {
      text = getString RENTAL_POLICY_DESC_1,
      visibility = VISIBLE,
      padding = Padding 16 16 0 0 ,
      color = Color.black700,
      textStyle = FontStyle.ParagraphText,
      width = MATCH_PARENT,
      accessibilityHint = getEN RENTAL_POLICY_DESC_1
    }
  , imageConfig {
      imageUrl = fetchImage FF_ASSET "ny_ic_policy",
      height = V 130,
      width = V 130,
      padding = Padding 0 2 2 0
    }
  , buttonConfig {
      text = getString GOT_IT,
      padding = PaddingVertical 16 20,
      accessibilityHint = (getEN GOT_IT) <> " : Button"
    }
  }
  in requestInfoCardConfig'