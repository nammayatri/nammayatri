{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SearchLocationScreen.ComponentConfig where

import ConfigProvider
import Helpers.TipConfig
import Screens
import Debug
import Common.Types.App (LazyCheck(..), RateCardType(..))
import Components.ChooseYourRide as ChooseYourRide
import Components.InputView as InputView
import Components.LocationTagBarV2 as LTB
import Components.MenuButton as MenuButton
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.RateCard as RateCard
import Components.SeparatorView.View as SeparatorView
import Data.Array as DA
import Data.Maybe (isJust, maybe, Maybe(..)) as MB
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Strings (getVarString)
import Language.Types (STR(..))
import MerchantConfig.Types as MT
import Mobility.Prelude (boolToVisibility)
import Prelude ((==), (<>), ($), (/=), (>), (||), map, not, identity, (-), (*))
import Prelude (show, (&&), identity)
import PrestoDOM (Margin(..), Padding(..), Length(..), Orientation(..), Gravity(..), Visibility(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Types.DomAttributes as PTD
import Screens.SearchLocationScreen.ScreenData (dummyQuote)
import Screens.Types as ST
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Data.Lens ((^.))
import Accessor (_amount)
import Resources.LocalizableV2.Strings (getEN)

locationTagBarConfig :: ST.SearchLocationScreenState -> ST.GlobalProps -> LTB.LocationTagBarConfig
locationTagBarConfig state globalProps = 
  let 
    homeExists = MB.isJust $ HU.getSavedLocationByTag globalProps.savedLocations ST.HOME_TAG 
    workExists = MB.isJust $ HU.getSavedLocationByTag globalProps.savedLocations ST.WORK_TAG
    home = if homeExists then { image : "ny_ic_home_blue", text : "Home", id : "HOME" }
            else { image : "ny_ic_add_address", text : "Home", id : "ADD_HOME" }
    work = if workExists then { image : "ny_ic_work_blue", text : "Work", id : "WORK" } 
            else { image : "ny_ic_add_address", text : "Work", id : "ADD_WORK" }

    locTagList =
      map 
        (\item -> 
          { imageConfig : 
              { height : V 16
              , width : V 16
              , imageWithFallback : item.image
              , margin : MarginRight 5
              } ,
            textConfig : 
              { text : item.text
              , fontStyle : FontStyle.Body1
              , fontSize : FontSize.a_14
              , color : Color.black900 
              },
            stroke : "1," <> Color.grey900 ,
            cornerRadius : Corners 8.0 true true true true ,
            background : Color.white900 ,
            orientation : HORIZONTAL ,
            height : WRAP_CONTENT ,
            width : WRAP_CONTENT,
            padding : Padding 8 8 8 8 ,
            id : item.id,
            enableRipple : false,
            rippleColor : Color.rippleShade,
            bannerConfig : {
              text : "Favourites",
              color : Color.white900,
              fontStyle : FontStyle.Body1,
              textSize : FontSize.a_14,
              cornerRadii : Corners 8.0 true true false false,
              background : Color.blue800
            },
            showBanner : GONE
          })
        [ { image : home.image, text : home.text, id : home.id },
          { image : work.image, text : work.text, id : work.id },
          { image : "ny_ic_fav_red", text : "Favourites", id : "ALL_FAVOURITES" }]
  in 
    { tagList : locTagList }

separatorConfig :: SeparatorView.Config
separatorConfig = 
  { orientation : VERTICAL
  , count : 3
  , height : V 4
  , width : V 1
  , layoutWidth : V 12
  , layoutHeight : V 15
  , color : Color.black500
  }


primaryButtonConfig :: ST.SearchLocationScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    buttonText = case state.props.searchLocStage of 
      ST.ConfirmLocationStage -> (getString CONFIRM_PICKUP_LOCATION)
      ST.LocateOnMapStage -> MB.maybe "" (\currTextField -> if currTextField == ST.SearchLocPickup then (getString CONFIRM_PICKUP_LOCATION) else (getString CONFIRM_DROP_LOCATION)) state.props.focussedTextField
      _ -> ""
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        { text = buttonText
        , color = state.appConfig.primaryTextColor
        , height = V 40
        }
      , height = V state.appConfig.searchLocationConfig.primaryButtonHeight
      , gravity = CENTER
      , cornerRadius = state.appConfig.primaryButtonCornerRadius
      , background = state.appConfig.primaryBackground
      , margin = MarginHorizontal 16 16
      , isClickable = true
      , id = "SelectLocationFromMapSLS"
      , enableRipple = true
      }
  in primaryButtonConfig'

confirmLocBtnConfig :: ST.SearchLocationScreenState -> PrimaryButton.Config 
confirmLocBtnConfig state = 
  let 
    config = PrimaryButton.config
    confirmLocBtnConfig' = 
      config
        { textConfig
          { text = getString CONFIRM_LOCATION
          , color = state.appConfig.primaryTextColor
          , accessibilityHint = (getEN CONFIRM_LOCATION) <> " Button"
          }
        , cornerRadius = state.appConfig.primaryButtonCornerRadius
        , margin = MarginTop 8 
        , id = "ConfirmLocationSLS"
        , enableRipple = true
        , background = state.appConfig.primaryBackground
        }
  in confirmLocBtnConfig'

mapInputViewConfig :: ST.SearchLocationScreenState -> Boolean -> InputView.InputViewConfig
-- mapInputViewConfig state isEditable = let 
--     getInputView = getInputViewConfigBasedOnActionType state.props.actionType
--     config = InputView.config 
--     inputViewConfig' = config
--       { headerText = MB.maybe ("Trip Details") ( \ currTextField -> if currTextField == ST.SearchLocPickup then "Edit Pickup" else "Add Stop") state.props.focussedTextField,
--       headerVisibility =  getInputView.headerVisibility,
--         imageLayoutMargin = getInputView.imageLayoutMargin,
--         inputView = map 
--           ( \item -> 
--             { margin : item.margin
--             , padding : Padding 8 7 8 7 
--             , textValue : item.textValue
--             , height : WRAP_CONTENT
--             , isFocussed : item.isFocussed
--             , id : show item.id
--             , placeHolder : item.placeHolder 
--             , canClearText : (item.canClearText  || state.props.canClearText ) && item.isFocussed
--             , isEditable : isEditable && item.isEditable
--             , isClickable : item.isEditable
--             , prefixImage : { 
--                 imageName : item.prefixImageName,
--                 height : V 15,
--                 width : V 15 ,
--                 padding : Padding 0 0 0 0 }
--             , stroke : ((if item.isFocussed then "1," else "0,") <> Color.yellow900)
--             , imageSeparator : separatorConfig 
--             , clearTextIcon : { 
--                 imageName : (state.appConfig.searchLocationConfig.clearTextImage) ,
--                 height : V 19,
--                 width : V 19 ,
--                 padding : PaddingVertical 10 2 }
--             , cornerRadius : 4.0
--             } 
--           ) 
--           (inputViewArray state)
mapInputViewConfig state isEditable = 
  let 
    getInputView = getInputViewConfigBasedOnActionType state.props.actionType
    headerVisibility = getInputView.headerVisibility
    imageLayoutMargin = if headerVisibility then MarginLeft 24 else MarginLeft 16
    backIconPadding = if headerVisibility then PaddingTop 16 else PaddingTop 8
    headerText = 
      MB.maybe (getString TRIP_DETAILS_) 
        (\currTextField -> 
          if currTextField == ST.SearchLocPickup then 
            getString EDIT_PICKUP 
          else 
            getString ADD_STOP
        ) 
        state.props.focussedTextField
    config = InputView.config 
    inputViewConfig' = config
      { headerText = headerText
      , suffixButtonVisibility = GONE
      , headerVisibility = headerVisibility
      , imageLayoutMargin = imageLayoutMargin
      , inputView = map (\item -> transformInputViewArray item) (inputViewArray state)
      }
  in inputViewConfig'

-- inputViewArray state = 
--   let 
--     srcLoc = MB.maybe "" (_.address) state.data.srcLoc 
--     destLoc = MB.maybe "" (_.address) state.data.destLoc
--     addressOnMap = state.data.latLonOnMap.address
--     pickUpFocussed = state.props.focussedTextField == MB.Just ST.SearchLocPickup 
--     dropLocFocussed = state.props.focussedTextField == MB.Just ST.SearchLocDrop 
--     getInputView = getInputViewConfigBasedOnActionType state.props.actionType
--   in 
--     [ { textValue : if addressOnMap /= "" && pickUpFocussed then addressOnMap else srcLoc
--       , isFocussed : pickUpFocussed
--       , prefixImageName : getInputView.srcPrefixImageName
--       , margin : getInputView.inputViewMargin
--       , placeHolder : getInputView.srcPlaceHolder
--       , canClearText : DS.length (if addressOnMap /= "" && pickUpFocussed then addressOnMap else srcLoc) > 2
--       , id : ST.SearchLocPickup
--       , isEditable : not $ (state.props.actionType == ST.AddingStopAction && (state.data.fromScreen == getScreen HOME_SCREEN))
--       } ,
--       { textValue : if addressOnMap /= "" && dropLocFocussed then addressOnMap else destLoc
--       , isFocussed : dropLocFocussed
--       , prefixImageName : getInputView.destPrefixImageName
--       , margin : MarginTop 8
--       , placeHolder : getInputView.destPlaceHolder
--       , canClearText : DS.length (if addressOnMap /= "" && dropLocFocussed then addressOnMap else destLoc) > 2
--       , id : ST.SearchLocDrop
--       , isEditable : true
--       }
--     ]

  where
    transformInputViewArray item = 
      { padding : Padding 8 7 8 7 
      , height : WRAP_CONTENT
      , canClearText : (item.canClearText || state.props.canClearText) && item.isFocussed
      , isEditable : isEditable && item.isEditable
      , isClickable : item.isEditable
      , prefixImage : 
          { imageName : item.prefixImageName
          , height : V 15
          , width : V 15
          , padding : Padding 0 0 0 0 
          }
      , stroke : if item.isFocussed && DS.null state.props.routeSelected then "1," <> Color.yellow900 else "0," <> Color.yellow900
      , alpha : item.alpha
      , imageSeparator : separatorConfig 
      , fontStyle : FontStyle.subHeading2 TypoGraphy
      , clearTextIcon : 
          { imageName : state.appConfig.searchLocationConfig.clearTextImage
          , height : V 19
          , width : V 19
          , padding : PaddingVertical 10 2 
          }
      , gravity : LEFT
      , inputTextConfig : 
         { textValue : item.textValue
         , isFocussed : item.isFocussed
         , id : show item.id
         , placeHolder : item.placeHolder 
         , cornerRadius : 4.0
         , margin : item.margin
         , imageName : item.prefixImageName
         , textColor : if DS.null item.textValue then Color.grey900 else Color.white900
         , prefixImageVisibility : GONE 
         , prefixImageConfig : InputView.dummyImageConfig
         }
      }

    inputViewArray state = 
      let 
        srcLoc = MB.maybe "" (_.address) state.data.srcLoc 
        destLoc = MB.maybe "" (_.address) state.data.destLoc
        addressOnMap = state.data.latLonOnMap.address
        pickUpFocussed = state.props.focussedTextField == MB.Just ST.SearchLocPickup 
        dropLocFocussed = state.props.focussedTextField == MB.Just ST.SearchLocDrop 
        getInputView = getInputViewConfigBasedOnActionType state.props.actionType
      in 
        [ { textValue :  srcLoc
          , isFocussed : pickUpFocussed
          , prefixImageName : getInputView.srcPrefixImageName
          , margin : getInputView.inputViewMargin
          , placeHolder : getInputView.srcPlaceHolder
          , canClearText : DS.length (if addressOnMap /= "" && pickUpFocussed then addressOnMap else srcLoc) > 2
          , id : ST.SearchLocPickup
          , isEditable : (not $ (state.data.fromScreen == getScreen RIDE_SCHEDULED_SCREEN) || (state.props.actionType == ST.AddingStopAction && (state.data.fromScreen == getScreen HOME_SCREEN)))
          , alpha : 1.0
          }] <> (if state.props.routeSearch then [] else 
          [{ textValue : destLoc
          , isFocussed : dropLocFocussed
          , prefixImageName : getInputView.destPrefixImageName
          , margin : getInputView.inputViewMargin
          , placeHolder : getInputView.destPlaceHolder
          , canClearText : DS.length (if addressOnMap /= "" && dropLocFocussed then addressOnMap else destLoc) > 2
          , id : ST.SearchLocDrop
          , alpha : if state.props.actionType /= ST.BusStopSelectionAction then 1.0 else 0.5
          , isEditable : if (DS.null state.props.routeSelected && state.props.actionType == ST.BusStopSelectionAction) then false else true
          }] )
      
    getInputViewConfigBasedOnActionType :: ST.SearchLocationActionType -> { srcPrefixImageName :: String, destPrefixImageName :: String, srcPlaceHolder :: String, destPlaceHolder :: String, inputViewMargin :: Margin, headerVisibility :: Boolean, imageLayoutMargin :: Margin }
    getInputViewConfigBasedOnActionType actionType =
      case actionType of 
            ST.AddingStopAction -> { srcPrefixImageName : "ny_ic_green_circle", destPrefixImageName : "ny_ic_blue_circle", srcPlaceHolder : (getString ENTER_PICKUP_LOC), destPlaceHolder : (getString ADD_STOP),  inputViewMargin : MarginTop 8,  headerVisibility : true, imageLayoutMargin : MarginLeft 24 }
            ST.SearchLocationAction -> { srcPrefixImageName : "ny_ic_green_circle", destPrefixImageName : "ny_ic_blue_circle", srcPlaceHolder : (getString START_), destPlaceHolder : (getString WHERE_TO), inputViewMargin : MarginTop 8, headerVisibility : true, imageLayoutMargin : MarginLeft 24 }
            ST.MetroStationSelectionAction -> { srcPrefixImageName : "ny_ic_green_circle", destPrefixImageName : "ny_ic_red_circle", srcPlaceHolder : getString STARTING_FROM <>"?", destPlaceHolder : (getString WHERE_TO), inputViewMargin : MarginTop 8, headerVisibility : false, imageLayoutMargin : MarginLeft 0 }
            ST.BusStationSelectionAction -> { srcPrefixImageName :"ny_ic_green_circle", destPrefixImageName : "ny_ic_red_circle", srcPlaceHolder : getString ROUTE_BUS_NO , destPlaceHolder : (getString WHERE_TO), inputViewMargin : MarginTop 8, headerVisibility : false, imageLayoutMargin : MarginLeft 0 }
            ST.BusSearchSelectionAction -> { srcPrefixImageName :"", destPrefixImageName : "", srcPlaceHolder : getString ENTER_BUS_NUMBER_OR_DESTINATION , destPlaceHolder : "", inputViewMargin : MarginTop 8, headerVisibility : false, imageLayoutMargin : MarginLeft 0 }
            ST.BusRouteSelectionAction ->  { srcPrefixImageName :"", destPrefixImageName : "", srcPlaceHolder : getString DESTINATION_STOPS , destPlaceHolder : "", inputViewMargin : MarginTop 8, headerVisibility : false, imageLayoutMargin : MarginLeft 0 }
            ST.BusStopSelectionAction -> { srcPrefixImageName : "ny_ic_green_circle", destPrefixImageName : "ny_ic_red_circle", srcPlaceHolder : getString PICKUP_STOP, destPlaceHolder : getString DESTINATION_STOPS, inputViewMargin : MarginTop 8, headerVisibility : false, imageLayoutMargin : MarginLeft 0 }
            ST.NoBusRouteSelectionAction -> { srcPrefixImageName :"", destPrefixImageName : "", srcPlaceHolder : getString ROUTE_BUS_NO , destPlaceHolder : "", inputViewMargin : MarginTop 8, headerVisibility : false, imageLayoutMargin : MarginLeft 0 }

menuButtonConfig :: ST.SearchLocationScreenState -> JB.Location -> MenuButton.Config
menuButtonConfig state item = let
    isFocussed = item.place == state.data.defaultGate
    layoutBg = if isFocussed then Color.blue600 else Color.white900
    layoutStroke = if isFocussed then ("1," <> Color.blue700') else ("1," <> Color.grey900)
    config = MenuButton.config
    menuButtonConfig' = config {
      titleConfig{
          text = item.place
        , gravity = CENTER_VERTICAL
      }
    , accessibilityHint = item.place
    , radioButtonConfig {
        height = V 16
        , width = V 16
        , cornerRadius = 8.0
        , buttonWidth = V 8
        , buttonHeight = V 8
        , buttonColor = Color.positive
        , margin = (MarginRight 15)
        , activeStroke = ("2," <> Color.positive)
      }
      , id = item.place
      , lat = item.lat
      , lng = item.lng
      , leftsidebutton = true
      , padding = (Padding 14 14 14 14)
      , cornerRadius = 6.0
      , height = WRAP_CONTENT
      , width = MATCH_PARENT
      , isSelected = item.place == state.data.defaultGate
      , layoutStroke = layoutStroke
      , layoutBg = layoutBg
    }
    in menuButtonConfig'

locUnserviceablePopUpConfig :: ST.SearchLocationScreenState -> PopUpModal.Config
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
      text = getString ONLY_LOCATION_WITHIN_CITY_LIMITS,
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
      imageUrl = HU.fetchImage HU.FF_ASSET "ny_ic_unavailable"
      , visibility = VISIBLE
      , margin = Margin 16 16 16 24
      , width = MATCH_PARENT
      , height = V 200
    }
  }
  in popUpConfig'

primaryButtonRequestRideConfig :: ST.SearchLocationScreenState -> PrimaryButton.Config
primaryButtonRequestRideConfig state =
  let
    config = PrimaryButton.config
    nullQuoteList = DA.null state.data.quotesList
    primaryButtonConfig' =
      config
        { textConfig
          { text = "Book Rental Ride" 
          , color = state.appConfig.primaryTextColor
          , accessibilityHint = "Book Rental Ride : Button"
          }
        , cornerRadius = state.appConfig.primaryButtonCornerRadius
        , margin = (Margin 16 16 16 16)
        , id = "RequestRentalRideButton"
        , background = state.appConfig.primaryBackground
        , enableRipple = true
        , rippleColor = Color.rippleShade
        , isClickable = not nullQuoteList
        , alpha = if nullQuoteList then 0.5 else 1.0
        }
  in
    primaryButtonConfig'

rentalRateCardConfig :: ST.SearchLocationScreenState -> RateCard.Config
rentalRateCardConfig state =
  let config = RateCard.config
      currency = getCurrency appConfig
      extraDistance = state.data.rideDetails.rideDistance - (state.data.rideDetails.rideDuration * 10)
      selectedQuote = MB.maybe dummyQuote identity (state.data.selectedQuote)
      rentalRateCardConfig' = config
        { currentRateCardType = RentalRateCard
        , title = getString RATE_CARD
        , description = getString RENTAL_CHARGES
        , buttonText = MB.Just $ getString GOT_IT
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
            {key : "NIGHT_TIME_FEE_DESCRIPTION", val : (getVarString NIGHT_TIME_FEE_DESCRIPTION $ DA.singleton $ currency <> (show selectedQuote.fareDetails.nightShiftCharge))}
          , {key : "APPLICABLE_WAITING_CHARGES" , val : "Applicable waiting charges will be added to your final fare."}
          , {key : "PARKING_FEES_AND_TOLLS_NOT_INCLUDED", val : (getString PARKING_FEES_AND_TOLLS_NOT_INCLUDED)}
          ]
        , fareList = [
            {key : ("Base Rental Fare (incl. " <> show (state.data.rideDetails.rideDistance - extraDistance)  <> " km & " <> show state.data.rideDetails.rideDuration <> " hrs)"), val : (currency <> show (state.data.rideDetails.rideDuration * selectedQuote.fareDetails.perHourCharge))}
            ] <> 
            (if extraDistance > 0 then 
              [{key : "Fare for additional distance (" <> show ( extraDistance) <> " kms)", val : (currency <> show (extraDistance * selectedQuote.fareDetails.plannedPerKmRate))}] 
              else [] )
            <> 
            [ {key : "Fare for distance above " <> (show state.data.rideDetails.rideDistance) <> " kms", val : (currency <> show selectedQuote.fareDetails.perExtraKmRate <> "/km")}
              , {key : "Fare for usage above " <> (show state.data.rideDetails.rideDuration) <> " hrs", val : (currency <> show selectedQuote.fareDetails.perExtraMinRate <> "/min")}
            ] 
            <> 
            (case selectedQuote.fareDetails.tollCharges of
              MB.Just tollCharges -> [{key : getString TOLL_CHARGES_ESTIMATED, val : (currency <> show tollCharges)}]
              MB.Nothing -> [])
            <> 
            ( case selectedQuote.fareDetails.deadKmFare of
                MB.Just deadKmFare -> [ { key: getString PICKUP_CHARGES, val: (currency <> (show  $ deadKmFare ^. _amount) ) } ]
                MB.Nothing -> []
            )
                  
        }
  in rentalRateCardConfig'


chooseYourRideConfig state = 
  let 
    quoteSelected = MB.maybe dummyQuote identity state.data.selectedQuote
    tipConfig = getTipConfig quoteSelected.quoteDetails.vehicleVariant
    city = getValueToLocalStore CUSTOMER_LOCATION

  in 
  ChooseYourRide.config {
    quoteList = map (\quote -> quote.quoteDetails) state.data.quotesList,
    rideDistance = (show state.data.rideDetails.rideDistance) <> " km",
    rideDuration = (show state.data.rideDetails.rideDuration) <> " hrs",
    customerTipArray = tipConfig.customerTipArray,
    customerTipArrayWithValues = tipConfig.customerTipArrayWithValues,
    enableTips = false, 
    rideTime = formatDate "D" <> " " <> formatDate "MMM" <> ", " <> formatDate "hh" <> ":" <> formatDate "mm" <> " " <> formatDate "A",
    tipViewProps = getTipViewProps state.props.tipViewProps quoteSelected.quoteDetails.vehicleVariant MB.Nothing MB.Nothing,
    tipForDriver = state.props.customerTip.tipForDriver,
    fareProductType = state.props.fareProductType,
    activeIndex = quoteSelected.activeIndex ,
    currentEstimateHeight = state.props.currentEstimateHeight,
    selectedEstimateHeight = state.props.selectedEstimateHeight,
    showMultiProvider = false
  }

  where 
    formatDate formatSTR = 
      let startTime = if state.data.rideDetails.rideScheduledTimeUTC == "" then EHC.getCurrentUTC "" else state.data.rideDetails.rideScheduledTimeUTC
      in EHC.convertUTCtoISC startTime formatSTR

noStopRouteConfig :: ST.SearchLocationScreenState -> PopUpModal.Config
noStopRouteConfig state = let
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
      text = "Oops! No stops found nearby.", 
      gravity = CENTER,
      margin = MarginTop 16
      },
    secondaryText { 
      text = "Try searching a different location",
      margin = MarginTop 4
      },
    option1 {
      visibility = false
      },
    option2 { 
      text = "Search Different Location",
      padding = (Padding 16 0 16 0)
    },
    cornerRadius = (PTD.Corners 15.0 true true true true),
    coverImageConfig {
      imageUrl = HU.fetchImage HU.COMMON_ASSET "ny_ic_noroute_nobus"
      , visibility = VISIBLE
      , margin = Margin 16 16 16 24
      , width = MATCH_PARENT
      , height = V 200
    }
  }
  in popUpConfig'