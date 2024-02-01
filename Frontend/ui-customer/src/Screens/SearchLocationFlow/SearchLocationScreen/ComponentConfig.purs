{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SearchLocationScreen.ComponentConfig where

import PrestoDOM(Margin(..), Padding(..), Length(..), Orientation(..), Gravity(..))
import Components.PrimaryButton as PrimaryButton
import Components.SeparatorView.View as SeparatorView
import Components.LocationTagBarV2 as LTB
import Components.InputView as InputView
import Components.MenuButton as MenuButton
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import MerchantConfig.Types as MT
import Prelude ((==), (<>), ($), (/=), (>), (||), map, not)
import Styles.Colors as Color
import Font.Style as FontStyle
import Font.Size as FontSize
import Data.Maybe(isJust, maybe, Maybe(..)) as MB
import Helpers.Utils as HU
import Data.String (length) as DS
import Prelude (show, (&&))
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens 

locationTagBarConfig :: ST.SearchLocationScreenState -> ST.GlobalProps -> LTB.LocationTagBarConfig
locationTagBarConfig state globalProps = 
  let 
    homeExists = MB.isJust $ HU.getSavedLocationByTag globalProps.savedLocations ST.HOME_TAG 
    workExists = MB.isJust $ HU.getSavedLocationByTag globalProps.savedLocations ST.WORK_TAG
    home = if homeExists then { image : "ny_ic_home_blue", text : "Home", id : "HOME" }
            else { image : "ny_ic_add_address", text : "Add Home", id : "ADD_HOME" }
    work = if workExists then { image : "ny_ic_work_blue", text : "Work", id : "WORK" } 
            else { image : "ny_ic_add_address", text : "Add Work", id : "ADD_WORK" }

    locTagList =
      map 
        (\item -> 
          { imageConfig : 
              { height : V 16
              , width : V 16
              , imageWithFallback : item.image
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
            height : WRAP_CONTENT ,
            width : WRAP_CONTENT,
            padding : Padding 8 8 8 8 ,
            id : item.id
          })
        [ { image : home.image, text : home.text, id : home.id },
          { image : work.image, text : work.text, id : work.id },
          { image : "ny_ic_fav_red", text : "All Favourites", id : "ALL_FAVOURITES" }]
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
      ST.LocateOnMapStage -> MB.maybe "" (\currTextField -> if currTextField == ST.SearchLocPickup then (getString CONFIRM_PICKUP_LOCATION) else (getString CONFIRM_DROP)) state.props.focussedTextField
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
      }
  in primaryButtonConfig'

confirmLocBtnConfig :: ST.SearchLocationScreenState -> PrimaryButton.Config 
confirmLocBtnConfig state = 
  let 
    config = PrimaryButton.config
    confirmLocBtnConfig' = 
      config
        { textConfig
          { text = "Confirm Location"
          , color = state.appConfig.primaryTextColor
          }
        , cornerRadius = state.appConfig.primaryButtonCornerRadius
        , margin = MarginTop 8 
        , id = "ConfirmLocationSLS"
        , background = state.appConfig.primaryBackground
        }
  in confirmLocBtnConfig'

mapInputViewConfig :: ST.SearchLocationScreenState -> Boolean -> InputView.InputViewConfig
mapInputViewConfig state isEditable = let 
    getInputView = getInputViewConfigBasedOnActionType state.props.actionType
    config = InputView.config 
    inputViewConfig' = config
      { headerText = MB.maybe ("Trip Details") ( \ currTextField -> if currTextField == ST.SearchLocPickup then "Edit Pickup" else "Add Stop") state.props.focussedTextField,
      headerVisibility =  getInputView.headerVisibility,
        imageLayoutMargin = getInputView.imageLayoutMargin,
        inputView = map 
          ( \item -> 
            { margin : item.margin
            , padding : Padding 8 7 8 7 
            , textValue : item.textValue
            , height : WRAP_CONTENT
            , isFocussed : item.isFocussed
            , id : show item.id
            , placeHolder : item.placeHolder 
            , canClearText : (item.canClearText  || state.props.canClearText ) && item.isFocussed
            , isEditable : isEditable && item.isEditable
            , isClickable : item.isEditable
            , prefixImage : { 
                imageName : item.prefixImageName,
                height : V 15,
                width : V 15 ,
                padding : Padding 0 0 0 0 }
            , stroke : ((if item.isFocussed then "1," else "0,") <> Color.yellow900)
            , imageSeparator : separatorConfig 
            , clearTextIcon : { 
                imageName : (state.appConfig.searchLocationConfig.clearTextImage) ,
                height : V 19,
                width : V 19 ,
                padding : PaddingVertical 10 2 }
            , cornerRadius : 4.0
            } 
          ) 
          (inputViewArray state)
      }

  in inputViewConfig'

inputViewArray state = 
  let 
    srcLoc = MB.maybe "" (_.address) state.data.srcLoc 
    destLoc = MB.maybe "" (_.address) state.data.destLoc
    addressOnMap = state.data.latLonOnMap.address
    pickUpFocussed = state.props.focussedTextField == MB.Just ST.SearchLocPickup 
    dropLocFocussed = state.props.focussedTextField == MB.Just ST.SearchLocDrop 
    getInputView = getInputViewConfigBasedOnActionType state.props.actionType
  in 
    [ { textValue : if addressOnMap /= "" && pickUpFocussed then addressOnMap else srcLoc
      , isFocussed : pickUpFocussed
      , prefixImageName : getInputView.srcPrefixImageName
      , margin : getInputView.inputViewMargin
      , placeHolder : getInputView.srcPlaceHolder
      , canClearText : DS.length (if addressOnMap /= "" && pickUpFocussed then addressOnMap else srcLoc) > 2
      , id : ST.SearchLocPickup
      , isEditable : not $ (state.props.actionType == ST.AddingStopAction && (state.data.fromScreen == getScreen HOME_SCREEN))
      } ,
      { textValue : if addressOnMap /= "" && dropLocFocussed then addressOnMap else destLoc
      , isFocussed : dropLocFocussed
      , prefixImageName : getInputView.destPrefixImageName
      , margin : MarginTop 8
      , placeHolder : getInputView.destPlaceHolder
      , canClearText : DS.length (if addressOnMap /= "" && dropLocFocussed then addressOnMap else destLoc) > 2
      , id : ST.SearchLocDrop
      , isEditable : true
      }
    ]

getInputViewConfigBasedOnActionType :: ST.SearchLocationActionType -> { srcPrefixImageName :: String, destPrefixImageName :: String, srcPlaceHolder :: String, destPlaceHolder :: String, inputViewMargin :: Margin, headerVisibility :: Boolean, imageLayoutMargin :: Margin }
getInputViewConfigBasedOnActionType actionType =
  case actionType of 
        ST.AddingStopAction -> { srcPrefixImageName : "ny_ic_green_circle", destPrefixImageName : "ny_ic_blue_circle", srcPlaceHolder : "Enter Pickup Location", destPlaceHolder : "Add Stop",  inputViewMargin : MarginTop 8,  headerVisibility : true, imageLayoutMargin : MarginLeft 24 }
        ST.SearchLocationAction -> { srcPrefixImageName : "ny_ic_green_circle", destPrefixImageName : "ny_ic_blue_circle", srcPlaceHolder : "Enter Pickup Location", destPlaceHolder : "Add Stop", inputViewMargin : MarginTop 8, headerVisibility : true, imageLayoutMargin : MarginLeft 24 }
        ST.MetroStationSelectionAction -> { srcPrefixImageName : "ny_ic_green_circle", destPrefixImageName : "ny_ic_red_circle", srcPlaceHolder : "Starting From?", destPlaceHolder : "Where to?", inputViewMargin : MarginTop 0, headerVisibility : false, imageLayoutMargin : MarginLeft 0 }

menuButtonConfig :: ST.SearchLocationScreenState -> ST.Location -> MenuButton.Config
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