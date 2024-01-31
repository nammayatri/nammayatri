module Screens.SearchLocationScreen.ComponentConfig where

import PrestoDOM(Margin(..), Padding(..), Length(..), Orientation(..), Gravity(..), Visibility(..))
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
import Components.PopUpModal as PopUpModal
import Language.Types (STR(..))
import Common.Types.App (LazyCheck(..))
import PrestoDOM.Types.DomAttributes as PTD
import Mobility.Prelude (boolToVisibility)
import Screens 
import Debug(spy)

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
    headerVisibility = not (state.props.actionType == ST.SearchLocationAction)
    imageLayoutMargin = if headerVisibility then MarginLeft 24 else MarginLeft 16
    backIconPadding = if headerVisibility then PaddingTop 16 else PaddingTop 8
    config = InputView.config 
    inputViewConfig' = config
      { headerText = MB.maybe ("Trip Details") ( \ currTextField -> if currTextField == ST.SearchLocPickup then "Edit Pickup" else "Add Stop") state.props.focussedTextField,
        suffixButtonVisibility = GONE,
        headerVisibility = headerVisibility ,
        imageLayoutMargin = imageLayoutMargin ,
        inputView = map 
          ( \item -> 
            { padding : Padding 8 7 8 7 
            , height : WRAP_CONTENT
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
            , fontStyle : FontStyle.subHeading2 TypoGraphy
            , clearTextIcon : { 
                imageName : (state.appConfig.searchLocationConfig.clearTextImage) ,
                height : V 19,
                width : V 19 ,
                padding : PaddingVertical 10 2 }
            , gravity : LEFT
            , inputTextConfig : 
               { textValue : item.textValue
               , isFocussed : item.isFocussed
               , id : show item.id
               , placeHolder : item.placeHolder 
               , cornerRadius : 4.0
               , margin : item.margin
               , imageName : item.prefixImageName
               , textColor : Color.white900
               }
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
  in 
    [ { textValue :  if addressOnMap /= "" && pickUpFocussed then addressOnMap else (if state.props.textFieldText.pickUpLoc == "" then srcLoc else state.props.textFieldText.pickUpLoc )
      , isFocussed : pickUpFocussed
      , prefixImageName : "ny_ic_green_circle"
      , margin : MarginTop 8
      , placeHolder : if state.props.actionType == ST.SearchLocationAction then "Start" else "Enter Pickup Location"
      , canClearText : DS.length (if addressOnMap /= "" && pickUpFocussed then addressOnMap else srcLoc) > 2
      , id : ST.SearchLocPickup
      , isEditable : not $ (state.data.fromScreen == getScreen RIDE_SCHEDULED_SCREEN) || (state.props.actionType == ST.AddingStopAction && (state.data.fromScreen == getScreen HOME_SCREEN))
      } ,
      { textValue : if addressOnMap /= "" && dropLocFocussed then addressOnMap else destLoc
      , isFocussed : dropLocFocussed
      , prefixImageName : if state.props.actionType == ST.SearchLocationAction then "ny_ic_red_circle" else "ny_ic_blue_circle"
      , margin : MarginTop 8
      , placeHolder : if state.props.actionType == ST.SearchLocationAction then "Where To?" else "Add Stop"
      , canClearText : DS.length (if addressOnMap /= "" && dropLocFocussed then addressOnMap else destLoc) > 2
      , id : ST.SearchLocDrop
      , isEditable : true
      }
    ]
   

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
      text = getString ONLY_LOCATION_WITHIN_CITY_LIMITS ,
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
      imageUrl = HU.fetchImage HU.FF_ASSET "ny_ic_location_unserviceable"
      , visibility = VISIBLE
      , margin = Margin 16 16 16 24
      , width = MATCH_PARENT
      , height = V 200
    }
  }
  in popUpConfig'
