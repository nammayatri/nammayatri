{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AddNewAddressScreen.View where

import Common.Types.App
import Screens.CustomerUtils.AddNewAddressScreen.ComponentConfig

import Animation as Anim
import Components.GenericHeader as GenericHeader
import Components.LocationListItem (dummyAddress)
import Components.LocationListItem as LocationListItem
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (LANGUAGE_KEY(..), getString, getKey)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, not, pure, show, unit, ($), (&&), (*), (-), (/), (/=), (<<<), (<>), (==), (||))
import PrestoDOM (Length(..), Margin(..), Orientation(..), Gravity(..), Visibility(..), Padding(..), PrestoDOM, Screen, height, width, color, background, orientation, padding, margin, onBackPressed, linearLayout, gravity, textView, text, textSize, fontStyle, scrollView, scrollBarY, relativeLayout, editText, hint, singleLine, hintColor, ellipsize, cornerRadius, lineHeight, stroke, onChange, id, visibility, maxLines, onClick, imageView, imageUrl, alignParentBottom, afterRender, adjustViewWithKeyboard, weight, alpha, frameLayout, clickable, onFocus, imageWithFallback)
import Screens.AddNewAddressScreen.Controller (Action(..), ScreenOutput, eval, validTag)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App
import Screens.CustomerUtils.AddNewAddressScreen.ComponentConfig 
import Storage (KeyStore(..), getValueToLocalStore)

screen :: ST.AddNewAddressScreenState -> Screen Action ST.AddNewAddressScreenState ScreenOutput
screen initialState = 
  { initialState
  , view
  , name : "AddNewAddressScreen"
  , globalEvents : [(\push -> do 
                      if initialState.props.isLocateOnMap then do 
                        pure (pure unit) 
                        else do
                        _ <- HU.storeCallBackLocateOnMap push UpdateLocation 
                        pure unit
                        pure (pure unit))]
  , eval 
  }

view :: forall w . (Action  -> Effect Unit) -> ST.AddNewAddressScreenState -> PrestoDOM (Effect Unit) w
view push state = 
  Anim.screenAnimation $
  relativeLayout[
    width MATCH_PARENT
  , height MATCH_PARENT
  , onBackPressed push $ const BackPressed state
  , afterRender
    (\action -> do
          _ <- (JB.showMap (EHC.getNewIDWithTag "AddNewAddressHomeScreenMap") true "satellite" (19.0) push MAPREADY)
          _ <- HU.setText' (EHC.getNewIDWithTag "SavedLocationEditText") (state.data.address)
          _ <- HU.setText' (EHC.getNewIDWithTag "SaveAsEditText") (state.data.addressSavedAs)
          _ <- pure $ JB.locateOnMap true 0.0 0.0
          _ <- if (state.data.activeIndex == Just 2 && state.props.showSavePlaceView) then JB.requestKeyboardShow (EHC.getNewIDWithTag ("SaveAsEditText")) else pure unit
          pure unit 
          ) (const AfterRender)
  ][
   frameLayout[
     width MATCH_PARENT
    , height MATCH_PARENT
    , clickable true
   ][ 
      linearLayout
      [  height MATCH_PARENT
      , width MATCH_PARENT
      , margin (Margin 0 EHC.safeMarginTop 0 0)
      , id (EHC.getNewIDWithTag "AddNewAddressHomeScreenMap")
      ][]
    , linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , background Color.transparent
      , padding (PaddingBottom 45)
      , gravity CENTER
      ][ imageView
         [ width $ V 60
         , height $ V 60 
         , imageWithFallback $ (HU.getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) <> ",https://assets.juspay.in/nammayatri/images/user/ny_ic_customer_current_location.png"
         ]
       ]
    , relativeLayout 
      [ background (if state.props.isLocateOnMap then Color.transparent else "#F5F5F5")
      , height MATCH_PARENT
      , width MATCH_PARENT
      , padding (Padding 0 EHC.safeMarginTop 0 0)
      ]([
        linearLayout
        [ height $ V ((EHC.screenHeight unit) / 7)
        , width MATCH_PARENT
        , clickable true
        , background Color.black900 
        , padding (Padding 0 EHC.safeMarginTop 0 0)
        ][]
      , GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
      , addNewScreenView state push
      , savePlaceView state push
      , confirmLocationView state push 
      , bottomBtnsView state push

      ])]
  
  ]

confirmLocationView :: forall w . ST.AddNewAddressScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
confirmLocationView state push = 
  linearLayout[
    height WRAP_CONTENT
  , width MATCH_PARENT
  , alignParentBottom "true,-1"
  , orientation VERTICAL
  , padding (Padding 16 0 16 EHC.safeMarginBottom)
  , visibility if state.props.isLocateOnMap then VISIBLE else GONE
  ][  recenterButtonView state push 
    , PrimaryButton.view (push <<< PrimaryButtonConfirmLocAC) (primaryButtonConfigConfirmLoc state)]

recenterButtonView :: forall w . ST.AddNewAddressScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
recenterButtonView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity RIGHT
  ][  imageView
      [ imageWithFallback "ny_ic_recenter_btn,https://assets.juspay.in/nammayatri/images/common/ny_ic_recenter_btn.png"
      , height $ V 40 
      , width $ V 40 
      , onClick (\action -> do
        _ <- push action
        _ <- JB.getCurrentPosition push UpdateCurrentLocation
        pure unit 
       ) (const $ RecenterCurrentLocation)
      ]

  ]

bottomBtnsView :: forall w . ST.AddNewAddressScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
bottomBtnsView state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT  
    , orientation VERTICAL
    , alignParentBottom "true,-1"
    , visibility if (state.props.isLocateOnMap || state.props.showSavePlaceView || (DA.null state.data.locationList)) then GONE else VISIBLE
    , gravity CENTER_VERTICAL
    , adjustViewWithKeyboard "true"
    ][  linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.grey900
        ][]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900 
        ](DA.mapWithIndex (\idx item -> linearLayout
        [ height WRAP_CONTENT
        , width $ V ((EHC.screenWidth unit/2))
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ][ linearLayout
            [ height WRAP_CONTENT
            , width $ V 0
            , weight 1.0
            , gravity CENTER
            ][  linearLayout
                [height MATCH_PARENT
                , width WRAP_CONTENT
                , padding (Padding 0 16 0 16)
                , gravity CENTER 
                ][  imageView
                    [ height $ V 16
                    , width $ V 16
                    , imageWithFallback item.imageUrl
                    ]
                  ]
            , textView $ 
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text item.text
              , gravity CENTER
              , color Color.black800
              , padding (Padding 10 16 16 16)
              , onClick (\action -> if item.tag == "CURRENT_LOCATION" then do  
                                _ <- push action
                                HU.getLocationName push "9.9" "9.9" "Current Location" SelectedCurrentLocation
                                else do 
                                  _ <- push action
                                  pure unit) (const item.action)
              ] <> FontStyle.body1 TypoGraphy
            ] 
          , linearLayout
            [ width $ V 1
            , height $ V 20
            , background Color.brownishGrey
            , alpha 0.25
            , visibility if DA.length (btnData state) - 1 == idx then GONE else VISIBLE
            ][]
        ]) $ btnData state)]

btnData :: ST.AddNewAddressScreenState ->  Array {text :: String, imageUrl :: String, action :: Action, tag :: String}
btnData state = [ {text : (getString SELECT_ON_MAP), imageUrl : "ny_ic_locate_on_map,https://assets.juspay.in/nammayatri/images/user/ny_ic_locate_on_map.png", action : SetLocationOnMap, tag : "LOCATE_ON_MAP"},
                  {text : (getString CURRENT_LOCATION), imageUrl : "ny_ic_current_location,https://assets.juspay.in/nammayatri/images/user/ny_ic_current_location.png", action : CurrentLocationAction, tag : "CURRENT_LOCATION"}]

addNewScreenView :: forall w. ST.AddNewAddressScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
addNewScreenView state push = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.transparent
  , visibility if state.props.showSavePlaceView then GONE else VISIBLE
  , margin (MarginTop (( ((EHC.screenHeight unit) / 7))- 50) )
  , padding (Padding 16 0 16 0)
  ][  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin (MarginTop 10)
      ]([ linearLayout
          [ orientation HORIZONTAL
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER_HORIZONTAL
          , cornerRadius 8.0
          , visibility if state.props.isLocateOnMap then GONE else VISIBLE
          , stroke ("1,"<>Color.grey900)
          , background Color.white900
          ][  editText
              [ height WRAP_CONTENT
              , color Color.black800
              , hint (getString ENTER_A_LOCATION)
              , fontStyle $ FontStyle.semiBold LanguageStyle
              , singleLine true
              , weight 1.0
              , cornerRadius 8.0
              , background Color.white900
              , text state.data.address
              , id (EHC.getNewIDWithTag "SavedLocationEditText")
              , afterRender (\action -> do
                  _ <- push action
                  _ <- if (not state.props.showSavePlaceView) then pure $ JB.requestKeyboardShow $ EHC.getNewIDWithTag "SavedLocationEditText" else (pure (pure unit))
                  pure unit
                  ) (const RenderKeyboardActin)
              , ellipsize true
              , padding (Padding 21 27 16 27)
              , textSize FontSize.a_16
              , lineHeight "24"
              , onChange push AddressChanged
              , hintColor "#A7A7A7"
              ]
        ,linearLayout
          [  height MATCH_PARENT
          , width WRAP_CONTENT
          , gravity CENTER
          , padding (Padding 0 27 16 27)
          , onClick 
            (\action -> do 
                  _ <- push action 
                  _ <- HU.setText' (EHC.getNewIDWithTag "SavedLocationEditText") ("")
                  pure unit 
                  ) (const $ ClearEditText)
          , visibility if state.data.address /= ""  then VISIBLE else GONE
          ][imageView 
            [ height $ V 16
            , width $ V 16
            , imageWithFallback "ny_ic_clear,https://assets.juspay.in/nammayatri/images/user/ny_ic_clear.png"
            ]
          ]
        ]
      , linearLayout[
        orientation HORIZONTAL
        , width MATCH_PARENT
        , height if state.props.isLocateOnMap then (V 72) else (V 0)
        , background Color.white900
        , stroke ("1,"<>Color.grey800)
        , padding (Padding 21 0 16 0)
        , gravity CENTER_VERTICAL
        , cornerRadius 8.0
        , clickable true
        , visibility if state.props.isLocateOnMap then VISIBLE else GONE
      ][  imageView
          [ imageWithFallback "ny_ic_loc_grey,https://assets.juspay.in/nammayatri/images/user/ny_ic_loc_grey.png"
          , height $ V 21
          , width $ V 18
          , margin (MarginRight 11)
          ]
        , textView
        [ color Color.black800
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , singleLine true
        , width MATCH_PARENT
        , height MATCH_PARENT
        , gravity CENTER
        , text state.data.locSelectedFromMap
        , ellipsize true
        , textSize FontSize.a_16
        , lineHeight "24"
        ]]
    , bottomLayout state push 
      
  ]  ) ]

textViews :: forall w. ST.AddNewAddressScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
textViews state push = 
  linearLayout
  [ orientation HORIZONTAL
  , width MATCH_PARENT
  , height $ V 72
  , background Color.white900
  , stroke ("1,"<>Color.grey800)
  , padding (Padding 21 0 16 0)
  , gravity CENTER_VERTICAL
  , cornerRadius 8.0
][  imageView
    [ imageWithFallback "ny_ic_loc_grey,https://assets.juspay.in/nammayatri/images/user/ny_ic_loc_grey.png"
    , height $ V 21
    , width $ V 18
    , margin (MarginRight 11)
    ]
  , textView
  [ color Color.black800
  , fontStyle $ FontStyle.semiBold LanguageStyle
  , singleLine true
  , width MATCH_PARENT
  , text (state.data.selectedItem.description)
  , ellipsize true
  , textSize FontSize.a_16
  , lineHeight "24"
  ]]

bottomLayout :: forall w. ST.AddNewAddressScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bottomLayout state push = 
  linearLayout 
  [ width MATCH_PARENT
  , height $ V (2 * (EHC.screenHeight unit /3))
  , margin (Margin 0 12 0 0)
  , visibility if state.props.isLocateOnMap then GONE else VISIBLE
  , orientation VERTICAL
  ][searchResultsView state push
  , locationUnserviceableView state push 
  ]

searchResultsView :: forall w. ST.AddNewAddressScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
searchResultsView state push = 
  scrollView
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , cornerRadius 8.0
  , padding (PaddingVertical 0 3)
  , stroke "1,#E5E7EB"
  , visibility if state.props.isSearchedLocationServiceable then VISIBLE else GONE
  , background Color.white900
  , scrollBarY false 
  ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , cornerRadius 20.0
        , orientation VERTICAL
        , padding (PaddingBottom 10)
        ](DA.mapWithIndex (\index item -> 
              linearLayout[
                width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              ][  (LocationListItem.view (push <<< LocationListItemAC ) item )
                , linearLayout
                    [ height $ V 1
                    , width MATCH_PARENT
                    , background Color.lightGreyShade
                    ][]
              ]
              )  (if (DA.null state.data.locationList) then (if state.props.selectFromCurrentOrMap then bottomBtnsData else []) else state.data.locationList )) 
  ]

bottomBtnsData :: Array ST.LocationListItemState 
bottomBtnsData = 
  [ { prefixImageUrl : "ny_ic_locate_on_map,https://assets.juspay.in/nammayatri/images/user/ny_ic_locate_on_map.png"
    , title : (getString CHOOSE_ON_MAP) 
    , subTitle :  (getString DRAG_THE_MAP )
    , placeId : Nothing 
    , lat : Nothing
    , lon : Nothing
    , description : ""
    , tag : "Choose_On_Map"
    , postfixImageUrl : ""
    , postfixImageVisibility : false
    , tagType : Just $ show ST.LOCATE_ON_MAP
    , cardType : Nothing 
    , address : ""
    , tagName : ""
    , isEditEnabled : true
    , savedLocation : ""
    , placeName : ""
    , isClickable : true 
    , alpha : 1.0
    , fullAddress : dummyAddress
    , locationItemType : Nothing
    }
  , { prefixImageUrl : "ny_ic_current_location,https://assets.juspay.in/nammayatri/images/user/ny_ic_current_location.png"
    , title :  (getString USE_CURRENT_LOCATION)
    , subTitle : (getString FAVOURITE_YOUR_CURRENT_LOCATION)
    , placeId : Nothing 
    , lat : Nothing
    , lon : Nothing
    , description : ""
    , tag : "Current_Location"
    , postfixImageUrl : ""
    , postfixImageVisibility : false
    , tagType : Just $ show ST.CURR_LOC
    , cardType : Nothing 
    , address : ""
    , tagName : ""
    , isEditEnabled : true
    , savedLocation : ""
    , placeName : ""
    , isClickable : true 
    , alpha : 1.0
    , fullAddress : dummyAddress
    , locationItemType : Nothing
    }

  ]

savePlaceView :: forall w. ST.AddNewAddressScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
savePlaceView state push = 
  relativeLayout[
    height MATCH_PARENT
  , visibility if state.props.showSavePlaceView then VISIBLE else GONE
  , margin (MarginTop (( ((EHC.screenHeight unit) / 7))- 50) )
  , padding (Padding 16 0 16 0)
  , width MATCH_PARENT
  ][  scrollView[
    height WRAP_CONTENT
  , width MATCH_PARENT
  , scrollBarY false
  ][linearLayout
      [ orientation VERTICAL
      , width MATCH_PARENT
      , height WRAP_CONTENT
      , cornerRadius 8.0
      , padding (Padding 16 20 16 20)
      , background Color.white900
      ][  textView
          [ text (getString LOCATION)
          , color Color.black600
          , textSize FontSize.a_12 
          , lineHeight "15"
          , fontStyle $ FontStyle.medium LanguageStyle
          , width MATCH_PARENT
          , margin (MarginBottom 8)
          , gravity LEFT
          ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation HORIZONTAL
          , stroke if ((state.props.tagExists && state.data.existsAs /= "") ||( not state.props.isLocationServiceable ))then ("1,"<>Color.red) else  ("1,"<>Color.grey900)
          , cornerRadius 4.0
          , padding (Padding 16 16 16 16)
          , gravity CENTER_VERTICAL
          , onClick (\action -> do 
                          _ <- push action 
                          _ <- HU.setText' (EHC.getNewIDWithTag "SavedLocationEditText") state.data.address
                          pure unit)
              $ const ChangeAddress
          ][  textView
              [ text (state.data.selectedItem).description 
              , textSize FontSize.a_14
              , fontStyle $ FontStyle.medium LanguageStyle
              , color Color.black600
              , height WRAP_CONTENT
              , gravity CENTER_VERTICAL
              , padding (PaddingRight 8)
              , width $ V (4 * (EHC.screenWidth unit / 5) - 60)
              , maxLines 1
              , ellipsize true 
              ]
            , linearLayout[
              height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity RIGHT 
            ][  textView
                [ text (getString EDIT)
                , color Color.blue900
                , onFocus push $ const $ EditTextFocusChanged 
                , gravity CENTER_VERTICAL
                , textSize FontSize.a_14
                , fontStyle $ FontStyle.medium LanguageStyle
                , lineHeight "18"
                ]]
          ]
         , textView
          [ text if state.props.isLocationServiceable then (getText state) else (getString LOCATION_UNSERVICEABLE)
          , textSize FontSize.a_12 
          , gravity LEFT 
          , margin (MarginTop 4)
          , visibility if ((state.props.tagExists && state.data.existsAs /= "") || (not state.props.isLocationServiceable)) then VISIBLE else GONE 
          , fontStyle $ FontStyle.medium LanguageStyle
          , color Color.red
          ]
        , tagView state push 
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , margin (MarginTop 24)
          , padding (PaddingBottom 2)
          , visibility if state.data.activeIndex == Just 2 then VISIBLE else GONE
          ][ PrimaryEditText.view (push <<< PrimaryEditTextAC ) (primaryEditTextConfig state) ]
        ]]
    , linearLayout
      [ alignParentBottom "true,-1"
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.grey800
      , adjustViewWithKeyboard "true"
      ][  PrimaryButton.view (push <<< PrimaryButtonAC ) (primaryButtonConfig state)]  
      ]

tagView :: forall w. ST.AddNewAddressScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
tagView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin (MarginTop 24)
  , orientation VERTICAL
  ][  textView
      [ text (getString ADD_TAG)
      , color Color.black800
      , textSize FontSize.a_12
      , lineHeight "15"
      , fontStyle $ FontStyle.medium LanguageStyle
      , width MATCH_PARENT
      , margin (MarginBottom 8)
      , gravity LEFT 
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      ](DA.mapWithIndex (\index item -> 
          linearLayout
          [ height $ V 36
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER 
          , padding (Padding 10 10 10 10)
          , background if (Just index) == state.data.activeIndex then Color.catskillWhite else Color.grey800
          , onClick push $ const (TagSelected index)
          , margin (MarginRight 12)
          , cornerRadius 6.0
          , alpha case item.tag of 
                      "FAVOURITE"     -> 1.0
                      _               -> if (validTag state.data.savedTags item.tag state.data.placeName) || (Just index == state.data.activeIndex) then 1.0 else 0.5 
          , stroke if (Just index) == state.data.activeIndex then "1,#0066FF" else "1," <> Color.catskillWhite 
          ][  imageView
              [ imageWithFallback if (Just index) == state.data.activeIndex then item.activeImageUrl else item.inActiveImageUrl
              , width $ V 15
              , height $ V 15
              , margin (MarginRight 8)
              ]
            , textView
              [ text  item.text
              , textSize FontSize.a_12
              , lineHeight "16"
              , height $ V 36
              , gravity CENTER
              , color if (Just index) == state.data.activeIndex then Color.blue900 else Color.black800
              , fontStyle $ FontStyle.medium LanguageStyle
              ]
          ]) [  { activeImageUrl : "ny_ic_home_blue,https://assets.juspay.in/nammayatri/images/user/ny_ic_home_blue.png", inActiveImageUrl : "ny_ic_home,https://assets.juspay.in/nammayatri/images/user/ny_ic_home.png", text : (getString HOME), tag : "HOME"},
                { activeImageUrl : "ny_ic_work_blue,https://assets.juspay.in/nammayatri/images/user/ny_ic_work_blue.png", inActiveImageUrl : "ny_ic_work,https://assets.juspay.in/nammayatri/images/user/ny_ic_work.png", text : (getString WORK), tag : "WORK"},
                { activeImageUrl : "ny_ic_fav_blue,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav_blue.png",inActiveImageUrl : "ny_ic_fav_tag,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav_inactive.png", text : (getString FAVOURITE), tag : "FAVOURITE"}] )

  ]

locationUnserviceableView :: forall w. ST.AddNewAddressScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
locationUnserviceableView state push = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL 
  , clickable true
  , visibility if state.props.isSearchedLocationServiceable then GONE else VISIBLE
  , background "#F5F5F5"
  , gravity CENTER
  ][  imageView 
      [ imageWithFallback "ny_ic_location_unserviceable,https://assets.juspay.in/nammayatri/images/user/ny_ic_location_unserviceable.png"
      , height $ V 99
      , width $ V 133
      , margin $ (MarginBottom 20)
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER
      , margin (MarginBottom 10)
      ][  textView
          [ text (getString LOCATION_UNSERVICEABLE)
          , textSize FontSize.a_18
          , color Color.black800
          , gravity CENTER  
          , fontStyle $ FontStyle.bold LanguageStyle
          ]
        ]
    , linearLayout
      [ width (V (EHC.screenWidth unit - 40 ))
      , height WRAP_CONTENT
      , gravity CENTER
      ][  textView 
          [ text (getString  CURRENTLY_WE_ARE_LIVE_IN_)
          , textSize FontSize.a_14
          , gravity CENTER
          , color Color.black700
          , fontStyle $ FontStyle.regular LanguageStyle
          ]
        ]
  ]

getText ::  ST.AddNewAddressScreenState -> String
getText state = let language = getKey $ JB.getKeyInSharedPrefKeys "LANGUAGE_KEY"
              in case language of
                          EN_US -> ((getString LOCATION_ALREADY_EXISTS_AS) <> " ' " <> state.data.existsAs <> " '")
                          _     -> ((getString LOCATION_ALREADY) <> " ' " <> state.data.existsAs <> " ' " <>(getString EXISTS_AS) )