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
import Common.Types.App
import Debug
import Screens.CustomerUtils.AddNewAddressScreen.ComponentConfig
import Screens.CustomerUtils.AddNewAddressScreen.ComponentConfig

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.LocationListItem (dummyAddress)
import Components.LocationListItem as LocationListItem
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (runEffectFn1, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getAssetLink)
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, not, pure, show, unit, ($), (&&), (*), (-), (/), (/=), (<<<), (<>), (==), (||), (>), void)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..), adjustViewWithKeyboard, afterRender, alignParentBottom, alpha, background, clickable, color, cornerRadius, editText, ellipsize, fontStyle, frameLayout, gravity, height, hint, hintColor, id, imageUrl, imageView, imageWithFallback, lineHeight, linearLayout, margin, maxLines, onBackPressed, onChange, onClick, onFocus, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, accessibilityHint, accessibility)
import Screens.AddNewAddressScreen.Controller (Action(..), ScreenOutput, eval, validTag)
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore)
import Debug (spy)
import Data.String as DS
import Styles.Colors as Color
import Common.Resources.Constants (pickupZoomLevel)
import Locale.Utils
import Mobility.Prelude

screen :: ST.AddNewAddressScreenState -> Screen Action ST.AddNewAddressScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "AddNewAddressScreen"
  , globalEvents : [(\push -> do
                      if initialState.props.isLocateOnMap then do
                        pure (pure unit)
                      else do
                        void $ runEffectFn2 JB.storeCallBackLocateOnMap (\key lat lon -> push $ UpdateLocation key lat lon) (JB.handleLocateOnMapCallback "AddNewAddressScreen")
                        pure (pure unit))]
  , eval : \action state -> do
        let _ = spy "AddNewAddressScreenState action " action
        let _ = spy "AddNewAddressScreenState state " state
        eval action state
  }

view :: forall w . (Action  -> Effect Unit) -> ST.AddNewAddressScreenState -> PrestoDOM (Effect Unit) w
view push state =
  let showLabel = if state.props.defaultPickUpPoint == "" then false else true
  in
  Anim.screenAnimation $
  relativeLayout[
    width MATCH_PARENT
  , height MATCH_PARENT
  , onBackPressed push $ const BackPressed state
  , accessibility DISABLE
  , afterRender
    (\action -> do
          _ <- (JB.showMap (EHC.getNewIDWithTag "AddNewAddressHomeScreenMap") true "satellite" pickupZoomLevel 0.0 0.0 push MAPREADY)
          pure $ HU.setText (EHC.getNewIDWithTag "SavedLocationEditText") (state.data.address)
          pure $ HU.setText (EHC.getNewIDWithTag "SaveAsEditText") (state.data.addressSavedAs)
          -- _ <- runEffectFn1 JB.locateOnMap JB.locateOnMapConfig { goToCurrentLocation = true, lat = 0.0, lon = 0.0, geoJson = "", points = [], zoomLevel = pickupZoomLevel, labelId = EHC.getNewIDWithTag "AddAddressPin"}
          _ <- if (state.data.activeIndex == Just 2 && state.props.showSavePlaceView) then JB.requestKeyboardShow (EHC.getNewIDWithTag ("SaveAsEditText")) else pure unit
          pure unit
          ) (const AfterRender)
  ][
   frameLayout[
     width MATCH_PARENT
    , height MATCH_PARENT
    , clickable true
    , accessibility DISABLE
   ][
      linearLayout
      [  height MATCH_PARENT
      , width MATCH_PARENT
      , margin (Margin 0 EHC.safeMarginTop 0 0)
      , accessibility DISABLE_DESCENDANT
      , id (EHC.getNewIDWithTag "AddNewAddressHomeScreenMap")
      ][]
    , if not state.data.config.feature.enableSpecialPickup then
        linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , background Color.transparent
        , accessibility DISABLE_DESCENDANT
        , padding $ PaddingBottom $ if EHC.os == "IOS" then 40 else 80
        , gravity CENTER
        , orientation VERTICAL
        ][ textView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , background Color.black900
          , color Color.white900
          , text if DS.length state.props.defaultPickUpPoint > state.data.config.mapConfig.labelTextSize then
                      (DS.take (state.data.config.mapConfig.labelTextSize - 3) state.props.defaultPickUpPoint) <> "..."
                    else
                      state.props.defaultPickUpPoint
          , padding (Padding 7 7 7 7)
          , margin (MarginBottom 5)
          , cornerRadius 5.0
          , visibility $ boolToInvisibility showLabel
          , id (EHC.getNewIDWithTag "AddAddressPin")
          ]
        , imageView
          [ width $ V 60
          , height $ V 60 
          , imageWithFallback $ (HU.getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) <> "," <> (getAssetLink FunctionCall) <> "ny_ic_customer_current_location.png"
          ]
        ]
      else
          linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , background Color.transparent
          , accessibility DISABLE_DESCENDANT
          , padding $ PaddingBottom if EHC.os == "IOS" then 70 else 110
          , gravity CENTER
          , orientation VERTICAL
          ][ imageView
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , accessibility DISABLE_DESCENDANT
              , visibility $ boolToVisibility showLabel
              , id (EHC.getNewIDWithTag "AddAddressPin")
              ]
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , background Color.transparent
          , padding $ PaddingBottom if EHC.os == "IOS" then 0 else 46
          , gravity CENTER
          , accessibility DISABLE
          , orientation VERTICAL
          , visibility $ boolToVisibility state.data.config.feature.enableSpecialPickup
          ]
          [ imageView
              [ width $ V 60
              , height $ V 60
              , accessibility DISABLE
              , imageWithFallback $ (HU.getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) <> "," <> (getAssetLink FunctionCall) <> "ny_ic_customer_current_location.png"
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
        , accessibility DISABLE
        , background state.data.config.searchLocationConfig.searchLocationTheme
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
  , accessibility DISABLE_DESCENDANT
  , gravity RIGHT
  ][  imageView
      [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_recenter_btn"
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
    , accessibility DISABLE_DESCENDANT
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
        , width MATCH_PARENT
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
                                HU.getLocationName push 9.9 9.9 "Current Location" SelectedCurrentLocation
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
btnData state = [ {text : (getString SELECT_ON_MAP), imageUrl : fetchImage FF_ASSET "ny_ic_locate_on_map", action : SetLocationOnMap, tag : "LOCATE_ON_MAP"}
                  -- ,{text : (getString CURRENT_LOCATION), imageUrl : "ny_ic_current_location," <> (getAssetLink FunctionCall) <> "ny_ic_current_location.png", action : CurrentLocationAction, tag : "CURRENT_LOCATION"}
                  ]

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
          ][  editText $
              [ height WRAP_CONTENT
              , color Color.black800
              , hint (getString ENTER_A_LOCATION)
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
              , lineHeight "24"
              , onChange push AddressChanged
              , hintColor Color.black600
              ] <> FontStyle.subHeading1 LanguageStyle
        ,linearLayout
          [  height MATCH_PARENT
          , width WRAP_CONTENT
          , gravity CENTER
          , padding (Padding 0 27 16 27)
          , onClick
            (\action -> do
                  _ <- push action
                  _ <- pure $ HU.setText (EHC.getNewIDWithTag "SavedLocationEditText") ("")
                  pure unit
                  ) (const $ ClearEditText)
          , visibility if state.data.address /= ""  then VISIBLE else GONE
          ][imageView
            [ height $ V 16
            , width $ V 16
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_clear"
            , accessibilityHint "Clear Text : Button"
            , accessibility ENABLE
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
          [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_loc_grey"
          , height $ V 21
          , width $ V 18
          , margin (MarginRight 11)
          ]
        , textView $
        [ color Color.black800
        , singleLine true
        , width MATCH_PARENT
        , height MATCH_PARENT
        , gravity CENTER
        , text state.data.locSelectedFromMap
        , ellipsize true
        ] <> FontStyle.subHeading1 LanguageStyle]
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
    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_loc_grey"
    , height $ V 21
    , width $ V 18
    , margin (MarginRight 11)
    ]
  , textView $
  [ color Color.black800
  , singleLine true
  , width MATCH_PARENT
  , text (state.data.selectedItem.description)
  , ellipsize true
  ] <> FontStyle.subHeading1 LanguageStyle]

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
  , stroke $ "1," <> Color.grey900
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
              ][  (LocationListItem.view (push <<< LocationListItemAC ) item false)
                , linearLayout
                    [ height $ V 1
                    , width MATCH_PARENT
                    , background Color.lightGreyShade
                    ][]
              ]
              )  (if (DA.null state.data.locationList) then (if state.props.selectFromCurrentOrMap then bottomBtnsData state else []) else state.data.locationList ))
  ]

bottomBtnsData :: ST.AddNewAddressScreenState ->  Array ST.LocationListItemState 
bottomBtnsData state = 
  [ { prefixImageUrl : fetchImage FF_ASSET "ny_ic_locate_on_map"
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
    , distance : Nothing
    , showDistance : Just false
    , actualDistance : Nothing
    , frequencyCount : Nothing
    , recencyDate : Nothing
    , locationScore : Nothing
    , dynamicAction : Nothing
    , types : Nothing
    }
  , { prefixImageUrl : fetchImage FF_ASSET "ny_ic_current_location"
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
    , distance : Nothing
    , showDistance : Just false
    , actualDistance : Nothing
    , frequencyCount : Nothing
  , recencyDate : Nothing
  , locationScore : Nothing
  , dynamicAction : Nothing
  , types : Nothing
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
      ][  textView $
          [ text (getString LOCATION)
          , color Color.black600
          , width MATCH_PARENT
          , margin (MarginBottom 8)
          , gravity LEFT
          ] <> FontStyle.tags LanguageStyle
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
                          _ <- pure $ HU.setText (EHC.getNewIDWithTag "SavedLocationEditText") state.data.address
                          pure unit)
              $ const ChangeAddress
          ][  textView $ 
              [ text (state.data.selectedItem).description
              , color Color.black600
              , height WRAP_CONTENT
              , gravity CENTER_VERTICAL
              , padding (PaddingRight 8)
              , maxLines 1
              , ellipsize true 
              ] <> FontStyle.body1 LanguageStyle
                <> (if EHC.os == "IOS" then [width $ V (4 * (EHC.screenWidth unit / 5) - 75)] else [weight 1.0])
            , linearLayout(
              [
                height WRAP_CONTENT
              , gravity RIGHT 
              ] <> if EHC.os == "IOS" then [weight 1.0] else [width WRAP_CONTENT])
              [  textView $
                [ text (getString EDIT)
                , accessibilityHint "Edit : Button"
                , accessibility ENABLE
                , color Color.blue900
                , width MATCH_PARENT
                , onFocus push $ const $ EditTextFocusChanged
                , gravity CENTER_VERTICAL 
                ] <> FontStyle.body1 LanguageStyle
              ]
          ]
         , textView $
          [ text if state.props.isLocationServiceable then (getText state) else (getString LOCATION_UNSERVICEABLE)
          , gravity LEFT
          , margin (MarginTop 4)
          , visibility if ((state.props.tagExists && state.data.existsAs /= "") || (not state.props.isLocationServiceable)) then VISIBLE else GONE
          , color Color.red
          ] <> FontStyle.body3 TypoGraphy
        , tagView state push
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , margin (MarginTop 24)
          , padding (PaddingBottom 2)
          ][PrimaryEditText.view (push <<< PrimaryEditTextAC ) (primaryEditTextConfig state)]
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
  ][  textView $
      [ text (getString ADD_TAG)
      , color Color.black800
      , width MATCH_PARENT
      , margin (MarginBottom 8)
      , gravity LEFT
      ] <> FontStyle.tags LanguageStyle
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      ](DA.mapWithIndex (\index item ->
          linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER
          , padding (Padding 10 10 10 10)
          , background if (Just index) == state.data.activeIndex then Color.catskillWhite else Color.grey800
          , onClick push $ const (TagSelected index)
          , margin (MarginRight 12)
          , cornerRadius 6.0
          , accessibility ENABLE
          , accessibilityHint if (Just index) == state.data.activeIndex then (item.text <> ":  Selected") else if (validTag state.data.savedTags item.tag state.data.placeName) then (item.text <> ": Un Selected") else (item.text <> " : Already Saved")
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
            , textView $
              [ text  item.text
              , gravity CENTER
              , color if (Just index) == state.data.activeIndex then Color.blue900 else Color.black800
              ] <> FontStyle.tags LanguageStyle
          ]) [  { activeImageUrl : fetchImage FF_ASSET "ny_ic_home_blue", inActiveImageUrl : fetchImage FF_ASSET "ny_ic_home", text : (getString HOME), tag : "HOME"},
                { activeImageUrl : fetchImage FF_ASSET "ny_ic_work_blue", inActiveImageUrl : fetchImage FF_ASSET "ny_ic_work", text : (getString WORK), tag : "WORK"},
                { activeImageUrl : fetchImage FF_ASSET "ny_ic_fav_blue",inActiveImageUrl : fetchImage FF_ASSET "ny_ic_fav_tag", text : (getString FAVOURITE), tag : "FAVOURITE"}] )

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
      [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_location_unserviceable"
      , height $ V 99
      , width $ V 133
      , margin $ (MarginBottom 20)
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER
      , margin (MarginBottom 10)
      ][  textView $
          [ text (getString LOCATION_UNSERVICEABLE)
          , color Color.black800
          , gravity CENTER
          ] <> FontStyle.h2 LanguageStyle
        ]
    , linearLayout
      [ width (V (EHC.screenWidth unit - 40 ))
      , height WRAP_CONTENT
      , gravity CENTER
      ][  textView $
          [ text $ getString $ CURRENTLY_WE_ARE_LIVE_IN_ "CURRENTLY_WE_ARE_LIVE_IN_"
          , gravity CENTER
          , color Color.black700
          ] <> FontStyle.paragraphText LanguageStyle
        ]
  ]

getText ::  ST.AddNewAddressScreenState -> String
getText state = case (getLanguageLocale languageKey) of
                  "EN_US" -> ((getString LOCATION_ALREADY_EXISTS_AS) <> " ' " <> state.data.existsAs <> " '")
                  _     -> ((getString LOCATION_ALREADY) <> " ' " <> state.data.existsAs <> " ' " <>(getString EXISTS_AS) )
