{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverSavedLocationScreen.View where

import Data.Maybe (Maybe(..), isJust)
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GoToLocationModal as GoToLocationModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Data.Array (length)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Int (toNumber)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons (flowRunner, liftFlow, parseFloat)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils (loaderText, toggleLoader)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, map, pure, show, unit, void, ($), (-), (<<<), (<>), (==), (>), (/), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), ScopedScreen, afterRender, background, color, cornerRadius, editText, ellipsize, fontStyle, frameLayout, gravity, height, hintColor, id, imageView, imageWithFallback, lineHeight, linearLayout, margin, onAnimationEnd, onBackPressed, onChange, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Screens.DriverSavedLocationScreen.ComponentConfig (confirmDeletePopupConfig, locationListItemConfig, primaryButtonConfig)
import Screens.DriverSavedLocationScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.DriverSavedLocationScreen.Transformer (tagAlreadySaved)
import Screens.Types (DriverSavedLocationScreenState, GoToScrEntryType(..), PredictionItem, SavedLocationScreenType(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (defaultGlobalState)

screen :: DriverSavedLocationScreenState -> ScopedScreen Action DriverSavedLocationScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "DriverSavedLocationScreen"
  , globalEvents:
      [ ( \push -> do
            if initialState.props.viewType == LOCATE_ON_MAP then
              JB.storeCallBackLocateOnMap push UpdateLocation
            else if initialState.props.viewType == GoToList then do
              void $ launchAff $ flowRunner defaultGlobalState
                $ do
                    void $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
                    toggleLoader true
                    resp <- Remote.getDriverHomeLocation ""
                    case resp of
                      Right response -> liftFlow $ push $ Respones response
                      Left error -> liftFlow $ push $ Error error
                    toggleLoader false
            else
              pure unit
            pure $ pure unit
        )
      ]
  , eval:
      ( \action state -> do
          let
            _ = spy "DriverSavedLocationScreen ----- state" state
          let
            _ = spy "DriverSavedLocationScreen --------action" action
          eval action state
      )
  , parent: Nothing
  }

view :: forall w. (Action -> Effect Unit) -> DriverSavedLocationScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push $ const BackPressed
        , afterRender push $ const AfterRender
        , background Color.white900
        ]
        [ if (state.props.viewType == GoToList) then
            gotoLocationsList state push (state.props.viewType == GoToList)
          else if (state.props.viewType == SearchLocation) then
            searchLocation state push (state.props.viewType == SearchLocation)
          else if (state.props.viewType == ConfirmLocation) then confirmLocation state push (state.props.viewType == ConfirmLocation) else linearLayout [] []
        , locationAndMap state push (state.props.viewType == LOCATE_ON_MAP)
        , confirmDeletePopup state push
        ]

confirmDeletePopup :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
confirmDeletePopup state push =
  PrestoAnim.animationSet [ Anim.fadeIn state.props.confirmDelete ]
    $ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , background Color.lightBlack900
        , visibility if state.props.confirmDelete then VISIBLE else GONE
        ]
        [ PopUpModal.view (push <<< PopUpModalAction) (confirmDeletePopupConfig state) ]

gotoLocationsList :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> Boolean -> PrestoDOM (Effect Unit) w
gotoLocationsList state push visibility' =
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ]
    $ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        , onAnimationEnd push $ const OnAnimationEnd
        , visibility if visibility' then VISIBLE else GONE
        , margin $ MarginTop 15
        ]
        [ header push
        , linearLayout
            [ width MATCH_PARENT
            , height $ V 1
            , background Color.grey900
            ]
            []
        , if DA.null state.data.savedLocationsArray then noSavedLocationView push state else savedLocationListView push state
        , linearLayout
            [ width MATCH_PARENT
            , orientation VERTICAL
            , gravity BOTTOM
            , margin $ Margin 16 15 16 24
            ]
            [ textView
                $ [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , gravity CENTER
                  , text $ (getString GOTO_LOC_LEFT) <> (show $ state.data.maxGotoLocations - length state.data.savedLocationsArray)
                  , color Color.black700
                  , padding $ Padding 10 10 10 10
                  , cornerRadius 12.0
                  , background Color.blue600
                  ]
                <> FontStyle.subHeading1 TypoGraphy
            , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
            ]
        ]

searchLocation :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> Boolean -> PrestoDOM (Effect Unit) w
searchLocation state push visibility' =
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ]
    $ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , visibility if visibility' then VISIBLE else GONE
        , onAnimationEnd push $ const OnAnimationEnd
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , onAnimationEnd push $ const OnAnimationEnd
            , background Color.black900
            , padding $ PaddingVertical 40 25
            , gravity CENTER_VERTICAL
            ]
            [ imageView
                [ height $ V 24
                , width $ V 24
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left_white"
                , margin $ MarginLeft 10
                , onClick push $ const BackPressed
                ]
            , imageView
                [ height $ V 32
                , width $ V 32
                , margin $ MarginLeft 10
                , imageWithFallback $ fetchImage FF_ASSET "ny_pin_check_white"
                ]
            , linearLayout
                [ weight 1.0
                , height WRAP_CONTENT
                , gravity CENTER_VERTICAL
                , margin $ MarginHorizontal 10 10
                , orientation VERTICAL
                , gravity CENTER_VERTICAL
                ]
                [ linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , gravity CENTER_VERTICAL
                    ]
                    [ editText
                        [ height WRAP_CONTENT
                        , color Color.white900
                        , fontStyle $ FontStyle.semiBold LanguageStyle
                        , singleLine true
                        , weight 1.0
                        , cornerRadius 8.0
                        , text state.data.address
                        , id $ EHC.getNewIDWithTag "SavedLocationEditText"
                        , afterRender push $ const OnAnimationEnd
                        , ellipsize true
                        , padding $ Padding 12 4 4 4
                        , textSize FontSize.a_16
                        , lineHeight "24"
                        , hintColor Color.grey700
                        , onChange
                            ( \action -> do
                                _ <- JB.debounceFunction 800 push DebounceCallback false
                                _ <- push action
                                pure unit
                            )
                            OnTextChanged
                        ]
                    , imageView
                        [ height $ V 22
                        , width $ V 22
                        , imageWithFallback $ fetchImage FF_ASSET "ic_cancel_unfilled"
                        , padding $ Padding 4 4 4 4
                        , onClick push $ const ClearSearch
                        ]
                    ]
                , linearLayout
                    [ width MATCH_PARENT
                    , height $ V 1
                    , background Color.grey900
                    ]
                    []
                ]
            ]
        , suggestionsListView state push
        , bottomButtons state push
        ]

bottomButtons :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bottomButtons state push =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , gravity BOTTOM
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.grey900
        ]
        []
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , background Color.white900
        , padding $ Padding 20 20 20 20
        , gravity CENTER
        ]
        [ linearLayout
            [ weight 1.0
            , height WRAP_CONTENT
            , gravity CENTER
            , onClick push $ const LocateOnMap
            ]
            [ imageView
                [ height $ V 18
                , width $ V 18
                , imageWithFallback $ fetchImage FF_ASSET "ic_location_marker"
                ]
            , textView
                $ [ height MATCH_PARENT
                  , color Color.black800
                  , text $ getString SELECT_ON_MAP
                  , margin $ MarginHorizontal 8 13
                  ]
                <> FontStyle.body1 TypoGraphy
            ]
        , linearLayout
            [ height MATCH_PARENT
            , width $ V 1
            , background Color.black800
            , margin $ MarginHorizontal 10 10
            ]
            []
        , linearLayout
            [ weight 1.0
            , height WRAP_CONTENT
            , gravity CENTER
            , onClick push $ const LocateOnMap
            ]
            [ imageView
                [ height $ V 18
                , width $ V 18
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_fill"
                ]
            , textView
                $ [ height MATCH_PARENT
                  , color Color.black800
                  , text $ getString CURRENT_LOCATION
                  , margin $ MarginHorizontal 8 13
                  ]
                <> FontStyle.body1 TypoGraphy
            ]
        ]
    ]

suggestionsListView :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
suggestionsListView state push =
  scrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 8.0
    , stroke "1,#E5E7EB"
    , background Color.white900
    , scrollBarY false
    , margin $ Margin 10 10 10 10
    , visibility if length state.data.predictions > 0 then VISIBLE else GONE
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , cornerRadius 20.0
        , orientation VERTICAL
        , padding $ PaddingBottom 10
        ]
        ( DA.mapWithIndex
            ( \index item ->
                linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  , onClick push $ const $ SuggestionClick item
                  , margin $ MarginTop 10
                  ]
                  [ perdictionItem item push
                  , linearLayout
                      [ height $ V 1
                      , width MATCH_PARENT
                      , margin $ MarginTop 10
                      , background Color.lightGreyShade
                      , visibility if index == (length state.data.predictions) - 1 then GONE else VISIBLE
                      ]
                      []
                  ]
            )
            state.data.predictions
        )
    ]

perdictionItem :: forall w. PredictionItem -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
perdictionItem prediction push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ Padding 10 10 10 10
    , onClick push $ const $ SuggestionClick prediction
    ]
    [ linearLayout
        [ width $ V 50
        , height WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER
        ]
        [ imageView
            [ height $ V 14
            , width $ V 16
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_loc_grey"
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , color Color.black700
              , margin $ MarginTop 3
              , ellipsize true
              , singleLine true
              ]
            <> FontStyle.body3 TypoGraphy
            <> case prediction.distance of
                Just dist -> [ text $ (parseFloat (toNumber dist / 1000.0) 2) <> " km" ]
                Nothing -> [ visibility GONE ]
        ]
    , linearLayout
        [ weight 1.0
        , height WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER_VERTICAL
        , margin $ MarginLeft 15
        ]
        [ textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text prediction.title
              , color Color.black800
              , singleLine true
              , ellipsize true
              ]
            <> FontStyle.body1 TypoGraphy
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text prediction.description
              , color Color.black700
              , singleLine true
              , ellipsize true
              ]
            <> FontStyle.body3 TypoGraphy
        ]
    ]

locationAndMap :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> Boolean -> PrestoDOM (Effect Unit) w
locationAndMap state push visibility' =
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ]
    $ frameLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        , onAnimationEnd push $ const OnAnimationEnd
        , visibility if visibility' then VISIBLE else GONE
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , id (EHC.getNewIDWithTag "DriverSavedLoc")
            , afterRender
                ( \action -> do
                    _ <- (JB.showMap (EHC.getNewIDWithTag "DriverSavedLoc") true "satellite" (19.0) push MAPREADY)
                    pure unit
                )
                (const AfterRender)
            ]
            []
        , locateOnMap state push
        ]

locateOnMap :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
locateOnMap state push =
  frameLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity CENTER
        , orientation VERTICAL
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text $ getString DRAG_TO_ADJUST
              , color Color.black800
              , cornerRadius 4.0
              , padding $ Padding 8 2 8 5
              , background Color.white900
              , stroke $ "1," <> Color.grey900
              , gravity CENTER
              ]
            <> FontStyle.body1 TypoGraphy
        , imageView
            [ width $ V 35
            , height $ V 35
            , imageWithFallback $ fetchImage FF_COMMON_ASSET"ny_ic_src_marker"
            ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , padding $ Padding 8 8 8 8
        , stroke $ "1," <> Color.grey900
        , background Color.white900
        , margin $ Margin 16 32 0 0
        , cornerRadius 32.0
        ]
        [ imageView
            [ height $ V 24
            , width $ V 24
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
            , onClick push $ const BackPressed
            ]
        ]
    , linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity BOTTOM
        , weight 1.0
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , orientation VERTICAL
            , gravity BOTTOM
            , weight 1.0
            , background Color.white900
            , cornerRadius 12.0
            , stroke $ "1," <> Color.grey900
            ]
            [ textView
                $ [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , margin $ Margin 16 20 16 20
                  , text $ getString CONF_GOTO_LOC
                  , color Color.black800
                  , gravity CENTER
                  ]
                <> FontStyle.h1 TypoGraphy
            , linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation HORIZONTAL
                , stroke $ "1," <> Color.grey900
                , padding $ Padding 0 10 15 10
                , margin $ Margin 16 5 16 20
                , cornerRadius 6.0
                , gravity CENTER_VERTICAL
                ]
                [ imageView
                    [ height $ V 28
                    , width $ V 28
                    , imageWithFallback $ fetchImage FF_ASSET "ny_ic_ellipse"
                    ]
                , textView
                    $ [ height WRAP_CONTENT
                      , color Color.black800
                      , fontStyle $ FontStyle.semiBold LanguageStyle
                      , singleLine true
                      , margin $ MarginLeft 10
                      , text state.data.saveLocationObject.address
                      ]
                    <> FontStyle.subHeading1 TypoGraphy
                ]
            , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , margin $ Margin 16 0 16 24
                ]
                [ PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state) ]
            ]
        ]
    ]

confirmLocationBG :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
confirmLocationBG state push =
  linearLayout
    [ height $ V 150
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding $ Padding 10 20 10 20
    , background Color.black900
    ]
    [ imageView
        [ height $ V 18
        , width $ V 18
        , margin $ MarginTop 5
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close_white"
        , onClick push $ const BackPressed
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation VERTICAL
        , margin $ MarginLeft 20
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width MATCH_PARENT
              , text $ getString GOTO_LOCS
              , gravity CENTER
              , color Color.white900
              ]
            <> FontStyle.h3 TypoGraphy
        ]
    ]

confirmLocation :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> Boolean -> PrestoDOM (Effect Unit) w
confirmLocation state push visibility' =
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ]
    $ frameLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , onAnimationEnd push $ const OnAnimationEnd
        , visibility if visibility' then VISIBLE else GONE
        ]
        [ linearLayout
            [ height $ V 130
            , width MATCH_PARENT
            ]
            [ confirmLocationBG state push ]
        , linearLayout
            [ orientation VERTICAL
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , margin $ Margin 16 65 16 0
            , padding $ Padding 16 20 16 20
            , cornerRadius 8.0
            , background Color.white900
            , stroke $ "1," <> Color.grey900
            ]
            [ textView
                $ [ text $ getString LOCATION_STR
                  , color Color.black900
                  , lineHeight "15"
                  , fontStyle $ FontStyle.medium LanguageStyle
                  , width MATCH_PARENT
                  , gravity LEFT
                  ]
                <> FontStyle.tags TypoGraphy
            , linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation HORIZONTAL
                , stroke $ "1," <> Color.grey900
                , padding $ Padding 5 0 15 0
                , margin $ MarginTop 5
                , cornerRadius 6.0
                ]
                [ textView
                    $ [ height WRAP_CONTENT
                      , color Color.black600
                      , text state.data.saveLocationObject.address
                      , weight 1.0
                      , margin $ MarginLeft 5
                      , padding $ Padding 5 10 15 10
                      , singleLine true
                      , ellipsize true
                      , textSize FontSize.a_16
                      ]
                    <> FontStyle.body1 TypoGraphy
                , textView
                    $ [ text $ getString EDIT
                      , color Color.blue900
                      , textSize FontSize.a_16
                      , onClick push $ const BackPressed
                      , visibility if state.props.fromEditButton == (Just FromEdit) then GONE else VISIBLE
                      ]
                    <> FontStyle.body1 TypoGraphy
                ]
            , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , margin $ MarginTop 20
                ]
                [ textView
                    $ [ textFromHtml $ getString ADD_TAG <> " <small>" <> getString ONLY_ONE_LOC_CAN_ADDED <> "</small>"
                      , color Color.black900
                      , width WRAP_CONTENT
                      ]
                    <> FontStyle.tags TypoGraphy
                ]
            , linearLayout
                [ height MATCH_PARENT
                , orientation HORIZONTAL
                , margin $ Margin 0 10 0 15
                , background if isActive then Color.blue600 else Color.grey800
                , stroke $ "1," <> if isActive then Color.blue900 else Color.grey800
                , padding $ Padding 10 10 10 10
                , cornerRadius 8.0
                , gravity CENTER
                ]
                [ imageView
                    [ height $ V 13
                    , width $ V 13
                    , imageWithFallback $ fetchImage FF_ASSET $ if isActive then "ny_ic_home_blue" else "ny_ic_home"
                    ]
                , textView
                    $ [ height WRAP_CONTENT
                      , width WRAP_CONTENT
                      , text $ getString HOME
                      , margin $ MarginLeft 10
                      , color if isActive then Color.blue900 else Color.black800
                      ]
                    <> FontStyle.tags TypoGraphy
                ]
            , textView
                $ [ text $ getString SAVE_AS
                  , color Color.black900
                  , width MATCH_PARENT
                  ]
                <> FontStyle.tags TypoGraphy
            , linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation HORIZONTAL
                , stroke $ "1," <> if isJust state.props.errorText then Color.red900 else Color.grey900
                , margin $ MarginTop 5
                , padding $ PaddingHorizontal 5 5
                , cornerRadius 6.0
                ]
                [ editText
                    $ [ height WRAP_CONTENT
                      , color Color.black800
                      , singleLine true
                      , weight 1.0
                      , cornerRadius 8.0
                      , background Color.white900
                      , text state.data.address
                      , id $ EHC.getNewIDWithTag "ConfirmLocEDT"
                      , ellipsize true
                      , padding $ Padding 10 8 10 8
                      , onChange push ConfirmLocEDT
                      , afterRender push $ const OnAnimationEnd
                      ]
                    <> FontStyle.body1 TypoGraphy
                ]
            , textView
                $ [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , padding $ Padding 5 5 5 5
                  , margin $ MarginTop 5
                  , color Color.red900
                  ]
                <> FontStyle.tags TypoGraphy
                <> case state.props.errorText of
                    Just txt -> [ text txt ]
                    Nothing -> [ visibility GONE ]
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation VERTICAL
            , gravity BOTTOM
            , weight 1.0
            , margin $ Margin 16 0 16 24
            ]
            [ PrimaryButton.view (push <<< ConfirmChangesAC) (primaryButtonConfig state) ]
        ]
  where
  isActive = not (tagAlreadySaved state.data.savedLocationsArray state.props.defTag)

header :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
header push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin $ Margin 10 10 10 10
    , background Color.white900
    , gravity CENTER_VERTICAL
    ]
    [ imageView
        [ height $ V 32
        , width $ V 32
        , padding $ Padding 4 4 4 4
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
        , onClick push $ const BackPressed
        ]
    , textView
        $ [ height WRAP_CONTENT
          , width MATCH_PARENT
          , text $ getString ADD_A_GOTO_LOC
          , gravity CENTER_VERTICAL
          , color Color.black900
          , margin $ MarginLeft 15
          ]
        <> FontStyle.subHeading1 TypoGraphy
    ]

savedLocationListView :: forall w. (Action -> Effect Unit) -> DriverSavedLocationScreenState -> PrestoDOM (Effect Unit) w
savedLocationListView push state =
  PrestoAnim.animationSet [ Anim.fadeIn $ not DA.null state.data.savedLocationsArray ]
    $ scrollView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , scrollBarY true
        , weight 1.0
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            ]
            (map (\item -> GoToLocationModal.view (push <<< GoToLocationModalAC) (locationListItemConfig item)) state.data.savedLocationsArray)
        ]

noSavedLocationView :: forall w. (Action -> Effect Unit) -> DriverSavedLocationScreenState -> PrestoDOM (Effect Unit) w
noSavedLocationView push state =
  PrestoAnim.animationSet [ Anim.fadeIn $ DA.null state.data.savedLocationsArray ]
    $ linearLayout
        [ width MATCH_PARENT
        , weight 1.0
        , orientation VERTICAL
        , gravity CENTER
        , visibility if DA.null state.data.savedLocationsArray then VISIBLE else GONE
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation VERTICAL
            , gravity CENTER
            , margin $ MarginTop 100
            ]
            [ imageView
                [ height $ V 240
                , width $ V 240
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_no_goto_loc"
                , margin $ Margin 10 20 10 0
                ]
            , textView
                $ [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , text $ getString NO_GOTO_LOC_ADDED
                  , gravity CENTER
                  , color Color.black900
                  , margin $ Margin 20 12 20 0
                  , fontStyle $ FontStyle.semiBold LanguageStyle
                  ]
                <> FontStyle.h2 TypoGraphy
            , textView
                $ [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , text $ getString GOTO_LOC_HELPS_YOU
                  , gravity CENTER
                  , color Color.black700
                  , margin $ Margin 20 12 20 0
                  ]
                <> FontStyle.paragraphText TypoGraphy
            ]
        ]
