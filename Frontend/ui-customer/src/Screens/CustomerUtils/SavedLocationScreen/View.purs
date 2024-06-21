{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTIEHULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SavedLocationScreen.View where

import Common.Types.App
import Screens.CustomerUtils.SavedLocationScreen.ComponentConfig
import Animation as Anim
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.SavedLocationCard as SavedLocationCard
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Either (Either(..))
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String as DS
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils as HU
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), (<<<), (/=), const, map, pure, unit, discard, bind, not, void, show, (<>), (==), (&&), (*), (/), (||), (&&))
import Presto.Core.Types.Language.Flow (Flow, doAff, getState)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..), alignParentBottom, background, color, fontStyle, frameLayout, gravity, height, linearLayout, onBackPressed, orientation, padding, relativeLayout, scrollBarY, scrollView, text, textSize, textView, visibility, width, relativeLayout, alignParentRight, margin, stroke, onClick, cornerRadius, afterRender, accessibility, maxLines, imageView, imageWithFallback, weight)
import Screens.SavedLocationScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Services.API (SavedLocationReq(..), SavedLocationsListRes(..), GetFavouriteDriverListRes(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (GlobalState(..), defaultGlobalState, FlowBT)
import Services.FlowCache as FlowCache
import Engineering.Helpers.BackTrack
import Data.Int (fromNumber)

screen :: ST.SavedLocationScreenState -> GlobalState -> Screen Action ST.SavedLocationScreenState ScreenOutput
screen initialState st =
  { initialState
  , view
  , name : "SavedLocationScreen"
  , globalEvents : [
      (\push -> do
        void $ pure $ spy "Hello i am in global Events" 
        _ <- launchAff $ EHC.flowRunner st $ runExceptT $ runBackT $ getFavouriteDriverList GetFavouriteDriversListAPIResponseAction push initialState
        _ <- launchAff $ EHC.flowRunner st $ runExceptT $ runBackT $ getSavedLocationsList SavedLocationListAPIResponseAction push initialState
        pure $ pure unit
          )
  ]
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> ST.SavedLocationScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $ relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , afterRender (\action -> do
                    _ <- push action
                    pure unit
                    ) (const AfterRender)
  , orientation VERTICAL
  , padding $ Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 24 else EHC.safeMarginBottom)
  , onBackPressed push $ const BackPressed state.props.showDeleteLocationModel
  ][
    linearLayout
      [
        height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][
      GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
    , if (not state.data.config.nyBrandingVisibility) then 
        linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.greySmoke
        ][]
      else
        linearLayout[][] 
    , tabView push state ]
  , favDriverView push state
  , favouriteLocationView push state
  , linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.lightBlack900
    , visibility if (state.props.showDeleteLocationModel) then VISIBLE else GONE
    ][ PopUpModal.view (push <<<  PopUpModalAction) (requestDeletePopUp state )]]


tabView :: forall w. (Action -> Effect Unit) -> ST.SavedLocationScreenState -> PrestoDOM (Effect Unit) w
tabView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 24.0
    , background Color.blue600
    , padding $ Padding 4 4 4 4
    , margin $ Margin 16 24 16 24
    , gravity CENTER
    , visibility VISIBLE
    ]
    [ tabItem push state "Drivers" "DRIVERS"
    , tabItem push state "Locations" "LOCATIONS"
    ]

tabItem :: forall w. (Action -> Effect Unit) -> ST.SavedLocationScreenState -> String -> String -> PrestoDOM (Effect Unit) w
tabItem push state text' scr =
  let
    imageHeight = V 16 

    imageWidth = V 16 
  in
    linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , padding $ PaddingVertical 0 4
      , weight 1.0
      , background if (state.data.current == "Drivers" && "DRIVERS" == scr) || (state.data.current == "Locations" && "LOCATIONS" == scr) then Color.black900 else Color.blue600 
      , gravity CENTER
      , cornerRadius 24.0
      , onClick push $ const $ ChangeView if state.data.current == "Drivers" then "Locations" else "Drivers"
      ]
      [ textView
          $ [ height WRAP_CONTENT
            , text $ text'
            , color if (state.data.current == "Drivers" && "DRIVERS" == scr) || (state.data.current == "Locations" && "LOCATIONS" == scr) then Color.white900 else Color.black700 
            , padding $ PaddingBottom 3
            , margin $ MarginTop 4 
            ]
          <> FontStyle.tags TypoGraphy
      ]

favDriverView :: forall w. (Action -> Effect Unit) -> ST.SavedLocationScreenState -> PrestoDOM (Effect Unit) w
favDriverView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 150
    ][
      linearLayout
      [
          height WRAP_CONTENT
        , width WRAP_CONTENT
        , visibility if state.data.current == "Drivers" then VISIBLE else GONE
      ][
        textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity LEFT
          , text "This list includes all the drivers you've marked as favourites."
          , color Color.black700
          , margin $ Margin 15 0 15 0
          , textSize FontSize.a_17
          , maxLines 2
          ]
      ] 
    , frameLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , visibility if state.data.current == "Drivers" then VISIBLE else GONE
    ][  linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ][  linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ][ 
              favouriteDriverViews push state
            ]
          ]
      ]
    ]

favouriteLocationView :: forall w. (Action -> Effect Unit) -> ST.SavedLocationScreenState -> PrestoDOM (Effect Unit) w
favouriteLocationView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 130
    , accessibility if (state.props.showDeleteLocationModel) then DISABLE_DESCENDANT else DISABLE 
    , visibility if state.data.current == "Locations" then VISIBLE else GONE
    ][
    frameLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ][  relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ][  linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ][ savedLocationsView push state
              ]
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , background Color.white900
            , alignParentBottom "true,-1"
            , visibility if (DA.length state.data.savedLocations )/= 0 && state.props.apiRespReceived then VISIBLE else GONE
            ][  PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state) ]
          ]
      , linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , visibility if (DA.length state.data.savedLocations )== 0 && state.props.apiRespReceived then VISIBLE else GONE
        ][  ErrorModal.view (push <<< ErrorModalAC) (errorModalConfig state )]
      ]
    ]

savedLocationsView :: forall w.(Action -> Effect Unit) -> ST.SavedLocationScreenState -> PrestoDOM (Effect Unit) w
savedLocationsView push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , padding (PaddingBottom 85)
  ][  scrollView
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , scrollBarY true
      ][linearLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][linearLayout
          [ width MATCH_PARENT
          , orientation VERTICAL
          , margin (MarginTop 8)
          , height WRAP_CONTENT
          ](map (\item -> SavedLocationCard.view (push <<< SavedLocationCardAction)({
                cardType : Just $ show $ case (DS.toLower item.tag) of
                              "home" -> ST.HOME_TAG
                              "work" -> ST.WORK_TAG
                              _      -> ST.OTHER_TAG
              , tagName : item.tag
              , savedLocation : item.address
              , lat : item.lat
              , lon : item.lon
              , isEditEnabled : true
              , address : item.address
              , prefixImageUrl : ""
              , postfixImageUrl : ""
              , postfixImageVisibility : false
              , title : ""
              , subTitle : ""
              , placeId : item.placeId
              , description : ""
              , tag : ""
              , tagType : Nothing
              , placeName : ""
              , isClickable : true
              , alpha : 1.0
              , fullAddress : item.fullAddress
              , locationItemType : Just ST.SAVED_LOCATION
              , distance : Nothing
              , showDistance : Just false
              , actualDistance : Nothing
              , frequencyCount : Nothing
              , recencyDate : Nothing
              , locationScore : Nothing
            }))state.data.savedLocations)
        , linearLayout
          [ height $ V 100
          , width MATCH_PARENT
          ][]
        ]]
    ]

getSavedLocationsList :: forall action. (SavedLocationsListRes -> action) -> (action -> Effect Unit) -> ST.SavedLocationScreenState -> FlowBT String Unit
getSavedLocationsList action push state = do
  void $ lift $ lift $ EHU.toggleLoader true
  (SavedLocationsListRes savedLocationResp ) <- FlowCache.updateAndFetchSavedLocations false
  void $ lift $ lift $ EHU.toggleLoader false
  liftFlowBT $ push $ action ( SavedLocationsListRes savedLocationResp)
  pure unit

getFavouriteDriverList :: forall action. (GetFavouriteDriverListRes -> action) -> (action -> Effect Unit) -> ST.SavedLocationScreenState -> FlowBT String Unit
getFavouriteDriverList action push state = do
  void $ lift $ lift $ EHU.toggleLoader true
  (GetFavouriteDriverListRes favouriteDriverResp ) <- FlowCache.updateAndFetchFavouriteDriver
  void $ lift $ lift $ EHU.toggleLoader false
  liftFlowBT $ push $ action ( GetFavouriteDriverListRes favouriteDriverResp)
  pure unit


favouriteDriverViews :: forall w.(Action -> Effect Unit) -> ST.SavedLocationScreenState -> PrestoDOM (Effect Unit) w
favouriteDriverViews push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , padding (PaddingBottom 85)
  ][  scrollView
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , scrollBarY true
      , margin $ Margin 16 7 16 16
      ][
        linearLayout[
          height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin $ MarginTop 6
        ]
        (map 
          (\item ->
            linearLayout 
              [
                height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , padding $ Padding 16 16 16 16
              , stroke $ "1,"<> Color.grey900
              , cornerRadius 10.0
              , margin $ MarginTop 10
              , onClick push $ const $ ChangeScreen item.driverPhone item.driverName item.id
              ][
                linearLayout
                  [
                    height WRAP_CONTENT
                  , width MATCH_PARENT
                  , orientation HORIZONTAL
                  , margin $ MarginTop 5
                  ][
                    imageView [ 
                    width $ V 55
                  , height $ V 55
                  , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_driver_avatar"
                  , cornerRadius 50.0
                    ]
                  , linearLayout [
                      height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , orientation VERTICAL
                    , margin $ Margin 10 3 0 0
                      ][ 
                    textView $ [ 
                      height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , gravity LEFT
                    , text item.driverName
                    , color Color.black900
                    , textSize FontSize.a_18
                    , maxLines 1
                    ] <> FontStyle.subHeading1 TypoGraphy
                  , textView $ [ 
                      height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , gravity LEFT
                    , text "Know your Driver >"
                    , color Color.blue900
                    -- , margin $ Margin 15 0 15 0
                    , textSize FontSize.a_16
                    , maxLines 1
                    ]
                  ]
                ]
                , linearLayout
                    [
                      height $ V 2
                    , width MATCH_PARENT
                    , stroke $ "1," <> Color.grey800
                    , margin $ Margin 4 13 4 14
                    ][

                    ]
                , linearLayout
                  [
                    height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , orientation HORIZONTAL
                  , margin $ MarginLeft 4
                  ][
                    imageView [ 
                    width $ V 18
                  , height $ V 18
                  , imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_greenthumbsup"
                    ]
                  , textView $
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , gravity CENTER
                    , text $ show (fromMaybe 0 (fromNumber (item.driverRating * 4.0))) <> "% Approval rate"
                    , color Color.black800
                    , margin $ MarginLeft 8
                    , textSize FontSize.a_16
                    ]
                  , imageView [ 
                      width $ V 18
                    , height $ V 18
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_heart_red"
                    , margin $ MarginLeft 80
                    ]
                  , textView $
                      [ height WRAP_CONTENT
                      , width WRAP_CONTENT
                      , gravity CENTER
                      , text $ "By " <> show item.favCount <> " others"
                      , color Color.black800
                      , margin $ MarginLeft 8
                      , textSize FontSize.a_16
                    ]
                  ]
              ]
          ) state.data.favouriteDriversList)
      ]
  ]