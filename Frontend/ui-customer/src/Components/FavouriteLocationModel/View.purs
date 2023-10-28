{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.FavouriteLocationModel.View where

import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), color, cornerRadius, ellipsize, fontStyle, gravity, height, imageUrl, imageView, lineHeight, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, background, scrollView, scrollBarY, visibility, relativeLayout, alignParentBottom)
import Components.FavouriteLocationModel.Controller(Action(..))
import Components.SavedLocationCard.Controller as SavedLocationCardConfig
import Components.SavedLocationCard.View as SavedLocationCard
import Components.GenericHeader.Controller as GenericHeaderConfig
import Components.GenericHeader.View as GenericHeader
import Components.ErrorModal.View as ErrorModal
import Components.ErrorModal.Controller as ErrorModalConfig
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Prelude (Unit, const, ($), (<>), (==), map, (<<<), bind, pure, unit, show, (||), (/=), (-), (/), (*), (+), not)
import Styles.Colors as Color
import Screens.Types(LocationListItemState, CardType(..), LocationItemType(..))
import Language.Strings (getString)
import Data.Maybe
import Language.Types (STR(..))
import Debug (spy)
import Data.Array (filter, length, null)
import Data.String (toLower)
import Engineering.Helpers.Commons(safeMarginBottom, safeMarginTop, os, screenHeight)
import PrestoDOM.Animation as PrestoAnim
import Animation (translateYAnimFromTop)
import Animation.Config (translateFullYAnimWithDurationConfig)
import JBridge(getHeightFromPercent)
import Common.Types.App
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))

view :: forall w. (Action -> Effect Unit) -> Array LocationListItemState -> PrestoDOM ( Effect Unit ) w
view push state =
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.white900
  , padding (PaddingVertical safeMarginTop safeMarginBottom)
  , orientation VERTICAL
  , gravity BOTTOM
  ][
    linearLayout[
    orientation VERTICAL
   , height MATCH_PARENT
   , width MATCH_PARENT
  ][GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
     , linearLayout
         [ width MATCH_PARENT
         , height $ V 1
         , background Color.grey900
         ][]
     , if (null (getFavourites state)) then noSavedLocationView push state else savedLocationListView push state ]
     , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , weight 1.0
        , alignParentBottom "true,-1"
        , background Color.white900
        , orientation VERTICAL
        ][  linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.grey900
            ][]
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ Padding 16 12 16 16
            , gravity CENTER
            ][  textView $
                [ text (getString ADD_SAVED_LOCATION_FROM_SETTINGS)
                , gravity CENTER
                , color Color.black700
                ] <> FontStyle.body3 TypoGraphy
              ]
          ]
         ]



savedLocationListView :: forall w. (Action -> Effect Unit) -> Array LocationListItemState -> PrestoDOM (Effect Unit) w
savedLocationListView push state =
  PrestoAnim.animationSet [translateYAnimFromTop $ translateFullYAnimWithDurationConfig 400 ] $
  scrollView
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , scrollBarY true
      , visibility if (length (getFavourites state) == 0) then GONE else VISIBLE
      , padding (PaddingBottom safeMarginBottom)
      ][  linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , padding (PaddingBottom (100 + safeMarginBottom))
          , margin (MarginTop 8)
          , orientation VERTICAL
          ](map (\item -> SavedLocationCard.view (push <<< FavouriteLocationAC) (item) ) (getFavourites state))]

noSavedLocationView :: forall w. (Action -> Effect Unit) -> Array LocationListItemState -> PrestoDOM (Effect Unit) w
noSavedLocationView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , visibility if (null (getFavourites state)) then VISIBLE else GONE
    ][ ErrorModal.view (push <<< ErrorModalAC) (errorModalConfig state ) ]

errorModalConfig :: Array LocationListItemState ->  ErrorModalConfig.Config
errorModalConfig state = let
  config = ErrorModalConfig.config
  errorModalConfig' = config
    { imageConfig {
        imageUrl = fetchImage FF_ASSET "ny_ic_no_saved_address"
      , height = V 110
      , width = V 124
      , margin = (MarginBottom 31)
      }
    , errorConfig {
        text = (getString NO_FAVOURITES_SAVED_YET)
      , margin = (MarginBottom 7)
      , color = Color.black900
      }
    , errorDescriptionConfig {
        text = (getString SAVED_ADDRESS_HELPS_YOU_KEEP_YOUR_FAVOURITE_PLACES_HANDY)
      , color = Color.black700
      , margin = (Margin 33 0 33 0)
      , padding = (Padding 16 0 16 16)
      , textStyle =  FontStyle.ParagraphText
      }
    , buttonConfig {
      visibility = GONE
      }
    }
  in errorModalConfig'


genericHeaderConfig :: Array LocationListItemState -> GenericHeaderConfig.Config
genericHeaderConfig state = let
  config = GenericHeaderConfig.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , width = WRAP_CONTENT
    , background = Color.white900
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , margin = (Margin 12 12 12 12)
      }
    , textConfig {
        text = (getString SELECT_FAVOURITE)
      , color = Color.black800
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , padding = (Padding 0 5 0 5)
    }
  in genericHeaderConfig'


getFavourites :: Array LocationListItemState -> Array LocationListItemState
getFavourites arrayItem = do
    let home = (filter (\x -> (toLower x.tag) == "home") ( arrayItem))
    let work = (filter (\x -> (toLower x.tag) == "work" ) (arrayItem))
    let otherLocation = (filter (\x -> not  ((toLower x.tag) == "home" || (toLower x.tag) == "work")) ( arrayItem))
    map (\x -> getFavouritesItem x) (home <> work <> otherLocation)


getFavouritesItem :: LocationListItemState -> LocationListItemState
getFavouritesItem item = {
    prefixImageUrl : item.prefixImageUrl
  , postfixImageUrl : item.postfixImageUrl
  , postfixImageVisibility : item.postfixImageVisibility
  , title : item.title
  , subTitle : item.subTitle
  , placeId : item.placeId
  , lat : item.lat
  , lon : item.lon
  , description : item.description
  , tag : item.tag
  , tagType : item.tagType
  , cardType : Just $ show $ case (toLower item.tag) of
                              "home" -> HOME_TAG
                              "work" -> WORK_TAG
                              _      -> OTHER_TAG
  , address : item.address
  , tagName : item.tag
  , isEditEnabled : false
  , savedLocation : item.description
  , placeName : item.placeName
  , isClickable : true
  , alpha : 1.0
  , fullAddress : item.fullAddress
  , locationItemType : Just SAVED_LOCATION
  , distance : item.distance
  , showDistance: Just false
  , actualDistance : Nothing
  , frequencyCount : Just 0
  , recencyDate : Just ""
  , locationScore : Just 0.0
}
