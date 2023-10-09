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
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), (<<<), (/=), const, map, pure, unit, discard, bind, not, void, show, (<>), (==), (&&))
import Presto.Core.Types.Language.Flow (Flow, doAff, getState)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..), alignParentBottom, background, color, fontStyle, frameLayout, gravity, height, linearLayout, onBackPressed, orientation, padding, relativeLayout, scrollBarY, scrollView, text, textSize, textView, visibility, width, relativeLayout, alignParentRight, margin, stroke, onClick, cornerRadius, afterRender, accessibility)
import Screens.SavedLocationScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Services.API (SavedLocationReq(..), SavedLocationsListRes(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (GlobalState(..), defaultGlobalState)

screen :: ST.SavedLocationScreenState -> GlobalState -> Screen Action ST.SavedLocationScreenState ScreenOutput
screen initialState st =
  { initialState
  , view
  , name : "SavedLocationScreen"
  , globalEvents : [
      (\push -> do
        _ <- launchAff $ EHC.flowRunner st $ getSavedLocationsList SavedLocationListAPIResponseAction push initialState
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
  ]([  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , accessibility if (state.props.showDeleteLocationModel) then DISABLE_DESCENDANT else DISABLE 
      ][GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
    , if (not state.data.config.nyBrandingVisibility) then 
        linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.greySmoke
        ][]
      else
        linearLayout[][]
    , frameLayout
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
              , visibility if ((DA.length state.data.savedLocations )/= 0 && state.props.apiRespReceived == true) then VISIBLE else GONE
              ][  PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state) ]
            ]
        , linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , visibility if ((DA.length state.data.savedLocations )== 0 && state.props.apiRespReceived == true ) then VISIBLE else GONE
          ][  ErrorModal.view (push <<< ErrorModalAC) (errorModalConfig state )]
        ]
      ]
      
    ] <>  [linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , background Color.lightBlack900
          , visibility if (state.props.showDeleteLocationModel) then VISIBLE else GONE
          ][ PopUpModal.view (push <<<  PopUpModalAction) (requestDeletePopUp state )]])

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
              , frequencyCount : Just 0
              , recencyDate : Just ""
              , locationScore : Just 0.0
            }))state.data.savedLocations)
        , linearLayout
          [ height $ V 100
          , width MATCH_PARENT
          ][]
        ]]
    ]

getSavedLocationsList :: forall action. (SavedLocationsListRes -> action) -> (action -> Effect Unit) -> ST.SavedLocationScreenState -> Flow GlobalState Unit
getSavedLocationsList action push state = do
  _ <-  EHU.toggleLoader true
  (savedLocationResp ) <- Remote.getSavedLocationList ""
  case savedLocationResp of
      Right (SavedLocationsListRes listResp) -> do
        _ <-  EHU.toggleLoader false
        doAff do liftEffect $ push $ action ( SavedLocationsListRes (listResp))
        pure unit
      Left (err) -> do
        _ <-  EHU.toggleLoader false
        pure unit
