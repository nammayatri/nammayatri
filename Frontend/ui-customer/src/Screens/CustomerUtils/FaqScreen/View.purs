{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTIEHULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.FaqScreen.View where

import Common.Types.App
import Screens.CustomerUtils.FaqScreen.ComponentConfig
import Animation as Anim
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array as DA
import Data.String as DS
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (when, Unit, bind, const, discard, map, pure, unit, show, not, ($), (-), (/=), (<<<), (<=), (<>), (==), (||), (<), (<>), (&&))
import Presto.Core.Types.Language.Flow (Flow, doAff)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..), afterRender, alignParentRight, background, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, stroke, text, textSize, textView, visibility, width, imageWithFallback, weight, layoutGravity, clickable, alignParentBottom, scrollView, adjustViewWithKeyboard, lineHeight, singleLine, alpha, accessibility, accessibilityHint, textFromHtml, onAnimationEnd, id, horizontalScrollView, scrollBarX)
import Screens.FaqScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.FaqScreen.Transformer 
import Screens.Types as ST
import Styles.Colors as Color
import Types.App (GlobalState, defaultGlobalState)
import Locale.Utils
import Data.Maybe (Maybe(..))
import Components.DropDownCard as DropDownCard
import Data.Array as DA
import Helpers.CommonView (emptyTextView)
import Animation as Anim
import JBridge (renderBase64Image)
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Debug (spy)

screen :: ST.FaqScreenState -> Screen Action ST.FaqScreenState ScreenOutput
screen initialState =
  {
    initialState
  , view
  , name : "FaqScreen"
  , globalEvents : []
  , eval : \state  action -> do
      let _ = spy  "FaqScreen action " state
      let _ = spy  "FaqScreen state " action
      eval state action
  }

view :: forall w . (Action -> Effect Unit) -> ST.FaqScreenState -> PrestoDOM (Effect Unit) w
view push state = 
  Anim.screenAnimation $
 relativeLayout
 [  height MATCH_PARENT
  , width MATCH_PARENT
 ]$[linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , padding $ Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom
    , onBackPressed push $ const BackPressed
    , afterRender push (const AfterRender)
    ][  GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
      , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.grey900
        ][]
      , scrollView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ][  linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ](DA.mapWithIndex (\index item -> dropDownCardView state item push) $ state.data.dropDownList)
            
          ]
      , apiFailureView state push  
    ]
  ]

apiFailureView :: forall w. ST.FaqScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit ) w
apiFailureView state push=
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , gravity CENTER
  , visibility if state.props.apiFailure then VISIBLE else GONE
  ][  ErrorModal.view (push <<< APIFailureActionController) (apiErrorModalConfig state)]

dropDownCardView :: forall w. ST.FaqScreenState -> FaqCardDropDownInfo -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit ) w
dropDownCardView state dropDownCardInfo push =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , gravity CENTER
  , margin $ Margin 10 8 10 2
  ][  DropDownCard.view (push <<< DropDownCardActionController) (dropDownCardConfig state dropDownCardInfo (dropDownCardData state push dropDownCardInfo))]

dropDownCardData :: ST.FaqScreenState  -> (Action -> Effect Unit) -> FaqCardDropDownInfo -> forall w . PrestoDOM (Effect Unit ) w
dropDownCardData state push cardInfo =
  let stringsWithTypes = dropDownStringTypes $ cardInfo.description
  in 
   linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 16 12 16 16
    ][  linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ] (DA.mapWithIndex (\index item -> 
            case item.messageStringType of
              "{HEADING}" -> dropDownCardHeadingView item.value
              "{BODY}" -> dropDownCardBodyView item.value
              "{IMAGE}" -> dropDownCardImageView push cardInfo item.value index []
              "{IMAGE_GROUP}" -> dropDownCardImageGroupView push cardInfo item.value
              _ -> emptyTextView
          ) stringsWithTypes) 
      , case cardInfo.label of 
          Just "FAVOURITES" -> PrimaryButton.view (push <<< OpenFavourites) (primaryButtonConfig state cardInfo.action) 
          Just "CHANGE_LANGUAGE" -> PrimaryButton.view (push <<< OpenChangeLanguageScreen) (primaryButtonConfig state cardInfo.action)
          Just "SELECT_RIDE" -> case cardInfo.referenceCategoryId of
            Just categoryId -> PrimaryButton.view (push <<< (OpenSelectRideScreen categoryId cardInfo.referenceOptionId)) (primaryButtonConfig state cardInfo.action)
            _ -> emptyTextView
          _ -> case cardInfo.referenceCategoryId of
                        Just categoryId -> PrimaryButton.view (push <<< (OpenChat categoryId cardInfo.referenceOptionId)) (primaryButtonConfig state cardInfo.action)
                        _ -> emptyTextView
    ] 
      

dropDownCardHeadingView :: String -> forall w . PrestoDOM (Effect Unit ) w
dropDownCardHeadingView title =
  textView $
    [ text title
    , color Color.darkCharcoal
    , width MATCH_PARENT
    , margin $ MarginVertical 12 4
    ] <> FontStyle.subHeading1 TypoGraphy
  
dropDownCardBodyView :: String -> forall w . PrestoDOM (Effect Unit ) w
dropDownCardBodyView description =
  textView $
    [ textFromHtml description
    , color Color.black700
    , width MATCH_PARENT
    ] <> FontStyle.body1 TypoGraphy


dropDownCardImageView :: (Action -> Effect Unit) -> FaqCardDropDownInfo -> String -> Int ->  Array String -> forall w . PrestoDOM (Effect Unit ) w
dropDownCardImageView push cardInfo imageUrl index imageList = 
  PrestoAnim.animationSet [Anim.fadeIn true]$
    linearLayout
    [ height $ V 350
    , width $ V 160
    , margin $ Margin 8 8 8 8
    , cornerRadius 4.0
    , id $ EHC.getNewIDWithTag ("DropDownCardImageView" <> (show index))
    -- , onClick push $ const $ OpenPreviewImage imageUrl imageList
    , onAnimationEnd
        ( \action -> do 
            renderBase64Image imageUrl (EHC.getNewIDWithTag ("DropDownCardImageView" <> (show index))) true "FIT_CENTER"
        ) (const NoAction)
    ][]

dropDownCardImageGroupView :: (Action -> Effect Unit) -> FaqCardDropDownInfo -> String -> forall w . PrestoDOM (Effect Unit ) w
dropDownCardImageGroupView push cardInfo imageUrls = 
  let imageList = dropDownImageSplit imageUrls
  in
  horizontalScrollView
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , scrollBarX false
  ][linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    ](DA.mapWithIndex (\index item -> if item == "" then emptyTextView else (dropDownCardImageView push cardInfo item index imageList)) imageList)
  ]