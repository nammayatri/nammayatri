{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DataExplainWithFetch.View where

import Animation (screenAnimation)
import Common.Types.App (LazyCheck(..))
import Data.Maybe (Maybe(..), maybe, isJust, fromMaybe)
import Data.Array as Array
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Engineering.Helpers.Commons as EHC
import Helpers.Utils (FetchImageFrom(..), getAssetsBaseUrl, fetchImage)
import Types.App (GlobalState(..), defaultGlobalState)
import JBridge (lottieAnimationConfig, startLottieProcess)
import Prelude (Unit, const, discard, pure, void, ($), (<>), (<<<), (>), (==), (||), (&&), unit, bind, map, not)
import PrestoDOM
import Screens.DataExplainWithFetch.Controller (Action(..), ScreenOutput, eval, getStepConfig, getStageConfig)
import Screens.DataExplainWithFetch.ComponentConfig as CC
import Screens.Types (DataFetchScreenState, SafetyStepsConfig, Component(..), NammaSafetyStage(..), SafetyStageConfig(..), SubTitleConfig, TitleConfig, NoteBoxConfig, DropDownWithHeaderConfig, BoxContainerConfig, CheckBoxSelectionConfig, NewContacts, ImageComponentConfig)
import Styles.Colors as Color
import Components.GenericHeader as GenericHeader
import PrestoDOM.List (ListItem, preComputeListItem)
import CarouselHolder as CarouselHolder
import Components.BannerCarousel as BannerCarousel
import Effect.Aff (launchAff)
import Presto.Core.Types.Language.Flow (Flow)
import Font.Style as FontStyle
import Mobility.Prelude (boolToInvisibility, boolToVisibility)
import Components.BoxContainer as BoxContainer
import Components.InfoBox as InfoBox
import Components.DropDownWithHeader as DropDownWithHeader
import Debug
import Components.PrimaryButton as PrimaryButton
import Language.Strings (getString)
import Language.Types (STR(..))

screen :: DataFetchScreenState -> Screen Action DataFetchScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "DataExplainWithFetch"
  , globalEvents:
      [ ( \push -> do
            -- void $ launchAff $ EHC.flowRunner defaultGlobalState $ computeListItem push
            pure $ pure unit
        )
      ]
  , eval:
      ( \action state -> do
          let
            _ = spy "DataExplainWithFetch state -----" state
          let
            _ = spy "DataExplainWithFetch--------action" action
          eval action state
      )
  }

view ::
  forall w.
  (Action -> Effect Unit) ->
  DataFetchScreenState ->
  PrestoDOM (Effect Unit) w
view push state =
  screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding padding'
        , onBackPressed push $ const BackPressed
        , background Color.white900
        , gravity CENTER_HORIZONTAL
        , afterRender push (const AfterRender)
        ]
        [ GenericHeader.view (push <<< GenericHeaderActionController) (CC.genericHeaderConfig state)
        , linearLayout
            [ width MATCH_PARENT
            , weight 1.0
            ]
            [ scrollView
                [ height MATCH_PARENT 
                , width MATCH_PARENT
                , scrollBarY false
                ]
                [ explanationContentView push state
                ]
            ]
        , separator
        , PrimaryButton.view (push <<< PrimaryButtonAC) (CC.primaryButtonConfig state)
        ]
  where 
    padding' = if EHC.os == "IOS" then (Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 16 else EHC.safeMarginBottom)) else (PaddingLeft 0)

separator :: forall w. PrestoDOM (Effect Unit) w
separator =
  linearLayout
  [ width MATCH_PARENT
  , height $ V 1
  , background Color.grey900
  ][]

explanationContentView :: forall w. (Action -> Effect Unit) -> DataFetchScreenState -> PrestoDOM (Effect Unit) w
explanationContentView push state =
  let
    config = getStepConfig state
  in
    linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER_HORIZONTAL
      , padding (Padding 0 0 0 0)
      ]
      [ imageView
          [ height $ V 200
          , width MATCH_PARENT
          , visibility $ boolToVisibility $ not state.props.stageSetUpCompleted || config.primaryButtonAction == "SafetyTestDrill"
          , imageWithFallback $ fetchImage GLOBAL_COMMON_ASSET config.imageUrl
          ]
      , dynamicOptionsView push state
      ]

imageViewComponent :: forall w. (Action -> Effect Unit) -> DataFetchScreenState -> ImageComponentConfig -> PrestoDOM (Effect Unit) w
imageViewComponent push state config =
  imageView
    [ height $ V 200
    , width MATCH_PARENT
    , margin $ Margin 16 16 16 0
    , visibility $ boolToVisibility state.props.stageSetUpCompleted
    , imageWithFallback $ fetchImage GLOBAL_COMMON_ASSET config.imageUrl
    ]

dynamicOptionsView :: forall w. (Action -> Effect Unit) -> DataFetchScreenState -> PrestoDOM (Effect Unit) w
dynamicOptionsView push state =
  let
    c = 10
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , padding (Padding 0 0 0 0)
      ]
      $ if state.props.stageSetUpCompleted then
          combinedComponentFiltering push state
        else
          componentFiltering push state

combinedComponentFiltering :: forall w. (Action -> Effect Unit) -> DataFetchScreenState -> Array (PrestoDOM (Effect Unit) w)
combinedComponentFiltering push state =
  let
    combinedConfigData = getStageConfig state
  in
    Array.concatMap (filteringHelpers push state) combinedConfigData

componentFiltering :: forall w. (Action -> Effect Unit) -> DataFetchScreenState -> Array (PrestoDOM (Effect Unit) w)
componentFiltering push state = filteringHelpers push state $ getStepConfig state

filteringHelpers :: forall w. (Action -> Effect Unit) -> DataFetchScreenState -> SafetyStageConfig -> Array (PrestoDOM (Effect Unit) w)
filteringHelpers push state configData =
  map
    ( \component -> case component of
        BoxContainer config -> boxContainerView push state config configData.primaryButtonAction
        DropDownWithHeader config -> dropDownWithHeaderView push state config configData.primaryButtonAction
        NoteBox config -> noteBoxView push state config configData.primaryButtonAction
        Title config -> titleView push state config configData.primaryButtonAction
        SubTitle config -> subTitleView push state config configData.primaryButtonAction
        CheckBoxSelection config -> checkBoxSelectionView push state config configData.primaryButtonAction
        ImageComponent config -> imageViewComponent push state config
    )
    configData.dynamicViewData

checkBoxSelectionView :: forall w. (Action -> Effect Unit) -> DataFetchScreenState -> CheckBoxSelectionConfig -> String -> PrestoDOM (Effect Unit) w
checkBoxSelectionView push state config action =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (Padding 0 0 0 0)
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width MATCH_PARENT
          , text $ getString DEFAULT_CONTACT
          , padding $ PaddingLeft 16
          , color Color.black900
          ]
        <> FontStyle.body1 TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 16 16 16 16
        , margin $ Margin 16 12 16 0
        , cornerRadius 16.0
        , background Color.blue600
        , orientation VERTICAL
        ]
        $ Array.mapWithIndex (\index contact -> contactListView push state contact index) state.data.emergencyContactsList
    ]

contactListView :: forall w. (Action -> Effect Unit) -> DataFetchScreenState -> NewContacts -> Int -> PrestoDOM (Effect Unit) w
contactListView push state contact index =
  let
    isDefault = contact.priority == 0 --contact.number == state.data.defaultSelectedContact.number

    strokeColor = if isDefault then Color.black800 else Color.black500

    userColor = case index of
      0 -> Color.yellow900
      1 -> Color.blue800
      2 -> Color.yellow800
      _ -> Color.grey700
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ MarginVertical 12 12
      , orientation HORIZONTAL
      , onClick push $ const $ DefaultContactSelected contact
      , gravity CENTER_VERTICAL
      ]
      [ linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , background userColor
          , padding $ Padding 8 8 8 8
          , cornerRadius 50.0
          , margin $ MarginRight 12
          ]
          [ textView
              $ [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text "AR"
                , color Color.black
                ]
          ]
      , textView
          $ [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text contact.name
            , color Color.black900
            , weight 1.0
            ]
          <> FontStyle.body5 TypoGraphy
      , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , padding $ Padding 4 4 4 4
          , stroke $ "1," <> Color.black800
          , cornerRadius 50.0
          ]
          [ linearLayout
              [ height $ V 12
              , width $ V 12
              , cornerRadius 50.0
              , visibility $ boolToInvisibility isDefault
              , background Color.black800
              ]
              []
          ]
      ]

boxContainerView :: forall w. (Action -> Effect Unit) -> DataFetchScreenState -> BoxContainerConfig -> String -> PrestoDOM (Effect Unit) w
boxContainerView push state config action =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER_HORIZONTAL
    , padding (Padding 0 0 0 0)
    ]
    [ BoxContainer.view (push <<< BoxContainerAC) (CC.boxContainerConfig state config action)
    ]

dropDownWithHeaderView :: forall w. (Action -> Effect Unit) -> DataFetchScreenState -> DropDownWithHeaderConfig -> String -> PrestoDOM (Effect Unit) w
dropDownWithHeaderView push state config action =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER_HORIZONTAL
    , padding (Padding 0 0 0 0)
    ]
    [ DropDownWithHeader.view (push <<< DropDownWithHeaderAC) (CC.dropDownWithHeaderConfig state config action)
    ]

noteBoxView :: forall w. (Action -> Effect Unit) -> DataFetchScreenState -> NoteBoxConfig -> String -> PrestoDOM (Effect Unit) w
noteBoxView push state config action =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER_HORIZONTAL
    , padding (Padding 0 0 0 0)
    ]
    [ InfoBox.view push $ CC.infoBoxConfig state config action
    ]

titleView :: forall w. (Action -> Effect Unit) -> DataFetchScreenState -> TitleConfig -> String -> PrestoDOM (Effect Unit) w
titleView push state config action =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 16 16 16 0
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width MATCH_PARENT
          , text config.titleText
          , color Color.black900
          ]
        <> FontStyle.h3 TypoGraphy
    ]

subTitleView :: forall w. (Action -> Effect Unit) -> DataFetchScreenState -> SubTitleConfig -> String -> PrestoDOM (Effect Unit) w
subTitleView push state config action =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ if state.props.stageSetUpCompleted then (Padding 16 8 16 0) else (Padding 16 0 16 0)
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width MATCH_PARENT
          , textFromHtml config.subTitleText
          , color Color.black700
          ]
        <> FontStyle.body5 TypoGraphy
    ]
