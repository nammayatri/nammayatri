module Screens.ExtraChargeInfoScreen.View where

import Prelude
import PrestoDOM
import Screens.Types
import Data.Maybe
import Effect
import Language.Types
import Common.Types.App
import PrestoDOM.Types.DomAttributes
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Screens.ExtraChargeInfoScreen.Controller
import Engineering.Helpers.Commons as EHC
import Language.Strings
import JBridge as JB
import Screens.ExtraChargeInfoScreen.ScreenData
import Engineering.Helpers.Commons
import Debug
import PrestoDOM.Properties
import PrestoDOM.Elements.Elements
import Helpers.Utils
import Screens.ExtraChargeInfoScreen.ComponentConfig
import Components.PrimaryButton as PrimaryButton
import Common.Types.App as CTA
import Data.Function.Uncurried
import Data.Array as DA
import JBridge
import PrestoDOM.Core
import Debug
import Services.API (DriverTags(..), GetDriverInfoResp(..))
import Components.ExtraChargeCard as ExtraChargeCard
import Animation as Anim
import Resource.Localizable.StringsV2
import Resource.Localizable.TypesV2
import Storage
import RemoteConfig as RC

screen :: ExtraChargeInfoScreenState -> Screen Action ExtraChargeInfoScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "ExtraChargeInfoScreen"
  , globalEvents : []
  , eval : (\action state -> do
      let _ = spy "ExtraChargeInfoScreen state: " state
      let _ = spy "ExtraChargeInfoScreen action: " action
      eval action state)
  }


view ::forall w .  (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $ linearLayout[
    height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  , onBackPressed push $ const OnBackPressed
  ][
    headerView push state
  , frameLayout[
      width MATCH_PARENT
    , weight 1.0
    ][
      scrollView[
        width MATCH_PARENT
      , height WRAP_CONTENT
      , scrollBarY false
      , padding $ Padding 16 0 16 16
      ][
        bodyView push state
      ]
    , footerView push state
    ]
  ]

footerView :: forall w. (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
footerView push state =
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , layoutGravity "bottom"
  , orientation VERTICAL
  , background Color.white900
  , padding $ PaddingBottom 16
  ][
    linearLayout[
      width MATCH_PARENT
    , height $ V 1
    , background Color.grey900
    ][]
  , gotItBtnView push state
  , needHelpView push state
  ]

handlerView :: forall w. PrestoDOM (Effect Unit) w
handlerView =
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , margin $ MarginBottom 12
  ][
    linearLayout[
      width $ V 24
    , height $ V 4
    , cornerRadius 2.0
    , background Color.argent
    ][]
  ]

bodyView :: forall w . (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
bodyView push state =
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ MarginTop 8
  , padding $ PaddingBottom 130
  ][
    gaugeMeterView push state
  , questionAndAnswersView push state
  ]

headerView :: forall w . (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
headerView push state =
  linearLayout[
     width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][
    linearLayout[
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , margin $ Margin 16 16 16 16
    ][
      imageView[
        imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , width $ V 24
      , height $ V 24
      , onClick push $ const OnBackPressed
      ]
    , textView $ [
        text $ getStringV2 your_extra_charge_penalty
      , color Color.black800
      , margin $ Margin 16 0 0 2
      ] <> (FontStyle.h3 TypoGraphy)
    ]
  , linearLayout[
      width MATCH_PARENT
    , height $ V 1
    , background Color.grey900
    ][]
  ]


gaugeMeterView :: forall w . (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
gaugeMeterView push state =
  case state.driverInfoResp of
    Just (GetDriverInfoResp resp) ->
       case resp.driverTags >>= \(DriverTags tags) -> tags."DriverChargingBehaviour" of
        Just overchargingTag -> ExtraChargeCard.view (push <<< ExtraChargeCardAC) $ ExtraChargeCard.extraChargesConfig overchargingTag resp.ridesWithFareIssues resp.totalRidesConsideredForFareIssues GONE
        Nothing -> linearLayout [width $ V 0, height $ V 0][]
    Nothing -> linearLayout [width $ V 0, height $ V 0][]


questionAndAnswersView ::  forall w . (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
questionAndAnswersView push state =
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ] $ DA.mapWithIndex (\index item -> questionAndAnswerView item.question item.answerView index push state) (mainQA "lazy")

questionAndAnswerView :: forall w . String
  -> ((Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w)
  -> Int
  -> (Action  -> Effect Unit)
  -> ExtraChargeInfoScreenState
  -> PrestoDOM (Effect Unit) w
questionAndAnswerView questionStr answerView index push state =
  let optionOpened = fromMaybe false (state.optionOpened DA.!! index)
  in
    linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.aliceBlueLight
    , padding $ Padding 16 16 16 16
    , cornerRadius 16.0
    , orientation VERTICAL
    , margin $ MarginTop (if index == 0 then 0 else 24)
    ]$ [
      questionView questionStr index optionOpened push state
    ] <> (if optionOpened then [ answerView push state] else [])

questionView ::  forall w . String -> Int -> Boolean ->  (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
questionView questionStr ind optionOpened push state =
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , onClick push $ const $ OnQuestionClick ind
  ][
    textView $ [
        text questionStr
      , color Color.black800
      ] <> (FontStyle.h2 TypoGraphy)
  , linearLayout[weight 1.0, height WRAP_CONTENT][]
  , imageView[
      imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_down_light"
    , height $ V 24
    , width $ V 24
    , rotation if optionOpened then 180.0 else 0.0
    ]
  ]

answer1View :: forall w. (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
answer1View push state =
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginTop 12
  ][
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER
      , id $ getNewIDWithTag "extraChargeVideoView"
      , afterRender
          ( \action -> do
              let
                city = getValueToLocalStore DRIVER_LOCATION
                config = RC.getExtraChargeConfig city
                id = (getNewIDWithTag "extraChargeVideoView")
                url = getTagBasedVideo config.videos state.driverInfoResp
              void $ pure $ runFn5 setYoutubePlayer (getYoutubeData{videoType = "PORTRAIT_VIDEO",videoId = getVideoID url}) id (show CTA.PLAY) push YoutubeVideoStatus
          )
          (const NoAction)
      ][]
  ]



answer2View :: forall w. (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
answer2View push state =
  linearLayout [
    width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop 12
  ] (DA.mapWithIndex (\index item -> qAView index item ) (readMoreQA "lazy"))

  where
    qAView ind qa =
      linearLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin $ MarginTop if ind /= 0 then  8 else  0
      ][
        textView $ [
          text qa.question
        , color Color.black700
        ] <> (FontStyle.body4 TypoGraphy)
      , textView $ [
          text qa.answer
        , color Color.black700
        ] <> (FontStyle.body2 TypoGraphy)
      ]

mainQA :: forall w. String -> Array ({question:: String, answerView:: (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w})
mainQA lazy = [
    {
        question : getStringV2 what_does_this_mean
    ,   answerView : answer1View
    },
    {
        question : getStringV2 read_more,
        answerView : answer2View
    }
]


gotItBtnView :: forall w. (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
gotItBtnView push state = PrimaryButton.view (push <<< GotItBtn) (gotItBtnConfig state)

needHelpView :: forall w. (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
needHelpView push state =
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , margin $ MarginTop 18
  , gravity CENTER
  ][
    textView $ [
      text $ getStringV2 need_help
    , color Color.black650
    ] <> (FontStyle.body2 TypoGraphy)
  , imageView[
      imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_blue_call"
    , width $ V 12
    , height $ V 12
    , margin $ MarginHorizontal 4 4
    ]
  , textView $ [
      text $ getStringV2 call_support
    , color Color.blue800
    , onClick push $ const OnCallSupportAC
    ] <> (FontStyle.body2 TypoGraphy)
  ]
