module Screens.TopPriceView.View where

import Data.Maybe
import Debug
import Effect
import Prelude
import Prelude
import PrestoDOM
import Screens.TopPriceView.Controller
import Api.Types (NearBySearchRequestRes(..), SearchRequest(..))
import Components.SeparatorView.View as SeparatorView
import Data.Array (elem, foldMap, index, mapWithIndex)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Int (ceil, fromNumber, toNumber)
import Data.Time (Millisecond)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Uncurried (runEffectFn1)
import Font.Style as FontStyle
import Helpers.Colors (Color)
import Helpers.Colors as Color
import Helpers.Commons (flowRunner, getExpiryTime, getHeightFromPercent, getValueBtwRange, hideLoader, safeMarginBottom, screenHeight)
import Helpers.Commons (liftFlow, screenWidth)
import Helpers.Commons (safeMarginBottom, safeMarginTop, screenWidth)
import Helpers.Commons (screenWidth)
import Helpers.Pooling (delayViaTimer)
import Presto.Core.Types.Language.Flow (Flow)
import Presto.Core.Types.Language.Interaction (request)
import PrestoDOM.List (listDataV2, listItem, onClickHolder, textHolder, viewPager2)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.TopPriceView.ScreenData (TopPriceViewState, TabTimers)
import Services.Backend (nearBySearchRequest)
import Timers (clearAllTimers, clearTimerWithId, startTimer)
import Types (LazyCheck(..), OverlayData(..), defaultOverlayData)

type Layout w
  = PrestoDOM (Effect Unit) w

screen :: OverlayData -> ScopedScreen Action TopPriceViewState ScreenOutput
screen (OverlayData gState) =
  { initialState: gState.topPriceViewState
  , view: view
  , name: "TopPriceView"
  , globalEvents:
      [ ( \push -> do
            _ <- updateTimers push gState.topPriceViewState
            when (gState.topPriceViewState.timer == 0.0) $ startTimer 0 "RideRequestTimer" "0.001" (\id s dt -> push $ UpdateProgress id s dt)
            pure $ pure unit
        )
      ]
  , parent: Just "TopPriceView"
  , eval:
      ( \action state -> do
          case action of
            UpdateProgress _ _ _ -> pure unit
            _ -> do
              let
                _ = spy "RideRequestPopUp TopPriceView -> action " action

                _ = spy "RideRequestPopUp TopPriceView -> state " state
              pure unit
          eval action state
      )
  }

view :: forall w. (Action -> Effect Unit) -> TopPriceViewState -> Layout w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.transparent
    , padding $ PaddingTop safeMarginTop
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        ]
        [ rideRequestTab push state
        ]
    ]

rideRequestTab :: forall w. (Action -> Effect Unit) -> TopPriceViewState -> Layout w
rideRequestTab push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER
    , background Color.white900
    , cornerRadius 6.0
    , margin $ MarginHorizontal 16 16
    ]
    (mapWithIndex (\idx item -> singleTabView push idx item) state.tabs)

singleTabView :: forall w. (Action -> Effect Unit) -> Int -> TabTimers -> Layout w
singleTabView push idx item =
  let
    tabHeight = getSingleTab

    progressWidth = tabHeight - 20

    progress = getWidthFromProgress item.currentProgress progressWidth

    progressColor = getColorFromProgress progressWidth progress
  in
    linearLayout
      [ width $ V $ getSingleTab
      , height WRAP_CONTENT
      , orientation VERTICAL
      , background $ if item.selected then Color.black500 else Color.white900
      , padding $ Padding 14 20 14 20
      , gravity CENTER
      , onClick push $ const (OnTabClick idx)
      ]
      [ textView
          $ [ text $ if item.price == 0.0 then "--" else show item.price
            , color Color.black900
            ]
          <> FontStyle.body10 TypoGraphy
      , linearLayout
          [ width $ V $ getSingleTab - 20
          , height $ V 10
          , gravity LEFT
          , background Color.white900
          , cornerRadius 5.0
          ]
          [ linearLayout
              [ height $ V 10
              , width $ V $ progress
              , background progressColor
              ]
              []
          ]
      ]
  where
  getWidthFromProgress progress progressWidth = ceil $ getValueBtwRange progress 1.0 item.maxProgress 0.0 (toNumber progressWidth)

  getSingleTab ∷ Int
  getSingleTab = (((screenWidth unit) - 32) / 3) + 1

  getColorFromProgress :: Int -> Int -> Color
  getColorFromProgress tabHeight progress =
    let
      percent = (progress * 100) / tabHeight
    in
      if percent > 50 then
        Color.green900
      else
        Color.red900

getPriceFromArray :: TopPriceViewState -> Int -> String
getPriceFromArray state idx = case index state.rideRequests idx of
  Nothing -> " -- "
  Just (SearchRequest request) -> show $ request.baseFare

updateTimers :: (Action -> Effect Unit) -> TopPriceViewState -> Effect Unit
updateTimers push state =
  foldMapWithIndex
    ( \idx (SearchRequest item) -> do
        let expiryTime = getExpiryTime item.searchRequestValidTill
        push $ UpdateMaxProgress idx (SearchRequest item) $ toNumber expiryTime
    )
    state.rideRequests
