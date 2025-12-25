{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HotspotScreen.View where

import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..))
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Control.Monad.Except.Trans (runExceptT, lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.Time.Duration (Milliseconds(..))
import Debug
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Uncurried (runEffectFn2)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Utils (getColorWithOpacity, toggleLoader, loaderText)
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge as JB
import Language.Strings (getString)
import Language.Types
import MerchantConfig.Types
import Mobility.Prelude (boolToVisibility)
import Prelude (Unit, bind, discard, pure, unit, const, ($), void, (<>), map, (||), (>=), (<<<))
import Presto.Core.Types.Language.Flow (getState, delay)
import PrestoDOM
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Events (globalOnScroll)
import Screens.HotspotScreen.ComponentConfig
import Screens.HotspotScreen.Controller (Action(..), eval, ScreenOutput(..))
import Screens.Types as ST
import Services.API as API
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (defaultGlobalState, GlobalState(..))

screen :: ST.HotspotScreenState -> LoggableScreen Action ST.HotspotScreenState ScreenOutput
screen initialState = 
  { initialState
  , view
  , name : "HotspotScreen"
  , globalEvents:
      [ globalOnScroll "HotspotScreen"
      , ( \push -> do
            void $ runEffectFn2 JB.getCircleCallback push CircleOnClick
            void $ runEffectFn2 JB.storeCallbackHotspotMap push UpdateHotspotCircles 
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
              let currentTime = EHC.getCurrentUTC ""
              if DS.null initialState.data.dataExpiryAt || EHC.compareUTCDate currentTime initialState.data.dataExpiryAt >= 0 then do
                void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
                void $ lift $ lift $ toggleLoader true
                (API.DemandHotspotsResp demandHotspotsResponse) <- Remote.getDemandHotspotsBT ""
                liftFlowBT $ push $ DemandHotspotApiResponseAction (API.DemandHotspotsResp demandHotspotsResponse)
                void $ lift $ lift $ toggleLoader false
              else do 
                void $ lift $ lift $ delay $ Milliseconds 1000.0
                liftFlowBT $ push $ UpdateRefreshAnimation
              pure unit
            pure $ pure unit
        )
      ]
  , eval :
      ( \action state -> do
          let _ = spy "HotspotScreen state ----> " state
          let _ = spy "HotspotScreen action ----> " action
          eval action state
      ) 
  , parent : Nothing
  , logWhitelist: initialState.data.config.logWhitelistConfig.hotspotScreenLogWhitelist
  }


view :: forall w. (Action -> Effect Unit) -> ST.HotspotScreenState -> PrestoDOM (Effect Unit) w 
view push state = 
    let textAndImage = getTextAndImage state.props.selectedCircleColor state.data.config.hotspotConfig
    in
    Anim.screenAnimationFadeInOut $
    relativeLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , onBackPressed push (const BackPressed)
        , orientation VERTICAL
        ]
        [ headerLayout state push
        , linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          ]
          [ relativeLayout 
            [ width MATCH_PARENT
            , height MATCH_PARENT
            ]
            [ linearLayout
              [ width MATCH_PARENT
              , id (EHC.getNewIDWithTag "HotspotScreen")
              , afterRender
                  (\action -> do
                      void $ JB.showMap (EHC.getNewIDWithTag "HotspotScreen") true "hotspots" 13.0 0.0 0.0 push ShowMap
                      pure unit
                  ) (const AfterRender)
              ][]
            , lastUpdatedView state push
            ]
          ]
        ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , alignParentBottom "true,-1"
      , visibility $ boolToVisibility state.props.showNavigationSheet
      ]
      [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background Color.white900
        , padding $ Padding 16 16 16 16
        , orientation VERTICAL
        , cornerRadius 20.0
        ]
        [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation HORIZONTAL
          , gravity CENTER_VERTICAL
          , margin $ Margin 10 4 10 10
          ]
          [ imageView
            [ height $ V 20
            , width $ V 20
            , imageWithFallback $ fetchImage FF_ASSET $ textAndImage.imageName
            , margin $ MarginRight 10
            ]
          , textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text textAndImage.text
            , color Color.black900
            ] <> FontStyle.h2 TypoGraphy
          ]
        , textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text textAndImage.subText
          , color Color.black700
          , gravity LEFT
          , margin $ MarginHorizontal 10 10
          ] <> FontStyle.paragraphText TypoGraphy
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , margin $ MarginTop 20
          ] [ PrimaryButton.view (push <<< PrimaryButtonAC) (navigateButtonConfig state) ]
        ]
      ]
    ]


headerLayout :: forall w. ST.HotspotScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerLayout state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , background Color.white900
  ]
  [ linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation HORIZONTAL
    , layoutGravity "center_vertical"
    , gravity CENTER_VERTICAL
    , padding $ Padding 5 8 5 8
    ]
    [ imageView
      [ width $ V 40
      , height $ V 40
      , imageWithFallback $ fetchImage FF_ASSET $ "ny_ic_chevron_left"
      , gravity CENTER_VERTICAL
      , onClick push $ const BackButtonClicked
      , padding $ Padding 7 7 7 7
      , rippleColor Color.rippleShade
      , cornerRadius 20.0
      ]
    , textView $ 
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ getString HOTSPOTS
      , margin $ MarginLeft 10
      , weight 1.0
      , gravity CENTER_VERTICAL
      , color Color.black900
      , padding $ PaddingBottom 3
      ]
      <> FontStyle.h3 TypoGraphy
    ]
  , linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , background Color.greyLight
      ]
      []
  , linearLayout 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , background Color.white900
    , padding $ Padding 12 12 12 12
    ](map(\item -> hotspotsTypeView item ) hotspotTypes)
  , linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , background Color.greyLight
      ]
      []
  ]
  where hotspotTypes = [{text: getString VERY_HIGH, image: "ny_ic_very_high_hotspot_dashed"}, {text: getString HIGH, image: "ny_ic_high_hotspot_dashed"}, {text: getString MODERATE, image: "ny_ic_moderate_hotspot_dashed"}]

hotspotsTypeView :: forall w. {text :: String, image :: String} -> PrestoDOM (Effect Unit) w
hotspotsTypeView item = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER
  , weight 1.0
  ]
  [ imageView
    [ width $ V 20
    , height $ V 20
    , imageWithFallback $ fetchImage FF_ASSET item.image
    ]
  , textView $ 
    [ height WRAP_CONTENT 
    , width WRAP_CONTENT
    , margin $ MarginLeft 10
    , text item.text
    , color Color.black900
    ] <> FontStyle.body1 TypoGraphy
  ]

lastUpdatedView :: forall w. ST.HotspotScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
lastUpdatedView state push = 
  let lastUpdatedText = getString LAST_UPDATED <> EHC.convertUTCtoISC state.props.lastUpdatedTime "hh" <> ":" <> EHC.convertUTCtoISC state.props.lastUpdatedTime "mm" <> " " <> EHC.convertUTCtoISC state.props.lastUpdatedTime "a"
  in 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 12 12 12 12
  , background Color.white900
  , padding $ Padding 16 8 16 8
  , cornerRadius 22.0
  ]
  [ textView
  $ [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , text $ lastUpdatedText
    , weight 1.0
    , gravity CENTER_VERTICAL
    , color Color.black900
    , padding $ PaddingBottom 3
    ] <> FontStyle.body1 TypoGraphy
  , updateButtonIconAndText push state
  ]

updateButtonIconAndText :: forall w . (Action -> Effect Unit) -> ST.HotspotScreenState -> PrestoDOM (Effect Unit) w
updateButtonIconAndText push state =
  linearLayout
  [ width WRAP_CONTENT
  , height MATCH_PARENT
  , orientation HORIZONTAL
  , onClick (\action -> do
            void $ JB.getCurrentPosition push RefreshHotspots
            pure unit
          ) (const NoAction)
  , gravity CENTER
  , cornerRadius 6.0
  , rippleColor Color.rippleShade
  ]
  [ PrestoAnim.animationSet [Anim.rotateAnim (AnimConfig.rotateAnimConfig state.props.refreshAnimation)]
    $ imageView
    [ width $ V 17
    , height $ V 17
    , margin $ MarginRight 5
    , imageWithFallback $ fetchImage FF_ASSET "ny_ic_refresh"
    ]
  , textView $ 
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , text $ getString REFRESH_STRING
    , color Color.blue900
    ] <> FontStyle.body1 TypoGraphy
  ]