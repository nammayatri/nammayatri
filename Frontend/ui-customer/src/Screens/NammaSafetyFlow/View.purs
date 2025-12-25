{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.View where

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
import Prelude (Unit, const, discard, pure, void, show, ($), (<>), (+), (<<<), (>), (&&), unit, bind, map, (/=), (==))
import PrestoDOM
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Screens.NammaSafetyFlow.Controller (Action(..), ScreenOutput, eval, getBannerConfigs)
import Screens.NammaSafetyFlow.ComponentConfig as CC
import Screens.Types (NammaSafetyScreenState, SafetyStepsConfig)
import Styles.Colors as Color
import Components.GenericHeader as GenericHeader
import PrestoDOM.List (ListItem, preComputeListItem)
import CarouselHolder as CarouselHolder
import Components.LargeBannerCarousel as LargeBannerCarousel
import Effect.Aff (launchAff)
import Presto.Core.Types.Language.Flow (Flow)
import Debug
import Font.Style as FontStyle
import Mobility.Prelude (boolToVisibility)
import Services.Backend as Remote
import Services.API as API
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)
import Language.Strings (getString)
import Language.Types (STR(..))
import Storage (getValueToLocalStore, KeyStore(..))

screen :: NammaSafetyScreenState -> Screen Action NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "NammaSafetyFlow"
  , globalEvents:
      [ ( \push -> do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ computeListItem push
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ getSafetySettings push UpdateSafetySettings
            pure $ pure unit
        )
      ]
  , eval:
      ( \action state -> do
          let
            _ = spy "NammaSafetyFlow state -----" state
          let
            _ = spy "NammaSafetyFlow--------action" action
          eval action state
      )
  }

getSafetySettings :: forall action. (action -> Effect Unit) -> (API.GetEmergencySettingsRes -> action) -> Flow GlobalState Unit
getSafetySettings push action = do
  eiResponse <- Remote.getEmergencySettings ""
  case eiResponse of
    Right response -> do
      doAff do liftEffect $ push $ action response
      pure unit
    Left err -> pure unit

computeListItem :: (Action -> Effect Unit) -> Flow GlobalState Unit
computeListItem push = do
  bannerItem <- preComputeListItem $ LargeBannerCarousel.view push (LargeBannerCarousel.config BannerCarousel)
  void $ EHC.liftFlow $ push (SetBannerItem bannerItem)

view ::
  forall w.
  (Action -> Effect Unit) ->
  NammaSafetyScreenState ->
  PrestoDOM (Effect Unit) w
view push state =
  screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , onBackPressed push $ const BackPressed
        , orientation VERTICAL
        , padding padding'
        , background Color.white900
        , afterRender push (const AfterRender)
        ]
        [ GenericHeader.view (push <<< GenericHeaderActionController) (CC.genericHeaderConfig state)
        , scrollView
            [ width MATCH_PARENT
            , scrollBarY false
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , accessibility DISABLE
                    ]
                    $ maybe ([]) (\item -> [ bannersCarousal item state push ]) state.data.bannerData.bannerItem
                , safetySetupSection state push
                , textView
                    $ [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , text $ getString MORE_SAFETY_MEASURES
                      , padding $ Padding 16 16 16 16
                      , color Color.black900
                      ]
                    <> FontStyle.h3 TypoGraphy
                , linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , orientation VERTICAL
                    ]
                    (map (\item -> listItem push item state) state.data.extraSafetyExplaination)
                ]
            ]
        ]
  where 
    padding' = if EHC.os == "IOS" then (Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 16 else EHC.safeMarginBottom)) else (PaddingLeft 0)

safetySetupSection :: forall w. NammaSafetyScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
safetySetupSection state push =
  let
    filteredSafetySteps = Array.filter (\step -> getString step.title /= getString SAFETY_DRILL) state.data.safetySetupSteps
    safetySteps = if getValueToLocalStore IS_SOS_ACTIVE == "true" 
                    then filteredSafetySteps
                    else state.data.safetySetupSteps
    stepsCompleted = show $ foldl (\acc step -> if step.isCompleted then acc + 1 else acc) 0 safetySteps
    totalSteps = show $ Array.length safetySteps
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      [ linearLayout -- Heading/Title
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation HORIZONTAL
          , padding $ Padding 16 16 16 16
          ]
          [ textView
              $ [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , weight 1.0
                , padding $ Padding 0 0 0 0
                , text $ getString SAFETY_SETUP
                , color Color.black900
                ]
              <> FontStyle.h1 TypoGraphy
          , textView
              $ [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , padding $ Padding 0 0 0 0
                , text $ stepsCompleted <> "/" <> totalSteps <> " " <> (getString COMPLETE)
                , color Color.black700
                ]
              <> FontStyle.body2 TypoGraphy
          ]
      , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          ]
          (map (\item -> listItem push item state) safetySteps)
      ]

listItem :: forall w. (Action -> Effect Unit) -> SafetyStepsConfig -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
listItem push item state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ Margin 16 0 16 16
    , cornerRadius 9.0
    , background Color.blue900
    , onClick push $ const $ SafetyNavigation item
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , padding $ Padding 12 12 12 12
        , cornerRadius 8.0
        , stroke $ "1," <> Color.grey900
        , background Color.white900
        , gravity CENTER_VERTICAL
        ]
        [ linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , margin (MarginRight 14)
            ]
            [ imageView
                [ imageWithFallback $ fetchImage GLOBAL_COMMON_ASSET $ if item.isCompleted then item.prefixImageCompleted else item.prefixImage
                , width $ V 38
                , height $ V 38
                ]
            ]
        , textView
            $ [ text $ getString item.title
              , color Color.black800
              , weight 1.0
              ]
            <> FontStyle.body1 TypoGraphy
        , imageView
            [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_chevron_right"
            , width (V 20)
            , height (V 20)
            ]
        ]
    , textView
        $ [ text $ getString $ fromMaybe NEXT item.labelText
          , color Color.white900
          , visibility $ boolToVisibility $ isJust item.labelText
          , height WRAP_CONTENT
          , padding $ Padding 12 6 12 6
          , width WRAP_CONTENT
          ]
        <> FontStyle.body6 TypoGraphy
    ]

bannersCarousal :: forall w. ListItem -> NammaSafetyScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bannersCarousal view state push =
  let
    banners = getBannerConfigs state BannerCarousel

    len = Array.length banners
  in
    if len > 0 then
      linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ MarginTop 12
        , accessibility DISABLE
        ]
        [ CarouselHolder.carouselView push $ carouselConfigTransform view state banners ]
    else
      dummyView state

dummyView :: forall w. NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
dummyView state =
  linearLayout
    [ height $ V 0
    , width $ V 0
    ]
    []

carouselConfigTransform ∷ ListItem → NammaSafetyScreenState → Array (LargeBannerCarousel.Config (LargeBannerCarousel.Action → Action)) → CarouselHolder.CarouselHolderConfig LargeBannerCarousel.PropConfig Action
carouselConfigTransform view state banners =
  { view
  , items: LargeBannerCarousel.bannerTransformer banners
  , orientation: HORIZONTAL
  , currentPage: state.data.bannerData.currentPage
  , autoScroll: state.data.config.bannerCarousel.enableAutoScroll
  , autoScrollDelay: state.data.config.bannerCarousel.autoScrollDelay
  , id: "bannerCarousel"
  , autoScrollAction: Just UpdateBanner
  , onPageSelected: Just BannerChanged
  , onPageScrollStateChanged: Just BannerStateChanged
  , onPageScrolled: Nothing
  , currentIndex: state.data.bannerData.currentBanner
  , showScrollIndicator: true
  , layoutHeight: if EHC.os == "IOS" then V 265 else V 137
  , overlayScrollIndicator: false
  }
