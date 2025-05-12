{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.OperationHubScreen.View where

import Animation as Anim
import Data.Maybe
import Debug
import Effect (Effect)
import Prelude
import PrestoDOM
import PrestoDOM.Types.Core
import Screens.Types as ST
import Screens.OperationHubScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.OperationHubScreen.ComponentConfig
import Components.PrimaryButton as PrimaryButton
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Engineering.Helpers.Commons
import Styles.Colors as Color
import Helpers.Utils
import Components.OptionsMenu as OptionsMenu
import Components.BottomDrawerList as BottomDrawerList
import Common.Types.App as CTA
import Screens.RegistrationScreen.ComponentConfig (logoutPopUp)
import Components.PopUpModal as PopUpModal
import Font.Style as FontStyle
import PrestoDOM.Animation as PrestoAnim
import Data.Array as DA
import Engineering.Helpers.Commons as EHC
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))
import Services.API as API
import Screens.OperationHubScreen.ScreenData
import Resource.Localizable.StringsV2
import Mobility.Prelude
import Resource.Localizable.TypesV2 as LT2

screen :: ST.OperationHubScreenState -> Screen Action ST.OperationHubScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "OperationHubScreen"
  , globalEvents: []
  , eval:
      \action state -> do
        let
          _ = spy "OperationHubScreen ----- state" state
        let
          _ = spy "OperationHubScreen --------action" action
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> ST.OperationHubScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , onBackPressed push $ const BackPressed
        ]
        [ Keyed.relativeLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            ]
            $ []
            <> (if not state.props.showOptions then [ Tuple "operationHubView" $ operationHubView push state ] else [ Tuple "operationHubScrollView" $ operationHubView push state ])
            <> [ Tuple "PrimaryButton"
                  $ linearLayout
                      [ height MATCH_PARENT
                      , width MATCH_PARENT
                      , gravity BOTTOM
                      ]
                      [ PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state) ]
              ]
            <> if state.props.contactSupportModal /= ST.HIDE then
                [ Tuple "bottomDrawerList" $ BottomDrawerList.view (push <<< BottomDrawerListAC) (bottomDrawerListConfig state) ]
              else
                [ Tuple "" $ linearLayout [] [] ]
                  <> if state.props.menuOptions then
                      [ Tuple "menuOptionModal" $ menuOptionModal push state ]
                    else
                      []
                        <> if state.props.logoutModalView then [ Tuple "logoutModal" $ logoutModal push state ] else []
        -- ,  if state.props.contactSupportModal /= ST.HIDE then BottomDrawerList.view (push <<< BottomDrawerListAC) (bottomDrawerListConfig state) else linearLayout[][]
        -- ,  if state.props.menuOptions then menuOptionModal push state else linearLayout[][]
        -- ,  if state.props.logoutModalView then logoutModal push state else linearLayout[][]
        ]

operationHubScrollView :: forall w. (Action -> Effect Unit) -> ST.OperationHubScreenState -> PrestoDOM (Effect Unit) w
operationHubScrollView push state =
  scrollView
    [ height $ V $ EHC.screenHeight unit
    , width MATCH_PARENT
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , onBackPressed push $ const BackPressed
        ]
        [ AppOnboardingNavBar.view (push <<< AppOnboardingNavBarAC) (appOnboardingNavBarConfig state)
        , linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , margin $ Margin 0 20 0 0
            ]
            [ contentLayout push state ]
        ]
    ]

operationHubView :: forall w. (Action -> Effect Unit) -> ST.OperationHubScreenState -> PrestoDOM (Effect Unit) w
operationHubView push state =
  linearLayout
    [ height $ V $ EHC.screenHeight unit
    , width MATCH_PARENT
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        ]
        [ AppOnboardingNavBar.view (push <<< AppOnboardingNavBarAC) (appOnboardingNavBarConfig state)
        , linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , margin $ Margin 0 20 0 0
            ]
            [ contentLayout push state ]
        ]
    ]

menuOptionModal :: forall w. (Action -> Effect Unit) -> ST.OperationHubScreenState -> PrestoDOM (Effect Unit) w
menuOptionModal push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , padding $ PaddingTop 55
    ]
    [ OptionsMenu.view (push <<< OptionsMenuAction) (optionsMenuConfig state) ]

logoutModal :: forall w. (Action -> Effect Unit) -> ST.OperationHubScreenState -> PrestoDOM (Effect Unit) w
logoutModal push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blackLessTrans
    ]
    [ PopUpModal.view (push <<< PopUpModalLogoutAction) (logoutPopUp CTA.Language) ]

contentLayout :: forall w. (Action -> Effect Unit) -> ST.OperationHubScreenState -> PrestoDOM (Effect Unit) w
contentLayout push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (Padding 16 0 16 0)
    ]
    [ hubItem push state
    , if (isNothing state.data.selectedHub || state.props.showOptions) then linearLayout [] [] else selectedHubItemView push state
    , relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        ]
        [ linearLayout
            [ height MATCH_PARENT -- $ V $ EHC.screenHeight unit
            , width MATCH_PARENT
            , orientation VERTICAL
            , margin $ Margin 16 16 16 0
            , gravity BOTTOM
            ]
            [ textView
                $ [ text $ getStringV2 LT2.visit_your_nearest_hub_to_complete_a_short_vehicle_inspection_to_start_earning_with_us
                  , textSize 14
                  , color Color.black800
                  , margin $ MarginBottom 8
                  , gravity BOTTOM
                  ]
                <> (FontStyle.body2 CTA.TypoGraphy)
            , textView
                $ [ text $ getStringV2 LT2.you_will_be_contacted_by_your_selected_hub_within_24_hours
                  , color Color.black800
                  , padding $ Padding 16 16 16 16
                  , margin $ MarginTop 8
                  , background Color.yellow300
                  , width MATCH_PARENT
                  , height WRAP_CONTENT
                  , cornerRadius 8.0
                  , gravity BOTTOM
                  ]
                <> (FontStyle.body3 CTA.TypoGraphy)
            , linearLayout [ height WRAP_CONTENT, width WRAP_CONTENT, visibility INVISIBLE ] [ PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfigInvisible state) ]
            ]
        , if state.props.showOptions then dropDownView push state else linearLayout [] []
        ]
    ]

hubItem :: forall w. (Action -> Effect Unit) -> ST.OperationHubScreenState -> PrestoDOM (Effect Unit) w
hubItem push state =
  let
    (API.OperationHub hub) = fromMaybe dummyOperationHub state.data.selectedHub
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , gravity CENTER
      , padding (Padding 16 16 16 16)
      , margin (MarginBottom 8)
      , background state.data.config.themeColors.radioInactiveBackground
      , cornerRadius 8.0
      , onClick push $ const $ ShowOptions
      ]
      [ textView
          [ text $ maybe ("Operation Hub 123") (\_ -> hub.name) state.data.selectedHub
          , height WRAP_CONTENT
          , textSize 16
          , color if isNothing state.data.selectedHub then Color.greyDarker else Color.black900
          , weight 1.0
          ]
      , imageView
          [ imageWithFallback $ fetchImage FF_ASSET (if state.props.showOptions then "ny_ic_arrow_up" else "ny_ic_arrow_down")
          , height $ V 24
          , width $ V 24
          , margin $ Margin 0 0 8 0
          ]
      ]

selectedHubItemView :: forall w. (Action -> Effect Unit) -> ST.OperationHubScreenState -> PrestoDOM (Effect Unit) w
selectedHubItemView push state =
  let
    (API.OperationHub hub) = fromMaybe dummyOperationHub state.data.selectedHub
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.grey100
      , padding (Padding 16 16 16 16)
      , margin $ MarginVertical 8 8
      , cornerRadius 8.0
      , orientation VERTICAL
      ]
      [ textView
          [ text $ getStringV2 LT2.address
          , height WRAP_CONTENT
          , color Color.grey600
          ]
      , textView
          [ text hub.address
          , height WRAP_CONTENT
          , color Color.black800
          , margin $ MarginTop 8
          ]
      , textView
          [ text $ (getStringV2 LT2.contact_number) <> " : " <> hub.mobileNumber
          , height WRAP_CONTENT
          , color Color.black800
          , margin $ MarginTop 8
          ]
      , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation HORIZONTAL
          , gravity LEFT
          , margin $ MarginTop 8
          , layoutGravity "center_vertical"
          , onClick push $ const $ OpenMaps
          ]
          [ imageView
              [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_locate_on_map_purple"
              , height $ V 20
              , width $ V 20
              , margin $ Margin 0 0 8 0
              ]
          , textView
              [ text $ getStringV2 LT2.locate_on_map
              , height WRAP_CONTENT
              , color state.data.config.themeColors.highlightedTextColor
              , gravity CENTER
              ]
          ]
      ]

dropDownView :: forall w. (Action -> Effect Unit) -> ST.OperationHubScreenState -> PrestoDOM (Effect Unit) w
dropDownView push state =
  let
    operationHubList = fromMaybe [] state.data.operationHubList
  in
    PrestoAnim.animationSet
      ([] <> [ Anim.listExpandingAnimation $ listExpandingAnimationConfig state.props.showOptions ])
      $ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ]
          [ linearLayout
              [ weight 1.0
              , width MATCH_PARENT
              ]
              [ scrollView
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  ]
                  [ linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , margin $ MarginVertical 8 8
                      , background Color.grey700
                      , orientation VERTICAL
                      , stroke $ "1," <> Color.grey900
                      , visibility $ if state.props.showOptions then VISIBLE else GONE
                      -- , onAnimationEnd push $ AnimationEnd
                      , cornerRadius 8.0
                      ]
                      ( DA.mapWithIndex
                          ( \index item -> do
                              let
                                (API.OperationHub hub) = item
                              linearLayout
                                [ height WRAP_CONTENT
                                , width MATCH_PARENT
                                , onClick push $ const $ HubSelected item
                                , accessibility ENABLE
                                , orientation VERTICAL
                                ]
                                [ textView
                                    [ text hub.name
                                    , color Color.black900
                                    , margin $ Margin 16 15 16 15
                                    ]
                                , linearLayout
                                    [ height $ V 1
                                    , width MATCH_PARENT
                                    , background Color.grey900
                                    , visibility $ boolToVisibility $ index /= (DA.length operationHubList - 1)
                                    , margin $ MarginHorizontal 16 16
                                    ]
                                    []
                                ]
                          )
                          (operationHubList)
                      )
                  ]
              ]
          , linearLayout [ height WRAP_CONTENT, width WRAP_CONTENT, visibility INVISIBLE ] [ PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfigInvisible state) ]
          ]
