{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.ReferralScreen.View where

import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.Utils (adjustViewWithKeyboard, FetchImageFrom(..), fetchImage)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, ($), (<<<), (<>), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alpha, background, color, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, text, textFromHtml, textView, visibility, weight, width, imageWithFallback)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.ReferralScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Screens.ReferralScreen.ComponentConfig
import Storage (KeyStore(..), getValueToLocalStore)

screen :: ST.ReferralScreenState -> Screen Action ST.ReferralScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "ReferralScreen"
  , globalEvents: []
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , onBackPressed push (const $ BackPressed)
    , padding (PaddingVertical EHC.safeMarginTop EHC.safeMarginBottom)
    , afterRender
        ( \action -> do
            _ <- adjustViewWithKeyboard "false"
            push action
        )
        (const AfterRender)
    ]
    [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.grey900
        , visibility if state.config.nyBrandingVisibility then GONE else VISIBLE
        ]
        []
    , referralCodeView push state
    , thanksView push state
    ]

referralCodeView :: forall w. (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
referralCodeView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , padding (Padding 16 40 16 16)
    , visibility if state.showThanks then GONE else VISIBLE
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_referral"
        , gravity CENTER
        , height $ V 112
        , width $ V 140
        ]
    , textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text (getString REFEREAL_CODE_DISCRIPTION)
          , color Color.black600
          , gravity CENTER
          ]
        <> FontStyle.body1 TypoGraphy
    , PrimaryEditText.view (push <<< ReferralEditText) (primaryEditTextConfig state)
    , PrimaryButton.view (push <<< ContinueButtonAC) (continueButtonConfig state)
    , weightSeperatorView
    , referenceView push state
    ]

thanksView :: forall w. (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
thanksView push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , visibility if state.showThanks then VISIBLE else GONE
    , gravity CENTER
    , padding (Padding 16 0 16 16)
    ]
    [ weightSeperatorView
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_HORIZONTAL
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_thanks"
            , gravity CENTER
            , height $ V 230
            , width $ V 280
            ]
        ]
    , textView
        $ [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (MarginTop 16)
          , textFromHtml (getString REFERRAL_CODE_SUCCESSFULL)
          , color Color.black800
          , gravity CENTER
          ]
        <> FontStyle.h2 TypoGraphy
    , weightSeperatorView
    , PrimaryButton.view (push <<< GoToHomeButtonAC) (goToHomeButtonConfig state)
    ]

referenceView :: forall w. (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
referenceView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , background Color.grey900
    , cornerRadii $ Corners 16.0 true true true true
    , padding (Padding 16 16 16 16)
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , onClick push (const $ ExpandReference)
        , gravity CENTER_VERTICAL
        ]
        [ textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text (getString ABOUT_REFERRAL_PROGRAM)
              , padding (Padding 3 3 3 3)
              , color if state.isExpandReference then Color.black800 else Color.black700
              , gravity START
              ]
            <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , weight 1.0
            ]
            []
        , linearLayout
            [ width $ V 20
            , height $ V 20
            , gravity CENTER
            , alpha 0.7
            ]
            [ imageView
                [ height $ V 15
                , width $ V 20
                , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if state.isExpandReference then "ny_ic_chevron_up" else "ny_ic_chevron_down"
                ]
            ]
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , visibility if state.isExpandReference then VISIBLE else GONE
        , margin (MarginTop 8)
        , orientation VERTICAL
        ]
        [ textView
            $ [ width MATCH_PARENT
              , height WRAP_CONTENT
              , text (getString $ ABOUT_REFERRAL_PROGRAM_DISCRIPTION "ABOUT_REFERRAL_PROGRAM_DISCRIPTION")
              , color Color.black700
              , gravity START
              ]
            <> FontStyle.body3 TypoGraphy
        , textView
            $ [ width MATCH_PARENT
              , height WRAP_CONTENT
              , text (getString $ YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER")
              , color Color.black700
              , gravity START
              ]
            <> FontStyle.tags TypoGraphy
        ]
    ]

weightSeperatorView :: forall w. PrestoDOM (Effect Unit) w
weightSeperatorView =
  linearLayout
    [ width MATCH_PARENT
    , weight 1.0
    ]
    []
