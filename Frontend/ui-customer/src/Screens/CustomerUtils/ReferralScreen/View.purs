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
import Components.Referral as ReferralComponent
import Effect (Effect)
import Engineering.Helpers.Commons  as EHC
import Helpers.Utils ( FetchImageFrom(..), fetchImage, generateQR)
import Helpers.Referral (generateReferralLink)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, ($), (<<<), (<>), (==), (&&), pure, unit, (/=), (-), (/), (*))
import PrestoDOM
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.ReferralScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Screens.ReferralScreen.ComponentConfig
import Storage (KeyStore(..), getValueToLocalStore)
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Engineering.Helpers.Commons
import Font.Size as FontSize
import Font.Style as FontStyle
import Debug (spy)
import Effect.Uncurried (runEffectFn4)
import Data.Maybe (Maybe(..))

screen :: ST.ReferralScreenState -> Screen Action ST.ReferralScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "ReferralScreen"
  , globalEvents: []
  , eval:
      \action state -> do
        let _ = spy "ReferralScreen action " action
        let _ = spy "ReferralScreen state " state
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  ][  linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , background Color.white900
      , orientation VERTICAL
      , onBackPressed push (const $ BackPressed)
      , padding (PaddingVertical EHC.safeMarginTop EHC.safeMarginBottom)
      , adjustViewWithKeyboard "true"
      , afterRender
          ( \action -> do
              push action
          )(const AfterRender)
      ]
      [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
      , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.grey900
        , visibility if state.config.nyBrandingVisibility then GONE else VISIBLE 
        ][]
      , if state.referralType == ST.GET_REFERRED then
          enterReferralCodeView push state
        else
          showReferralCodeView push state 
      ]
    , if state.showQRCodePopUp then appQRCodeView push state else dummyView
    , referralComponentView push state
  ]

showReferralCodeView :: forall w. (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
showReferralCodeView push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  ][ linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , margin $ Margin 16 16 16 16
     , background Color.yellow900
     , cornerRadius 12.0
     , orientation VERTICAL
     ][ tabView push state
      , referralCodeViews push state
      , shareAndReferView push state 
      -- , seperatorView -- to be enabled when referred user data is available from API
      -- , referredUserData push state
      ]
    -- , enterNowReferralCodeView push state -- may need in future
   ]

tabView :: forall w. (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
tabView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 20 16 20 16
  , background Color.white300
  , cornerRadius if EHC.os == "IOS" then 20.0 else 100.0
  , gravity CENTER
  ][  imageView
      [ height $ V 30
      , width $ V 30
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_new_avatar_profile_customer"
      ]
    , textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , color Color.black900
      , margin $ Margin 10 10 10 10
      , text $ getString REFER_YOUR_FRIENDS
      ] <> FontStyle.body2 TypoGraphy
   ]

referralCodeViews :: forall w. (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
referralCodeViews push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER
  , orientation VERTICAL
  ][  textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , color Color.black900
      , text $ getString YOUR_REFERRAL_CODE
      , margin $ Margin 4 4 4 0
      ] <> FontStyle.paragraphText TypoGraphy
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ Margin 4 0 4 8
      , gravity CENTER
      , onClick push $ const CopyToClipboard
      ][ textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , color Color.black900
        , text state.referralCode
        ] <> FontStyle.h0 TypoGraphy
      , imageView
        [ width $ V 20
        , height $ V 20
        , margin $ MarginLeft 5
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_copy"
        ]
      ]
  ]

shareAndReferView :: forall w. (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
shareAndReferView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginBottom 15
  , padding $ PaddingHorizontal 16 16
  , gravity CENTER
  ][ sharePillView push ShareAndRefer state (getString SHARE_AND_REFER) "ny_ic_share_grey" (MarginRight 8)
   , sharePillView push ShowQR state (getString SHOW_APP_QR) "ny_ic_qr_code" (MarginRight 0)
  ]

sharePillView :: forall w. (Action -> Effect Unit) -> Action -> ST.ReferralScreenState -> String -> String -> Margin -> PrestoDOM (Effect Unit) w
sharePillView push action state text' image' margin' =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , weight 1.0
  , onClick push (const action)
  , cornerRadius if EHC.os == "IOS" then 15.0 else 100.0
  , padding $ Padding 16 5 16 5
  , background Color.white900
  , gravity CENTER_VERTICAL
  , margin margin'
  ][  imageView
      [ height $ V 20
      , width $ V 20
      , imageWithFallback $ fetchImage FF_COMMON_ASSET image'
      ]
    , textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , color Color.black900
      , text text'
      , padding $ PaddingLeft 10
      ] <> FontStyle.body24 TypoGraphy
   ]

seperatorView :: forall w. PrestoDOM (Effect Unit) w
seperatorView = 
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , margin $ Margin 20 8 20 16
  , background Color.white900
  ][]

referredUserData :: forall w. (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
referredUserData push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 20 0 20 16
  ][  linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity LEFT
      ][  linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER_VERTICAL
          , onClick push $ const ReferredUserInfoAction
          ][  imageView
              [ height $ V 15
              , width $ V 15
              , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_star_black"
              ]
            , textView $
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text $ getString REFERRED_USERS
              , margin $ MarginLeft 5
              ] <> FontStyle.body20 TypoGraphy
            , imageView
              [ height $ V 15
              , width $ V 15
              , margin $ MarginLeft 5
              , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_info_black"
              ]
           ] 
       ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity RIGHT
      ][  textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text "0"
          ] <> FontStyle.body23 TypoGraphy
       ]
   ]

enterNowReferralCodeView :: forall w. (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
enterNowReferralCodeView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ Margin 16 16 16 0
  ][  textView $
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , color Color.black900
      , text $ getString ENTER_REFERRAL_CODE_
      ] <> FontStyle.h2 TypoGraphy
    , textView $
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , color Color.black900
      , margin $ MarginTop 10
      , text $ getString GOT_A_REFERRAL_FROM_A_DRIVER_OR_FRIEND
      ] <> FontStyle.body2 TypoGraphy
    , PrimaryButton.view (push <<< ReferralButton) (referralButtonConfig state)
   ]

------------------------------------------------------------------------------------- enter referral code view -----------------------------------------------
enterReferralCodeView :: forall w. (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
enterReferralCodeView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][  referralCodeView push state
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


appQRCodeView :: forall w. (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
appQRCodeView push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , gravity CENTER
    , background Color.blackLessTrans
    , clickable true
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER
        , cornerRadius 16.0
        , background state.config.popupBackground
        , margin $ Margin 20 10 20 0
        , padding $ Padding 24 12 24 12
        ]
        [ imageView
          [ width $ if EHC.os == "IOS" then V $ (EHC.screenWidth unit) * 3 / 4 else MATCH_PARENT
          , height $ V 280
          , gravity CENTER
          , id (getNewIDWithTag "ExpandedReferralQRCode")
          , padding (Padding 5 5 5 5)
          , afterRender (\action -> do
                          runEffectFn4 generateQR (generateReferralLink (getValueToLocalStore CUSTOMER_LOCATION) "share" "referral" "refer" state.referralCode) (getNewIDWithTag "ExpandedReferralQRCode") 280 0
                        ) (const RenderQRCode)
          ]
        , PrimaryButton.view (push <<< QRCodeAction) (primaryButtonConfig state)
        ]
    ]

referralComponentView :: forall w. (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
referralComponentView push state =
  ReferralComponent.view (push <<< ReferralComponentAction) state.referralComponentProps

dummyView :: forall w. PrestoDOM (Effect Unit) w
dummyView = linearLayout [ visibility GONE ] []