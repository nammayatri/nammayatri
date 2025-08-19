{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.AboutUsScreen.View where

import ConfigProvider
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.ComplaintsModel as ComplaintsModel
import Components.PopUpModal as PopUpModal
import Data.Function.Uncurried (runFn3)
import Data.Maybe (Maybe(..), fromMaybe)
import DecodeUtil (getAnyFromWindow)
import Effect (Effect)
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, ($), (<>), (==), bind, pure, unit, (<<<), (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, LoggableScreen, Visibility(..), afterRender, background, color, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, text, textView, visibility, weight, width, cornerRadius, rippleColor)
import Prim.TypeError (Above)
import Screens.AboutUsScreen.ComponentConfig (demoModePopUpConfig)
import Screens.AboutUsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color

screen :: ST.AboutUsScreenState -> LoggableScreen Action ST.AboutUsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "AboutUsScreen"
  , globalEvents: []
  , eval
  , parent: Nothing
  , logWhitelist : initialState.appConfig.logWhitelistConfig.aboutUsScreenLogWhitelist
  }

view ::
  forall w.
  (Action -> Effect Unit) ->
  ST.AboutUsScreenState ->
  PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , onBackPressed push (const BackPressed state.props.demoModePopup)
        , afterRender push (const AfterRender)
        ]
        ( [ linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              ]
              [ headerLayout state push
              , scrollView
                  [ width MATCH_PARENT
                  , height MATCH_PARENT
                  , scrollBarY false
                  ]
                  [ linearLayout
                      [ width MATCH_PARENT
                      , height MATCH_PARENT
                      , orientation VERTICAL
                      ]
                      [ applicationInformationLayout state push
                      , linearLayout
                          [ width MATCH_PARENT
                          , weight 1.0
                          , gravity BOTTOM
                          ]
                          [ footerView state ]
                      ]
                  ]
              ]
          ]
            <> if state.props.demoModePopup then [ demoModePopUpView push state ] else []
        )

demoModePopUpView :: forall w. (Action -> Effect Unit) -> ST.AboutUsScreenState -> PrestoDOM (Effect Unit) w
demoModePopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ PopUpModal.view (push <<< PopUpModalDemoModeAction) (demoModePopUpConfig state) ]

------------------------------------------------------------------- headerLayout ------------------------------------------------
headerLayout :: ST.AboutUsScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
headerLayout state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , layoutGravity "center_vertical"
        , padding (Padding 5 12 5 12)
        ]
        [ imageView
            [ width $ V 40
            , height $ V 40
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_back"
            , gravity CENTER_VERTICAL
            , onClick push (const $ BackPressed state.props.demoModePopup)
            , padding (Padding 10 10 10 10)
            , margin (MarginLeft 5)
            , cornerRadius 20.0
            , rippleColor Color.rippleShade
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height MATCH_PARENT
              , text (getString ABOUT)
              , margin (MarginLeft 10)
              , color Color.black
              , weight 1.0
              , gravity CENTER_VERTICAL
              ]
            <> FontStyle.h3 TypoGraphy
        ]
    , horizontalLine 0 0
    ]

--------------------------------------- footerView ----------------------
footerView :: ST.AboutUsScreenState -> forall w. PrestoDOM (Effect Unit) w
footerView state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin (MarginBottom 30)
    ]
    [ textView
        $ [ width MATCH_PARENT
          , height WRAP_CONTENT
          , text $ "v" <> (getValueToLocalStore VERSION_NAME) <> "-[" <> (getValueToLocalStore BUNDLE_VERSION) <> "]"
          , color Color.black800
          , gravity CENTER
          ]
        <> FontStyle.paragraphText TypoGraphy
    ]

--------------------------------- applicationInformationLayout ----------------------------
applicationInformationLayout :: ST.AboutUsScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
applicationInformationLayout state push =
  let appName = fromMaybe state.appConfig.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
  in
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin (MarginTop 20)
    , padding (PaddingHorizontal 20 20)
    ]
    [ imageView
        ( [ width $ V 150
          , height $ V 100
          , layoutGravity "center_horizontal"
          , imageWithFallback $ fetchImage FF_ASSET "ic_launcher"
          ]
            <> if getValueToLocalStore DRIVER_STATUS == "true" then [ onClick push (const ShowDemoPopUp) ] else []
        )
    , textView
        $ [ width MATCH_PARENT
          , height WRAP_CONTENT
          , text $ appName <> getString ABOUT_TEXT
          , color Color.black800
          , gravity LEFT
          , margin (MarginTop 20)
          , padding (Padding 20 0 20 0)
          ]
        <> FontStyle.body5 TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , visibility if state.appConfig.showCorporateAddress || state.appConfig.showRegisteredAddress then VISIBLE else GONE
        ][ComplaintsModel.view (ComplaintsModel.config { cardData = contactUsData state })]
    , underlinedTextView (getString T_C) push
    , underlinedTextView (getString PRIVACY_POLICY) push
    ]

--------------------------------- underlinedTextView ----------------------
underlinedTextView :: String -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
underlinedTextView value push =
  let config = getAppConfig appConfig
  in
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , onClick
        ( \action -> do
            _ <- push action
            _ <-
              JB.openUrlInApp
                if (value == (getString T_C)) then
                  config.termsLink
                else
                  config.privacyLink
            pure unit
        )
        (const TermsAndConditionAction)
    , margin (Margin 20 30 0 0)
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text value
          , color Color.primaryBlue
          ]
        <> FontStyle.body5 TypoGraphy
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.primaryBlue
        , margin (Margin 1 0 2 0)
        ]
        []
    ]

-------------------------------------- horizontalLine ---------------------
horizontalLine :: Int -> Int -> forall w. PrestoDOM (Effect Unit) w
horizontalLine marginLeft marginRight =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.greyBackDarkColor
    , margin (Margin marginLeft 0 marginRight 15)
    ]
    []

contactUsData :: ST.AboutUsScreenState -> Array ComplaintsModel.CardData
contactUsData state =
  []
    <> (if state.appConfig.showCorporateAddress then
          [ { title: (getString $ CORPORATE_ADDRESS "CORPORATE_ADDRESS")
            , subTitle: (getString $ CORPORATE_ADDRESS_DESCRIPTION "CORPORATE_ADDRESS_DESCRIPTION")
            , addtionalData: Just (getString $ CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL")
            }
          ] 
        else
          [])
    <> (if state.appConfig.showRegisteredAddress then
          [ { title: (getString $ REGISTERED_ADDRESS "REGISTERED_ADDRESS")
            , subTitle: (getString $ REGISTERED_ADDRESS_DESCRIPTION "REGISTERED_ADDRESS_DESCRIPTION")
            , addtionalData: Just (getString $ REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL")
            }
          ]
        else
          [])
