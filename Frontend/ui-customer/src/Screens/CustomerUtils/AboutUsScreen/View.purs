{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.AboutUsScreen.View where

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.ComplaintsModel as ComplaintsModel
import Components.GenericHeader as GenericHeader
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Merchant.Utils (getValueFromConfig)
import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (<>), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, lineHeight, linearLayout, margin, onBackPressed, onClick, orientation, padding, text, textSize, textView, visibility, weight, width, scrollView, scrollBarY)
import Screens.AboutUsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.CustomerUtils.AboutUsScreen.ComponentConfig (genericHeaderConfig)
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color

screen :: ST.AboutUsScreenState -> Screen Action ST.AboutUsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "AboutUsScreen"
  , globalEvents: []
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> ST.AboutUsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push (const BackPressed)
        , background Color.white900
        , padding if EHC.os == "IOS" then (Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom) else (Padding 0 0 0 10)
        , gravity CENTER_HORIZONTAL
        , afterRender push (const AfterRender)
        ]
        [ GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
        , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.greySmoke
            ]
            []
        , scrollView
            [ width MATCH_PARENT
            , scrollBarY false
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                [ topTextView push state
                , linearLayout
                    [ orientation VERTICAL
                    , weight 1.0
                    ]
                    []
                , bottomLinksView state
                ]
            ]
        ]

--------------------------------------------------- topTextView -----------------------------------------------------
topTextView :: (Action -> Effect Unit) -> ST.AboutUsScreenState -> forall w. PrestoDOM (Effect Unit) w
topTextView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (Padding 20 0 20 10)
    ]
    [ logoView state
    , textView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , textSize FontSize.a_16
        , text (getString ABOUT_APP_DESCRIPTION)
        , color Color.black800
        , fontStyle $ FontStyle.regular LanguageStyle
        , gravity LEFT
        , lineHeight "22"
        , margin (Margin 0 40 0 32)
        ]
    , ComplaintsModel.view (ComplaintsModel.config { cardData = contactUsData state })
    , linearLayout
        [ gravity LEFT
        , width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ softwareLicenseView
        , termsAndConditionsView state
        , privacyPolicyView state
        ]
    ]

--------------------------------------------------- logoView -----------------------------------------------------
logoView :: forall w. ST.AboutUsScreenState -> PrestoDOM (Effect Unit) w
logoView state =
  linearLayout
    [ height $ V 48
    , width MATCH_PARENT
    , margin (MarginTop 48)
    , gravity CENTER
    , cornerRadius 10.0
    ]
    [ imageView
        [ height $ V 48
        , width $ V 48
        , imageWithFallback "ic_launcher,https://assets.juspay.in/nammayatri/images/common/ny_ic_launcher.png"
        ]
    ]

--------------------------------------------------- bottomLinksView -----------------------------------------------------
bottomLinksView :: ST.AboutUsScreenState -> forall w. PrestoDOM (Effect Unit) w
bottomLinksView state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    , orientation VERTICAL
    , margin (Margin 0 10 0 10)
    ]
    [ textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ "v" <> (getValueToLocalStore VERSION_NAME) <> " [ " <> (getValueToLocalStore BUNDLE_VERSION) <> " ]"
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color "#354052"
        ]
    ]

--------------------------------------------------- softwareLicenseView -----------------------------------------------------
softwareLicenseView :: forall w. PrestoDOM (Effect Unit) w
softwareLicenseView =
  linearLayout
    [ height WRAP_CONTENT
    , orientation VERTICAL
    , width WRAP_CONTENT
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , orientation VERTICAL
        , width WRAP_CONTENT
        , visibility GONE
        ]
        [ textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text (getString SOFTWARE_LICENSE)
            , textSize FontSize.a_14
            , fontStyle $ FontStyle.regular LanguageStyle
            , color Color.blue900
            , margin (Margin 0 0 0 0)
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height (V 1)
            , background Color.blue900
            ]
            []
        ]
    ]

--------------------------------------------------- termsAndConditionsView -----------------------------------------------------      
termsAndConditionsView :: ST.AboutUsScreenState -> forall w. PrestoDOM (Effect Unit) w
termsAndConditionsView state =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getString TERMS_AND_CONDITIONS)
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.regular LanguageStyle
        , color Color.blue900
        , onClick
            ( \action -> do
                _ <- pure action
                _ <- JB.openUrlInApp $ getValueFromConfig "DOCUMENT_LINK"
                pure unit
            )
            (const TermsAndConditions)
        , margin (Margin 0 20 0 0)
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height (V 1)
        , background Color.blue900
        ]
        []
    ]

--------------------------------------------------- privacyPolicyView -----------------------------------------------------
privacyPolicyView :: forall w. ST.AboutUsScreenState -> PrestoDOM (Effect Unit) w
privacyPolicyView state =
  linearLayout
    [ height WRAP_CONTENT
    , orientation VERTICAL
    , width WRAP_CONTENT
    ]
    [ textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getString PRIVACY_POLICY)
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.regular LanguageStyle
        , color Color.blue900
        , margin (Margin 0 20 0 0)
        , onClick
            ( \action -> do
                _ <- pure action
                _ <- JB.openUrlInApp $ getValueFromConfig "PRIVACY_POLICY_LINK"
                pure unit
            )
            (const PrivacyPolicy)
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height (V 1)
        , background Color.blue900
        ]
        []
    ]

contactUsData :: ST.AboutUsScreenState -> Array ComplaintsModel.CardData
contactUsData state =
  [ { title: (getString CORPORATE_ADDRESS)
    , subTitle: (getString CORPORATE_ADDRESS_DESCRIPTION)
    , addtionalData: Just (getString CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL)
    }
  , { title: (getString REGISTERED_ADDRESS)
    , subTitle: (getString REGISTERED_ADDRESS_DESCRIPTION)
    , addtionalData: Just (getString REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL)
    }
  ]
