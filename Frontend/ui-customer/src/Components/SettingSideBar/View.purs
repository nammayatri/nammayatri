{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SettingSideBar.View where

import Animation (translateInXSidebarAnim, translateOutXSidebarAnim)
import Components.SettingSideBar.Controller (Action(..), SettingSideBarState, Status(..), Tag(..), Item)
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth, safeMarginBottom, safeMarginTop, os)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, unit, ($), (*), (/), (<>), (==), (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, background, clickable, color, disableClickFeedback, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onAnimationEnd, onBackPressed, onClick, orientation, padding, text, textSize, textView, width, weight, ellipsize, maxLines, imageWithFallback)
import PrestoDOM.Animation as PrestoAnim
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Debug.Trace (spy)
import Common.Types.App

view :: forall w .  (Action  -> Effect Unit) -> SettingSideBarState -> PrestoDOM (Effect Unit) w
view push state =
   linearLayout [
    width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , gravity BOTTOM 
    , background Color.black9000
    , disableClickFeedback true
    , onClick push $ const OnClose
    , onBackPressed push $ const OnClose
    ][ PrestoAnim.animationSet 
      [ translateInXSidebarAnim $ state.opened == OPEN
      , translateOutXSidebarAnim $ state.opened == CLOSING
      ] $ linearLayout
          [ height MATCH_PARENT
          , width WRAP_CONTENT
          , background Color.black900
          , orientation VERTICAL
          , onAnimationEnd push $ if state.opened == CLOSING then const OnClosed else const NoAction
          , padding ( Padding 0 safeMarginTop 0 0 )
          ][  linearLayout
              [ width $ V ((screenWidth unit) / 10 * 8)
              , height MATCH_PARENT
              , background Color.white900
              , orientation VERTICAL
              , clickable true
              ][  profileView state push
                , settingsView state push
                , logoutView state push
                ]
            ]
      ]


------------------------------ settingsView --------------------------------
settingsView :: forall w. SettingSideBarState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
settingsView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding (Padding 18 24 18 24)
  , orientation VERTICAL
  ][
     settingsMenuView {imageUrl : "ny_ic_past_rides,https://assets.juspay.in/nammayatri/images/user/ny_ic_past_rides.png", text : (getString MY_RIDES), tag : SETTINGS_RIDES} push
    , settingsMenuView {imageUrl : "ny_ic_fav,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav.png", text : (getString FAVOURITES)  , tag : SETTINGS_FAVOURITES} push
    , settingsMenuView {imageUrl : "ny_ic_change_language,https://assets.juspay.in/nammayatri/images/user/ny_ic_change_language.png", text : (getString LANGUAGE), tag : SETTINGS_LANGUAGE} push
    , settingsMenuView {imageUrl : "ny_ic_help,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png", text : (getString HELP_AND_SUPPORT), tag : SETTINGS_HELP} push
    , settingsMenuView {imageUrl : "ny_ic_share,https://assets.juspay.in/nammayatri/images/user/ny_ic_share.png", text : (getString SHARE_APP), tag : SETTINGS_SHARE_APP} push
    , settingsMenuView {imageUrl : "ny_ic_info,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png", text : (getString ABOUT), tag : SETTINGS_ABOUT} push
  ]
  

------------------------------ logoutView --------------------------------
logoutView ::  forall w. SettingSideBarState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
logoutView state push =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ,  gravity BOTTOM
  , orientation VERTICAL
  , padding (Padding 0 0 0 safeMarginBottom)
  , margin (MarginBottom 10) 
  ][ linearLayout
      [ width MATCH_PARENT
      , height (V 1)
      , background Color.greyDark
      , margin (Margin 20 0 20 0)
      ][]
  , settingsMenuView {imageUrl : "ny_ic_logout,https://assets.juspay.in/nammayatri/images/user/ny_ic_logout.png", text : (getString LOGOUT_), tag : SETTINGS_LOGOUT} push
    ]

------------------------------ profileView --------------------------------
profileView :: forall w. SettingSideBarState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
profileView state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , background Color.black900
  , gravity CENTER_VERTICAL
  , padding (Padding 18 24 0 24)
  -- , onClick push (const EditProfile) TODO :: add profile view in future
  ][ imageView
      [ width ( V 48 )
      , height ( V 48 )
      , imageWithFallback "ny_ic_user,https://assets.juspay.in/nammayatri/images/user/ny_ic_user.png"
      ]
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , padding (PaddingLeft 16)
      , onClick push $ (const GoToMyProfile)
      ][ linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          , orientation HORIZONTAL
          , padding (PaddingRight 15)
          ][ textView
              ([ height WRAP_CONTENT
              , text if ((getValueToLocalStore USER_NAME) == "__failed" || (getValueToLocalStore USER_NAME) == "") then (getString USER) else (getValueToLocalStore USER_NAME) 
              , textSize FontSize.a_18
              , color Color.white900
              , fontStyle $ FontStyle.medium LanguageStyle
              , margin (MarginRight 5)
              , ellipsize true
              , maxLines 1 
              ] <> (if os == "IOS" then [ width (V (screenWidth unit /2))]else [weight 1.0]))
            , imageView
              [ width $ V 12
              , height (V 12)
              , imageWithFallback "ny_ic_chevron_right_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_right_white.png"
              ]
          ]
        , textView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text ("+91 " <> (getValueToLocalStore MOBILE_NUMBER))
          , textSize FontSize.a_14
          , color Color.profilePhoneNumber
          , fontStyle $ FontStyle.regular LanguageStyle
          ]
      ]]

------------------------------ settingsMenuView --------------------------------
settingsMenuView :: forall w. Item -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
settingsMenuView item push  = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , disableClickFeedback false
  , padding if item.tag == SETTINGS_LOGOUT  then (Padding 16 16 16 16 ) else (Padding 0 16 16 16 )
  , onClick push $ ( const case item.tag of 
                              SETTINGS_RIDES          -> PastRides 
                              SETTINGS_FAVOURITES     -> GoToFavourites
                              SETTINGS_HELP           -> OnHelp 
                              SETTINGS_LANGUAGE       -> ChangeLanguage 
                              SETTINGS_ABOUT          -> GoToAbout 
                              SETTINGS_LOGOUT         -> OnLogout
                              SETTINGS_SHARE_APP      -> ShareAppLink)
  ][  imageView
      [ width ( V 25 )
      , height ( V 25 )
      , imageWithFallback item.imageUrl
      ]
    , textView
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , text item.text
      , textSize FontSize.a_18
      , color Color.charcoalGrey
      , fontStyle $ FontStyle.medium LanguageStyle
      , padding (PaddingLeft 20)
      ]  
    ]

            