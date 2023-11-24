{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SettingSideBar.View where

import Common.Types.App

import Animation (translateInXSidebarAnim, translateOutXSidebarAnim)
import Common.Types.App (LazyCheck(..))
import Components.SettingSideBar.Controller (Action(..), SettingSideBarState, Status(..), Tag(..), Item)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth, safeMarginBottom, safeMarginTop, os, isPreviousVersion)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Prelude (Unit, const, unit, ($), (*), (/), (<>), (==), (||), (&&), (/=), map)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), Accessiblity(..), PrestoDOM, visibility, background, clickable, color, disableClickFeedback, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onAnimationEnd, onBackPressed, onClick, orientation, padding, text, textSize, textView, width, weight, ellipsize, maxLines, imageWithFallback, scrollView, scrollBarY, accessibility, accessibilityHint)
import PrestoDOM.Animation as PrestoAnim
import Storage (getValueToLocalStore, KeyStore(..), isLocalStageOn)
import Styles.Colors as Color
import Data.Maybe (Maybe(..))
import Common.Types.App (LazyCheck(..))
import Data.Array as DA
import Screens.Types (Stage(..))
import Data.String as DS

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
          , background state.appConfig.profileBackground
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
                , scrollView
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , scrollBarY true
                  ]
                  [
                    settingsView state push
                  ]
                ]
            ]
      ]


------------------------------ settingsView --------------------------------
settingsView :: forall w. SettingSideBarState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
settingsView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding (Padding 18 24 18 8)
  , orientation VERTICAL
  ](map (\item -> 
        case item of
        "MyRides" -> settingsMenuView {imageUrl : fetchImage FF_ASSET "ic_past_rides", text : (getString MY_RIDES), accessibilityHint : "My Rides " ,tag : SETTINGS_RIDES, iconUrl : ""} push
        "Tickets" -> settingsMenuView {imageUrl : fetchImage FF_ASSET "ny_ic_ticket_grey", text : getString MY_TICKETS, accessibilityHint : "Tickets", tag : SETTINGS_TICKETS, iconUrl : ""} push
        "Favorites" -> if DA.any (\stage -> isLocalStageOn stage)  [RideStarted, RideAccepted, RideCompleted] then emptyLayout else settingsMenuView {imageUrl : fetchImage FF_ASSET "ic_fav", text : (getString FAVOURITES) , accessibilityHint : "Favourites " , tag : SETTINGS_FAVOURITES, iconUrl : ""} push
        "EmergencyContacts" ->  settingsMenuView {imageUrl : fetchImage FF_COMMON_ASSET "ny_ic_emergency_contacts" , text : (getString EMERGENCY_CONTACTS) , accessibilityHint : "Emergency Contacts " , tag : SETTINGS_EMERGENCY_CONTACTS, iconUrl : ""} push
        "HelpAndSupport" -> settingsMenuView (helpAndSupportConfig state.appConfig.features.enableSupport) push
        "Language" -> settingsMenuView {imageUrl : fetchImage FF_ASSET "ic_change_language", text : (getString LANGUAGE), accessibilityHint : "Language ", tag : SETTINGS_LANGUAGE, iconUrl : ""} push
        "ShareApp" -> settingsMenuView {imageUrl : fetchImage FF_ASSET "ic_share", text : (getString SHARE_APP), accessibilityHint : "Share App ", tag : SETTINGS_SHARE_APP, iconUrl : ""} push
        "LiveStatsDashboard" -> settingsMenuView {imageUrl : fetchImage FF_ASSET "ic_graph_black", accessibilityHint : "Live Stats Dashboard ",text : (getString LIVE_STATS_DASHBOARD), tag : SETTINGS_LIVE_DASHBOARD, iconUrl : fetchImage FF_ASSET  "ic_red_icon"} push
        "About" -> settingsMenuView {imageUrl : fetchImage FF_ASSET "ic_info", text : (getString ABOUT), accessibilityHint : "About " , tag : SETTINGS_ABOUT, iconUrl : ""} push
        "Logout" -> logoutView state push
        "Separator" -> separator
        _ -> emptyLayout
      ) state.appConfig.sideBarList
    )

helpAndSupportConfig :: Boolean -> Item
helpAndSupportConfig enableContactSupport = {
  imageUrl : fetchImage FF_ASSET $  if enableContactSupport then "ic_help" else  "ny_ic_help_menu" ,
  text : if enableContactSupport then (getString HELP_AND_SUPPORT) else  (getString HELP) ,
  accessibilityHint : if enableContactSupport then "Help And Support" else "Help",
  tag : SETTINGS_HELP, 
  iconUrl : ""
}

getPreviousVersion :: String -> String 
getPreviousVersion _ = 
  if os == "IOS" then 
    case getMerchant FunctionCall of 
      NAMMAYATRI -> "1.2.5"
      YATRISATHI -> "0.0.0"
      _ -> "0.0.0"
    else do 
      case getMerchant FunctionCall of 
        NAMMAYATRI -> "1.2.1"
        YATRISATHI -> "0.0.0"
        _ -> "0.0.0"

separator :: forall w. PrestoDOM (Effect Unit) w
separator = linearLayout
      [ width MATCH_PARENT
      , height (V 1)
      , background Color.grey900
      , margin ( MarginVertical 8 8 )
      ][]
------------------------------ emptylayout --------------------------------
emptyLayout = linearLayout
              [ height $ V 0
              , width $ V 0
              , visibility GONE
              ][]
------------------------------ logoutView --------------------------------
logoutView ::  forall w. SettingSideBarState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
logoutView state push =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin ( MarginBottom 100 )
  ][ linearLayout
      [ width MATCH_PARENT
      , height (V 1)
      , background Color.grey900
      , margin ( MarginVertical 8 8 )
      ][]
  , settingsMenuView {imageUrl : fetchImage FF_ASSET "ic_logout", text : (getString LOGOUT_), accessibilityHint : "Logout", tag : SETTINGS_LOGOUT, iconUrl : ""} push
    ]

------------------------------ profileView --------------------------------
profileView :: forall w. SettingSideBarState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
profileView state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , background state.appConfig.profileBackground
  , gravity CENTER_VERTICAL
  , padding (Padding 18 24 0 24)
  -- , onClick push (const EditProfile) TODO :: add profile view in future
  ][ imageView
      [ width ( V 48 )
      , height ( V 48 )
      , accessibilityHint "Close Menu Bar Button"
      , accessibility ENABLE
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_user"
      , onClick push $ (const OnClose)
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
              , color state.appConfig.profileName
              , margin (MarginRight 5)
              , ellipsize true
              , maxLines 1
              ] <> (if os == "IOS" then [ width (V (screenWidth unit /2))]else [weight 1.0]) <> FontStyle.body13 TypoGraphy)
            , imageView
              [ width $ V 22
              , height (V 22)
              , color state.appConfig.profileName
              , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right_white"
              ]
          ]
        , textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , accessibilityHint $ DS.replaceAll (DS.Pattern "") (DS.Replacement " ") (getValueToLocalStore MOBILE_NUMBER)
          , accessibility ENABLE
          , text ((getValueToLocalStore COUNTRY_CODE) <> " " <> (getValueToLocalStore MOBILE_NUMBER))
          , color state.appConfig.profileName
          ] <> FontStyle.paragraphText TypoGraphy
        , linearLayout[
          height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , margin $ MarginTop 4
        , visibility case profileCompleteValue state of
            "100" -> GONE
            _ -> VISIBLE
        ][textView $
          [ text $ (getString PROFILE_COMPLETION) <> ":"
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , color state.appConfig.profileCompletion
          ] <> FontStyle.body3 TypoGraphy
        , imageView
          [ imageWithFallback $ fetchImage FF_ASSET $ case profileCompleteValue state of
              "50" -> "ic_50_percent"
              "75" -> "ic_75_percent"
              _    -> ""
          , height $ V 10
          , width $ V 10
          , margin $ Margin 4 4 4 2
          ]
        , textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , color state.appConfig.profileCompletion
          , text $ (profileCompleteValue state) <> " %"
          ] <> FontStyle.body3 TypoGraphy
        ]
      ]]

------------------------------ settingsMenuView --------------------------------
settingsMenuView :: forall w. Item -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
settingsMenuView item push  =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , disableClickFeedback false
  , padding (Padding 0 16 16 16 )
  , accessibilityHint $ item.accessibilityHint <> " : Button"
  , onClick push $ ( const case item.tag of
                              SETTINGS_RIDES          -> PastRides
                              SETTINGS_TICKETS        -> GoToMyTickets
                              SETTINGS_FAVOURITES     -> GoToFavourites
                              SETTINGS_HELP           -> OnHelp
                              SETTINGS_LANGUAGE       -> ChangeLanguage
                              SETTINGS_ABOUT          -> GoToAbout
                              SETTINGS_LOGOUT         -> OnLogout
                              SETTINGS_SHARE_APP      -> ShareAppLink
                              SETTINGS_EMERGENCY_CONTACTS       -> GoToEmergencyContacts
                              SETTINGS_LIVE_DASHBOARD -> LiveStatsDashboard)
  , accessibility case item.tag of
                              SETTINGS_RIDES          -> ENABLE
                              SETTINGS_TICKETS        -> ENABLE
                              SETTINGS_FAVOURITES     -> ENABLE
                              SETTINGS_HELP           -> ENABLE
                              SETTINGS_LANGUAGE       -> ENABLE
                              SETTINGS_ABOUT          -> ENABLE
                              SETTINGS_LOGOUT         -> ENABLE
                              SETTINGS_SHARE_APP      -> DISABLE_DESCENDANT
                              SETTINGS_EMERGENCY_CONTACTS       -> ENABLE
                              SETTINGS_LIVE_DASHBOARD -> DISABLE_DESCENDANT
  ][  imageView
      [ width ( V 25 )
      , height ( V 25 )
      , imageWithFallback item.imageUrl
      ]
    , textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text item.text
      , color Color.charcoalGrey
      , padding (PaddingLeft 20)
      ] <> FontStyle.body13 TypoGraphy
    , imageView
      [ width ( V 8 )
      , height ( V 8 )
      , visibility if item.tag == SETTINGS_LIVE_DASHBOARD && getValueToLocalStore LIVE_DASHBOARD /= "LIVE_DASHBOARD_SELECTED" then VISIBLE else GONE
      , margin ( Margin 6 1 0 0)
      , imageWithFallback item.iconUrl
      ]
    ]

profileCompleteValue :: SettingSideBarState -> String
profileCompleteValue state =
    case state.email , state.gender of
      Nothing, Nothing  -> "50"
      Nothing, Just _   -> "75"
      Just _ , Nothing  -> "75"
      Just _ , Just _   -> "100"
