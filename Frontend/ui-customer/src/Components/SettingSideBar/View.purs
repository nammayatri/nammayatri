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
import Common.Types.App (LazyCheck(..), City(..))
import Components.SettingSideBar.Controller (Action(..), SettingSideBarState, Status(..), Tag(..), Item)
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe(..))
import Data.String as DS
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth, safeMarginBottom, safeMarginTop, os, isPreviousVersion)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Prelude (Unit, const, unit, ($), (*), (/), (<>), (==), (||), (&&), (/=), map, not)
import Mobility.Prelude (boolToVisibility)
import PrestoDOM 
import PrestoDOM.Animation as PrestoAnim
import Screens.Types (Stage(..))
import Storage (getValueToLocalStore, KeyStore(..), isLocalStageOn)
import Styles.Colors as Color
import Data.Maybe (Maybe(..))
import Data.Array as DA
import Data.Function.Uncurried (runFn3)
import DecodeUtil (getAnyFromWindow)
import Data.Maybe (fromMaybe)
import Screens.Types (Stage(..))
import Data.String as DS
import Mobility.Prelude
import Debug
import Engineering.Helpers.Utils (getCityFromString)

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
  let city = getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION
      appName = fromMaybe "" $ runFn3 getAnyFromWindow "appName" Nothing Just
      isOdishaApp = appName == "Odisha Yatri" -- TODO: Need to make this city config instead of app config and replace hard coded values
  in 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding (Padding 8 24 8 8)
  , orientation VERTICAL
  ](map (\item -> 
        case item of
        "MyRides" -> settingsMenuView {imageUrl : fetchImage FF_ASSET "ic_past_rides", text : (getString MY_RIDES), accessibilityHint : "My Rides " ,tag : SETTINGS_RIDES, iconUrl : "", showNewTag : false} push
        "Tickets" -> settingsMenuView {imageUrl : fetchImage FF_ASSET "ny_ic_ticket_grey", text : getString MY_TICKETS, accessibilityHint : "Tickets", tag : SETTINGS_TICKETS, iconUrl : "", showNewTag : false} push
        "MetroTickets" -> if (DA.any (_ == city) [Kochi, Chennai, Delhi]) then settingsMenuView {imageUrl : fetchImage FF_ASSET "ny_ic_ticket_grey_small", text : getString MY_TICKETS, accessibilityHint : "Tickets", tag : SETTINGS_MY_METRO_TICKETS, iconUrl : "", showNewTag: true} push else linearLayout[visibility GONE][]
        "Favorites" -> if DA.any (\stage -> isLocalStageOn stage)  [RideStarted, RideAccepted, RideCompleted] then emptyLayout else settingsMenuView {imageUrl : fetchImage FF_ASSET "ic_fav", text : (getString FAVOURITES) , accessibilityHint : "Favourites " , tag : SETTINGS_FAVOURITES, iconUrl : "", showNewTag : false} push
        "NammaSafety" -> if state.appConfig.feature.enableSafetyFlow then settingsMenuView {imageUrl : fetchImage FF_ASSET "ny_ic_shield_heart", text : getString NAMMA_SAFETY, accessibilityHint : " Safety ", tag : SETTINGS_NAMMASAFETY, iconUrl : "", showNewTag : not state.hasCompletedSafetySetup} push else emptyLayout
        "HelpAndSupport" -> settingsMenuView {imageUrl : fetchImage FF_ASSET  "ny_ic_help", text :  if state.appConfig.feature.enableHelpAndSupport && not isOdishaApp then getString HELP_AND_SUPPORT else getString CONTACT_SUPPORT, accessibilityHint : "Help And Support", tag : SETTINGS_HELP, iconUrl : "", showNewTag : false} push
        "Language" -> settingsMenuView {imageUrl : fetchImage FF_ASSET "ic_change_language", text : (getString LANGUAGE), accessibilityHint : "Language ", tag : SETTINGS_LANGUAGE, iconUrl : "", showNewTag : false} push
        "ShareApp" -> settingsMenuView {imageUrl : fetchImage FF_ASSET "ic_share", text : (getString SHARE_AND_REFER), accessibilityHint : "Refer And Share ", tag : SETTINGS_SHARE_APP, iconUrl : "", showNewTag : false} push
        "LiveStatsDashboard" -> settingsMenuView {imageUrl : fetchImage FF_ASSET "ic_graph_black", accessibilityHint : "Live Stats Dashboard ",text : (getString LIVE_STATS_DASHBOARD), tag : SETTINGS_LIVE_DASHBOARD, iconUrl : liveStatsDashboardImage, showNewTag : false} push
        "About" -> settingsMenuView {imageUrl : fetchImage FF_ASSET "ic_info", text : (getString ABOUT), accessibilityHint : "About " , tag : SETTINGS_ABOUT, iconUrl : "", showNewTag : false} push
        "Logout" -> logoutView state push
        "Separator" -> separator
        _ -> emptyLayout
      ) state.appConfig.sideBarList
    )
  where
    liveStatsDashboardImage = 
      if getValueToLocalStore LIVE_DASHBOARD /= "LIVE_DASHBOARD_SELECTED" 
        then fetchImage FF_ASSET "ic_red_icon"
        else ""

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
  , settingsMenuView {imageUrl : fetchImage FF_ASSET "ic_logout", text : (getString LOGOUT_), accessibilityHint : "Logout", tag : SETTINGS_LOGOUT, iconUrl : "", showNewTag : false} push
    ]

------------------------------ profileView --------------------------------
profileView :: forall w. SettingSideBarState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
profileView state push =
  let profileStatusVisibility = case state.appConfig.showProfileStatus, profileCompleteValue state of
                  true , "100" -> GONE
                  false, _ -> GONE
                  _, _ -> VISIBLE
  in 
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
      , accessibilityHint "Close Menu Bar"
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
              , imageWithFallback state.appConfig.profileArrowImage
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
        , visibility $ profileStatusVisibility
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
  , padding (Padding 10 16 16 16 )
  , accessibilityHint $ item.accessibilityHint <> " : Button"
  , rippleColor Color.rippleShade
  , cornerRadius 12.0
  , onClick push $ ( const case item.tag of
                              SETTINGS_RIDES          -> PastRides
                              SETTINGS_TICKETS        -> GoToMyTickets
                              SETTINGS_FAVOURITES     -> GoToFavourites
                              SETTINGS_HELP           -> OnHelp
                              SETTINGS_LANGUAGE       -> ChangeLanguage
                              SETTINGS_ABOUT          -> GoToAbout
                              SETTINGS_NAMMASAFETY    -> GoToNammaSafety
                              SETTINGS_LOGOUT         -> OnLogout
                              SETTINGS_SHARE_APP      -> ShareAppLink
                              SETTINGS_LIVE_DASHBOARD -> LiveStatsDashboard
                              SETTINGS_MY_METRO_TICKETS -> GoToMyMetroTickets)
  , accessibility case item.tag of
                              SETTINGS_RIDES          -> ENABLE
                              SETTINGS_TICKETS        -> ENABLE
                              SETTINGS_FAVOURITES     -> ENABLE
                              SETTINGS_HELP           -> ENABLE
                              SETTINGS_LANGUAGE       -> ENABLE
                              SETTINGS_ABOUT          -> ENABLE
                              SETTINGS_LOGOUT         -> ENABLE
                              SETTINGS_SHARE_APP      -> DISABLE_DESCENDANT
                              SETTINGS_NAMMASAFETY       -> ENABLE
                              SETTINGS_LIVE_DASHBOARD -> DISABLE_DESCENDANT
                              SETTINGS_MY_METRO_TICKETS -> ENABLE
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
      , visibility $ boolToVisibility $ not $ DS.null item.iconUrl 
      , margin ( Margin 6 1 0 0)
      , imageWithFallback item.iconUrl
      ]
    , textView $
      [ text $ getString NEW <> "✨"
      , color Color.white900
      , padding $ Padding 5 3 5 3
      , background Color.blue900
      , visibility $ boolToVisibility item.showNewTag
      , margin $ MarginLeft 4
      , cornerRadius 4.0
      ] <> FontStyle.body19 TypoGraphy
    ]

profileCompleteValue :: SettingSideBarState -> String
profileCompleteValue state =
    case state.email , state.gender of
      Nothing, Nothing  -> "50"
      Nothing, Just _   -> "75"
      Just _ , Nothing  -> "75"
      Just _ , Just _   -> "100"
