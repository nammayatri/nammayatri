{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SettingSideBar.Controller where

import Prelude (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Maybe (Maybe(..))
import MerchantConfig.Types (AppConfig)

data Action = ChangeLanguage
            | EditProfile
            | PastRides
            | GoToEmergencyContacts
            | GoToAbout
            | GoToNammaSafety
            | ShareAppLink
            | OnLogout
            | NoAction
            | OnHelp
            | OnClose
            | OnClosed
            | GoToFavourites
            | GoToMyTickets
            | GoToMyProfile
            | LiveStatsDashboard

data Status = OPEN | CLOSING | CLOSED

data Tag = SETTINGS_LOGOUT | SETTINGS_ABOUT | SETTINGS_FAVOURITES | SETTINGS_HELP | SETTINGS_LANGUAGE | SETTINGS_RIDES | SETTINGS_SHARE_APP | SETTINGS_LIVE_DASHBOARD | SETTINGS_TICKETS  | SETTINGS_NAMMASAFETY

derive instance genericStatus :: Generic Status _
instance eqStatus :: Eq Status where eq = genericEq

derive instance genericTag :: Generic Tag _
instance eqTag :: Eq Tag where eq = genericEq

type SettingSideBarState =
  { opened :: Status
  , name :: String
  , number :: String
  , email :: Maybe String
  , gender :: Maybe String
  , appConfig :: AppConfig
  , sideBarList :: Array String
  , isLocalPoliceSupportEnabled :: Boolean
  , hasCompletedSafetySetup :: Boolean
  }

type Item =
  { imageUrl :: String
  , text :: String
  , tag :: Tag
  , iconUrl :: String
  , accessibilityHint :: String
  }
