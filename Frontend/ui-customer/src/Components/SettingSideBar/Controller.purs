module Components.SettingSideBar.Controller where

import Prelude (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)

data Action = ChangeLanguage
            | EditProfile
            | PastRides
            | GoToAbout
            | ShareAppLink
            | OnLogout
            | NoAction
            | OnHelp
            | OnClose
            | OnClosed
            | GoToFavourites
            | GoToMyProfile

data Status = OPEN | CLOSING | CLOSED

data Tag = SETTINGS_LOGOUT | SETTINGS_ABOUT | SETTINGS_FAVOURITES | SETTINGS_HELP | SETTINGS_LANGUAGE | SETTINGS_RIDES | SETTINGS_SHARE_APP

derive instance genericStatus :: Generic Status _
instance eqStatus :: Eq Status where eq = genericEq

derive instance genericTag :: Generic Tag _
instance eqTag :: Eq Tag where eq = genericEq

type SettingSideBarState =
  { opened :: Status
  , name :: String 
  , number :: String
  }

type Item =
  { imageUrl :: String
  , text :: String
  , tag :: Tag
  }