{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens where

import Data.Array (find)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..), snd, fst)
import Prelude (class Eq, (==), ($))

data ScreenName = SPLASH_SCREEN
                | CHOOSE_LANGUAGE_SCREEN
                | ENTER_MOBILE_NUMBER_SCREEN
                | HOME_SCREEN
                | PERMISSION_SCREEN
                | MY_PROFILE_SCREEN
                | MY_RIDES_SCREEN
                | SELECT_LANGUAGE_SCREEN
                | HELP_AND_SUPPORT_SCREEN
                | ABOUT_US_SCREEN
                | EMERGENCY_CONTACS_SCREEN
                | ACCOUNT_SET_UP_SCREEN
                | CONTACT_US_SCREEN
                | TRIP_DETAILS_SCREEN
                | INVOICE_SCREEN
                | RIDE_RATING_SCREEN
                | EDIT_PROFILE_SCREEN
                | ADD_NEW_ADDRESS_SCREEN
                | SAVED_LOCATION_SCREEN
                | SUCCESS_SCREEN
                | REFERRAL_SCREEN
                | APP_UPDATE_POPUP_SCREEN
                | ENTER_OTP_NUMBER_SCREEN
                | WELCOME_SCREEN
                | TICKET_BOOKING_SCREEN
                | RIDE_SCHEDULED_SCREEN
                | RIDE_SELECTION_SCREEN
                | REPORT_ISSUE_CHAT_SCREEN
                | SEARCH_LOCATION_SCREEN
                | NAMMASAFETY_SCREEN
                | METRO_TICKET_BOOKING_SCREEN
                | RENTAL_SCREEN

derive instance genericScreenName :: Generic ScreenName _
instance eqScreenName :: Eq ScreenName where eq = genericEq


screenNameMap :: Array (Tuple String ScreenName)
screenNameMap =
  [ Tuple "report_issue_chat_screen" REPORT_ISSUE_CHAT_SCREEN
  , Tuple "ride_selection_screen" RIDE_SELECTION_SCREEN
  , Tuple "splash_screen" SPLASH_SCREEN
  , Tuple "choose_language_screen" CHOOSE_LANGUAGE_SCREEN
  , Tuple "enter_mobile_number_screen" ENTER_MOBILE_NUMBER_SCREEN
  , Tuple "home_screen" HOME_SCREEN
  , Tuple "permission_screen" PERMISSION_SCREEN
  , Tuple "my_profile_screen" MY_PROFILE_SCREEN
  , Tuple "my_rides_screen" MY_RIDES_SCREEN
  , Tuple "select_language_screen" SELECT_LANGUAGE_SCREEN
  , Tuple "help_and_support_screen" HELP_AND_SUPPORT_SCREEN
  , Tuple "about_us_screen" ABOUT_US_SCREEN
  , Tuple "emergency_contacts_screen" EMERGENCY_CONTACS_SCREEN
  , Tuple "account_set_up_screen" ACCOUNT_SET_UP_SCREEN
  , Tuple "contact_us_screen" CONTACT_US_SCREEN
  , Tuple "trip_details_screen" TRIP_DETAILS_SCREEN
  , Tuple "invoice_screen" INVOICE_SCREEN
  , Tuple "ride_rating_screen" RIDE_RATING_SCREEN
  , Tuple "edit_profile_screen" EDIT_PROFILE_SCREEN
  , Tuple "add_new_address_screen" ADD_NEW_ADDRESS_SCREEN
  , Tuple "saved_location_screen" SAVED_LOCATION_SCREEN
  , Tuple "success_screen" SUCCESS_SCREEN
  , Tuple "referral_screen" REFERRAL_SCREEN
  , Tuple "app_update_popup_screen" APP_UPDATE_POPUP_SCREEN
  , Tuple "enter_otp_number_screen" ENTER_OTP_NUMBER_SCREEN
  , Tuple "welcome_screen" WELCOME_SCREEN
  , Tuple "ticket_booking_screen" TICKET_BOOKING_SCREEN
  , Tuple "ride_scheduled_screen" RIDE_SCHEDULED_SCREEN
  ]

getScreen :: ScreenName -> String
getScreen screenName = maybe ("") (\val -> (fst val)) $ find (\x -> (snd x) == screenName) screenNameMap

getScreenType :: String -> ScreenName
getScreenType str = maybe HOME_SCREEN (\val -> (snd val)) $ find (\x -> (fst x) == str) screenNameMap
