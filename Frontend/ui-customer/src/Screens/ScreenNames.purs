{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens where

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

getScreen :: ScreenName -> String
getScreen str = case str of 
        REPORT_ISSUE_CHAT_SCREEN      -> "report_issue_chat_screen"
        RIDE_SELECTION_SCREEN         -> "ride_selection_screen"
        SPLASH_SCREEN                 -> "splash_screen"
        CHOOSE_LANGUAGE_SCREEN        -> "choose_language_screen"
        ENTER_MOBILE_NUMBER_SCREEN    -> "enter_mobile_number_screen"
        HOME_SCREEN                   -> "home_screen"
        PERMISSION_SCREEN             -> "permission_screen"
        MY_PROFILE_SCREEN             -> "my_profile_screen"
        MY_RIDES_SCREEN               -> "my_rides_screen"
        SELECT_LANGUAGE_SCREEN        -> "select_language_screen"
        HELP_AND_SUPPORT_SCREEN       -> "help_and_support_screen"
        ABOUT_US_SCREEN               -> "about_us_screen"
        EMERGENCY_CONTACS_SCREEN      -> "emergency_contacts_screen"
        ACCOUNT_SET_UP_SCREEN         -> "account_set_up_screen"
        CONTACT_US_SCREEN             -> "contact_us_screen"
        TRIP_DETAILS_SCREEN           -> "trip_details_screen"
        INVOICE_SCREEN                -> "invoice_screen"
        RIDE_RATING_SCREEN            -> "ride_rating_screen"
        EDIT_PROFILE_SCREEN           -> "edit_profile_screen"
        ADD_NEW_ADDRESS_SCREEN        -> "add_new_address_screen"
        SAVED_LOCATION_SCREEN         -> "saved_location_screen"
        SUCCESS_SCREEN                -> "success_screen"
        REFERRAL_SCREEN               -> "referral_screen"
        APP_UPDATE_POPUP_SCREEN       -> "app_update_popup_screen"
        ENTER_OTP_NUMBER_SCREEN       -> "enter_otp_number_screen"
        WELCOME_SCREEN                -> "welcome_screen"
        TICKET_BOOKING_SCREEN         -> "ticket_booking_screen"
        RIDE_SCHEDULED_SCREEN         -> "ride_scheduled_screen" 
        SEARCH_LOCATION_SCREEN        -> "search_location_screen"