{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens where

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Prelude (class Eq, class Show, (==), ($))
import Data.Tuple(Tuple(..) ,snd, fst)
import Data.Maybe (fromMaybe, Maybe(..), maybe)
import Data.Array (find)


data ScreenName = SPLASH_SCREEN
                | CHOOSE_LANGUAGE_SCREEN
                | ENTER_MOBILE_NUMBER_SCREEN
                | ENTER_OTP_NUMBER_SCREEN
                | REGISTRATION_SCREEN
                | UPLOAD_DRIVING_LICENSE_SCREEN                
                | UPLOAD_AADHAR_SCREEN
                | ADD_VEHICLE_DETAILS_SCREEN
                | BANK_DETAILS_SCREEN
                | APPLICATION_STATUS_SCREEN
                | NEED_ACCESS_SCREEN
                | HOME_SCREEN
                | DRIVER_PROFILE_SCREEN
                | REPORT_ISSUE_CHAT_SCREEN
                | DRIVER_DETAILS_SCREEN
                | RIDE_HISTORY_SCREEN
                | RIDE_SELECTION_SCREEN
                | SELECT_LANGUAGE_SCREEN
                | HELP_AND_SUPPORT_SCREEN
                | VEHICLE_DETAILS_SCREEN
                | RIDE_DETAILS_SCREEN
                | EDIT_BANK_DETAILS_SCREEN
                | EDIT_AADHAR_DETAILS_SCREEN
                | TRIP_DETAILS_SCREEN
                | WRITE_TO_US_SCREEN
                | ABOUT_US_SCREEN
                | DRIVER_RIDE_RATING_SCREEN
                | REFERRAL_SCREEN
                | APP_UPDATE_POPUP_SCREEN
                | NO_INTERNET_SCREEN
                | POPUP_SCREEEN
                | ALERTS_SCREEN
                | ACKNOWLEDGEMENT_SCREEN
                | AADHAAR_VERIFICATION_SCREEN
                | SUBSCRIPTION_SCREEN
                | ONBOARDING_SUBSCRIPTION_SCREEN
                | DRIVER_SAVED_LOCATION_SCREEN
                | WELCOME_SCREEN
                | CHOOSE_CITY_SCREEN
                | DRIVER_EARNINGS_SCREEN

derive instance genericScreenName :: Generic ScreenName _
instance eqScreenName :: Eq ScreenName where eq = genericEq


screenNameMap :: Array (Tuple String ScreenName)
screenNameMap =
  [Tuple "splash_screen" SPLASH_SCREEN
  , Tuple "choose_langauge_screen" CHOOSE_LANGUAGE_SCREEN
  , Tuple "enter_mobile_screen" ENTER_MOBILE_NUMBER_SCREEN
  , Tuple "enter_otp_number_screen" ENTER_OTP_NUMBER_SCREEN
  , Tuple "registration_screen" REGISTRATION_SCREEN
  , Tuple "upload_driving_license_screen" UPLOAD_DRIVING_LICENSE_SCREEN
  , Tuple "upload_aadhar_screen" UPLOAD_AADHAR_SCREEN
  , Tuple "add_vehicle_details_screen" ADD_VEHICLE_DETAILS_SCREEN
  , Tuple "bank_details_screen" BANK_DETAILS_SCREEN
  , Tuple "application_status_screen" APPLICATION_STATUS_SCREEN
  , Tuple "need_access_screen" NEED_ACCESS_SCREEN
  , Tuple "home_screen" HOME_SCREEN
  , Tuple "driver_profile_screen" DRIVER_PROFILE_SCREEN
  , Tuple "report_issue_chat_screen" REPORT_ISSUE_CHAT_SCREEN
  , Tuple "driver_details_screen" DRIVER_DETAILS_SCREEN
  , Tuple "ride_history_screen" RIDE_HISTORY_SCREEN
  , Tuple "ride_selection_screen" RIDE_SELECTION_SCREEN
  , Tuple "select_langauge_screen" SELECT_LANGUAGE_SCREEN
  , Tuple "help_and_support_screen" HELP_AND_SUPPORT_SCREEN
  , Tuple "vehicle_details_screen" VEHICLE_DETAILS_SCREEN
  , Tuple "ride_details_screen" RIDE_DETAILS_SCREEN
  , Tuple "edit_bank_details_screen" EDIT_BANK_DETAILS_SCREEN
  , Tuple "edit_aadhar_details_screen" EDIT_AADHAR_DETAILS_SCREEN
  , Tuple "trip_details_screen" TRIP_DETAILS_SCREEN
  , Tuple "write_to_us_screen" WRITE_TO_US_SCREEN
  , Tuple "about_us_screen" ABOUT_US_SCREEN
  , Tuple "driver_ride_rating_screen" DRIVER_RIDE_RATING_SCREEN
  , Tuple "referral_screen" REFERRAL_SCREEN
  , Tuple "app_update_popup_screen" APP_UPDATE_POPUP_SCREEN
  , Tuple "no_internet_screen" NO_INTERNET_SCREEN
  , Tuple "popup_screen" POPUP_SCREEEN
  , Tuple "alerts_screen" ALERTS_SCREEN
  , Tuple "acknowledgement_screen" ACKNOWLEDGEMENT_SCREEN
  , Tuple "aadhaar_verification_screen" AADHAAR_VERIFICATION_SCREEN
  , Tuple "subscription_screen" SUBSCRIPTION_SCREEN
  , Tuple "onboarding_subscription_screen" ONBOARDING_SUBSCRIPTION_SCREEN
  , Tuple "driver_saved_location_screen" DRIVER_SAVED_LOCATION_SCREEN
  , Tuple "welcome_screen" WELCOME_SCREEN
  , Tuple "choose_city_screen" CHOOSE_CITY_SCREEN
  , Tuple "driver_earnings_screen" DRIVER_EARNINGS_SCREEN
  ]

getScreen :: ScreenName -> String
getScreen screenName = maybe ("") (\val -> (fst val)) $ find (\x -> (snd x) == screenName) screenNameMap

getScreenType :: String -> ScreenName
getScreenType str = maybe HOME_SCREEN (\val -> (snd val)) $ find (\x -> (fst x) == str) screenNameMap