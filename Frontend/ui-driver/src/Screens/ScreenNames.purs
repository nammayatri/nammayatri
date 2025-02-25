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
import Prelude (class Eq, class Show)


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
                | LMS_VIDEO_SCREEN
                | LMS_QUIZ_SCREEN
                | RATE_CARD_SCREEN
                | CUSTOMER_REFERRAL_TRACKER_SCREEN
                | UPLOAD_PARCEL_IMAGE_SCREEN
                | METER_SCREEN

derive instance genericScreenName :: Generic ScreenName _
instance eqScreenName :: Eq ScreenName where eq = genericEq

getScreen :: ScreenName -> String
getScreen str = case str of
    SPLASH_SCREEN                      -> "splash_screen"
    CHOOSE_LANGUAGE_SCREEN             -> "choose_langauge_screen"
    ENTER_MOBILE_NUMBER_SCREEN         -> "enter_mobile_screen"
    ENTER_OTP_NUMBER_SCREEN            -> "enter_otp_number_screen"
    REGISTRATION_SCREEN                -> "registration_screen"
    UPLOAD_DRIVING_LICENSE_SCREEN      -> "upload_driving_license_screen"
    UPLOAD_AADHAR_SCREEN               -> "upload_aadhar_screen"
    ADD_VEHICLE_DETAILS_SCREEN         -> "add_vehicle_details_screen"
    BANK_DETAILS_SCREEN                -> "bank_details_screen"
    APPLICATION_STATUS_SCREEN          -> "application_status_screen"
    NEED_ACCESS_SCREEN                 -> "need_access_screen"
    HOME_SCREEN                        -> "home_screen"
    DRIVER_PROFILE_SCREEN              -> "driver_profile_screen"
    REPORT_ISSUE_CHAT_SCREEN           -> "report_issue_chat_screen"
    DRIVER_DETAILS_SCREEN              -> "driver_details_screen"
    RIDE_HISTORY_SCREEN                -> "ride_history_screen"
    RIDE_SELECTION_SCREEN              -> "ride_selection_screen"
    SELECT_LANGUAGE_SCREEN             -> "select_langauge_screen"
    HELP_AND_SUPPORT_SCREEN            -> "help_and_support_screen"
    VEHICLE_DETAILS_SCREEN             -> "vehicle_details_screen"
    RIDE_DETAILS_SCREEN                -> "ride_details_screen"
    EDIT_BANK_DETAILS_SCREEN           -> "edit_bank_details_screen"
    EDIT_AADHAR_DETAILS_SCREEN         -> "edit_aadhar_details_screen"
    TRIP_DETAILS_SCREEN                -> "trip_details_screen"
    WRITE_TO_US_SCREEN                 -> "write_to_us_screen"
    ABOUT_US_SCREEN                    -> "about_us_screen"
    DRIVER_RIDE_RATING_SCREEN          -> "driver_ride_rating_screen"
    REFERRAL_SCREEN                    -> "referral_screen"
    APP_UPDATE_POPUP_SCREEN            -> "app_update_popup_screen"
    NO_INTERNET_SCREEN                 -> "no_internet_screen"
    POPUP_SCREEEN                      -> "popup_screen"
    ALERTS_SCREEN                      -> "alerts_screen"
    ACKNOWLEDGEMENT_SCREEN             -> "acknowledgement_screen"
    AADHAAR_VERIFICATION_SCREEN        -> "aadhaar_verification_screen"
    SUBSCRIPTION_SCREEN                -> "subscription_screen"
    ONBOARDING_SUBSCRIPTION_SCREEN     -> "onboarding_subscription_screen"
    DRIVER_SAVED_LOCATION_SCREEN       -> "driver_saved_location_screen"
    WELCOME_SCREEN                     -> "welcome_screen"
    CHOOSE_CITY_SCREEN                 -> "choose_city_screen"
    DRIVER_EARNINGS_SCREEN             -> "driver_earnings_screen"
    LMS_VIDEO_SCREEN                   -> "lms_video_screen"
    LMS_QUIZ_SCREEN                    -> "lms_quiz_screen"
    RATE_CARD_SCREEN                   -> "rate_card_screen"
    CUSTOMER_REFERRAL_TRACKER_SCREEN   -> "customer_referral_tracker_screen"
    UPLOAD_PARCEL_IMAGE_SCREEN         -> "upload_parcel_image_screen"
    METER_SCREEN                       -> "meter_screen"
