module Resource.Localizable.StringsV2 where

import Prelude (($))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons, class Lacks)
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))
import Resource.Localizable.BN (getBn)
import Resource.Localizable.EN (getEn)
import Resource.Localizable.HI (getHi)
import Resource.Localizable.KN (getKn)
import Resource.Localizable.ML (getMl)
import Resource.Localizable.TA (getTa)
import Resource.Localizable.TE (getTe)
import Resource.Localizable.OD (getOd)
import Resource.Localizable.TypesV2
import Locale.Utils
import Language.Types

stringsMap :: Languages
stringsMap =
  Languages
    { english: getEn
    , hindi: getHi
    , malayalam: getMl
    , bengali: getBn
    , tamil: getTa
    , kannada: getKn
    , telugu: getTe
    , odiya: getOd
    }

infixl 1 readFromNT as @~

readFromNT ::
  forall l a r r_ t.
  Cons l a r_ r =>
  IsSymbol l =>
  Lacks l r_ =>
  Newtype t (Record r) =>
  t -> (Proxy l) -> a
readFromNT a label = unsafeGet (reflectSymbol label) $ unwrap a

getStringV2 ::
  forall l r_ r.
  Cons l String r_ r =>
  IsSymbol l =>
  Lacks l r_ =>
  Newtype Keymap (Record r) =>
  Proxy l -> String
getStringV2 key =
  let
    language = getLanguageLocale languageKey
  in
    case language of
      "BN_IN" -> stringsMap @~ bengali @~ key
      "HI_IN" -> stringsMap @~ hindi @~ key
      "KN_IN" -> stringsMap @~ kannada @~ key
      "ML_IN" -> stringsMap @~ malayalam @~ key
      "TA_IN" -> stringsMap @~ tamil @~ key
      "TE_IN" -> stringsMap @~ telugu @~ key
      "OD_IN" -> stringsMap @~ odiya @~ key
      _ -> stringsMap @~ english @~ key

getString :: String -> STR -> String
getString language str =
  let
    proxy = getProxy str

    langMap = case language of
      "BN_IN" -> stringsMap @~ bengali
      "HI_IN" -> stringsMap @~ hindi
      "KN_IN" -> stringsMap @~ kannada
      "ML_IN" -> stringsMap @~ malayalam
      "TA_IN" -> stringsMap @~ tamil
      "TE_IN" -> stringsMap @~ telugu
      "OD_IN" -> stringsMap @~ odiya
      _ -> stringsMap @~ english
  in
    proxy $ langMap

getProxy :: STR -> (Keymap -> String)
getProxy str = case str of
  LETS_GET_STARTED -> \a -> a @~ lets_get_started
  LANGUAGE_UPDATED -> \a -> a @~ language_updated
  YOUR_APPLICATION_HAS_BEEN_SUBMITTED_SUCCESSFULLY_AND_IS_UNDER_VERIFICATION -> \a -> a @~ your_application_has_been_submitted_successfully_and_is_under_verification
  VIEW_STATUS -> \a -> a @~ view_status
  GO_HOME -> \a -> a @~ go_home
  SELECT_LANGUAGE -> \a -> a @~ select_language
  WHICH_LANGUAGE_DO_YOU_PREFER -> \a -> a @~ which_language_do_you_prefer
  T_C -> \a -> a @~ t_c
  ENTER_MOBILE_NUMBER -> \a -> a @~ enter_mobile_number
  BY_CLICKING_CONTINUE_YOU_WILL_BE_AGREEING_TO_OUR -> \a -> a @~ by_clicking_continue_you_will_be_agreeing_to_our
  ENTER_OTP -> \a -> a @~ enter_otp
  DIDNT_RECIEVE_OTP -> \a -> a @~ didnt_recieve_otp
  RESEND_OTP -> \a -> a @~ resend_otp
  PLEASE_ENTER_VALID_OTP -> \a -> a @~ please_enter_valid_otp
  INVALID_MOBILE_NUMBER -> \a -> a @~ invalid_mobile_number
  REGISTER -> \a -> a @~ register
  MOBILE_NUMBER -> \a -> a @~ mobile_number
  AUTO_READING_OTP -> \a -> a @~ auto_reading_otp
  UPLOAD_DRIVING_LICENSE -> \a -> a @~ upload_driving_license
  UPLOAD_BACK_SIDE -> \a -> a @~ upload_back_side
  UPLOAD_FRONT_SIDE -> \a -> a @~ upload_front_side
  BACK_SIDE -> \a -> a @~ back_side
  FRONT_SIDE -> \a -> a @~ front_side
  NEXT -> \a -> a @~ next
  LICENSE_INSTRUCTION_PICTURE -> \a -> a @~ license_instruction_picture
  LICENSE_INSTRUCTION_CLARITY -> \a -> a @~ license_instruction_clarity
  REGISTRATION_STEPS -> \a -> a @~ registration_steps
  PROGRESS_SAVED -> \a -> a @~ progress_saved
  DRIVING_LICENSE -> \a -> a @~ driving_license
  AADHAR_CARD -> \a -> a @~ aadhar_card
  BANK_DETAILS -> \a -> a @~ bank_details
  VEHICLE_DETAILS -> \a -> a @~ vehicle_details
  UPLOAD_FRONT_BACK -> \a -> a @~ upload_front_back
  EARNINGS_WILL_BE_CREDITED -> \a -> a @~ earnings_will_be_credited
  FILL_VEHICLE_DETAILS -> \a -> a @~ fill_vehicle_details
  FOLLOW_STEPS -> \a -> a @~ follow_steps
  REGISTRATION -> \a -> a @~ registration
  UPLOAD_ADHAAR_CARD -> \a -> a @~ upload_adhaar_card
  ADHAAR_INTRUCTION_PICTURE -> \a -> a @~ adhaar_intruction_picture
  ADD_VEHICLE_DETAILS -> \a -> a @~ add_vehicle_details
  VEHICLE_REGISTRATION_NUMBER -> \a -> a @~ vehicle_registration_number
  ENTER_VEHICLE_NO -> \a -> a @~ enter_vehicle_no
  VEHICLE_TYPE -> \a -> a @~ vehicle_type
  VEHICLE_MODEL_NAME -> \a -> a @~ vehicle_model_name
  ENTER_MODEL_NAME -> \a -> a @~ enter_model_name
  VEHICLE_COLOUR -> \a -> a @~ vehicle_colour
  ENTER_VEHICLE_COLOUR -> \a -> a @~ enter_vehicle_colour
  UPLOAD_REGISTRATION_CERTIFICATE -> \a -> a @~ upload_registration_certificate
  UPLOAD_RC -> \a -> a @~ upload_rc
  PREVIEW -> \a -> a @~ preview
  CHOOSE_VEHICLE_TYPE -> \a -> a @~ choose_vehicle_type
  MAX_IMAGES -> \a -> a @~ max_images
  RE_ENTER_BENIFICIARY_NUMBER -> \a -> a @~ re_enter_benificiary_number
  IFSC_CODE -> \a -> a @~ ifsc_code
  BENIFICIARY_NUMBER -> \a -> a @~ benificiary_number
  SENDING_OTP -> \a -> a @~ sending_otp
  LOADING -> \a -> a @~ loading
  PLEASE_WAIT_WHILE_IN_PROGRESS -> \a -> a @~ please_wait_while_in_progress
  YOUR_REQUEST_HAS_TIMEOUT_TRY_AGAIN -> \a -> a @~ your_request_has_timeout_try_again
  ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER -> \a -> a @~ error_occured_please_try_again_later
  COUNTRY_CODE_INDIA -> \a -> a @~ country_code_india
  ENTER_OTP_SENT_TO -> \a -> a @~ enter_otp_sent_to
  OTP_SENT_TO -> \a -> a @~ otp_sent_to
  ENTER_ACCOUNT_NUMBER -> \a -> a @~ enter_account_number
  ADD_BANK_DETAILS -> \a -> a @~ add_bank_details
  ENTER_IFSC_CODE -> \a -> a @~ enter_ifsc_code
  SUBMIT -> \a -> a @~ submit
  PERSONAL_DETAILS -> \a -> a @~ personal_details
  LANGUAGES -> \a -> a @~ languages
  HELP_AND_FAQ -> \a -> a @~ help_and_faq
  ABOUT -> \a -> a @~ about
  LOGOUT -> \a -> a @~ logout
  UPDATE -> \a -> a @~ update
  EDIT -> \a -> a @~ edit
  DELETE -> \a -> a @~ delete
  VIEW -> \a -> a @~ view
  ISSUE_NO -> \a -> a @~ issue_no
  ADD_VOICE_NOTE -> \a -> a @~ add_voice_note
  VOICE_NOTE_ADDED -> \a -> a @~ voice_note_added
  ADDED_IMAGES -> \a -> a @~ added_images
  NO_IMAGES_ADDED -> \a -> a @~ no_images_added
  ASK_DETAILS_MESSAGE -> \a -> a @~ ask_details_message
  ASK_DETAILS_MESSAGE_REVERSED -> \a -> a @~ ask_details_message_reversed
  SELECT_OPTION -> \a -> a @~ select_option
  SELECT_OPTION_REVERSED -> \a -> a @~ select_option_reversed
  ISSUE_SUBMITTED_MESSAGE -> \a -> a @~ issue_submitted_message
  SUBMIT_ISSUE_DETAILS -> \a -> a @~ submit_issue_details
  IMAGE_PREVIEW -> \a -> a @~ image_preview
  RIDE_REPORT_ISSUE -> \a -> a @~ ride_report_issue
  I_DONT_KNOW_WHICH_RIDE -> \a -> a @~ i_dont_know_which_ride
  REPORT_ISSUE_CHAT_PLACEHOLDER arg -> \a -> a @~ report_issue_chat_placeholder $ arg
  ADDED_VOICE_NOTE -> \a -> a @~ added_voice_note
  NO_VOICE_NOTE_ADDED -> \a -> a @~ no_voice_note_added
  CALL_CUSTOMER_TITLE -> \a -> a @~ call_customer_title
  CALL_CUSTOMER_DESCRIPTION -> \a -> a @~ call_customer_description
  PLACE_CALL -> \a -> a @~ place_call
  PLACE_CALL_REQUEST -> \a -> a @~ place_call_request
  ADD_IMAGE -> \a -> a @~ add_image
  ADD_ANOTHER -> \a -> a @~ add_another
  IMAGES_ADDED -> \a -> a @~ images_added
  ISSUE_SUBMITTED_TEXT -> \a -> a @~ issue_submitted_text
  CHOOSE_AN_OPTION -> \a -> a @~ choose_an_option
  IMAGE_ADDED -> \a -> a @~ image_added
  DONE -> \a -> a @~ done
  RECORD_VOICE_NOTE -> \a -> a @~ record_voice_note
  AUTO -> \a -> a @~ auto
  NAME -> \a -> a @~ name
  PRIVACY_POLICY -> \a -> a @~ privacy_policy
  LOGO -> \a -> a @~ logo
  ABOUT_APP_DESCRIPTION -> \a -> a @~ about_app_description
  TERMS_AND_CONDITIONS -> \a -> a @~ terms_and_conditions
  UPDATE_VEHICLE_DETAILS -> \a -> a @~ update_vehicle_details
  HELP_AND_SUPPORT -> \a -> a @~ help_and_support
  NOTE -> \a -> a @~ note
  VISIT_MY_RIDES_SCREEN_FOR_SPECIFIC_COMPLAINTS -> \a -> a @~ visit_my_rides_screen_for_specific_complaints
  THANK_YOU_FOR_WRTITTING_US -> \a -> a @~ thank_you_for_wrtitting_us
  GO_TO_HOME -> \a -> a @~ go_to_home
  YOUR_RECENT_RIDE -> \a -> a @~ your_recent_ride
  YOUR_RECENT_TRIP -> \a -> a @~ your_recent_trip
  ALL_TOPICS -> \a -> a @~ all_topics
  REPORT_AN_ISSUE_WITH_THIS_TRIP -> \a -> a @~ report_an_issue_with_this_trip
  YOU_RATED -> \a -> a @~ you_rated
  VIEW_ALL_RIDES -> \a -> a @~ view_all_rides
  WRITE_TO_US -> \a -> a @~ write_to_us
  SUBJECT -> \a -> a @~ subject
  YOUR_EMAIL_ID -> \a -> a @~ your_email_id
  MORE_OPTIONS -> \a -> a @~ more_options
  DESCRIBE_YOUR_ISSUE -> \a -> a @~ describe_your_issue
  GETTING_STARTED_AND_FAQ -> \a -> a @~ getting_started_and_faq
  ONGOING_ISSUES -> \a -> a @~ ongoing_issues
  RESOLVED_ISSUES -> \a -> a @~ resolved_issues
  FOR_OTHER_ISSUES_WRITE_TO_US -> \a -> a @~ for_other_issues_write_to_us
  CALL_SUPPORT_CENTER -> \a -> a @~ call_support_center
  YOU_CAN_DESCRIBE_ISSUE_THAT_YOU_FACED_HERE -> \a -> a @~ you_can_describe_issue_that_you_faced_here
  REGISTRATION_CERTIFICATE_IMAGE -> \a -> a @~ registration_certificate_image
  HOME -> \a -> a @~ home
  RIDES -> \a -> a @~ rides
  MY_RIDES -> \a -> a @~ my_rides
  PROFILE -> \a -> a @~ profile
  ENTER_DRIVING_LICENSE_NUMBER -> \a -> a @~ enter_driving_license_number
  TRIP_DETAILS -> \a -> a @~ trip_details
  BY_CASH -> \a -> a @~ by_cash
  ONLINE_ -> \a -> a @~ online_
  GO_ONLINE_POPUP -> \a -> a @~ go_online_popup
  DISTANCE -> \a -> a @~ distance
  COIN_BALANCE -> \a -> a @~ coin_balance
  POINTS_BALANCE -> \a -> a @~ points_balance
  TOTAL_EARNED -> \a -> a @~ total_earned
  RIDE_HISTORY -> \a -> a @~ ride_history
  TRANSACTION_HISTORY -> \a -> a @~ transaction_history
  POINTS_EARNED -> \a -> a @~ points_earned
  NO_RIDES -> \a -> a @~ no_rides
  POINTS_USED -> \a -> a @~ points_used
  USE_POINTS -> \a -> a @~ use_points
  INSIGHTS -> \a -> a @~ insights
  USAGE_HISTORY -> \a -> a @~ usage_history
  NO_POINTS_EARNED -> \a -> a @~ no_points_earned
  NO_POINTS_USED -> \a -> a @~ no_points_used
  EARN_POINTS_BY_TAKING_RIDES_AND_REFERRING_THE_APP_TO_OTHERS arg1 -> \a -> (a @~ earn_points_by_taking_rides_and_referring_the_app_to_others) arg1
  USE_THEM_BEFORE_THEY_EXPIRE -> \a -> a @~ use_them_before_they_expire
  NO_RIDE_HISTORY_AVAILABLE -> \a -> a @~ no_ride_history_available
  YOU_HAVE_NOT_COMPLETED_A_RIDE_YET -> \a -> a @~ you_have_not_completed_a_ride_yet
  COMPLETE_FIRST_RIDE_TO_UNLOCK_POINTS -> \a -> a @~ complete_first_ride_to_unlock_points
  DESTINATION -> \a -> a @~ destination
  YOU_DID_NOT_TAKE_ANY_RIDES_ON_PREFIX -> \a -> a @~ you_did_not_take_any_rides_on_prefix
  YOU_DID_NOT_TAKE_ANY_RIDES_ON_SUFFIX -> \a -> a @~ you_did_not_take_any_rides_on_suffix
  CONVERT -> \a -> a @~ convert
  CASH_CONVERTED -> \a -> a @~ cash_converted
  WILL_BE_ADJUSTED_IN_YOUR_FUTURE_SUBSCRIPTION_DUES -> \a -> a @~ will_be_adjusted_in_your_future_subscription_dues
  HAS_BEEN_ADJUSTED_IN_YOUR_SUBSCRIPTION_DUES -> \a -> a @~ has_been_adjusted_in_your_subscription_dues
  USING_POINTS_REQUIRES_AN_ACTIVE_PLAN -> \a -> a @~ using_points_requires_an_active_plan
  TO_GET_STARTED -> \a -> a @~ to_get_started
  CONVERTED_FROM_POINTS -> \a -> a @~ converted_from_points
  REPORT_AN_ISSUE -> \a -> a @~ report_an_issue
  TIME_TAKEN -> \a -> a @~ time_taken
  MAPS -> \a -> a @~ maps
  CALL -> \a -> a @~ call
  START_RIDE -> \a -> a @~ start_ride
  CANCEL_RIDE -> \a -> a @~ cancel_ride
  PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL -> \a -> a @~ please_tell_us_why_you_want_to_cancel
  MANDATORY -> \a -> a @~ mandatory
  END_RIDE -> \a -> a @~ end_ride
  RIDE_COMPLETED_WITH -> \a -> a @~ ride_completed_with
  COLLECT_AMOUNT_IN_CASH -> \a -> a @~ collect_amount_in_cash
  CASH_COLLECTED -> \a -> a @~ cash_collected
  OFFLINE -> \a -> a @~ offline
  ACCEPT_FOR -> \a -> a @~ accept_for
  DECLINE -> \a -> a @~ decline
  REQUEST -> \a -> a @~ request
  YOU_ARE_OFFLINE -> \a -> a @~ you_are_offline
  YOU_ARE_CURRENTLY_BUSY_GO_ONLINE_TO_RECIEVE_TRIP_REQUESTS -> \a -> a @~ you_are_currently_busy_go_online_to_recieve_trip_requests
  GOING_OFFLINE_WILL_NOT_GET_YOU_ANY_RIDE -> \a -> a @~ going_offline_will_not_get_you_any_ride
  CANCEL -> \a -> a @~ cancel
  GO_OFFLINE -> \a -> a @~ go_offline
  IS_WAITING_FOR_YOU -> \a -> a @~ is_waiting_for_you
  YOU_ARE_ON_A_RIDE -> \a -> a @~ you_are_on_a_ride
  PLEASE_ASK_THE_CUSTOMER_FOR_THE_OTP -> \a -> a @~ please_ask_the_customer_for_the_otp
  ENTER_CURRENT_ODOMETER_READING -> \a -> a @~ enter_current_odometer_reading
  ENTER_FINAL_ODO_READING -> \a -> a @~ enter_final_odo_reading
  ENTER_THE_LAST_4_DIGITS_OF_ODOMETER -> \a -> a @~ enter_the_last_4_digits_of_odometer
  ENTER_THE_DIGITS_OF_ODOMETER -> \a -> a @~ enter_the_digits_of_odometer
  ODOMETER_READING_VALIDATION_FAILED -> \a -> a @~ odometer_reading_validation_failed
  COMPLETED_ -> \a -> a @~ completed_
  CANCELLED_ -> \a -> a @~ cancelled_
  WHERE_IS_MY_LICENSE_NUMBER -> \a -> a @~ where_is_my_license_number
  WE_NEED_SOME_ACCESS -> \a -> a @~ we_need_some_access
  ALLOW_ACCESS -> \a -> a @~ allow_access
  ENTER_RC_NUMBER -> \a -> a @~ enter_rc_number
  WHERE_IS_MY_RC_NUMBER -> \a -> a @~ where_is_my_rc_number
  WE_HAVE_RECIEVED_YOUR_ISSUE -> \a -> a @~ we_have_recieved_your_issue
  THANK_YOU_FOR_WRITING_TO_US -> \a -> a @~ thank_you_for_writing_to_us
  RIDER -> \a -> a @~ rider
  TRIP_ID -> \a -> a @~ trip_id
  NEED_IT_TO_SHOW_YOU_INCOMING_RIDE_REQUEST -> \a -> a @~ need_it_to_show_you_incoming_ride_request
  NEED_IT_TO_DISABLE_BATTERY_OPTIMIZATION_FOR_THE_APP -> \a -> a @~ need_it_to_disable_battery_optimization_for_the_app
  NEED_IT_TO_AUTOSTART_YOUR_APP -> \a -> a @~ need_it_to_autostart_your_app
  NEED_IT_TO_ENABLE_LOCATION arg1 -> \a -> (a @~ need_it_to_enable_location) arg1
  OVERLAY_TO_DRAW_OVER_APPLICATIONS -> \a -> a @~ overlay_to_draw_over_applications
  BATTERY_OPTIMIZATIONS -> \a -> a @~ battery_optimizations
  AUTO_START_APPLICATION_IN_BACKGROUND -> \a -> a @~ auto_start_application_in_background
  LOCATION_ACCESS -> \a -> a @~ location_access
  STEP -> \a -> a @~ step
  PAID -> \a -> a @~ paid
  ENTERED_WRONG_OTP -> \a -> a @~ entered_wrong_otp
  OTP_INVALID_FOR_THIS_VEHICLE_VARIANT -> \a -> a @~ otp_invalid_for_this_vehicle_variant
  COPIED -> \a -> a @~ copied
  BANK_NAME -> \a -> a @~ bank_name
  AADHAR_DETAILS -> \a -> a @~ aadhar_details
  AADHAR_NUMBER -> \a -> a @~ aadhar_number
  FRONT_SIDE_IMAGE -> \a -> a @~ front_side_image
  BACK_SIDE_IMAGE -> \a -> a @~ back_side_image
  STILL_NOT_RESOLVED -> \a -> a @~ still_not_resolved
  CASE_TWO -> \a -> a @~ case_two
  NON_DISCLOUSER_AGREEMENT -> \a -> a @~ non_disclouser_agreement
  DATA_COLLECTION_AUTHORITY -> \a -> a @~ data_collection_authority
  SOFTWARE_LICENSE -> \a -> a @~ software_license
  LOAD_MORE -> \a -> a @~ load_more
  ARE_YOU_SURE_YOU_WANT_TO_LOGOUT -> \a -> a @~ are_you_sure_you_want_to_logout
  GO_BACK -> \a -> a @~ go_back
  THANK_YOU_FOR_REGISTERING_US -> \a -> a @~ thank_you_for_registering_us
  UNFORTANUTELY_WE_ARE_NOT_AVAILABLE__YET_FOR_YOU -> \a -> a @~ unfortanutely_we_are_not_available__yet_for_you
  ARE_YOU_SURE_YOU_WANT_TO_END_THE_RIDE -> \a -> a @~ are_you_sure_you_want_to_end_the_ride
  EMPTY_RIDES -> \a -> a @~ empty_rides
  YOU_HAVE_NOT_TAKEN_A_TRIP_YET -> \a -> a @~ you_have_not_taken_a_trip_yet
  BOOK_NOW -> \a -> a @~ book_now
  RESEND_OTP_IN -> \a -> a @~ resend_otp_in
  WE_NEED_ACCESS_TO_YOUR_LOCATION -> \a -> a @~ we_need_access_to_your_location
  YOUR_LOCATION_HELPS_OUR_SYSTEM arg1 -> \a -> (a @~ your_location_helps_our_system) arg1
  NO_INTERNET_CONNECTION -> \a -> a @~ no_internet_connection
  PLEASE_CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN -> \a -> a @~ please_check_your_internet_connection_and_try_again
  TRY_AGAIN -> \a -> a @~ try_again
  GRANT_ACCESS -> \a -> a @~ grant_access
  ENTER_REFERRAL_MOBILE_NUMBER -> \a -> a @~ enter_referral_mobile_number
  APPLY -> \a -> a @~ apply
  HAVE_A_REFERRAL -> \a -> a @~ have_a_referral
  ADD_HERE -> \a -> a @~ add_here
  REFERRAL_APPLIED -> \a -> a @~ referral_applied
  SMALLEDIT -> \a -> a @~ smalledit
  ADD_DRIVING_LICENSE -> \a -> a @~ add_driving_license
  HELP -> \a -> a @~ help
  INVALID_DL_NUMBER -> \a -> a @~ invalid_dl_number
  DRIVING_LICENSE_NUMBER -> \a -> a @~ driving_license_number
  ENTER_DL_NUMBER -> \a -> a @~ enter_dl_number
  SELECT_DATE_OF_BIRTH -> \a -> a @~ select_date_of_birth
  DATE_OF_BIRTH -> \a -> a @~ date_of_birth
  WATCH_A_TUTORIAL_FOR_EASY_REGISTRATION -> \a -> a @~ watch_a_tutorial_for_easy_registration
  ENTER_MINIMUM_FIFTEEN_CHARACTERS -> \a -> a @~ enter_minimum_fifteen_characters
  ADD_YOUR_FRIEND -> \a -> a @~ add_your_friend
  PLEASE_WAIT_WHILE_VALIDATING_THE_IMAGE -> \a -> a @~ please_wait_while_validating_the_image
  VALIDATING -> \a -> a @~ validating
  VERIFICATION_PENDING -> \a -> a @~ verification_pending
  VERIFICATION_FAILED -> \a -> a @~ verification_failed
  NO_DOC_AVAILABLE -> \a -> a @~ no_doc_available
  ISSUE_WITH_DL_IMAGE -> \a -> a @~ issue_with_dl_image
  STILL_HAVE_SOME_DOUBT -> \a -> a @~ still_have_some_doubt
  ISSUE_WITH_RC_IMAGE -> \a -> a @~ issue_with_rc_image
  PLEASE_CHECK_FOR_IMAGE_IF_VALID_DOCUMENT_IMAGE_OR_NOT -> \a -> a @~ please_check_for_image_if_valid_document_image_or_not
  OOPS_YOUR_APPLICATION_HAS_BEEN_REJECTED -> \a -> a @~ oops_your_application_has_been_rejected
  INVALID_DRIVING_LICENSE -> \a -> a @~ invalid_driving_license
  LIMIT_EXCEEDED_FOR_DL_UPLOAD -> \a -> a @~ limit_exceeded_for_dl_upload
  INVALID_VEHICLE_REGISTRATION_CERTIFICATE -> \a -> a @~ invalid_vehicle_registration_certificate
  LIMIT_EXCEEDED_FOR_RC_UPLOAD -> \a -> a @~ limit_exceeded_for_rc_upload
  YOUR_DOCUMENTS_ARE_APPROVED -> \a -> a @~ your_documents_are_approved
  APPLICATION_STATUS -> \a -> a @~ application_status
  FOR_SUPPORT -> \a -> a @~ for_support
  CONTACT_US -> \a -> a @~ contact_us
  IMAGE_VALIDATION_FAILED -> \a -> a @~ image_validation_failed
  IMAGE_NOT_READABLE -> \a -> a @~ image_not_readable
  IMAGE_LOW_QUALITY -> \a -> a @~ image_low_quality
  IMAGE_INVALID_TYPE -> \a -> a @~ image_invalid_type
  IMAGE_DOCUMENT_NUMBER_MISMATCH -> \a -> a @~ image_document_number_mismatch
  IMAGE_EXTRACTION_FAILED -> \a -> a @~ image_extraction_failed
  IMAGE_NOT_FOUND -> \a -> a @~ image_not_found
  IMAGE_NOT_VALID -> \a -> a @~ image_not_valid
  DRIVER_ALREADY_LINKED -> \a -> a @~ driver_already_linked
  DL_ALREADY_UPDATED -> \a -> a @~ dl_already_updated
  RC_ALREADY_LINKED -> \a -> a @~ rc_already_linked
  RC_ALREADY_UPDATED -> \a -> a @~ rc_already_updated
  DL_ALREADY_LINKED -> \a -> a @~ dl_already_linked
  SOMETHING_WENT_WRONG -> \a -> a @~ something_went_wrong
  PICKUP -> \a -> a @~ pickup
  TRIP -> \a -> a @~ trip
  CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER arg1 -> \a -> (a @~ currently_we_allow_only_karnataka_registered_number) arg1
  RE_ENTER_VEHICLE_REGISTRATION_NUMBER -> \a -> a @~ re_enter_vehicle_registration_number
  RE_ENTER_DRIVING_LICENSE_NUMBER -> \a -> a @~ re_enter_driving_license_number
  UPDATED_AT -> \a -> a @~ updated_at
  TRIP_COUNT -> \a -> a @~ trip_count
  TODAYS_EARNINGS -> \a -> a @~ todays_earnings
  TODAYS_EARNINGS_STR -> \a -> a @~ todays_earnings_str
  BONUS_EARNED -> \a -> a @~ bonus_earned
  WHAT_IS_NAMMA_YATRI_BONUS arg1 -> \a -> (a @~ what_is_namma_yatri_bonus) arg1
  BONUS_PRIMARY_TEXT arg1 -> \a -> (a @~ bonus_primary_text) arg1
  BONUS_SECONDARY_TEXT arg1 -> \a -> (a @~ bonus_secondary_text) arg1
  DATE_OF_REGISTRATION -> \a -> a @~ date_of_registration
  SELECT_DATE_OF_REGISTRATION -> \a -> a @~ select_date_of_registration
  DATE_OF_ISSUE -> \a -> a @~ date_of_issue
  PROVIDE_DATE_OF_ISSUE_TEXT -> \a -> a @~ provide_date_of_issue_text
  PROVIDE_DATE_OF_REGISTRATION_TEXT -> \a -> a @~ provide_date_of_registration_text
  SELECT_DATE_OF_ISSUE -> \a -> a @~ select_date_of_issue
  SAME_REENTERED_RC_MESSAGE -> \a -> a @~ same_reentered_rc_message
  SAME_REENTERED_DL_MESSAGE -> \a -> a @~ same_reentered_dl_message
  WHERE_IS_MY_ISSUE_DATE -> \a -> a @~ where_is_my_issue_date
  WHERE_IS_MY_REGISTRATION_DATE -> \a -> a @~ where_is_my_registration_date
  EARNINGS_CREDITED_IN_ACCOUNT -> \a -> a @~ earnings_credited_in_account
  INVALID_PARAMETERS -> \a -> a @~ invalid_parameters
  UNAUTHORIZED -> \a -> a @~ unauthorized
  INVALID_TOKEN -> \a -> a @~ invalid_token
  SOME_ERROR_OCCURED_IN_OFFERRIDE -> \a -> a @~ some_error_occured_in_offerride
  SELECT_VEHICLE_TYPE -> \a -> a @~ select_vehicle_type
  RIDE -> \a -> a @~ ride
  NO_LOCATION_UPDATE -> \a -> a @~ no_location_update
  GOT_IT_TELL_US_MORE -> \a -> a @~ got_it_tell_us_more
  WRITE_A_COMMENT -> \a -> a @~ write_a_comment
  HOW_WAS_YOUR_RIDE_WITH -> \a -> a @~ how_was_your_ride_with
  RUDE_BEHAVIOUR -> \a -> a @~ rude_behaviour
  LONG_WAITING_TIME -> \a -> a @~ long_waiting_time
  DIDNT_COME_TO_PICUP_LOCATION -> \a -> a @~ didnt_come_to_picup_location
  HELP_US_WITH_YOUR_REASON -> \a -> a @~ help_us_with_your_reason
  MAX_CHAR_LIMIT_REACHED -> \a -> a @~ max_char_limit_reached
  SHOW_ALL_OPTIONS -> \a -> a @~ show_all_options
  UPDATE_REQUIRED -> \a -> a @~ update_required
  PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE -> \a -> a @~ please_update_app_to_continue_service
  NOT_NOW -> \a -> a @~ not_now
  OF -> \a -> a @~ of_
  DROP -> \a -> a @~ drop
  PLEASE_WAIT -> \a -> a @~ please_wait
  SETTING_YOU_OFFLINE -> \a -> a @~ setting_you_offline
  SETTING_YOU_ONLINE -> \a -> a @~ setting_you_online
  SETTING_YOU_SILENT -> \a -> a @~ setting_you_silent
  VIEW_BREAKDOWN -> \a -> a @~ view_breakdown
  APP_INFO -> \a -> a @~ app_info
  OTHER -> \a -> a @~ other
  VEHICLE_ISSUE -> \a -> a @~ vehicle_issue
  FARE_UPDATED -> \a -> a @~ fare_updated
  FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES -> \a -> a @~ frequent_cancellations_will_lead_to_less_rides
  FREQUENT_CANCELLATIONS_WILL_LEAD_TO_BLOCKING -> \a -> a @~ frequent_cancellations_will_lead_to_blocking
  CONTINUE -> \a -> a @~ continue
  CONFIRM_PASSWORD -> \a -> a @~ confirm_password
  DEMO_MODE -> \a -> a @~ demo_mode
  PASSWORD -> \a -> a @~ password
  ENTER_DEMO_MODE_PASSWORD -> \a -> a @~ enter_demo_mode_password
  DEMO_MODE_DISABLED -> \a -> a @~ demo_mode_disabled
  ONLINE_VIA_DEMO_MODE -> \a -> a @~ online_via_demo_mode
  MORE -> \a -> a @~ more
  LESS -> \a -> a @~ less
  YOU_ARE_AT_PICKUP -> \a -> a @~ you_are_at_pickup
  WAITING_FOR_CUSTOMER -> \a -> a @~ waiting_for_customer
  CUSTOMER_NOTIFIED -> \a -> a @~ customer_notified
  PICKUP_TOO_FAR -> \a -> a @~ pickup_too_far
  CUSTOMER_NOT_PICKING_CALL -> \a -> a @~ customer_not_picking_call
  TRAFFIC_JAM -> \a -> a @~ traffic_jam
  CUSTOMER_WAS_RUDE -> \a -> a @~ customer_was_rude
  ALL_MESSAGES -> \a -> a @~ all_messages
  MESSAGES -> \a -> a @~ messages
  ADD_A_COMMENT -> \a -> a @~ add_a_comment
  POST_COMMENT -> \a -> a @~ post_comment
  ENTER_YOUR_COMMENT -> \a -> a @~ enter_your_comment
  NO_NOTIFICATIONS_RIGHT_NOW -> \a -> a @~ no_notifications_right_now
  NO_NOTIFICATIONS_RIGHT_NOW_DESC -> \a -> a @~ no_notifications_right_now_desc
  ALERTS -> \a -> a @~ alerts
  YOUR_COMMENT -> \a -> a @~ your_comment
  SHOW_MORE -> \a -> a @~ show_more
  LOAD_OLDER_ALERTS -> \a -> a @~ load_older_alerts
  CONTEST -> \a -> a @~ contest
  YOUR_REFERRAL_CODE_IS_LINKED -> \a -> a @~ your_referral_code_is_linked
  YOU_CAN_NOW_EARN_REWARDS -> \a -> a @~ you_can_now_earn_rewards
  COMING_SOON -> \a -> a @~ coming_soon
  COMING_SOON_DESCRIPTION -> \a -> a @~ coming_soon_description
  REFERRAL_CODE_NUMBER -> \a -> a @~ referral_code_number
  REFERRAL_CODE_HINT -> \a -> a @~ referral_code_hint
  CONFIRM_REFERRAL_CODE -> \a -> a @~ confirm_referral_code
  CONFIRM_REFERRAL_CODE_HINT -> \a -> a @~ confirm_referral_code_hint
  YOUR_REFERRAL_CODE -> \a -> a @~ your_referral_code
  FIRST_REFERRAL_SUCCESSFUL -> \a -> a @~ first_referral_successful
  AWAITING_REFERRAL_RIDE -> \a -> a @~ awaiting_referral_ride
  CHECK_THIS_SPACE_WHEN_YOU_GET_REFERRAL_ALERT -> \a -> a @~ check_this_space_when_you_get_referral_alert
  REFERRED_CUSTOMERS -> \a -> a @~ referred_customers
  ACTIVATED_CUSTOMERS -> \a -> a @~ activated_customers
  REFERRAL_CODE_LINKING -> \a -> a @~ referral_code_linking
  CONTACT_SUPPORT -> \a -> a @~ contact_support
  AC_CHECK_TITILE -> \a -> a @~ ac_check_titile
  CALL_SUPPORT -> \a -> a @~ call_support
  YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT arg1 -> \a -> (a @~ you_are_about_to_call_namma_yatri_support) arg1
  REFERRAL_ENROLMENT -> \a -> a @~ referral_enrolment
  REFERRALS -> \a -> a @~ referrals
  LINK_REFERRAL_CODE -> \a -> a @~ link_referral_code
  DRIVER_DETAILS -> \a -> a @~ driver_details
  FOR_UPDATES_SEE_ALERTS -> \a -> a @~ for_updates_see_alerts
  SHARE_OPTIONS -> \a -> a @~ share_options
  ENTER_PASSWORD -> \a -> a @~ enter_password
  WELCOME_TEXT -> \a -> a @~ welcome_text
  ABOUT_TEXT -> \a -> a @~ about_text
  YOUR_VEHICLE -> \a -> a @~ your_vehicle
  BOOKING_OPTIONS -> \a -> a @~ booking_options
  CONFIRM_AND_CHANGE -> \a -> a @~ confirm_and_change
  MAKE_YOURSELF_AVAILABLE_FOR -> \a -> a @~ make_yourself_available_for
  OTP_ -> \a -> a @~ otp_
  CHOOSE_LANGUAGE -> \a -> a @~ choose_language
  RIDE_FARE -> \a -> a @~ ride_fare
  RIDE_DISTANCE -> \a -> a @~ ride_distance
  MESSAGE -> \a -> a @~ message
  START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS -> \a -> a @~ start_your_chat_using_these_quick_chat_suggestions
  START_YOUR_CHAT_WITH_THE_DRIVER -> \a -> a @~ start_your_chat_with_the_driver
  I_AM_ON_MY_WAY -> \a -> a @~ i_am_on_my_way
  GETTING_DELAYED_PLEASE_WAIT -> \a -> a @~ getting_delayed_please_wait
  UNREACHABLE_PLEASE_CALL_BACK -> \a -> a @~ unreachable_please_call_back
  ARE_YOU_STARING -> \a -> a @~ are_you_staring
  PLEASE_COME_SOON -> \a -> a @~ please_come_soon
  OK_I_WILL_WAIT -> \a -> a @~ ok_i_will_wait
  I_HAVE_ARRIVED -> \a -> a @~ i_have_arrived
  PLEASE_COME_FAST_I_AM_WAITING -> \a -> a @~ please_come_fast_i_am_waiting
  PLEASE_WAIT_I_WILL_BE_THERE -> \a -> a @~ please_wait_i_will_be_there
  LOOKING_FOR_YOU_AT_PICKUP -> \a -> a @~ looking_for_you_at_pickup
  SILENT -> \a -> a @~ silent
  TRY_SILENT_MODE -> \a -> a @~ try_silent_mode
  SILENT_MODE_PROMPT -> \a -> a @~ silent_mode_prompt
  GO_SILENT -> \a -> a @~ go_silent
  GO_ONLINE -> \a -> a @~ go_online
  GO_ONLINE_PROMPT -> \a -> a @~ go_online_prompt
  LIVE_DASHBOARD -> \a -> a @~ live_dashboard
  CLICK_TO_ACCESS_YOUR_ACCOUNT -> \a -> a @~ click_to_access_your_account
  ADD_ALTERNATE_NUMBER -> \a -> a @~ add_alternate_number
  ENTER_ALTERNATE_MOBILE_NUMBER -> \a -> a @~ enter_alternate_mobile_number
  PLEASE_ENTER_A_VALID_10_DIGIT_NUMBER -> \a -> a @~ please_enter_a_valid_10_digit_number
  ALTERNATE_MOBILE_NUMBER -> \a -> a @~ alternate_mobile_number
  REMOVE -> \a -> a @~ remove
  REMOVE_ALTERNATE_NUMBER -> \a -> a @~ remove_alternate_number
  ARE_YOU_SURE_YOU_WANT_TO_REMOVE_YOUR_ALTERNATE_MOBILE_NUMBER -> \a -> a @~ are_you_sure_you_want_to_remove_your_alternate_mobile_number
  YES_REMOVE_IT -> \a -> a @~ yes_remove_it
  NUMBER_REMOVED_SUCCESSFULLY -> \a -> a @~ number_removed_successfully
  EDIT_ALTERNATE_MOBILE_NUMBER -> \a -> a @~ edit_alternate_mobile_number
  NUMBER_ADDED_SUCCESSFULLY -> \a -> a @~ number_added_successfully
  NUMBER_EDITED_SUCCESSFULLY -> \a -> a @~ number_edited_successfully
  ALTERNATE_MOBILE_OTP_LIMIT_EXCEED -> \a -> a @~ alternate_mobile_otp_limit_exceed
  ATTEMPTS_LEFT -> \a -> a @~ attempts_left
  WRONG_OTP -> \a -> a @~ wrong_otp
  OTP_LIMIT_EXCEEDED -> \a -> a @~ otp_limit_exceeded
  OTP_LIMIT_EXCEEDED_MESSAGE -> \a -> a @~ otp_limit_exceeded_message
  TRY_AGAIN_LATER -> \a -> a @~ try_again_later
  ATTEMPT_LEFT -> \a -> a @~ attempt_left
  NUMBER_ALREADY_EXIST_ERROR -> \a -> a @~ number_already_exist_error
  PLEASE_ASK_RIDER_FOR_THE_OTP -> \a -> a @~ please_ask_rider_for_the_otp
  YOUR_LIMIT_EXCEEDED_TRY_AGAIN_AFTER_10_MIN -> \a -> a @~ your_limit_exceeded_try_again_after_10_min
  I_ARRIVED -> \a -> a @~ i_arrived
  ESTIMATED_RIDE_FARE -> \a -> a @~ estimated_ride_fare
  COMPLETE_ONBOARDING -> \a -> a @~ complete_onboarding
  PERSON_WITH_THIS_NUMBER_ALREADY_EXISTS -> \a -> a @~ person_with_this_number_already_exists
  RESOLVED_ISSUE -> \a -> a @~ resolved_issue
  ONGOING_ISSUE -> \a -> a @~ ongoing_issue
  LOST_ITEM -> \a -> a @~ lost_item
  RIDE_RELATED_ISSUE -> \a -> a @~ ride_related_issue
  APP_RELATED_ISSUE -> \a -> a @~ app_related_issue
  FARE_RELATED_ISSUE -> \a -> a @~ fare_related_issue
  ISSUE_NUMBER -> \a -> a @~ issue_number
  REMOVE_ISSUE -> \a -> a @~ remove_issue
  CALL_SUPPORT_NUMBER -> \a -> a @~ call_support_number
  YEARS_AGO -> \a -> a @~ years_ago
  MONTHS_AGO -> \a -> a @~ months_ago
  DAYS_AGO -> \a -> a @~ days_ago
  HOURS_AGO -> \a -> a @~ hours_ago
  MIN_AGO -> \a -> a @~ min_ago
  SEC_AGO -> \a -> a @~ sec_ago
  VERIFICATION_IS_TAKING_A_BIT_LONGER -> \a -> a @~ verification_is_taking_a_bit_longer
  DEMO -> \a -> a @~ demo
  RIDE_RELATED -> \a -> a @~ ride_related
  FARE -> \a -> a @~ fare
  APP_RELATED -> \a -> a @~ app_related
  LOST_AND_FOUND -> \a -> a @~ lost_and_found
  REPORT_LOST_ITEM -> \a -> a @~ report_lost_item
  CORPORATE_ADDRESS arg1 -> \a -> (a @~ corporate_address) arg1
  CORPORATE_ADDRESS_DESCRIPTION arg1 -> \a -> (a @~ corporate_address_description) arg1
  CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL arg1 -> \a -> (a @~ corporate_address_description_additional) arg1
  REGISTERED_ADDRESS arg1 -> \a -> (a @~ registered_address) arg1
  REGISTERED_ADDRESS_DESCRIPTION arg1 -> \a -> (a @~ registered_address_description) arg1
  REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL arg1 -> \a -> (a @~ registered_address_description_additional) arg1
  SELECT_THE_LANGUAGES_YOU_CAN_SPEAK -> \a -> a @~ select_the_languages_you_can_speak
  GENDER -> \a -> a @~ gender
  SELECT_YOUR_GENDER -> \a -> a @~ select_your_gender
  MALE -> \a -> a @~ male
  FEMALE -> \a -> a @~ female
  PREFER_NOT_TO_SAY -> \a -> a @~ prefer_not_to_say
  SET_NOW -> \a -> a @~ set_now
  COMPLETE_YOUR_PROFILE_AND_FIND_MORE_RIDES -> \a -> a @~ complete_your_profile_and_find_more_rides
  UPDATE_NOW -> \a -> a @~ update_now
  CONFIRM -> \a -> a @~ confirm
  GENDER_UPDATED -> \a -> a @~ gender_updated
  ZONE_CANCEL_TEXT_DROP -> \a -> a @~ zone_cancel_text_drop
  ZONE_CANCEL_TEXT_PICKUP -> \a -> a @~ zone_cancel_text_pickup
  RANKINGS -> \a -> a @~ rankings
  GETTING_THE_LEADERBOARD_READY -> \a -> a @~ getting_the_leaderboard_ready
  PLEASE_WAIT_WHILE_WE_UPDATE_THE_DETAILS -> \a -> a @~ please_wait_while_we_update_the_details
  LAST_UPDATED -> \a -> a @~ last_updated
  CONGRATULATIONS_YOU_ARE_RANK -> \a -> a @~ congratulations_you_are_rank
  YOU -> \a -> a @~ you
  DAILY -> \a -> a @~ daily
  INACCURATE_DATE_AND_TIME -> \a -> a @~ inaccurate_date_and_time
  ADJUST_YOUR_DEVICE_DATE_AND_TIME_AND_TRY_AGAIN -> \a -> a @~ adjust_your_device_date_and_time_and_try_again
  THE_CURRENT_DATE_AND_TIME_IS -> \a -> a @~ the_current_date_and_time_is
  GO_TO_SETTING -> \a -> a @~ go_to_setting
  ACCEPT_RIDES_TO_ENTER_RANKINGS -> \a -> a @~ accept_rides_to_enter_rankings
  OTP_HAS_BEEN_RESENT -> \a -> a @~ otp_has_been_resent
  OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_RESENDING_OTP -> \a -> a @~ otp_entering_limit_exhausted_please_try_resending_otp
  OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER -> \a -> a @~ otp_resent_limit_exhausted_please_try_again_later
  OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN -> \a -> a @~ otp_page_has_been_expired_please_request_otp_again
  SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN -> \a -> a @~ something_went_wrong_please_try_again
  INVALID_REFERRAL_CODE -> \a -> a @~ invalid_referral_code
  ISSUE_REMOVED_SUCCESSFULLY -> \a -> a @~ issue_removed_successfully
  OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER -> \a -> a @~ otp_entering_limit_exhausted_please_try_again_later
  TOO_MANY_ATTEMPTS_PLEASE_TRY_AGAIN_LATER -> \a -> a @~ too_many_attempts_please_try_again_later
  INVALID_REFERRAL_NUMBER -> \a -> a @~ invalid_referral_number
  SOMETHING_WENT_WRONG_TRY_AGAIN_LATER -> \a -> a @~ something_went_wrong_try_again_later
  WAIT_TIME -> \a -> a @~ wait_time
  WAIT_TIMER -> \a -> a @~ wait_timer
  HOW_LONG_WAITED_FOR_PICKUP -> \a -> a @~ how_long_waited_for_pickup
  CUSTOMER_WILL_PAY_FOR_EVERY_MINUTE arg1 arg2 -> \a -> (a @~ customer_will_pay_for_every_minute) arg1 arg2
  OTHERS -> \a -> a @~ others
  ENTER_SECOND_SIM_NUMBER -> \a -> a @~ enter_second_sim_number
  ALTERNATE_NUMBER -> \a -> a @~ alternate_number
  LIMIT_EXCEEDED_FOR_ALTERNATE_NUMBER -> \a -> a @~ limit_exceeded_for_alternate_number
  ADD_ALTERNATE_NUMBER_IN_MEANTIME -> \a -> a @~ add_alternate_number_in_meantime
  OTP_RESEND_LIMIT_EXCEEDED -> \a -> a @~ otp_resend_limit_exceeded
  ALTERNATE_NUMBER_CANNOT_BE_ADDED -> \a -> a @~ alternate_number_cannot_be_added
  OTP_RESENT -> \a -> a @~ otp_resent
  SEDAN -> \a -> a @~ sedan
  SUV -> \a -> a @~ suv
  HATCHBACK -> \a -> a @~ hatchback
  AUTO_RICKSHAW -> \a -> a @~ auto_rickshaw
  TAXI -> \a -> a @~ taxi
  TAXI_PLUS -> \a -> a @~ taxi_plus
  MY_PROFILE -> \a -> a @~ my_profile
  SETTINGS -> \a -> a @~ settings
  REG_NUMBER -> \a -> a @~ reg_number
  TYPE -> \a -> a @~ type_
  MODEL_NAME -> \a -> a @~ model_name
  COLOUR -> \a -> a @~ colour
  BADGES -> \a -> a @~ badges
  EDIT_RC -> \a -> a @~ edit_rc
  DEACTIVATE_RC -> \a -> a @~ deactivate_rc
  ACTIVATE_RC -> \a -> a @~ activate_rc
  DELETE_RC -> \a -> a @~ delete_rc
  CALL_DRIVER -> \a -> a @~ call_driver
  CALL_CUSTOMER_SUPPORT -> \a -> a @~ call_customer_support
  ACTIVE_RC_ON_ANOTHER_DRIVER -> \a -> a @~ active_rc_on_another_driver
  CALL_DRIVER_OR_CONTACT_SUPPORT -> \a -> a @~ call_driver_or_contact_support
  SKIP -> \a -> a @~ skip
  ACTIVE_STR -> \a -> a @~ active_str
  INACTIVE_RC -> \a -> a @~ inactive_rc
  CONFIRMATION_FOR_DELETING_RC -> \a -> a @~ confirmation_for_deleting_rc
  YES_DELETE -> \a -> a @~ yes_delete
  ADD_NEW_RC -> \a -> a @~ add_new_rc
  CONNECT_CALL_ANONYMOUSLY -> \a -> a @~ connect_call_anonymously
  YES_ACTIVATE -> \a -> a @~ yes_activate
  YES_DEACTIVATE -> \a -> a @~ yes_deactivate
  CONFIRMATION_FOR_DEACTIVATING_RC -> \a -> a @~ confirmation_for_deactivating_rc
  CONFIRMATION_FOR_ACTIVATING_RC -> \a -> a @~ confirmation_for_activating_rc
  THIS_WILL_DEACTIVATE_CURRENTLY_ACTIVE_RC -> \a -> a @~ this_will_deactivate_currently_active_rc
  REMOVED -> \a -> a @~ removed
  DEACTIVATED -> \a -> a @~ deactivated
  RIDE_TYPE_SELECT -> \a -> a @~ ride_type_select
  DEACTIVATE -> \a -> a @~ deactivate
  VEHICLES_PENDING -> \a -> a @~ vehicles_pending
  IS_ACTIVE_NOW -> \a -> a @~ is_active_now
  SINGLE_RC_CANNOT_BE_DELETED -> \a -> a @~ single_rc_cannot_be_deleted
  CANCELLATION_RATE -> \a -> a @~ cancellation_rate
  RIDES_CANCELLED -> \a -> a @~ rides_cancelled
  EARNINGS_MISSED -> \a -> a @~ earnings_missed
  SUMMARY -> \a -> a @~ summary
  NAMMA_BONUS arg1 -> \a -> (a @~ namma_bonus) arg1
  TRIPS_COMPLETED -> \a -> a @~ trips_completed
  LATE_NIGHT_TRIPS -> \a -> a @~ late_night_trips
  ABOUT_ME -> \a -> a @~ about_me
  ABOUT_VEHICLE -> \a -> a @~ about_vehicle
  ADD -> \a -> a @~ add
  YEARS_OLD -> \a -> a @~ years_old
  FROM_WHERE -> \a -> a @~ from_where
  MISSED_OPPORTUNITY -> \a -> a @~ missed_opportunity
  EARNED_ON_APP arg1 -> \a -> (a @~ earned_on_app) arg1
  TRAVELLED_ON_APP arg1 -> \a -> (a @~ travelled_on_app) arg1
  HOW_OLD_IS_YOUR_VEHICLE -> \a -> a @~ how_old_is_your_vehicle
  ENTER_NAME_OF_VEHICLE -> \a -> a @~ enter_name_of_vehicle
  NEW_ -> \a -> a @~ new_
  WITH -> \a -> a @~ with
  TOTAL_MONEY_COLLECTED -> \a -> a @~ total_money_collected
  FARE_EARNED_OF_THE_DAY -> \a -> a @~ fare_earned_of_the_day
  GST_PLUS_PAYABLE -> \a -> a @~ gst_plus_payable
  TO_CONTINUE_USING_YATRI_SATHI -> \a -> a @~ to_continue_using_yatri_sathi
  PAY -> \a -> a @~ pay
  LATER -> \a -> a @~ later
  GREAT_JOB -> \a -> a @~ great_job
  FEE_BREAKUP -> \a -> a @~ fee_breakup
  YATRI_SATHI_FEE_PAYABLE_FOR_DATE arg1 -> \a -> (a @~ yatri_sathi_fee_payable_for_date) arg1
  FEE_CORRESPONDING_TO_THE_DISTANCE -> \a -> a @~ fee_corresponding_to_the_distance
  PLATFORM_FEE -> \a -> a @~ platform_fee
  GST -> \a -> a @~ gst
  TOTAL_PAYABLE -> \a -> a @~ total_payable
  GOT_IT -> \a -> a @~ got_it
  VIEW_DETAILS -> \a -> a @~ view_details
  PAYMENT_SUCCESSFUL -> \a -> a @~ payment_successful
  PAYMENT_PENDING -> \a -> a @~ payment_pending
  PAYMENT_FAILED -> \a -> a @~ payment_failed
  PAYMENT_PENDING_DESC -> \a -> a @~ payment_pending_desc
  PAYMENT_FAILED_DESC arg1 -> \a -> (a @~ payment_failed_desc) arg1
  WE_WILL_NOTIFY_WHEN_PAYMENT_SUCCESS -> \a -> a @~ we_will_notify_when_payment_success
  CONTINUE_TAKING_RIDES -> \a -> a @~ continue_taking_rides
  YOUR_PREVIOUS_PAYMENT_IS_PENDING -> \a -> a @~ your_previous_payment_is_pending
  GOVERMENT_CHARGES -> \a -> a @~ goverment_charges
  TODAY -> \a -> a @~ today
  OKAY -> \a -> a @~ okay
  NO_PAYMENT_HISTORY_AVAILABLE -> \a -> a @~ no_payment_history_available
  YOU_DONT_HAVE_ANY_PAYMENTS -> \a -> a @~ you_dont_have_any_payments
  ENTER_AADHAAR_NUMBER -> \a -> a @~ enter_aadhaar_number
  ENTER_AADHAAR_DETAILS -> \a -> a @~ enter_aadhaar_details
  ENTER_AADHAAR_OTP_ -> \a -> a @~ enter_aadhaar_otp_
  AADHAAR_LINKING_REQUIRED -> \a -> a @~ aadhaar_linking_required
  CUSTOMER_ADDED_A_STOP -> \a -> a @~ customer_added_a_stop
  AADHAAR_LINKING_REQUIRED_DESCRIPTION arg1 -> \a -> (a @~ aadhaar_linking_required_description) arg1
  BY_CLICKING_THIS_YOU_WILL_BE_AGREEING_TO_OUR_TC -> \a -> a @~ by_clicking_this_you_will_be_agreeing_to_our_tc
  TERMS_AND_CONDITIONS_SHORT -> \a -> a @~ terms_and_conditions_short
  OTP_SENT_TO_AADHAAR_NUMBER -> \a -> a @~ otp_sent_to_aadhaar_number
  ENTER_SIX_DIGIT_OTP -> \a -> a @~ enter_six_digit_otp
  TC_TAIL -> \a -> a @~ tc_tail
  LINK_AADHAAR_ID -> \a -> a @~ link_aadhaar_id
  NAVIGATE_TO_LOCATION -> \a -> a @~ navigate_to_location
  NO_MOBILE_NUMBER_REGISTERED -> \a -> a @~ no_mobile_number_registered
  EXCEED_OTP_GENERATION_LIMIT -> \a -> a @~ exceed_otp_generation_limit
  AADHAAR_NUMBER_NOT_EXIST -> \a -> a @~ aadhaar_number_not_exist
  INVALID_OTP -> \a -> a @~ invalid_otp
  NO_SHARE_CODE -> \a -> a @~ no_share_code
  WRONG_SHARE_CODE -> \a -> a @~ wrong_share_code
  INVALID_SHARE_CODE -> \a -> a @~ invalid_share_code
  SESSION_EXPIRED -> \a -> a @~ session_expired
  OTP_ATTEMPT_EXCEEDED -> \a -> a @~ otp_attempt_exceeded
  UPSTREAM_INTERNAL_SERVER_ERROR -> \a -> a @~ upstream_internal_server_error
  TRANSACTION_ALREADY_COMPLETED -> \a -> a @~ transaction_already_completed
  GOTO_YOUR_NEAREST_BOOTH -> \a -> a @~ goto_your_nearest_booth
  AADHAAR_ALREADY_LINKED -> \a -> a @~ aadhaar_already_linked
  OPTIONAL -> \a -> a @~ optional
  DOWNLOAD_STATEMENT -> \a -> a @~ download_statement
  SELECT_A_DATE_RANGE -> \a -> a @~ select_a_date_range
  FEE_PAYMENT_HISTORY -> \a -> a @~ fee_payment_history
  LANGUAGES_SPOKEN -> \a -> a @~ languages_spoken
  VIEW_PAYMENT_HISTORY -> \a -> a @~ view_payment_history
  RIDE_TYPE -> \a -> a @~ ride_type
  RC_STATUS -> \a -> a @~ rc_status
  RATED_BY_USERS1 -> \a -> a @~ rated_by_users1
  RATED_BY_USERS2 -> \a -> a @~ rated_by_users2
  MONTHS -> \a -> a @~ months
  RC_ADDED_SUCCESSFULLY -> \a -> a @~ rc_added_successfully
  CALL_REQUEST_HAS_BEEN_PLACED -> \a -> a @~ call_request_has_been_placed
  TRIP_DATE -> \a -> a @~ trip_date
  OFFER_APPLIED -> \a -> a @~ offer_applied
  YOUR_EARNINGS -> \a -> a @~ your_earnings
  NUMBER_OF_RIDES -> \a -> a @~ number_of_rides
  FARE_BREAKUP -> \a -> a @~ fare_breakup
  MY_PLAN -> \a -> a @~ my_plan
  YOUR_DUES -> \a -> a @~ your_dues
  YOUR_DUES_DESCRIPTION -> \a -> a @~ your_dues_description
  YOUR_DUES_DESCRIPTION_MANUAL -> \a -> a @~ your_dues_description_manual
  CURRENT_DUES -> \a -> a @~ current_dues
  YOUR_LIMIT -> \a -> a @~ your_limit
  DUE_DETAILS -> \a -> a @~ due_details
  AMOUNT -> \a -> a @~ amount
  VIEW_DUE_DETAILS -> \a -> a @~ view_due_details
  SETUP_AUTOPAY -> \a -> a @~ setup_autopay
  CURRENT_PLAN -> \a -> a @~ current_plan
  ALTERNATE_PLAN -> \a -> a @~ alternate_plan
  AUTOPAY_DETAILS -> \a -> a @~ autopay_details
  CANCEL_AUTOPAY_STR -> \a -> a @~ cancel_autopay_str
  WE_MIGHT_BE_LOST -> \a -> a @~ we_might_be_lost
  EXEPERIENCING_ERROR -> \a -> a @~ exeperiencing_error
  ENJOY_THESE_BENEFITS -> \a -> a @~ enjoy_these_benefits
  CHOOSE_YOUR_PLAN arg1 -> \a -> (a @~ choose_your_plan) arg1
  SKIP_FOR_NOW -> \a -> a @~ skip_for_now
  N_DAY_FREE_TRIAL_ACTIVATED arg1 -> \a -> (a @~ n_day_free_trial_activated) arg1
  TAKE_N_RIDES_FOR_THE_NEXT_N_DAYS arg1 arg2 -> \a -> (a @~ take_n_rides_for_the_next_n_days) arg1 arg2
  EVERY_RIDE_AT_ZERO_COMMISSION -> \a -> a @~ every_ride_at_zero_commission
  EARN_UPTO_PER_DAY -> \a -> a @~ earn_upto_per_day
  HOW_THIS_WORKS -> \a -> a @~ how_this_works
  SIGN_UP_FOR_AUTOPAY_BY_PAYING_JUST -> \a -> a @~ sign_up_for_autopay_by_paying_just
  GET_REMINDED_ABOUT_YOUR_PLAN_SETUP -> \a -> a @~ get_reminded_about_your_plan_setup
  FREE_TRIAL_REMINDER_N_DAYS_M_RIDES arg1 arg2 -> \a -> (a @~ free_trial_reminder_n_days_m_rides) arg1 arg2
  PLAN_STARTS_N_DAYS_M_RIDES arg1 arg2 -> \a -> (a @~ plan_starts_n_days_m_rides) arg1 arg2
  EASY_AUTOMATIC_PAYMENTS_START -> \a -> a @~ easy_automatic_payments_start
  FREE_UNTIL -> \a -> a @~ free_until
  PER_RIDE -> \a -> a @~ per_ride
  PER_DAY -> \a -> a @~ per_day
  OFFER -> \a -> a @~ offer
  OFFERS -> \a -> a @~ offers
  YOU_ARE_ON_THE_FREE_TRIAL -> \a -> a @~ you_are_on_the_free_trial
  SETUP_AUTOPAY_BEFORE_THE_TRAIL_PERIOD_EXPIRES -> \a -> a @~ setup_autopay_before_the_trail_period_expires
  GET_FREE_TRAIL_UNTIL -> \a -> a @~ get_free_trail_until
  CLEAR_DUES -> \a -> a @~ clear_dues
  PAYMENT_PENDING_ALERT -> \a -> a @~ payment_pending_alert
  PAYMENT_PENDING_ALERT_DESC arg1 -> \a -> (a @~ payment_pending_alert_desc) arg1
  LOW_ACCOUNT_BALANCE -> \a -> a @~ low_account_balance
  LOW_ACCOUNT_BALANCE_DESC -> \a -> a @~ low_account_balance_desc
  OKAY_GOT_IT -> \a -> a @~ okay_got_it
  LIMITED_TIME_OFFER -> \a -> a @~ limited_time_offer
  JOIN_NOW -> \a -> a @~ join_now
  AUTOMATIC_PAYMENTS_WILL_APPEAR_HERE -> \a -> a @~ automatic_payments_will_appear_here
  MANUAL_PAYMENTS_WILL_APPEAR_HERE -> \a -> a @~ manual_payments_will_appear_here
  MANUAL_PAYMENTS -> \a -> a @~ manual_payments
  NO_AUTOMATIC_PAYMENTS_DESC -> \a -> a @~ no_automatic_payments_desc
  NO_MANUAL_PAYMENTS_DESC -> \a -> a @~ no_manual_payments_desc
  PAYMENT_HISTORY -> \a -> a @~ payment_history
  PLAN -> \a -> a @~ plan
  DAY -> \a -> a @~ day
  TAP_A_PLAN_TO_VIEW_DETAILS -> \a -> a @~ tap_a_plan_to_view_details
  PLANS -> \a -> a @~ plans
  HOW_IT_WORKS arg1 -> \a -> (a @~ how_it_works) arg1
  ZERO_COMMISION -> \a -> a @~ zero_commision
  EARN_TODAY_PAY_TOMORROW -> \a -> a @~ earn_today_pay_tomorrow
  PAY_ONLY_IF_YOU_TAKE_RIDES -> \a -> a @~ pay_only_if_you_take_rides
  MANAGE_PLAN -> \a -> a @~ manage_plan
  VIEW_AUTOPAY_DETAILS -> \a -> a @~ view_autopay_details
  SWITCH_AND_SAVE -> \a -> a @~ switch_and_save
  SWITCH_AND_SAVE_DESC -> \a -> a @~ switch_and_save_desc
  SWITCH_NOW -> \a -> a @~ switch_now
  PAYMENT_MODE_CHANGED_TO_MANUAL -> \a -> a @~ payment_mode_changed_to_manual
  PAYMENT_MODE_CHANGED_TO_MANUAL_DESC -> \a -> a @~ payment_mode_changed_to_manual_desc
  AUTOPAY_PAYMENTS -> \a -> a @~ autopay_payments
  SUCCESS -> \a -> a @~ success
  TRANSACTION_ON -> \a -> a @~ transaction_on
  DEBITED_ON -> \a -> a @~ debited_on
  RIDES_TAKEN_ON -> \a -> a @~ rides_taken_on
  JOIN_PLAN -> \a -> a @~ join_plan
  JOIN_NAMMAA_YATRI -> \a -> a @~ join_nammaa_yatri
  CANCEL_AUTOPAY_AND_PAY_MANUALLY -> \a -> a @~ cancel_autopay_and_pay_manually
  PLAN_ACTIVATED_SUCCESSFULLY -> \a -> a @~ plan_activated_successfully
  DUES_CLEARED_SUCCESSFULLY -> \a -> a @~ dues_cleared_successfully
  NOT_PLANNING_TO_TAKE_RIDES -> \a -> a @~ not_planning_to_take_rides
  RETRY_PAYMENT_STR -> \a -> a @~ retry_payment_str
  PAUSE_AUTOPAY_STR -> \a -> a @~ pause_autopay_str
  SETUP_AUTOPAY_STR -> \a -> a @~ setup_autopay_str
  VIEW_RIDE_DETAILS -> \a -> a @~ view_ride_details
  ACCOUNT -> \a -> a @~ account
  AUTOPAY_IS_NOT_ENABLED_YET -> \a -> a @~ autopay_is_not_enabled_yet
  ENABLE_AUTOPAY_DESC -> \a -> a @~ enable_autopay_desc
  ENABLE_AUTOPAY_NOW -> \a -> a @~ enable_autopay_now
  AUTOPAY_SETUP_PENDING_STR -> \a -> a @~ autopay_setup_pending_str
  AUTOPAY_PENDING_DESC_STR -> \a -> a @~ autopay_pending_desc_str
  REFRESH_STR -> \a -> a @~ refresh_str
  TRANSACTION_DETAILS -> \a -> a @~ transaction_details
  RIDE_DETAILS -> \a -> a @~ ride_details
  MY_PLAN_TITLE -> \a -> a @~ my_plan_title
  SWITCH_TO -> \a -> a @~ switch_to
  YOUR_RENTAL_RIDE_STARTS_IN -> \a -> a @~ your_rental_ride_starts_in
  YOUR_INTERCITY_RIDE_STARTS_IN -> \a -> a @~ your_intercity_ride_starts_in
  PLEASE_TRY_AGAIN -> \a -> a @~ please_try_again
  PLAN_NOT_FOUND -> \a -> a @~ plan_not_found
  MANDATE_NOT_FOUND -> \a -> a @~ mandate_not_found
  ACTIVE_MANDATE_EXISTS -> \a -> a @~ active_mandate_exists
  NO_ACTIVE_MANDATE_EXIST -> \a -> a @~ no_active_mandate_exist
  NO_PLAN_FOR_DRIVER -> \a -> a @~ no_plan_for_driver
  INVALID_PAYMENT_MODE -> \a -> a @~ invalid_payment_mode
  INVALID_AUTO_PAY_STATUS -> \a -> a @~ invalid_auto_pay_status
  MAX_AMOUNT -> \a -> a @~ max_amount
  FREQUENCY -> \a -> a @~ frequency
  STATRED_ON -> \a -> a @~ statred_on
  EXPIRES_ON -> \a -> a @~ expires_on
  SWITCHED_PLAN -> \a -> a @~ switched_plan
  RESUMED_AUTOPAY -> \a -> a @~ resumed_autopay
  ONETIME -> \a -> a @~ onetime
  WEEKLY -> \a -> a @~ weekly
  FORTNIGHTLY -> \a -> a @~ fortnightly
  MONTHLY -> \a -> a @~ monthly
  BIMONTHLY -> \a -> a @~ bimonthly
  QUARTERLY -> \a -> a @~ quarterly
  HALFYEARLY -> \a -> a @~ halfyearly
  YEARLY -> \a -> a @~ yearly
  ASPRESENTED -> \a -> a @~ aspresented
  FIRST_FREE_RIDE -> \a -> a @~ first_free_ride
  DAILY_PER_RIDE_DESC -> \a -> a @~ daily_per_ride_desc
  JOIN_THE_UNLIMITED_PLAN -> \a -> a @~ join_the_unlimited_plan
  MAYBE_LATER -> \a -> a @~ maybe_later
  DO_YOU_WANT_TO_CANCEL -> \a -> a @~ do_you_want_to_cancel
  DO_YOU_WANT_TO_CANCEL_DESC -> \a -> a @~ do_you_want_to_cancel_desc
  YOUR_PAYMENT_WAS_UNSUCCESSFUL -> \a -> a @~ your_payment_was_unsuccessful
  PAYMENT_CANCELLED -> \a -> a @~ payment_cancelled
  MANUAL_PAYMENT_STR -> \a -> a @~ manual_payment_str
  UPI_AUTOPAY_S -> \a -> a @~ upi_autopay_s
  DAILY_UNLIMITED -> \a -> a @~ daily_unlimited
  DAILY_PER_RIDE -> \a -> a @~ daily_per_ride
  DAILY_UNLIMITED_PLAN_DESC -> \a -> a @~ daily_unlimited_plan_desc
  DAILY_PER_RIDE_PLAN_DESC arg1 -> \a -> (a @~ daily_per_ride_plan_desc) arg1
  AUTOPAY_CANCELLED -> \a -> a @~ autopay_cancelled
  NO -> \a -> a @~ no
  YES_CANCEL -> \a -> a @~ yes_cancel
  PAY_TO_JOIN_THIS_PLAN -> \a -> a @~ pay_to_join_this_plan
  OFFERS_NOT_APPLICABLE -> \a -> a @~ offers_not_applicable
  PAUSED_STR -> \a -> a @~ paused_str
  PENDING_STR -> \a -> a @~ pending_str
  SWITCH_PLAN_STR -> \a -> a @~ switch_plan_str
  OFFERS_APPLICABLE_ON_DAILY_UNLIMITED -> \a -> a @~ offers_applicable_on_daily_unlimited
  DAILY_UNLIMITED_OFFER_NOT_AVAILABLE -> \a -> a @~ daily_unlimited_offer_not_available
  PLAN_SWITCHED_TO -> \a -> a @~ plan_switched_to
  NO_RIDES_NO_CHARGE -> \a -> a @~ no_rides_no_charge
  GET_SPECIAL_OFFERS arg1 -> \a -> (a @~ get_special_offers) arg1
  VALID_ONLY_IF_PAYMENT -> \a -> a @~ valid_only_if_payment
  HELP_STR -> \a -> a @~ help_str
  REFRESH_STRING -> \a -> a @~ refresh_string
  CHAT_FOR_HELP -> \a -> a @~ chat_for_help
  VIEW_FAQs -> \a -> a @~ view_faqs
  FIND_HELP_CENTRE arg1 -> \a -> (a @~ find_help_centre) arg1
  CONTACT -> \a -> a @~ contact
  GO_TO_LOCATION -> \a -> a @~ go_to_location
  NO_HELP_CENTER_IS_ACTIVE_NOW -> \a -> a @~ no_help_center_is_active_now
  HELP_CENTERS_LOCATION_WILL_APPEAR_HERE_ONCE_THEY_ARE_ACTIVE -> \a -> a @~ help_centers_location_will_appear_here_once_they_are_active
  SUPPORT -> \a -> a @~ support
  NEED_HELP_JOINING_THE_PLAN -> \a -> a @~ need_help_joining_the_plan
  NEED_HELP -> \a -> a @~ need_help
  SETUP_AUTOPAY_NOW_TO_GET_SPECIAL_DISCOUNTS -> \a -> a @~ setup_autopay_now_to_get_special_discounts
  SETUP_NOW -> \a -> a @~ setup_now
  GO_TO_VEHICLE_DETAILS -> \a -> a @~ go_to_vehicle_details
  CLOSE -> \a -> a @~ close
  RC_DEACTIVATED -> \a -> a @~ rc_deactivated
  RC_DEACTIVATED_DETAILS -> \a -> a @~ rc_deactivated_details
  CUSTOMER_HAS_LOW_MOBILITY -> \a -> a @~ customer_has_low_mobility
  CUSTOMER_HAS_DISABILITY -> \a -> a @~ customer_has_disability
  CUSTOMER_HAS_LOW_VISION -> \a -> a @~ customer_has_low_vision
  CUSTOMER_HAS_HEARING_IMPAIRMENT -> \a -> a @~ customer_has_hearing_impairment
  HELP_WITH_THEIR_MOBILITY_AID -> \a -> a @~ help_with_their_mobility_aid
  PLEASE_ASSIST_THEM_IF_NEEDED -> \a -> a @~ please_assist_them_if_needed
  MESSAGE_THEM_AT_PICKUP -> \a -> a @~ message_them_at_pickup
  SOUND_HORN_ONCE_AT_PICKUP -> \a -> a @~ sound_horn_once_at_pickup
  PLEASE_CALL_AND_AVOID_CHATS -> \a -> a @~ please_call_and_avoid_chats
  PLEASE_CHAT_AND_AVOID_CALLS -> \a -> a @~ please_chat_and_avoid_calls
  PLEASE_GO_TO_EXACT_PICKUP -> \a -> a @~ please_go_to_exact_pickup
  CUSTOMER_HAS_POOR_VISION_SOUND_HORN_AT_PICKUP -> \a -> a @~ customer_has_poor_vision_sound_horn_at_pickup
  CUSTOMER_HAS_POOR_HEARING_MESSAGE_THEM_AT_PICKUP -> \a -> a @~ customer_has_poor_hearing_message_them_at_pickup
  CUSTOMER_HAS_LOW_MOBILITY_STORE_THEIR_SUPPORT_AT_PICKUP -> \a -> a @~ customer_has_low_mobility_store_their_support_at_pickup
  CUSTOMER_HAS_DISABILITY_PLEASE_ASSIST_THEM -> \a -> a @~ customer_has_disability_please_assist_them
  CUSTOMER_MAY_NEED_ASSISTANCE -> \a -> a @~ customer_may_need_assistance
  LEARN_MORE -> \a -> a @~ learn_more
  CUSTOMER_HAS_LOW_MOBILITY_GO_TO_EXACT_LOC -> \a -> a @~ customer_has_low_mobility_go_to_exact_loc
  CUSTOMER_HAS_POOR_HEARING_CHAT_WITH_THEM_INSTEAD_OF_CALLING -> \a -> a @~ customer_has_poor_hearing_chat_with_them_instead_of_calling
  CUSTOMER_HAS_LOW_VISION_CALL_THEM_INSTEAD_OF_CHATTING -> \a -> a @~ customer_has_low_vision_call_them_instead_of_chatting
  PLEASE_HELP_THEM_AS_YOU_CAN -> \a -> a @~ please_help_them_as_you_can
  LEARN_HOW_YOU_CAN_HELP_CUSTOMERS_REQUIRING_SPECIAL_ASSISTANCE -> \a -> a @~ learn_how_you_can_help_customers_requiring_special_assistance
  ASSISTANCE_REQUIRED -> \a -> a @~ assistance_required
  SAVED_DUE_TO_ZERO_COMMISSION -> \a -> a @~ saved_due_to_zero_commission
  TIP_EARNED_FROM_CUSTOMER -> \a -> a @~ tip_earned_from_customer
  COLLECT_VIA_CASE_UPI -> \a -> a @~ collect_via_case_upi
  FARE_COLLECTED -> \a -> a @~ fare_collected
  RATE_YOUR_RIDE_WITH1 -> \a -> a @~ rate_your_ride_with1
  RATE_YOUR_RIDE_WITH2 -> \a -> a @~ rate_your_ride_with2
  HELP_US_WITH_YOUR_FEEDBACK -> \a -> a @~ help_us_with_your_feedback
  COLLECT_CASH -> \a -> a @~ collect_cash
  ONLINE_PAYMENT -> \a -> a @~ online_payment
  RIDE_COMPLETED -> \a -> a @~ ride_completed
  SUBMIT_FEEDBACK -> \a -> a @~ submit_feedback
  BADGE_EARNED -> \a -> a @~ badge_earned
  PURPLE_RIDE_CHAMPION -> \a -> a @~ purple_ride_champion
  PURPLE_RIDE -> \a -> a @~ purple_ride
  PROCEED_TO_CHAT -> \a -> a @~ proceed_to_chat
  PLEASE_CONSIDER_CALLING_THEM -> \a -> a @~ please_consider_calling_them
  JOIN_A_PLAN_TO_START_EARNING -> \a -> a @~ join_a_plan_to_start_earning
  GO_ONLINE_PROMPT_SUBSCRIBE -> \a -> a @~ go_online_prompt_subscribe
  GO_ONLINE_PROMPT_PAYMENT_PENDING -> \a -> a @~ go_online_prompt_payment_pending
  COMPLETE_PAYMENT_TO_CONTINUE arg1 -> \a -> (a @~ complete_payment_to_continue) arg1
  DOWNGRADE_AVAILABLE_ONLY_FOR_AC_VEHICLES -> \a -> a @~ downgrade_available_only_for_ac_vehicles
  DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_1 -> \a -> a @~ downgrading_vehicle_will_allow_you_to_take_both_1
  DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_2 -> \a -> a @~ downgrading_vehicle_will_allow_you_to_take_both_2
  DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_3 -> \a -> a @~ downgrading_vehicle_will_allow_you_to_take_both_3
  AC_CAB -> \a -> a @~ ac_cab
  AC_SUV -> \a -> a @~ ac_suv
  DOWNGRADE_VEHICLE -> \a -> a @~ downgrade_vehicle
  RENTAL_BOOKINGS -> \a -> a @~ rental_bookings
  RENTAL_BOOKINGS_DESCRIPTION -> \a -> a @~ rental_bookings_description
  PENDING_CAPS -> \a -> a @~ pending_caps
  FAILURE -> \a -> a @~ failure
  PAYMENT_MODE -> \a -> a @~ payment_mode
  TXN_ID -> \a -> a @~ txn_id
  AMOUNT_PAID -> \a -> a @~ amount_paid
  NOTIFICATION_SCHEDULED -> \a -> a @~ notification_scheduled
  MANUAL_DUES -> \a -> a @~ manual_dues
  AUTOPAY_IN_PROGRESS -> \a -> a @~ autopay_in_progress
  MANUAL_DUE_OVERVIEW -> \a -> a @~ manual_due_overview
  AUTOPAY_DUE_OVERVIEW -> \a -> a @~ autopay_due_overview
  MANUAL_DUE_AS_AUTOPAY_EXECUTION_FAILED -> \a -> a @~ manual_due_as_autopay_execution_failed
  CLEAR_MANUAL_DUES -> \a -> a @~ clear_manual_dues
  DUE_OVERVIEW -> \a -> a @~ due_overview
  MANUAL_DUE_DETAILS -> \a -> a @~ manual_due_details
  AUTOPAY_DUE_DETAILS -> \a -> a @~ autopay_due_details
  SWITCHED_TO_MANUAL -> \a -> a @~ switched_to_manual
  SPLIT_PAYMENT -> \a -> a @~ split_payment
  GST_INCLUDE -> \a -> a @~ gst_include
  SCHEDULED_AT -> \a -> a @~ scheduled_at
  PAYMENT_STATUS -> \a -> a @~ payment_status
  NOTIFICATION_ATTEMPTING -> \a -> a @~ notification_attempting
  EXECUTION_SCHEDULED -> \a -> a @~ execution_scheduled
  EXECUTION_ATTEMPTING -> \a -> a @~ execution_attempting
  EXECUTION_SUCCESS -> \a -> a @~ execution_success
  SCHEDULED -> \a -> a @~ scheduled
  ONE_TIME_SETTLEMENT -> \a -> a @~ one_time_settlement
  PAYMENT_SCHEDULED -> \a -> a @~ payment_scheduled
  RETRY_AUTOPAY -> \a -> a @~ retry_autopay
  RETRY_STR -> \a -> a @~ retry_str
  ONGOING_PAYMENT_EXECUTION -> \a -> a @~ ongoing_payment_execution
  OFFER_CARD_BANNER_TITLE arg1 arg2 arg3 -> \a -> (a @~ offer_card_banner_title) arg1 arg2 arg3
  OFFER_CARD_BANNER_DESC -> \a -> a @~ offer_card_banner_desc
  OFFER_CARD_BANNER_ALERT -> \a -> a @~ offer_card_banner_alert
  OR -> \a -> a @~ or
  COLLECT_CASH_DIRECTLY -> \a -> a @~ collect_cash_directly
  OR_COLLECT_CASH_DIRECTLY -> \a -> a @~ or_collect_cash_directly
  SETUP_AUTOPAY_TO_ACCEPT_PAYMENT -> \a -> a @~ setup_autopay_to_accept_payment
  DOWNLOAD_QR -> \a -> a @~ download_qr
  USE_THIS_QR_TO_COLLECT_PAYMENT -> \a -> a @~ use_this_qr_to_collect_payment
  AMOUNT_WILL_DEPOSITED_TO_BANK_ACCOUNT -> \a -> a @~ amount_will_deposited_to_bank_account
  GET_DIRECTLY_TO_YOUR_BANK_ACCOUNT -> \a -> a @~ get_directly_to_your_bank_account
  PAYMENT -> \a -> a @~ payment
  QR_CODE -> \a -> a @~ qr_code
  GET_QR_CODE -> \a -> a @~ get_qr_code
  EXECUTION_FAILED -> \a -> a @~ execution_failed
  NOTIFICATION_FAILED -> \a -> a @~ notification_failed
  CLEAR_DUES_BANNER_TITLE -> \a -> a @~ clear_dues_banner_title
  PAY_NOW -> \a -> a @~ pay_now
  COLLECT_VIA_UPI_QR_OR_CASH -> \a -> a @~ collect_via_upi_qr_or_cash
  TRANSACTION_DEBITED_ON -> \a -> a @~ transaction_debited_on
  TRANSACTION_ATTEMPTED_ON -> \a -> a @~ transaction_attempted_on
  AUTOPAY_SETUP_AND_PAYMENT_SUCCESSFUL -> \a -> a @~ autopay_setup_and_payment_successful
  AUTOPAY_SETUP_SUCCESSFUL -> \a -> a @~ autopay_setup_successful
  AUTOPAY_SETUP_AND_PAYMENT_PENDING -> \a -> a @~ autopay_setup_and_payment_pending
  AUTOPAY_SETUP_PENDING -> \a -> a @~ autopay_setup_pending
  AUTOPAY_SETUP_AND_PAYMENT_FAILED -> \a -> a @~ autopay_setup_and_payment_failed
  AUTOPAY_SETUP_FAILED -> \a -> a @~ autopay_setup_failed
  ONE_TIME_REGISTERATION -> \a -> a @~ one_time_registeration
  CLEARANCE_AND_REGISTERATION -> \a -> a @~ clearance_and_registeration
  UPI_AUTOPAY_SETUP -> \a -> a @~ upi_autopay_setup
  WATCH_VIDEO_FOR_HELP -> \a -> a @~ watch_video_for_help
  PAYMENT_PENDING_SOFT_NUDGE -> \a -> a @~ payment_pending_soft_nudge
  CLEAR_YOUR_DUES_EARLY -> \a -> a @~ clear_your_dues_early
  DUE_LIMIT_WARNING_BANNER_TITLE -> \a -> a @~ due_limit_warning_banner_title
  SCHEDULED_ON -> \a -> a @~ scheduled_on
  ATTEMPTED_ON -> \a -> a @~ attempted_on
  FREE_TRIAL_ENDING_TOMORROW -> \a -> a @~ free_trial_ending_tomorrow
  FREE_TRIAL_ENDS_TONIGHT -> \a -> a @~ free_trial_ends_tonight
  JOIN_A_PLAN_TO_CONTINUE_TAKING_RIDES -> \a -> a @~ join_a_plan_to_continue_taking_rides
  SETUP_AUTOPAY_FOR_EASY_PAYMENTS -> \a -> a @~ setup_autopay_for_easy_payments
  LOW_DUES_CLEAR_POPUP_DESC -> \a -> a @~ low_dues_clear_popup_desc
  DUES_PENDING -> \a -> a @~ dues_pending
  DAYS -> \a -> a @~ days
  ACTIVE_PLAN -> \a -> a @~ active_plan
  WHAT_ARE_PURPLE_RIDES -> \a -> a @~ what_are_purple_rides
  ECONOMICAL -> \a -> a @~ economical
  SPACIOUS -> \a -> a @~ spacious
  COMFY -> \a -> a @~ comfy
  PEOPLE -> \a -> a @~ people
  GO_TO -> \a -> a @~ go_to
  SELECT_ON_MAP -> \a -> a @~ select_on_map
  CONFIRM_LOCATION_STR -> \a -> a @~ confirm_location_str
  SAVE_LOCATION_STR -> \a -> a @~ save_location_str
  REMOVE_PREF_LOC -> \a -> a @~ remove_pref_loc
  CONF_REMOVE_PREF_LOC -> \a -> a @~ conf_remove_pref_loc
  YES_REMOVE -> \a -> a @~ yes_remove
  ADD_LOCATION -> \a -> a @~ add_location
  ADD_ANOTHER_LOCATION -> \a -> a @~ add_another_location
  ADD_A_GOTO_LOC -> \a -> a @~ add_a_goto_loc
  GOTO_LOC_LEFT -> \a -> a @~ goto_loc_left
  CURRENT_LOCATION -> \a -> a @~ current_location
  CONF_GOTO_LOC -> \a -> a @~ conf_goto_loc
  GOTO_LOCS -> \a -> a @~ goto_locs
  LOCATION_STR -> \a -> a @~ location_str
  ADD_TAG -> \a -> a @~ add_tag
  ONLY_ONE_LOC_CAN_ADDED -> \a -> a @~ only_one_loc_can_added
  SAVE_AS -> \a -> a @~ save_as
  NO_GOTO_LOC_ADDED -> \a -> a @~ no_goto_loc_added
  GOTO_LOC_HELPS_YOU -> \a -> a @~ goto_loc_helps_you
  YOU_ARE_VERY_CLOSE -> \a -> a @~ you_are_very_close
  GOTO_IS_APPLICABLE_FOR -> \a -> a @~ goto_is_applicable_for
  CANCEL_ANYWAY -> \a -> a @~ cancel_anyway
  GOTO_MAYBE_REDUCED -> \a -> a @~ goto_maybe_reduced
  CANCEL_OF_GOTO -> \a -> a @~ cancel_of_goto
  MORE_GOTO_RIDE_COMING -> \a -> a @~ more_goto_ride_coming
  MORE_GOTO_RIDE_COMING_DESC -> \a -> a @~ more_goto_ride_coming_desc
  GOTO_REDUCED_TO_ZERO -> \a -> a @~ goto_reduced_to_zero
  DUE_TO_MULTIPLE_CANCELLATIONS -> \a -> a @~ due_to_multiple_cancellations
  OK_GOT_IT -> \a -> a @~ ok_got_it
  GOTO_REDUCED_TO -> \a -> a @~ goto_reduced_to
  VALIDITY_EXPIRED_STR -> \a -> a @~ validity_expired_str
  VALIDITY_EXPIRED_DESC -> \a -> a @~ validity_expired_desc
  KNOW_MORE -> \a -> a @~ know_more
  THIS_FEATURE_WILL_BE_APPLICABLE -> \a -> a @~ this_feature_will_be_applicable
  GOTO_LOC_ADDED -> \a -> a @~ goto_loc_added
  GOTO_LOC_REMOVED -> \a -> a @~ goto_loc_removed
  GOTO_LOC_UPDATED -> \a -> a @~ goto_loc_updated
  GOTO_LOC_IS_ENABLED -> \a -> a @~ goto_loc_is_enabled
  GOTO_LOC_IS_DISABLED -> \a -> a @~ goto_loc_is_disabled
  GOTO_LOCATIONS -> \a -> a @~ goto_locations
  CHOOSE_A_GOTO_LOC -> \a -> a @~ choose_a_goto_loc
  YOU_HAVE_ONLY_LEFT_FOR_TODAY -> \a -> a @~ you_have_only_left_for_today
  YES_ENABLE -> \a -> a @~ yes_enable
  NO_GOTO_LOCS_ADDED_YET -> \a -> a @~ no_goto_locs_added_yet
  NO_GOTO_LOCS_ADDED_YET_DESC -> \a -> a @~ no_goto_locs_added_yet_desc
  ENABLE_GOTO -> \a -> a @~ enable_goto
  GO_TO_CANCELLATION_TITLE -> \a -> a @~ go_to_cancellation_title
  GO_TO_CANCELLATION_DESC -> \a -> a @~ go_to_cancellation_desc
  DISABLE_GOTO_STR -> \a -> a @~ disable_goto_str
  YOU_STILL_HAVE_TIME_LEFT -> \a -> a @~ you_still_have_time_left
  YES_DISABLE -> \a -> a @~ yes_disable
  GOTO_LOC_REACHED -> \a -> a @~ goto_loc_reached
  YOU_ARE_ALMOST_AT_LOCATION -> \a -> a @~ you_are_almost_at_location
  DRIVER_HOME_LOCATION_NOT_FOUND -> \a -> a @~ driver_home_location_not_found
  DRIVER_HOME_LOCATION_DOES_NOT_EXIST -> \a -> a @~ driver_home_location_does_not_exist
  DRIVER_HOME_LOCATION_LIMIT_REACHED -> \a -> a @~ driver_home_location_limit_reached
  DRIVER_GO_HOME_REQUEST_NOT_FOUND -> \a -> a @~ driver_go_home_request_not_found
  DRIVER_GO_HOME_REQUEST_DOES_NOT_EXIST -> \a -> a @~ driver_go_home_request_does_not_exist
  DRIVER_GO_HOME_REQUEST_DAILY_USAGE_LIMIT_REACHED -> \a -> a @~ driver_go_home_request_daily_usage_limit_reached
  DRIVER_GO_HOME_REQUEST_ALREADY_ACTIVE -> \a -> a @~ driver_go_home_request_already_active
  REPORT_ISSUE -> \a -> a @~ report_issue
  DRIVER_HOME_LOCATION_OUTSIDE_SERVICE_AREA -> \a -> a @~ driver_home_location_outside_service_area
  NEW_LOCATION_TOO_CLOSE_TO_PREVIOUS_HOME_LOCATION -> \a -> a @~ new_location_too_close_to_previous_home_location
  DRIVER_HOME_LOCATION_DOES_NOT_BELONG_TO_DRIVER -> \a -> a @~ driver_home_location_does_not_belong_to_driver
  DRIVER_HOME_LOCATION_DELETE_WHILE_ACTIVE_ERROR -> \a -> a @~ driver_home_location_delete_while_active_error
  DRAG_TO_ADJUST -> \a -> a @~ drag_to_adjust
  LOCATION_ALREADY_EXISTS -> \a -> a @~ location_already_exists
  MIN_LEFT -> \a -> a @~ min_left
  GET_READY_FOR_YS_SUBSCRIPTION arg1 -> \a -> (a @~ get_ready_for_ys_subscription) arg1
  SIGNUP_EARLY_FOR_SPECIAL_OFFERS -> \a -> a @~ signup_early_for_special_offers
  GUARANTEED_FIXED_PRICE arg1 -> \a -> (a @~ guaranteed_fixed_price) arg1
  INTRODUCTORY_OFFER_TO_BE_ANNOUNCED_SOON -> \a -> a @~ introductory_offer_to_be_announced_soon
  NO_CHARGES_TILL -> \a -> a @~ no_charges_till
  DRIVER_GO_HOME_REQUEST_NOT_PRESENT -> \a -> a @~ driver_go_home_request_not_present
  AND -> \a -> a @~ and
  DIRECT_PAYMENT_NO_COMMISSIONS -> \a -> a @~ direct_payment_no_commissions
  CUSTOMER_PAYS_DIRECTLY -> \a -> a @~ customer_pays_directly
  HUNDRED_PERCENT_FARE_GOES_TO_YOU -> \a -> a @~ hundred_percent_fare_goes_to_you
  FARE_SHOWN_IS_FARE_YOU_GET -> \a -> a @~ fare_shown_is_fare_you_get
  BE_A_PART_OF_OPEN_MOBILITY_REVOLUTION -> \a -> a @~ be_a_part_of_open_mobility_revolution
  OUR_DATA_AND_PRODUCT_ARE_TRANSPARENT -> \a -> a @~ our_data_and_product_are_transparent
  YOUR_DETECTED_LOCATION_IS -> \a -> a @~ your_detected_location_is
  LANGUAGE_DETECTED -> \a -> a @~ language_detected
  CHANGE_LANGUAGE_STR -> \a -> a @~ change_language_str
  SELECT_LOCATION -> \a -> a @~ select_location
  SELECT_LOCATION_DESC -> \a -> a @~ select_location_desc
  SELECT_LANGUAGE_DESC -> \a -> a @~ select_language_desc
  CONFIRM_LANGUAGE -> \a -> a @~ confirm_language
  GET_STARTED -> \a -> a @~ get_started
  ENABLE_LOCATION_PERMISSION -> \a -> a @~ enable_location_permission
  PLEASE_ENABLE_LOCATION_PERMISSION_FOR -> \a -> a @~ please_enable_location_permission_for
  ENABLE_LOCATION -> \a -> a @~ enable_location
  BY_CLICKING_NEXT_YOU_WILL_BE_AGREEING_TO_OUR -> \a -> a @~ by_clicking_next_you_will_be_agreeing_to_our
  ENTER_YOUR_MOBILE_NUMBER -> \a -> a @~ enter_your_mobile_number
  NOTIFICATION_ACCESS -> \a -> a @~ notification_access
  NOTIFICATION_ACCESS_DESC -> \a -> a @~ notification_access_desc
  WATCH_VIDEO -> \a -> a @~ watch_video
  DL_VERIFICATION_FAILED -> \a -> a @~ dl_verification_failed
  RC_VERIFICATION_FAILED -> \a -> a @~ rc_verification_failed
  DL_UPLOAD_FAILED -> \a -> a @~ dl_upload_failed
  RC_UPLOAD_FAILED -> \a -> a @~ rc_upload_failed
  PLEASE_RETRY_THE_UPLOAD_AGAIN -> \a -> a @~ please_retry_the_upload_again
  RC_AND_DL_UPLOAD_FAILED -> \a -> a @~ rc_and_dl_upload_failed
  RC_UPLOAD_LIMIT_REACHED -> \a -> a @~ rc_upload_limit_reached
  DL_UPLOAD_LIMIT_REACHED -> \a -> a @~ dl_upload_limit_reached
  RETRY_UPLOAD -> \a -> a @~ retry_upload
  VEHICLE_REGISTERATON_CERTIFICATE -> \a -> a @~ vehicle_registeraton_certificate
  GRANT_PERMISSIONS -> \a -> a @~ grant_permissions
  SUBSCRIPTION_PLAN_STR arg1 -> \a -> (a @~ subscription_plan_str) arg1
  COMPLETE_AUTOPAY_LATER -> \a -> a @~ complete_autopay_later
  START_EARNING_IN_FOUR_STEPS -> \a -> a @~ start_earning_in_four_steps
  COMPLETE -> \a -> a @~ complete
  HOW_TO_UPLOAD -> \a -> a @~ how_to_upload
  TAKE_CLEAR_PICTURE_DL -> \a -> a @~ take_clear_picture_dl
  ENSURE_ADEQUATE_LIGHT -> \a -> a @~ ensure_adequate_light
  FIT_DL_CORRECTLY -> \a -> a @~ fit_dl_correctly
  TAKE_PHOTO -> \a -> a @~ take_photo
  FIT_RC_CORRECTLY -> \a -> a @~ fit_rc_correctly
  TAKE_CLEAR_PICTURE_RC -> \a -> a @~ take_clear_picture_rc
  DL_UPLOADED -> \a -> a @~ dl_uploaded
  RC_UPLOADED -> \a -> a @~ rc_uploaded
  DL_UPLOADING -> \a -> a @~ dl_uploading
  RC_UPLOADING -> \a -> a @~ rc_uploading
  RETAKE_RC -> \a -> a @~ retake_rc
  RETAKE_DL -> \a -> a @~ retake_dl
  CONFIRM_AND_UPLOAD -> \a -> a @~ confirm_and_upload
  RETAKE_PHOTO -> \a -> a @~ retake_photo
  CHANGE_CITY -> \a -> a @~ change_city
  LETS_GET_YOU_TRIP_READY -> \a -> a @~ lets_get_you_trip_ready
  GOT_AN_OTP -> \a -> a @~ got_an_otp
  DRIVING_LICENSE_DETAILS -> \a -> a @~ driving_license_details
  VEHICLE_REGISTRATION_DETAILS -> \a -> a @~ vehicle_registration_details
  UPLOAD_REGISTRATION_CERTIFICATE_STR -> \a -> a @~ upload_registration_certificate_str
  UPLOAD_PHOTO -> \a -> a @~ upload_photo
  CLEAR_IMAGE -> \a -> a @~ clear_image
  BLURRY_IMAGE -> \a -> a @~ blurry_image
  CROPPED_CORRECTLY -> \a -> a @~ cropped_correctly
  WRONG_CROPPING -> \a -> a @~ wrong_cropping
  CHANGE_LOCATION -> \a -> a @~ change_location
  RC_VERIFICATION_IN_PROGRESS -> \a -> a @~ rc_verification_in_progress
  RC_VERIFICATION_FAILED_STATUS -> \a -> a @~ rc_verification_failed_status
  RC_VERIFICATION_SUCCESS -> \a -> a @~ rc_verification_success
  RC_IN_PROGRESS_DESC -> \a -> a @~ rc_in_progress_desc
  RC_FAILED_DESC -> \a -> a @~ rc_failed_desc
  TAKE_A_PHOTO -> \a -> a @~ take_a_photo
  GALLERY -> \a -> a @~ gallery
  UNABLE_TO_DETECT_YOUR_LOCATION -> \a -> a @~ unable_to_detect_your_location
  DETECTING_LOCATION -> \a -> a @~ detecting_location
  GET_FULL_PAYMENT -> \a -> a @~ get_full_payment
  SELECT_CITY_STR -> \a -> a @~ select_city_str
  WE_ARE_NOT_LIVE_IN_YOUR_AREA -> \a -> a @~ we_are_not_live_in_your_area
  LOCATION_UNSERVICEABLE -> \a -> a @~ location_unserviceable
  UNABLE_TO_GET_YOUR_LOCATION -> \a -> a @~ unable_to_get_your_location
  TURN_OFF_ANY_MOCK_LOCATION_APP_AND_RESTART -> \a -> a @~ turn_off_any_mock_location_app_and_restart
  THIS_EXTRA_AMOUNT_THE_CUSTOMER_WILL_PAY -> \a -> a @~ this_extra_amount_the_customer_will_pay
  TEN_DIGIT_MOBILE_NUMBER -> \a -> a @~ ten_digit_mobile_number
  BOOTH_CHARGES -> \a -> a @~ booth_charges
  BOOTH_CHARGES_INCLUDED -> \a -> a @~ booth_charges_included
  TOTAL_AMOUNT -> \a -> a @~ total_amount
  PLEASE_ADD_RC -> \a -> a @~ please_add_rc
  LOCATION_CANNOT_BE_ADDED_WHILE_ON_RIDE -> \a -> a @~ location_cannot_be_added_while_on_ride
  LOCATION_CANNOT_BE_ADDED_WHILE_GOTO_ACTIVE -> \a -> a @~ location_cannot_be_added_while_goto_active
  ADD_GOTO -> \a -> a @~ add_goto
  NO_OPEN_MARKET_RIDES arg1 -> \a -> (a @~ no_open_market_rides) arg1
  ACCOUNT_BLOCKED -> \a -> a @~ account_blocked
  YOU_HAVE_BEEN_BLOCKED_FROM_TAKING_RIDES -> \a -> a @~ you_have_been_blocked_from_taking_rides
  DISMISS -> \a -> a @~ dismiss
  EARNINGS -> \a -> a @~ earnings
  YATRI_POINTS -> \a -> a @~ yatri_points
  DISCOUNT_POINTS -> \a -> a @~ discount_points
  YATRI_POINTS_STR -> \a -> a @~ yatri_points_str
  INTRODUCING_YATRI_POINTS -> \a -> a @~ introducing_yatri_points
  NOW_EARN_POINTS_FOR_EVERY_RIDE_AND_REFERRAL_AND_USE_THEM_TO_GET_REWARDS arg1 -> \a -> (a @~ now_earn_points_for_every_ride_and_referral_and_use_them_to_get_rewards) arg1
  PAID_BY_YATRI_POINTS -> \a -> a @~ paid_by_yatri_points
  DISCOUNT_POINTS_SMALL -> \a -> a @~ discount_points_small
  YATRI_POINTS_USAGE_POPUP arg1 -> \a -> (a @~ yatri_points_usage_popup) arg1
  YATRI_POINTS_USAGE_SECONDARY arg1 arg2 -> \a -> (a @~ yatri_points_usage_secondary) arg1 arg2
  USE_POINTS_NOW -> \a -> a @~ use_points_now
  BUY_NOW -> \a -> a @~ buy_now
  SELECT_DATE -> \a -> a @~ select_date
  WHAT_WILL_MY_POINTS_BE_CONVERTED_TO -> \a -> a @~ what_will_my_points_be_converted_to
  POINTS_EXPIRING_IN_THE_NEXT -> \a -> a @~ points_expiring_in_the_next
  DAYS_USE_THEM_BEFORE_THEY_EXPIRE -> \a -> a @~ days_use_them_before_they_expire
  POINTS_EXPIRING -> \a -> a @~ points_expiring
  NO_POINTS_AVAILABLE -> \a -> a @~ no_points_available
  FAILED_TO_USE_POINTS_PLEASE_TRY_AGAIN_LATER -> \a -> a @~ failed_to_use_points_please_try_again_later
  BAD_RATING_BY_CUSTOMER -> \a -> a @~ bad_rating_by_customer
  GOOD_RATING_BY_CUSTOMER -> \a -> a @~ good_rating_by_customer
  RIDE_CANCELLATION -> \a -> a @~ ride_cancellation
  CUSTOMER_REFERRAL -> \a -> a @~ customer_referral
  CUSTOMER_SHOULD_COMPLETE_A_VALID_RIDE -> \a -> a @~ customer_should_complete_a_valid_ride
  DRIVER_REFERRAL -> \a -> a @~ driver_referral
  PURPLE_RIDE_COMPLETED -> \a -> a @~ purple_ride_completed
  TRAINING_COMPLTED -> \a -> a @~ training_complted
  RIDES_IN_A_DAY -> \a -> a @~ rides_in_a_day
  TOP -> \a -> a @~ top
  IN_WEEKLY_LEADERBOARD -> \a -> a @~ in_weekly_leaderboard
  TRIP_EARNINGS -> \a -> a @~ trip_earnings
  EXTRA_EARNINGS -> \a -> a @~ extra_earnings
  TRIPS -> \a -> a @~ trips
  VIEW_MORE -> \a -> a @~ view_more
  MINIMUM -> \a -> a @~ minimum
  POINTS_IS_REQUIRED_FOR_CONVERSION -> \a -> a @~ points_is_required_for_conversion
  DISCOUNT -> \a -> a @~ discount
  CHECK_NOW -> \a -> a @~ check_now
  YATRI_POINTS_FAQS -> \a -> a @~ yatri_points_faqs
  LEARN_ABOUT_YATRI_POINTS -> \a -> a @~ learn_about_yatri_points
  YATRI_POINTS_FAQS_QUES1 arg1 -> \a -> (a @~ yatri_points_faqs_ques1) arg1
  YATRI_POINTS_FAQS_QUES1_ANS1 arg1 -> \a -> (a @~ yatri_points_faqs_ques1_ans1) arg1
  YATRI_POINTS_FAQS_QUES1_ANS2 arg1 -> \a -> (a @~ yatri_points_faqs_ques1_ans2) arg1
  YATRI_POINTS_FAQS_QUES1_ANS3 arg1 -> \a -> (a @~ yatri_points_faqs_ques1_ans3) arg1
  YATRI_POINTS_FAQS_QUES2 arg1 -> \a -> (a @~ yatri_points_faqs_ques2) arg1
  YATRI_POINTS_FAQS_QUES2_ANS1 arg1 -> \a -> (a @~ yatri_points_faqs_ques2_ans1) arg1
  YATRI_POINTS_FAQS_QUES2_ANS2 arg1 -> \a -> (a @~ yatri_points_faqs_ques2_ans2) arg1
  YATRI_POINTS_FAQS_QUES3 arg1 -> \a -> (a @~ yatri_points_faqs_ques3) arg1
  YATRI_POINTS_FAQS_QUES3_ANS1 arg1 -> \a -> (a @~ yatri_points_faqs_ques3_ans1) arg1
  YATRI_POINTS_FAQS_QUES3_ANS2 arg1 -> \a -> (a @~ yatri_points_faqs_ques3_ans2) arg1
  YATRI_POINTS_FAQS_QUES4 arg1 -> \a -> (a @~ yatri_points_faqs_ques4) arg1
  YATRI_POINTS_FAQS_QUES4_ANS1 arg1 -> \a -> (a @~ yatri_points_faqs_ques4_ans1) arg1
  YATRI_POINTS_FAQS_QUES4_ANS2 arg1 -> \a -> (a @~ yatri_points_faqs_ques4_ans2) arg1
  YATRI_POINTS_FAQS_QUES4_ANS3 arg1 -> \a -> (a @~ yatri_points_faqs_ques4_ans3) arg1
  YATRI_POINTS_FAQS_QUES5 arg1 -> \a -> (a @~ yatri_points_faqs_ques5) arg1
  YATRI_POINTS_FAQS_QUES5_ANS1 arg1 -> \a -> (a @~ yatri_points_faqs_ques5_ans1) arg1
  YATRI_POINTS_FAQS_QUES5_ANS2 arg1 -> \a -> (a @~ yatri_points_faqs_ques5_ans2) arg1
  YATRI_POINTS_FAQS_QUES6 arg1 -> \a -> (a @~ yatri_points_faqs_ques6) arg1
  YATRI_POINTS_FAQS_QUES6_ANS1 arg1 -> \a -> (a @~ yatri_points_faqs_ques6_ans1) arg1
  TASK_COMPLETED -> \a -> a @~ task_completed
  RIDES_IN_A_DAY_PREFIX -> \a -> a @~ rides_in_a_day_prefix
  RIDES_IN_A_DAY_SUFFIX -> \a -> a @~ rides_in_a_day_suffix
  STAR_RATING_FOR_THE_TRIP -> \a -> a @~ star_rating_for_the_trip
  ONE_TWO_START_RATING -> \a -> a @~ one_two_start_rating
  BOOKING_CANCELLATION -> \a -> a @~ booking_cancellation
  PAID_BY -> \a -> a @~ paid_by
  DRIVER_REFERRAL_CODE -> \a -> a @~ driver_referral_code
  APP_QR_CODE -> \a -> a @~ app_qr_code
  START_TAKING_RIDES_AND_REFER arg1 -> \a -> (a @~ start_taking_rides_and_refer) arg1
  REFERRED_DRIVERS -> \a -> a @~ referred_drivers
  RIDE_LEADERBOARD -> \a -> a @~ ride_leaderboard
  YOUR_RANK -> \a -> a @~ your_rank
  NOT_AVAILABLE_YET -> \a -> a @~ not_available_yet
  ENTER_REFERRAL_CODE -> \a -> a @~ enter_referral_code
  HAVE_A_REFERRAL_CODE -> \a -> a @~ have_a_referral_code
  COMPLETE_STEPS_TO_APPLY_REFERRAL -> \a -> a @~ complete_steps_to_apply_referral
  DOWNLOAD_NAMMA_YATRI arg1 -> \a -> (a @~ download_namma_yatri) arg1
  ENTER_CODE -> \a -> a @~ enter_code
  COMPLETE_REGISTRATION -> \a -> a @~ complete_registration
  CANT_FIND_OPTION -> \a -> a @~ cant_find_option
  CONVERT_POINTS -> \a -> a @~ convert_points
  HELP_FAQ -> \a -> a @~ help_faq
  BONUS_POINTS -> \a -> a @~ bonus_points
  MAX -> \a -> a @~ max
  COINS -> \a -> a @~ coins
  POINTS_ADDED -> \a -> a @~ points_added
  WATCH_NOW -> \a -> a @~ watch_now
  CHOOSE_A_PLAN -> \a -> a @~ choose_a_plan
  REFERRAL -> \a -> a @~ referral
  BENEFITS -> \a -> a @~ benefits
  YOUR_DAILY_RANK -> \a -> a @~ your_daily_rank
  CLICK_TO_EXPAND -> \a -> a @~ click_to_expand
  REFERRED -> \a -> a @~ referred
  ACTIVATED -> \a -> a @~ activated
  REFER_DRIVER -> \a -> a @~ refer_driver
  REFER_CUSTOMER -> \a -> a @~ refer_customer
  REFERRED_DRIVERS_INFO arg1 -> \a -> (a @~ referred_drivers_info) arg1
  REFERRED_CUSTOMERS_INFO arg1 -> \a -> (a @~ referred_customers_info) arg1
  ACTIVATED_CUSTOMERS_INFO -> \a -> a @~ activated_customers_info
  CUSTOMER_REFERRAL_CODE -> \a -> a @~ customer_referral_code
  ACCEPT_RIDE_TO_ENTER_LEADERBOARD -> \a -> a @~ accept_ride_to_enter_leaderboard
  CONTACT_SUPPORT_VIA -> \a -> a @~ contact_support_via
  YOU_CAN_SHARE_SCREENSHOT -> \a -> a @~ you_can_share_screenshot
  PLACE_A_CALL -> \a -> a @~ place_a_call
  TERMS_AND_CONDITIONS_UPDATED -> \a -> a @~ terms_and_conditions_updated
  SAFETY_IS_OUR_RESPONSIBILITY -> \a -> a @~ safety_is_our_responsibility
  CUSTOMER_SAFETY_FIRST -> \a -> a @~ customer_safety_first
  LETS_ENSURE_SAFE_RIDE -> \a -> a @~ lets_ensure_safe_ride
  CUSTOMER_SAFETY_OUR_RESP_HAPPY_RIDE -> \a -> a @~ customer_safety_our_resp_happy_ride
  OUR_SAFETY_PARTNER -> \a -> a @~ our_safety_partner
  QUIZ -> \a -> a @~ quiz
  HINDI -> \a -> a @~ hindi
  KANNADA -> \a -> a @~ kannada
  TAMIL -> \a -> a @~ tamil
  TELUGU -> \a -> a @~ telugu
  FRENCH -> \a -> a @~ french
  MALAYALAM -> \a -> a @~ malayalam
  BENGALI -> \a -> a @~ bengali
  ENGLISH -> \a -> a @~ english
  YOU_HAVE_SUCCESSFULLY_COMPLETED arg1 -> \a -> (a @~ you_have_successfully_completed) arg1
  ALL_ANSWERS_SHOULD_BE_CORRECT_TO_COMPLETE arg1 -> \a -> (a @~ all_answers_should_be_correct_to_complete) arg1
  QUESTIONS_SHOULD_BE_CORRECT_TO_COMPLETE -> \a -> a @~ questions_should_be_correct_to_complete
  CORRECT -> \a -> a @~ correct
  RETAKE_QUIZ -> \a -> a @~ retake_quiz
  TAKE_A_QUIZ -> \a -> a @~ take_a_quiz
  PLAY_AGAIN -> \a -> a @~ play_again
  PLAY_NOW -> \a -> a @~ play_now
  WATCH_ALL_VIDEOS_TO_LEARN -> \a -> a @~ watch_all_videos_to_learn
  PLAY_QUIZ_TO_COMPLETE_YOUR_TRAINING -> \a -> a @~ play_quiz_to_complete_your_training
  TRAINING_COMPLETED -> \a -> a @~ training_completed
  WATCHED -> \a -> a @~ watched
  UH_OH_SOMETHING_WENT_WRONG -> \a -> a @~ uh_oh_something_went_wrong
  WATCH_ALL_VIDEOS_TO_UNLOCK_QUIZ -> \a -> a @~ watch_all_videos_to_unlock_quiz
  INCOMPLETE -> \a -> a @~ incomplete
  NEW_C -> \a -> a @~ new_c
  PENDING_STR_C -> \a -> a @~ pending_str_c
  COMPLETED_STR -> \a -> a @~ completed_str
  VIDEOS -> \a -> a @~ videos
  LEARN_AND_EARN -> \a -> a @~ learn_and_earn
  UNABLE_TO_CHANGE_LANGUAGE_PLEASE_TRY_AGAIN -> \a -> a @~ unable_to_change_language_please_try_again
  UNABLE_TO_LOAD_QUIZ_PLEASE_TRY_AGAIN -> \a -> a @~ unable_to_load_quiz_please_try_again
  WE_GUARANTEE_YOU -> \a -> a @~ we_guarantee_you
  LOWEST_FEES_FROM -> \a -> a @~ lowest_fees_from
  ZERO_FEE_TILL -> \a -> a @~ zero_fee_till
  ZERO_COMMISION_UNLIMITED_RIDES -> \a -> a @~ zero_commision_unlimited_rides
  WE_ARE_CURRENTLY_LIVE_WITH_VEHICLE -> \a -> a @~ we_are_currently_live_with_vehicle
  WE_ARE_CURRENTLY_LIVE_WITH_VEHICLE_DESC -> \a -> a @~ we_are_currently_live_with_vehicle_desc
  EXIT_THE_QUIZ -> \a -> a @~ exit_the_quiz
  EXIT_AND_START_AGAIN_LATER -> \a -> a @~ exit_and_start_again_later
  SELECT_THE_LANGUAGE_YOU_CAN_READ -> \a -> a @~ select_the_language_you_can_read
  CHECK_APP -> \a -> a @~ check_app
  CHECK_YOUR_APP_BY_TEST_RIDE_REQUEST -> \a -> a @~ check_your_app_by_test_ride_request
  PLEASE_TRY_THE_FOLLOWING_STEPS -> \a -> a @~ please_try_the_following_steps
  SEEMS_LIKE_THERE_IS_A_PROBLEM -> \a -> a @~ seems_like_there_is_a_problem
  DID_YOU_RECEIVE_TEST_RIDE -> \a -> a @~ did_you_receive_test_ride
  EVERYTHING_IS_OK -> \a -> a @~ everything_is_ok
  CALL_OUR_SUPPORT_TEAM -> \a -> a @~ call_our_support_team
  MOVE_TO_HIGH_DEMAND_AREA -> \a -> a @~ move_to_high_demand_area
  YES -> \a -> a @~ yes
  GET_SUPPORT_ON_WHATSAPP -> \a -> a @~ get_support_on_whatsapp
  KNOW_ABOUT_POINTS -> \a -> a @~ know_about_points
  NOT_ENOUGH_POINTS_DESCRIPTION -> \a -> a @~ not_enough_points_description
  SHARE -> \a -> a @~ share
  SHARE_NAMMA_YATRI arg1 -> \a -> (a @~ share_namma_yatri) arg1
  SHARE_NAMMA_YATRI_MESSAGE -> \a -> a @~ share_namma_yatri_message
  BE_OPEN_CHOOSE_OPEN -> \a -> a @~ be_open_choose_open
  NOW -> \a -> a @~ now
  ADD_VEHICLE -> \a -> a @~ add_vehicle
  SELECT_YOUR_VEHICLE_TYPE -> \a -> a @~ select_your_vehicle_type
  CAR -> \a -> a @~ car
  SPECIAL_PICKUP_ZONE_NEARBY -> \a -> a @~ special_pickup_zone_nearby
  ZONE_PICKUP -> \a -> a @~ zone_pickup
  SPECIAL_PICKUP_ZONE_RIDE -> \a -> a @~ special_pickup_zone_ride
  SPECIAL_PICKUP_ZONE -> \a -> a @~ special_pickup_zone
  SPECIAL_PICKUP_ZONE_POPUP_INFO -> \a -> a @~ special_pickup_zone_popup_info
  INSIDE_SPECIAL_PICKUP_ZONE_POPUP_INFO -> \a -> a @~ inside_special_pickup_zone_popup_info
  SELECT_A_GREEN_AREA_FOR_PRIORITY_RIDES -> \a -> a @~ select_a_green_area_for_priority_rides
  PRIORITY_RIDE_EXPIERENCE -> \a -> a @~ priority_ride_expierence
  DURATION -> \a -> a @~ duration
  RENTAL_FARE -> \a -> a @~ rental_fare
  START_TIME -> \a -> a @~ start_time
  START_ODO_READING -> \a -> a @~ start_odo_reading
  RIDE_START -> \a -> a @~ ride_start
  RIDE_END -> \a -> a @~ ride_end
  RIDE_STARTED_AT -> \a -> a @~ ride_started_at
  RIDE_ENDED_AT -> \a -> a @~ ride_ended_at
  ODOMETER_READING -> \a -> a @~ odometer_reading
  PICKED_UP_AT -> \a -> a @~ picked_up_at
  UPCOMING_STOP -> \a -> a @~ upcoming_stop
  LAST_STOP -> \a -> a @~ last_stop
  PREVIOUS_STOP -> \a -> a @~ previous_stop
  RIDE_TIME -> \a -> a @~ ride_time
  YOU_ARE_ON_A_RENTAL_RIDE -> \a -> a @~ you_are_on_a_rental_ride
  YOU_ARE_ON_A_INTERCITY_RIDE -> \a -> a @~ you_are_on_a_intercity_ride
  ENTER_END_RIDE_OTP -> \a -> a @~ enter_end_ride_otp
  YOU_ARE_NOT_AT_STOP_LOCATION -> \a -> a @~ you_are_not_at_stop_location
  ARRIVED_AT_STOP -> \a -> a @~ arrived_at_stop
  ENABLE_LOC_PERMISSION_TO_GET_RIDES -> \a -> a @~ enable_loc_permission_to_get_rides
  ENABLE_LOC_PER_FROM_SETTINGS -> \a -> a @~ enable_loc_per_from_settings
  ENABLE_PERMISSION_STR -> \a -> a @~ enable_permission_str
  CAPTURE_DOC_DESC_1 -> \a -> a @~ capture_doc_desc_1
  CAPTURE_DOC_DESC_2 -> \a -> a @~ capture_doc_desc_2
  CAPTURE_DOC_DESC_3 -> \a -> a @~ capture_doc_desc_3
  UPLOAD_DOC -> \a -> a @~ upload_doc
  REGISTER_YOUR_CAR -> \a -> a @~ register_your_car
  REGISTER_YOUR_AUTO -> \a -> a @~ register_your_auto
  REGISTER_YOUR_AMBULANCE -> \a -> a @~ register_your_ambulance
  DO_YOU_WANT_TO_CHANGE_VT -> \a -> a @~ do_you_want_to_change_vt
  YES_CHANGE_VEHICLE -> \a -> a @~ yes_change_vehicle
  CHANGE_VEHICLE -> \a -> a @~ change_vehicle
  VEHICLE_TYPE_MISMATCH -> \a -> a @~ vehicle_type_mismatch
  UPLOADED_DOC_DOESNT_MATCH -> \a -> a @~ uploaded_doc_doesnt_match
  CHANGE_VEHICLE_TYPE -> \a -> a @~ change_vehicle_type
  UPLOAD_DIFFERENT_RC -> \a -> a @~ upload_different_rc
  PROFILE_PHOTO_STR -> \a -> a @~ profile_photo_str
  AADHAAR_CARD_STR -> \a -> a @~ aadhaar_card_str
  PAN_CARD_STR -> \a -> a @~ pan_card_str
  VEHICLE_PERMIT_STR -> \a -> a @~ vehicle_permit_str
  FITNESS_CERTIFICATE_STR -> \a -> a @~ fitness_certificate_str
  VEHICLE_INSURANCE_STR -> \a -> a @~ vehicle_insurance_str
  VEHICLE_PUC_STR -> \a -> a @~ vehicle_puc_str
  RC_MANDATORY -> \a -> a @~ rc_mandatory
  DOCUMENT_UPLOADED_SUCCESSFULLY -> \a -> a @~ document_uploaded_successfully
  TOLL_CHARGES_INCLUDING arg1 -> \a -> (a @~ toll_charges_including) arg1
  TOLL_ROAD_CHANGED -> \a -> a @~ toll_road_changed
  RIDE_TOLL_FARE_INCLUDES arg1 -> \a -> (a @~ ride_toll_fare_includes) arg1
  TOLL_INCLUDED -> \a -> a @~ toll_included
  TRIP_TIME -> \a -> a @~ trip_time
  EARNINGS_PER_KM -> \a -> a @~ earnings_per_km
  OPTIONAL_DOCUMENT -> \a -> a @~ optional_document
  EARNINGS_PER_KM_DESC_1 -> \a -> a @~ earnings_per_km_desc_1
  EARNINGS_PER_KM_DESC_2 -> \a -> a @~ earnings_per_km_desc_2
  EARNINGS_P_KM -> \a -> a @~ earnings_p_km
  IS_YOUR_CAR_AC_WORKING -> \a -> a @~ is_your_car_ac_working
  HOW_DOES_AC_CONDITION_AFFECT -> \a -> a @~ how_does_ac_condition_affect
  WE_WILL_USE_THIS_INFO -> \a -> a @~ we_will_use_this_info
  YOU_CAN_ALWAYS_CHANGE_THIS_FROM_PROFILE -> \a -> a @~ you_can_always_change_this_from_profile
  IS_YOUR_CAR_AC_TURNED_ON_AND_WORKING -> \a -> a @~ is_your_car_ac_turned_on_and_working
  SET_THE_AC_ON_TO_ENABLE arg1 -> \a -> (a @~ set_the_ac_on_to_enable) arg1
  VARIANTS_ARE_SWITCHED -> \a -> a @~ variants_are_switched
  NON_AC_ARE_SWITCHED -> \a -> a @~ non_ac_are_switched
  NETWORK_ERROR -> \a -> a @~ network_error
  UNKNOWN_ERROR -> \a -> a @~ unknown_error
  CONNECTION_REFUSED -> \a -> a @~ connection_refused
  TIMEOUT -> \a -> a @~ timeout
  SERVER_ERROR -> \a -> a @~ server_error
  ALL_ELIGIBLE_VARIANTS_ARE_CHOSEN_PLEASE_CHECK -> \a -> a @~ all_eligible_variants_are_chosen_please_check
  RIDE_MORE_AND_EARN_POINTS -> \a -> a @~ ride_more_and_earn_points
  RIDE_MORE_EARN_MORE -> \a -> a @~ ride_more_earn_more
  TAKE_MORE_RIDES_TO_EARN_MORE_POINTS_AND_CONVERT_IT_TO_SUBSCRIPTION_DISCOUNTS -> \a -> a @~ take_more_rides_to_earn_more_points_and_convert_it_to_subscription_discounts
  CHECK_YATRI_POINTS -> \a -> a @~ check_yatri_points
  TWO_MORE_RIDES_TO_GO -> \a -> a @~ two_more_rides_to_go
  ONE_MORE_RIDE_TO_GO -> \a -> a @~ one_more_ride_to_go
  TAKE_ONE_MORE_RIDE_TO_EARN_POINTS arg1 -> \a -> (a @~ take_one_more_ride_to_earn_points) arg1
  TAKE_TWO_MORE_RIDES_TO_EARN_POINTS arg1 -> \a -> (a @~ take_two_more_rides_to_earn_points) arg1
  CONGRATULATIONS -> \a -> a @~ congratulations
  YOU_HAVE_EARNED_POINTS_FOR_COMPLETING_EIGHT_RIDES arg1 -> \a -> (a @~ you_have_earned_points_for_completing_eight_rides) arg1
  REFER_NAMMA_YATRI_APP_TO_CUSTOMERS_AND_EARN_POINTS arg1 -> \a -> (a @~ refer_namma_yatri_app_to_customers_and_earn_points) arg1
  REFER_NOW -> \a -> a @~ refer_now
  CONVERT_YOUR_POINTS_TO_DISCOUNT -> \a -> a @~ convert_your_points_to_discount
  CONVERT_YOUR_POINTS_TO_GET_DISCOUNT_ON_YOUR_SUBSCRIPTION -> \a -> a @~ convert_your_points_to_get_discount_on_your_subscription
  CONVERT_NOW -> \a -> a @~ convert_now
  MORE_RIDES -> \a -> a @~ more_rides
  SORT_BY -> \a -> a @~ sort_by
  ACCEPT -> \a -> a @~ accept
  PASS -> \a -> a @~ pass
  TERM_1A -> \a -> a @~ term_1a
  TERM_2A -> \a -> a @~ term_2a
  TERM_3A -> \a -> a @~ term_3a
  TERM_1B arg1 -> \a -> (a @~ term_1b) arg1
  TERM_2B arg1 -> \a -> (a @~ term_2b) arg1
  TERM_3B arg1 arg2 -> \a -> (a @~ term_3b) arg1 arg2
  EXCLUDED_CHARGES -> \a -> a @~ excluded_charges
  TOLLS -> \a -> a @~ tolls
  STATE_PERMIT -> \a -> a @~ state_permit
  EXCLUDED_FOOTER -> \a -> a @~ excluded_footer
  INCLUDED_CHARGES -> \a -> a @~ included_charges
  INC_1 -> \a -> a @~ inc_1
  INC_2A arg1 arg2 arg3 -> \a -> (a @~ inc_2a) arg1 arg2 arg3
  INC_2B arg1 arg2 -> \a -> (a @~ inc_2b) arg1 arg2
  PICKUP_DROP -> \a -> a @~ pickup_drop
  TOLL_CHARGES_INCLUDED -> \a -> a @~ toll_charges_included
  PLEASE_DO_NOT_DEMAND_EXTRA -> \a -> a @~ please_do_not_demand_extra
  FINAL_FARE_EXCLUDES_TOLL -> \a -> a @~ final_fare_excludes_toll
  PLEASE_COLLECT_SEPARATELY -> \a -> a @~ please_collect_separately
  TOLL_CHARGES_MAYBE_APPLICABLE -> \a -> a @~ toll_charges_maybe_applicable
  YOU_ARE_ALL_SET_TO_TAKE_RIDES arg1 -> \a -> (a @~ you_are_all_set_to_take_rides) arg1
  TOP_AC_DRIVER arg1 -> \a -> (a @~ top_ac_driver) arg1
  GO_TO_ADVANCED_RIDE -> \a -> a @~ go_to_advanced_ride
  GET_ADVANCED_RIDE -> \a -> a @~ get_advanced_ride
  ADVANCED_RIDE_POPUP_TITLE -> \a -> a @~ advanced_ride_popup_title
  ADVANCE -> \a -> a @~ advance
  CURRENT_BUTTON_TEXT -> \a -> a @~ current_button_text
  ADVANCE_BOOKING -> \a -> a @~ advance_booking
  FEATURE_UPDATE -> \a -> a @~ feature_update
  THIRD_PARTY_BOOKING -> \a -> a @~ third_party_booking
  SOME_FEATURE_ARE_NOT_AVAILABLE_WITH_THIS_PROVIDER arg1 -> \a -> (a @~ some_feature_are_not_available_with_this_provider) arg1
  GUARANTEED_RIDE -> \a -> a @~ guaranteed_ride
  CUSTOMER_CALLING_AND_MESSAGING -> \a -> a @~ customer_calling_and_messaging
  WAITING_CHARGES -> \a -> a @~ waiting_charges
  CUSTOMER_TIPS -> \a -> a @~ customer_tips
  CANCELLATION_CHARGES -> \a -> a @~ cancellation_charges
  MERCHANT_POINTS arg1 -> \a -> (a @~ merchant_points) arg1
  MERCHANT_NAME arg1 -> \a -> (a @~ merchant_name) arg1
  THIRD_PARTY_RIDES -> \a -> a @~ third_party_rides
  THIRD_PARTY_RIDES_ARE_REQUESTED_WITH_BY_A_USERS_FROM_ANOTHER_APP -> \a -> a @~ third_party_rides_are_requested_with_by_a_users_from_another_app
  SOME_FEATURES_MAY_NOT_BE_AVAILABLE -> \a -> a @~ some_features_may_not_be_available
  WHY -> \a -> a @~ why
  SOME_FEATURES_ARE_NOT_AVAILABLE_FOR_THIRD_PARTY_RIDES -> \a -> a @~ some_features_are_not_available_for_third_party_rides
  BOOKING_FROM arg1 -> \a -> (a @~ booking_from) arg1
  PICK_UP -> \a -> a @~ pick_up
  RATE_CARD -> \a -> a @~ rate_card
  TOLL_CHARGES -> \a -> a @~ toll_charges
  TOLL_CHARGES_DESC -> \a -> a @~ toll_charges_desc
  PARKING_CHARGE -> \a -> a @~ parking_charge
  FARE_FOR arg1 -> \a -> (a @~ fare_for) arg1
  WAITING_CHARGE_LIMIT arg1 -> \a -> (a @~ waiting_charge_limit) arg1
  PARKING_CHARGES_DESC -> \a -> a @~ parking_charges_desc
  TIP_CAN_BE_ADDED arg1 -> \a -> (a @~ tip_can_be_added) arg1
  DAY_TIME_CHARGES arg1 arg2 -> \a -> (a @~ day_time_charges) arg1 arg2
  CONGESTION_CHARGES_DESC arg1 -> \a -> (a @~ congestion_charges_desc) arg1
  TOLL_OR_PARKING_CHARGES -> \a -> a @~ toll_or_parking_charges
  TOLL_CHARGES_ESTIMATED -> \a -> a @~ toll_charges_estimated
  CONGESTION_CHARGES -> \a -> a @~ congestion_charges
  PICKUP_CHARGE -> \a -> a @~ pickup_charge
  NIGHT_TIME_CHARGES arg1 arg2 -> \a -> (a @~ night_time_charges) arg1 arg2
  MIN_FARE_UPTO arg1 -> \a -> (a @~ min_fare_upto) arg1
  MORE_THAN -> \a -> a @~ more_than
  RATE_ABOVE_MIN_FARE -> \a -> a @~ rate_above_min_fare
  DRIVER_PICKUP_CHARGES arg1 -> \a -> (a @~ driver_pickup_charges) arg1
  DAYTIME_CHARGES_APPLICABLE_AT_NIGHT arg1 arg2 -> \a -> (a @~ daytime_charges_applicable_at_night) arg1 arg2
  DAYTIME_CHARGES_APPLIED_AT_NIGHT arg1 arg2 arg3 -> \a -> (a @~ daytime_charges_applied_at_night) arg1 arg2 arg3
  TOTAL_FARE_MAY_CHANGE_DUE_TO_CHANGE_IN_ROUTE -> \a -> a @~ total_fare_may_change_due_to_change_in_route
  DRIVER_ADDITIONS -> \a -> a @~ driver_additions
  FARE_UPDATE_POLICY -> \a -> a @~ fare_update_policy
  DRIVER_ADDITIONS_OPTIONAL -> \a -> a @~ driver_additions_optional
  THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC -> \a -> a @~ the_driver_may_quote_extra_to_cover_for_traffic
  DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE -> \a -> a @~ driver_may_not_charge_this_additional_fare
  HIGHEST_EARNING_PEAK_TIME -> \a -> a @~ highest_earning_peak_time
  CHOOSE_RIDE_DIST -> \a -> a @~ choose_ride_dist
  RATES_CHANGE_AS_THE_DIST -> \a -> a @~ rates_change_as_the_dist
  VIEW_BOOKING_PREF -> \a -> a @~ view_booking_pref
  LIMITED_TIME_OFFER_UNTIL arg1 -> \a -> (a @~ limited_time_offer_until) arg1
  REGISTER_YOUR_BIKE -> \a -> a @~ register_your_bike
  BIKE_TAXI -> \a -> a @~ bike_taxi
  SELECT_FACILITIES -> \a -> a @~ select_facilities
  FIRST_AID_KIT -> \a -> a @~ first_aid_kit
  DRIVER_ACKNOWLEDGE -> \a -> a @~ driver_acknowledge
  BOOKING_PREFERENCE -> \a -> a @~ booking_preference
  INSPECTION -> \a -> a @~ inspection
  A_F -> \a -> a @~ a_f
  BY_PROCEEDING_YOU_ACCEPT_FULL_RESPONSIBILITY -> \a -> a @~ by_proceeding_you_accept_full_responsibility
  A_C -> \a -> a @~ a_c
  AMBULANCE -> \a -> a @~ ambulance
  NON_AC -> \a -> a @~ non_ac
  AC -> \a -> a @~ ac
  NO_OXYGEN -> \a -> a @~ no_oxygen
  OXYGEN -> \a -> a @~ oxygen
  VENTILATOR -> \a -> a @~ ventilator
  SELECT_ONE -> \a -> a @~ select_one
  FIRST_RIDE_FREE arg1 -> \a -> (a @~ first_ride_free) arg1
  FIRST_RIDES_FREE arg1 -> \a -> (a @~ first_rides_free) arg1
  ADDITIONAL_CHARGES_WILL_BE_APPLICABLE -> \a -> a @~ additional_charges_will_be_applicable
  REFERRAL_FIRST_RIDE_DESCRIPTION arg1 -> \a -> (a @~ referral_first_ride_description) arg1
  STEPS -> \a -> a @~ steps
  CUSTOMER_COMPLETED_FIRST_RIDE -> \a -> a @~ customer_completed_first_ride
  VERIFYING -> \a -> a @~ verifying
  PROCESSING -> \a -> a @~ processing
  PAYMENT_CREDITED -> \a -> a @~ payment_credited
  REFERRAL_BONUS -> \a -> a @~ referral_bonus
  REFERRAL_BONUS_TRACKER -> \a -> a @~ referral_bonus_tracker
  UPI_DETAILS -> \a -> a @~ upi_details
  CUSTOMER_REFERRAL_TRACKER -> \a -> a @~ customer_referral_tracker
  PAY_TO_ADD_UPI arg1 -> \a -> (a @~ pay_to_add_upi) arg1
  ADD_UPI_TO_RECEIVE_REWARD -> \a -> a @~ add_upi_to_receive_reward
  EARN_FOR_EACH_REFERRAL arg1 -> \a -> (a @~ earn_for_each_referral) arg1
  START_REFERRING_NOW -> \a -> a @~ start_referring_now
  WILL_GET_REFERRAL_TO_UPI_ID -> \a -> a @~ will_get_referral_to_upi_id
  DELETE_UPI_ID -> \a -> a @~ delete_upi_id
  CONFIRM_DELETE_UPI_ID arg1 -> \a -> (a @~ confirm_delete_upi_id) arg1
  PAYOUT_HISTORY -> \a -> a @~ payout_history
  HOW_TO_EARN -> \a -> a @~ how_to_earn
  RECEIVED arg1 -> \a -> (a @~ received) arg1
  CREDITED_ON arg1 -> \a -> (a @~ credited_on) arg1
  REFERRAL_BONUS_EARNED -> \a -> a @~ referral_bonus_earned
  NO_ACTIVATED_REFERRAL -> \a -> a @~ no_activated_referral
  NO_ACTIVE_REFERRAL_ON_DATE -> \a -> a @~ no_active_referral_on_date
  PAYMENT_IN_PROGRESS -> \a -> a @~ payment_in_progress
  REFRESH_PAYMENT -> \a -> a @~ refresh_payment
  BY -> \a -> a @~ by
  CUSTOMERS -> \a -> a @~ customers
  CUSTOMER -> \a -> a @~ customer
  RATING -> \a -> a @~ rating
  CANCELLATION -> \a -> a @~ cancellation
  I_SPEAK -> \a -> a @~ i_speak
  WITH_NAMMAYATRI_FOR arg1 -> \a -> (a @~ with_nammayatri_for) arg1
  YEARS -> \a -> a @~ years
  VEHICLE_NUMBER -> \a -> a @~ vehicle_number
  WHAT_PEOPLE_SAY -> \a -> a @~ what_people_say
  STAR_RATING -> \a -> a @~ star_rating
  CARD_TEXTS -> \a -> a @~ card_texts
  TRAININGS_I_COMPLETED -> \a -> a @~ trainings_i_completed
  I_PLEDGE -> \a -> a @~ i_pledge
  ONLY_5_MORE_RIDES_FOR_N_POINTS arg1 -> \a -> (a @~ only_5_more_rides_for_n_points) arg1
  ONLY_3_MORE_RIDES_FOR_N_POINTS arg1 -> \a -> (a @~ only_3_more_rides_for_n_points) arg1
  ONLY_4_MORE_RIDES_FOR_N_POINTS arg1 -> \a -> (a @~ only_4_more_rides_for_n_points) arg1
  YOU_GOT_N_POINTS arg1 -> \a -> (a @~ you_got_n_points) arg1
  DISCOUNTED -> \a -> a @~ discounted
  YATRI_POINTS_FAQS_QUES1_ANS4 arg1 -> \a -> (a @~ yatri_points_faqs_ques1_ans4) arg1
  YATRI_POINTS_TNC -> \a -> a @~ yatri_points_tnc
  YATRI_POINTS_FAQS_QUES2_ANS3 -> \a -> a @~ yatri_points_faqs_ques2_ans3
  HOTSPOTS -> \a -> a @~ hotspots
  VERY_HIGH -> \a -> a @~ very_high
  HIGH -> \a -> a @~ high
  VERY_HIGH_DEMAND_AREA -> \a -> a @~ very_high_demand_area
  HIGH_DEMAND_AREA -> \a -> a @~ high_demand_area
  MODERATE -> \a -> a @~ moderate
  AVERAGE_DEMAND_AREA -> \a -> a @~ average_demand_area
  THIS_AREA_IS_EXPERIENCING_AVERAGE_SEARCHES -> \a -> a @~ this_area_is_experiencing_average_searches
  THIS_AREA_IS_EXPERIENCING_VERY_HIGH_SEARCHES -> \a -> a @~ this_area_is_experiencing_very_high_searches
  THIS_AREA_IS_EXPERIENCING_HIGH_SEARCHES -> \a -> a @~ this_area_is_experiencing_high_searches
  NAVIGATE -> \a -> a @~ navigate
  HOTSPOTS_NOT_AVAILABLE_CURRENTLY -> \a -> a @~ hotspots_not_available_currently
  GST_WITH_PERCENTAGE arg1 -> \a -> (a @~ gst_with_percentage) arg1
  DISCOUNT_POINTS_UPTO arg1 -> \a -> (a @~ discount_points_upto) arg1
  CANNOT_DETECT_PAN_CARD -> \a -> a @~ cannot_detect_pan_card
  CANNOT_DETECT_AADHAAR -> \a -> a @~ cannot_detect_aadhaar
  DOCUMENT_ALREADY_VALIDATED -> \a -> a @~ document_already_validated
  DOCUMENT_UNDER_MANUAL_REVIEW -> \a -> a @~ document_under_manual_review
  DOCUMENT_ALREADY_LINKED_TO_ANOTHER_DRIVER -> \a -> a @~ document_already_linked_to_another_driver
  PAN_ALREADY_LINKED -> \a -> a @~ pan_already_linked
  EXITED_BY_USER -> \a -> a @~ exited_by_user
  APP_UPDATE -> \a -> a @~ app_update
  APP_UPDATE_MESSAGE -> \a -> a @~ app_update_message
  AADHAAR_FRONT_NOT_DETECTED -> \a -> a @~ aadhaar_front_not_detected
  AADHAAR_BACK_NOT_DETECTED -> \a -> a @~ aadhaar_back_not_detected
  UNABLE_TO_EXTRACT_NAME -> \a -> a @~ unable_to_extract_name
  UNABLE_TO_EXTRACT_DOB -> \a -> a @~ unable_to_extract_dob
  UNABLE_TO_EXTRACT_ID -> \a -> a @~ unable_to_extract_id
  IMAGE_B_W -> \a -> a @~ image_b_w
  PARTIAL_DOC_DETECTED -> \a -> a @~ partial_doc_detected
  DOC_IS_BLURRED -> \a -> a @~ doc_is_blurred
  FACE_MATCH_FAILED -> \a -> a @~ face_match_failed
  PAN_NOT_DETECTED -> \a -> a @~ pan_not_detected
  UNABLE_TO_VERIFY_SELFIE -> \a -> a @~ unable_to_verify_selfie
  BLURRED_SELFIE -> \a -> a @~ blurred_selfie
  EYES_CLOSED_SELFIE -> \a -> a @~ eyes_closed_selfie
  MULTIPLE_FACES_IN_SELFIE -> \a -> a @~ multiple_faces_in_selfie
  FACE_BLOCKED -> \a -> a @~ face_blocked
  REMOVE_EYEWERE -> \a -> a @~ remove_eyewere
  IMAGE_VALIDATION_EXCEED_LIMIT -> \a -> a @~ image_validation_exceed_limit
  PARKING_CHARGES_INCLUDED arg1 -> \a -> (a @~ parking_charges_included) arg1
  INCLUDED -> \a -> a @~ included
  DB_CHECK_AND_NAME_MATCH_FAILED -> \a -> a @~ db_check_and_name_match_failed
  COMPLETE_YOUR_PROFILE -> \a -> a @~ complete_your_profile
  ADD_PHOTOS -> \a -> a @~ add_photos
  ADD_UPTO_FOUR -> \a -> a @~ add_upto_four
  CARD_TEXT -> \a -> a @~ card_text
  PLEDGE -> \a -> a @~ pledge
  SAFE_JOURNEY -> \a -> a @~ safe_journey
  CLEAN_CAR -> \a -> a @~ clean_car
  ON_TIME_PICK_UP -> \a -> a @~ on_time_pick_up
  MAINTENANCE -> \a -> a @~ maintenance
  VEHICLE_OFFER -> \a -> a @~ vehicle_offer
  GAS -> \a -> a @~ gas
  RADIO -> \a -> a @~ radio
  ECO_FRIENDLY -> \a -> a @~ eco_friendly
  DEVICE_CHARGING -> \a -> a @~ device_charging
  BOOT_SPACE -> \a -> a @~ boot_space
  PET_FRIENDLY -> \a -> a @~ pet_friendly
  HOMETOWN -> \a -> a @~ hometown
  WHY_NY arg1-> \a -> (a @~ why_ny) arg1
  CAB -> \a -> a @~ cab
  NEW_HOME -> \a -> a @~ new_home
  KID_EDUCATION -> \a -> a @~ kid_education
  NEW_VEHICLE -> \a -> a @~ new_vehicle
  ADD_YOUR_PHOTOS -> \a -> a @~ add_your_photos
  ADD_PHOTO_CAPTION -> \a -> a @~ add_photo_caption
  COMPLETE_PROFILE -> \a -> a @~ complete_profile
  COMPLETE_PROFILE_MSG -> \a -> a @~ complete_profile_msg
  EDIT_PROFILE -> \a -> a @~ edit_profile
  SAVE -> \a -> a @~ save
  MANAGE_VEHICLE -> \a -> a @~ manage_vehicle
  IS_NOT_SUPPORTED_YET -> \a -> a @~ is_not_supported_yet
  WE_WILL_NOFITY_YOU_WHEN_IT_IS_AVAILABLE arg1 -> \a -> (a @~ we_will_nofity_you_when_it_is_available) arg1
  ADD_UPI_TO_RECEIVE_REFERRAL_REWARD -> \a -> a @~ add_upi_to_receive_referral_reward
  DO_YOU_WANT_TO_RECEIVE_AMOUNT_HERE -> \a -> a @~ do_you_want_to_receive_amount_here
  YES_PAY_TO_THIS_ACCOUNT -> \a -> a @~ yes_pay_to_this_account
  I_WILL_ADD_DIFFERENT_ACCOUNT -> \a -> a @~ i_will_add_different_account
  ADD_NOW -> \a -> a @~ add_now
  RECORDING_AUDIO -> \a -> a @~ recording_audio
  RECORDED_AUDIO -> \a -> a @~ recorded_audio
  SHARE_WITH_SAFETY_TEAM -> \a -> a @~ share_with_safety_team
  RECORD_AUDIO -> \a -> a @~ record_audio
  CANNOT_ENABLE_GO_HOME_FOR_DIFFERENT_CITY -> \a -> a @~ cannot_enable_go_home_for_different_city
  RIDE_CANCELLATION_RATE -> \a -> a @~ ride_cancellation_rate
  CANCELLATION_RATE_TRIVIA -> \a -> a @~ cancellation_rate_trivia
  HIGH_CANCELLATION_RATE -> \a -> a @~ high_cancellation_rate
  LAST_N_DAYS arg1 -> \a -> (a @~ last_n_days) arg1
  CANCELLATION_RATE_TRIVIA_2 -> \a -> a @~ cancellation_rate_trivia_2
  LIFETIME_STATS -> \a -> a @~ lifetime_stats
  TOTAL_RIDES_CANCELLED -> \a -> a @~ total_rides_cancelled
  RENTAL_RIDE -> \a -> a @~ rental_ride
  TOTAL_EARNINGS_MISSED -> \a -> a @~ total_earnings_missed
  MORE_ABOUT_ME -> \a -> a @~ more_about_me
  DRIVING_SINCE -> \a -> a @~ driving_since
  ERROR_OCCURED_TRY_AGAIN -> \a -> a @~ error_occured_try_again
  THERE_MIGHT_BE_MULTIPLE_STOPS_IN_THIS_RENTAL_RIDE arg1 -> \a -> (a @~ there_might_be_multiple_stops_in_this_rental_ride) arg1
  RENTAL_RIDE_ACCEPTED -> \a -> a @~ rental_ride_accepted
  MY_REFERRAL_BONUS -> \a -> a @~ my_referral_bonus
  ADD_UPI_ID -> \a -> a @~ add_upi_id
  LINKED_UPI_ID -> \a -> a @~ linked_upi_id
  TO_GET_MONEY -> \a -> a @~ to_get_money
  TILL arg1 -> \a -> (a @~ till) arg1
  REFERRAL_BONUS_WILL_BE_CREDITED_TO_BANK -> \a -> a @~ referral_bonus_will_be_credited_to_bank
  EXPERT_DRIVING -> \a -> a @~ expert_driving
  CLEAN_VEHICLE -> \a -> a @~ clean_vehicle
  SKILLED_NAVIGATOR -> \a -> a @~ skilled_navigator
  SAFE_RIDE -> \a -> a @~ safe_ride
  POLITE_DRIVER -> \a -> a @~ polite_driver
  ON_TIME -> \a -> a @~ on_time
  AC_NOT_TURNED_ON -> \a -> a @~ ac_not_turned_on
  LATE_PICK_UP_ARRIVAL -> \a -> a @~ late_pick_up_arrival
  ASKED_FOR_MORE_FARE -> \a -> a @~ asked_for_more_fare
  UNHYGIENIC_VEHICLE -> \a -> a @~ unhygienic_vehicle
  RASH_DRIVING -> \a -> a @~ rash_driving
  RUDE_DRIVER -> \a -> a @~ rude_driver
  TRAINING -> \a -> a @~ training
  FINANCIAL -> \a -> a @~ financial
  SAFETY -> \a -> a @~ safety
  KIDS_EDUCATION -> \a -> a @~ kids_education
  BUY_NEW_VEHICLE -> \a -> a @~ buy_new_vehicle
  NOT_AVAILABLE -> \a -> a @~ not_available
  PLEASE_WRITE_SOMETHING -> \a -> a @~ please_write_something
  BUY_NEW_HOME -> \a -> a @~ buy_new_home
  FAVOURITES -> \a -> a @~ favourites
  POINTS_EARNED_ arg1 -> \a -> (a @~ points_earned_) arg1
  FOR_METRO_PICKUP_RIDE -> \a -> a @~ for_metro_pickup_ride
  FOR_METRO_DROP_RIDE -> \a -> a @~ for_metro_drop_ride
  CONTINUE_WITH arg1 -> \a -> (a @~ continue_with) arg1
  CONTACT_SUPPORT_FOR_HELP -> \a -> a @~ contact_support_for_help
  YOU_HAVE_SWITCHED_CITY_OR_VEHICLE -> \a -> a @~ you_have_switched_city_or_vehicle
  XL_PLUS -> \a -> a @~ xl_plus
  RIDE_REQUESTS -> \a -> a @~ ride_requests
  SCHEDULED_RIDE_ACCEPTED -> \a -> a @~ scheduled_ride_accepted
  YOU_CAN_ACCESS_SCHEDULED_RIDES -> \a -> a @~ you_can_access_scheduled_rides
  FROM_YOUR_HOMESCREEN -> \a -> a @~ from_your_homescreen
  CURRENTLY_THERE_ARE_NO_RIDES_AVAILABLE -> \a -> a @~ currently_there_are_no_rides_available
  DUE_TO_HIGHER_CANCELLATION_RATE_YOU_ARE_BLOCKED -> \a -> a @~ due_to_higher_cancellation_rate_you_are_blocked
  BLOCKED_TILL arg1 arg2 -> \a -> (a @~ blocked_till) arg1 arg2
  CANCEL_BOOKING -> \a -> a @~ cancel_booking
  GO_TO_PICKUP -> \a -> a @~ go_to_pickup
  RIDE_SCHEDULED -> \a -> a @~ ride_scheduled
  PLEASE_BE_ONLINE arg1 -> \a -> (a @~ please_be_online) arg1
  BEFORE_THE_RIDE_STARTS -> \a -> a @~ before_the_ride_starts
  TRIP_WILL_BE_ASSIGNED_TO_ANOTHER_DRIVER -> \a -> a @~ trip_will_be_assigned_to_another_driver
  RIDE_SUMMARY -> \a -> a @~ ride_summary
  RIDE_ASSIGNED_TO_ANOTHER_DRIVER -> \a -> a @~ ride_assigned_to_another_driver
  YOU_CAN_SEE_OTHER_AVAILABLE_RIDE_IN_MORE_RIDES_SECTION -> \a -> a @~ you_can_see_other_available_ride_in_more_rides_section
  ROUND_TRIP -> \a -> a @~ round_trip
  UPCOMING -> \a -> a @~ upcoming
  FOLLOW_INSTRUCTIONS_TO_AVOID_REASSIGNMENT_OF_RIDE -> \a -> a @~ follow_instructions_to_avoid_reassignment_of_ride
  BE_WITHIN_10KM_OF_PICKUP -> \a -> a @~ be_within_10km_of_pickup
  PLEASE_COLLECT_PARKING_CHARGES -> \a -> a @~ please_collect_parking_charges
  INCURRED_DURING_TRIP -> \a -> a @~ incurred_during_trip
  BACK -> \a -> a @~ back
  YOUR_RIDE_STARTS_IN -> \a -> a @~ your_ride_starts_in
  AWAY -> \a -> a @~ away
  INTERCITY -> \a -> a @~ intercity
  LOCAL -> \a -> a @~ local
  INTERCITY_RETURN -> \a -> a @~ intercity_return
  RENTAL -> \a -> a @~ rental
  REGULAR -> \a -> a @~ regular
  UPCOMING_RIDE -> \a -> a @~ upcoming_ride
  ALL -> \a -> a @~ all
  TOMORROW -> \a -> a @~ tomorrow
  WE_ARE_NOT_ABLE_TO_FETCH_YOUR_CURRENT_LOCATION -> \a -> a @~ we_are_not_able_to_fetch_your_current_location
  YOU_HAVE_AN_UPCOMING -> \a -> a @~ you_have_an_upcoming
  BOOKING -> \a -> a @~ booking
  INTERCITY_RIDE_ACCEPTED -> \a -> a @~ intercity_ride_accepted
  INTERCITY_RIDE -> \a -> a @~ intercity_ride
  PLEASE_ENSURE_THAT_YOUR_VEHICLE_IS_READY_FOR_INTERCITY_TRIP -> \a -> a @~ please_ensure_that_your_vehicle_is_ready_for_intercity_trip
  PER_KM_CHARGE -> \a -> a @~ per_km_charge
  EXTRA_TIME_CHARGE -> \a -> a @~ extra_time_charge
  ADDED_AT_END_OF_TRIP -> \a -> a @~ added_at_end_of_trip
  DRIVER_ALLOWANCE -> \a -> a @~ driver_allowance
  ADD_ON_KM_CHARGE -> \a -> a @~ add_on_km_charge
  EXTRA_DISTANCE_CHARGES -> \a -> a @~ extra_distance_charges
  BASE_CHARGE arg1 -> \a -> (a @~ base_charge) arg1
  THE_CUSTOMER_WILL_PAY_POST_SCHEDULED_RIDE_START_TIME arg1 -> \a -> (a @~ the_customer_will_pay_post_scheduled_ride_start_time) arg1
  CLEAN_AUTO -> \a -> a @~ clean_auto
  CLEAN_CAB -> \a -> a @~ clean_cab
  METRO_RIDE_COMPLETED -> \a -> a @~ metro_ride_completed
  OR_RIDE_IS_CANCELLED_BY_CUSTOMER -> \a -> a @~ or_ride_is_cancelled_by_customer
  THE_RIDE_STARTS -> \a -> a @~ the_ride_starts
  GOOD_SERVICES -> \a -> a @~ good_services
  SMOOTH_DRIVING -> \a -> a @~ smooth_driving
  NO_CANCELLATION -> \a -> a @~ no_cancellation
  FREE_TRIAL_ENDING_IN_N_DAYS arg1 -> \a -> (a @~ free_trial_ending_in_n_days) arg1
  N_MORE_FREE_RIDES arg1 -> \a -> (a @~ n_more_free_rides_left) arg1
  N_FREE_RIDES_COMPLETED arg1 -> \a -> (a @~ n_free_rides_completed) arg1
  COLLECT_CASH_AT_DROP -> \a -> a @~ collect_cash_at_drop
  MORE_DETAILS -> \a -> a @~ more_details
  TAKE_PHOTO_OF_PARCEL -> \a -> a @~ take_photo_of_parcel
  SENDER_WILL_VERIFY_PARCEL -> \a -> a @~ sender_will_verify_parcel
  CALL_CUSTOMER_TEXT -> \a -> a @~ call_customer_text
  CALL_SENDER -> \a -> a @~ call_sender
  CALL_RECEIVER -> \a -> a @~ call_receiver
  START' -> \a -> a @~ start
  END' -> \a -> a @~ end
  DELIVERY_BIKE_SERVICE_TIER_DESC -> \a -> a @~ delivery_bike_service_tier_desc
  RATE_YOUR_DELIVERY_WITH -> \a -> a @~ rate_your_delivery_with
  DELIVERY_DETAILS -> \a -> a @~ delivery_details
  TAKE_CLEAR_PICTURE_PARCEL -> \a -> a @~ take_clear_picture_parcel
  ENSURE_ADEQUATE_LIGHT_PARCEL_DESC -> \a -> a @~ ensure_adequate_light_parcel_desc
  FIT_PARCEL_CORRECTLY -> \a -> a @~ fit_parcel_correctly
  CORRECT_POSITIONING -> \a -> a @~ correct_positioning
  INCORRECT_POSITIONING -> \a -> a @~ incorrect_positioning
  UPLOAD_PARCEL_IMAGE -> \a -> a @~ upload_parcel_image
  PICKUP_INSTRUCTION -> \a -> a @~ pickup_instruction
  DROP_INSTRUCTION -> \a -> a @~ drop_instruction
  TRUCK -> \a -> a @~ truck
  REGISTER_YOUR_TRUCK -> \a -> a @~ register_your_truck
  METRO_WARRIOR_MODE -> \a -> a @~ metro_warrior_mode
  CHOOSE_METRO_STATION -> \a -> a @~ choose_metro_station
  PRIMARY_METRO_STATION -> \a -> a @~ primary_metro_station
  PRIMARY_STATION_INFO -> \a -> a @~ primary_station_info
  NEARBY_STATIONS -> \a -> a @~ nearby_stations
  NEARBY_STATION_INFO -> \a -> a @~ nearby_station_info
  CHANGE -> \a -> a @~ change
  DISABLE_METRO_WARRIORS_INFO -> \a -> a @~ disable_metro_warriors_info
  CHOOSE_PREFERRED_METRO -> \a -> a @~ choose_preferred_metro
  METRO_WARRIORS -> \a -> a @~ metro_warriors
  SEARCH -> \a -> a @~ search
  BUS__ -> \a -> a @~ bus__
  DRIVER_UNSUBSCRIBED -> \a -> a @~ driver_unsubscribed
  RESUME_RIDE -> \a -> a @~ resume_ride
  END_RIDE_WITH_STOPS -> \a -> a @~ end_ride_with_stops
  STOP arg1 -> \a -> (a @~ stop) arg1
  
